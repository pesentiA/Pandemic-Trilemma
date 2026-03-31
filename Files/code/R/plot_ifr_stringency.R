# =============================================================================
#  plot_ifr_stringency.R
#  Produces:
#    1. fig_theta_excess_stringency_v2.pdf  (existing 3-series plot)
#    2. fig_ifr_proxy_stringency_v2.pdf     (NEW: excess-death-to-case ratio + S)
#
#  Standalone — loads datafordescriptives.RData directly
# =============================================================================

rm(list = ls())

# --- Packages ----------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(data.table)
  library(tidyr)
  library(knitr)
})

conflicted::conflict_prefer("select",    "dplyr")
conflicted::conflict_prefer("filter",    "dplyr")
conflicted::conflicts_prefer(dplyr::lag)
conflicted::conflicts_prefer(dplyr::first)
conflicted::conflicts_prefer(dplyr::last)
conflicted::conflicts_prefer(data.table::isoweek)
conflicted::conflicts_prefer(data.table::month)
conflicted::conflicts_prefer(lubridate::year)
conflicted::conflicts_prefer(lubridate::wday)
conflicted::conflicts_prefer(lubridate::quarter)
conflicted::conflicts_prefer(lubridate::intersect)

set.seed(1234)

# --- Paths -------------------------------------------------------------------
safedata  <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/Database/Analyse"
safeplots <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/text/main"

load(file.path(safedata, "datafordescriptives.RData"))

# --- Theme (AER style) -------------------------------------------------------
theme_aer <- theme_classic(base_size = 10) +
  theme(
    text               = element_text(family = "serif"),
    plot.title         = element_text(face = "bold", size = 11, hjust = 0),
    plot.subtitle      = element_text(size = 8, color = "grey30"),
    plot.caption       = element_text(size = 7, color = "grey50", hjust = 0),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    legend.position    = "bottom",
    legend.text        = element_text(size = 8),
    strip.text         = element_text(face = "bold", size = 8)
  )


# =============================================================================
#  1. DATA PREPARATION (minimal, from descriptives.R)
# =============================================================================

# --- 1a. Weekly excess mortality ----------------------------------------------
mort_w <- p_values_oecd_w %>%
  filter(time_unit == "weekly") %>%
  mutate(Country = entity, date = as.Date(date)) %>%
  filter(date >= as.Date("2020-01-01"), date <= as.Date("2022-12-31")) %>%
  select(
    Country, date, time,
    p_proj   = p_proj_all_ages,
    excess   = excess_proj_all_ages,
    expected = projected_deaths_since_2020_all_ages,
    observed = deaths_since_2020_all_ages
  ) %>%
  arrange(Country, date)

# --- 1b. Weekly stringency from daily oxd_d -----------------------------------
s_weekly <- oxd_d %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Date >= as.Date("2020-01-01"), Date <= as.Date("2022-12-31")) %>%
  mutate(isoyr = isoyear(Date), isowk = isoweek(Date)) %>%
  group_by(Country, isoyr, isowk) %>%
  summarise(
    S_mean = mean(StringencyIndex_PopWeighted, na.rm = TRUE) / 100,
    .groups = "drop"
  )

mort_w <- mort_w %>%
  mutate(isoyr = isoyear(date), isowk = isoweek(date))

panel_w <- s_weekly %>%
  left_join(mort_w, by = c("Country", "isoyr", "isowk"))

# --- 1c. Confirmed cases/deaths from oxd_spatial_d ----------------------------
conf_daily <- oxd_spatial_d %>%
  mutate(
    Date = as.Date(as.character(Date), format = "%Y%m%d"),
    ConfirmedCases  = as.numeric(ConfirmedCases),
    ConfirmedDeaths = as.numeric(ConfirmedDeaths)
  ) %>%
  filter(Date >= as.Date("2020-01-01"), Date <= as.Date("2022-12-31")) %>%
  select(Country = CountryCode, Date, ConfirmedCases, ConfirmedDeaths) %>%
  arrange(Country, Date) %>%
  group_by(Country) %>%
  mutate(
    new_cases  = pmax(0, ConfirmedCases  - lag(ConfirmedCases, 1)),
    new_deaths = pmax(0, ConfirmedDeaths - lag(ConfirmedDeaths, 1))
  ) %>%
  ungroup()

conf_weekly <- conf_daily %>%
  mutate(isoyr = isoyear(Date), isowk = isoweek(Date)) %>%
  group_by(Country, isoyr, isowk) %>%
  summarise(
    cases_w  = sum(new_cases, na.rm = TRUE),
    deaths_w = sum(new_deaths, na.rm = TRUE),
    .groups  = "drop"
  )

panel_w <- panel_w %>%
  left_join(conf_weekly, by = c("Country", "isoyr", "isowk"))

# --- 1d. Population -----------------------------------------------------------
pop <- qdata %>%
  filter(Quarter == "Q1.2020") %>%
  select(Country, pop_th = Qpopulation_th) %>%
  mutate(pop = pop_th * 1000)

panel_w <- panel_w %>%
  left_join(pop, by = "Country")

# --- 1e. Wave assignment -------------------------------------------------------
panel_w <- panel_w %>%
  mutate(
    wave = case_when(
      date < as.Date("2020-06-15")  ~ "W1",
      date < as.Date("2020-09-15")  ~ "W1_summer",
      date < as.Date("2021-03-01")  ~ "W2_wt",
      date < as.Date("2021-07-01")  ~ "W2_alpha",
      date < as.Date("2022-01-01")  ~ "W3_delta",
      TRUE                          ~ "W4_omicron"
    )
  )

# --- 1f. IFR by wave and theta_hat imputation --------------------------------
ifr_wave <- tibble::tibble(
  wave = c("W1", "W1_summer", "W2_wt", "W2_alpha", "W3_delta", "W4_omicron"),
  ifr  = c(0.009, 0.007, 0.006, 0.004, 0.003, 0.0004)
)

panel_w <- panel_w %>%
  left_join(ifr_wave, by = "wave")

# Impute theta_hat: theta = excess / (ifr * pop) but use lead to account for lag
lag_weeks <- 3
panel_w <- panel_w %>%
  group_by(Country) %>%
  arrange(date) %>%
  mutate(
    excess_lead = lead(excess, lag_weeks),
    theta_hat   = ifelse(!is.na(excess_lead) & !is.na(ifr) & !is.na(pop) & ifr > 0,
                         pmax(0, excess_lead / (ifr * pop)), NA_real_)
  ) %>%
  ungroup()

# Restrict to model period
theta_stats <- panel_w %>%
  filter(date >= as.Date("2020-03-01"), date <= as.Date("2022-12-31"),
         !is.na(theta_hat))

cat(sprintf("theta_stats: %d obs, %d countries\n",
            nrow(theta_stats), n_distinct(theta_stats$Country)))


# =============================================================================
#  2. PLOT 1: Existing theta + excess mortality + stringency
# =============================================================================

theta_agg <- theta_stats %>%
  group_by(date) %>%
  summarise(
    mean_theta  = mean(theta_hat, na.rm = TRUE) * 1e6,
    p25_theta   = quantile(theta_hat, 0.25, na.rm = TRUE) * 1e6,
    p75_theta   = quantile(theta_hat, 0.75, na.rm = TRUE) * 1e6,
    p10_theta   = quantile(theta_hat, 0.10, na.rm = TRUE) * 1e6,
    p90_theta   = quantile(theta_hat, 0.90, na.rm = TRUE) * 1e6,
    mean_excess = mean(excess / (pop / 1e6), na.rm = TRUE),
    mean_S      = mean(S_mean, na.rm = TRUE),
    .groups     = "drop"
  )

scale_excess <- max(theta_agg$mean_theta, na.rm = TRUE) /
  max(theta_agg$mean_excess, na.rm = TRUE)
scale_S <- max(theta_agg$p90_theta, na.rm = TRUE)

wave_labels_df <- data.frame(
  x     = as.Date(c("2020-04-15", "2020-12-01", "2021-08-15", "2022-02-01")),
  label = c("Wave 1\n(Original)", "Wave 2\n(Wt/Alpha)",
            "Wave 3\n(Delta)", "Wave 4\n(Omicron)")
)

label_date <- as.Date("2022-06-01")
label_row  <- theta_agg %>% filter(date == min(date[date >= label_date]))
ref_theta  <- label_row$mean_theta
ref_excess <- label_row$mean_excess * scale_excess
ref_S      <- label_row$mean_S * scale_S

p_theta_traj <- ggplot(theta_agg, aes(x = date)) +
  geom_area(aes(y = mean_S * scale_S), fill = "#2980B9", alpha = 0.12) +
  geom_line(aes(y = mean_S * scale_S), color = "#2980B9", linewidth = 0.5) +
  geom_ribbon(aes(ymin = p10_theta, ymax = p90_theta), fill = "grey85", alpha = 0.6) +
  geom_ribbon(aes(ymin = p25_theta, ymax = p75_theta), fill = "grey65", alpha = 0.6) +
  geom_line(aes(y = mean_theta), color = "black", linewidth = 0.7) +
  geom_line(aes(y = mean_excess * scale_excess), color = "#C0392B",
            linewidth = 0.7, linetype = "dashed") +
  geom_vline(xintercept = as.numeric(as.Date(c("2020-06-15", "2020-09-15",
                                                "2021-03-01", "2021-07-01",
                                                "2022-01-01"))),
             linetype = "dotted", color = "grey50", linewidth = 0.3) +
  geom_text(data = wave_labels_df,
            aes(x = x, y = max(theta_agg$p90_theta) * 0.95, label = label),
            size = 2.3, family = "serif", color = "grey40", lineheight = 0.85) +
  annotate("text", x = label_date, y = ref_theta * 1.15,
           label = "hat(theta)", parse = TRUE, hjust = 0, size = 2.8,
           family = "serif", color = "black", fontface = "bold") +
  annotate("text", x = label_date, y = ref_excess * 0.6,
           label = "excess\ndeaths/m", hjust = 0, size = 2.3,
           family = "serif", color = "#C0392B", lineheight = 0.85) +
  annotate("text", x = label_date, y = ref_S * 1.3,
           label = "S (scaled)", hjust = 0, size = 2.3,
           family = "serif", color = "#2980B9", lineheight = 0.85) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y",
               expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(
    expand   = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . / scale_excess,
                        name = "Excess deaths per million per week")
  ) +
  labs(
    y       = expression(hat(theta) ~ "(implied infections per million, weekly)"),
    title   = expression(paste("Imputed Infection Prevalence ",
                               hat(theta), ", Excess Mortality, and Stringency (OECD Mean)")),
    subtitle = paste0(
      "Black: OECD mean \u03b8\u0302. Bands: IQR (dark) and P10\u2013P90 (light).\n",
      "Red dashed: excess deaths per million (right axis). ",
      "Blue area: mean stringency S \u2208 [0,1] (scaled to left axis)."
    ),
    x       = NULL,
    caption = paste0(
      "Notes: 35 OECD economies, weekly. Excess mortality from OWID.\n",
      "Stringency: Oxford CGRT (C1\u2013C8), population-weighted."
    )
  ) +
  theme_aer +
  theme(
    axis.title.y       = element_text(color = "black", angle = 90, vjust = 0.5,
                                       margin = margin(r = 10)),
    axis.title.y.right = element_text(color = "#C0392B", angle = 90, vjust = 0.5,
                                       margin = margin(l = 10)),
    axis.text.y.right  = element_text(color = "#C0392B"),
    axis.ticks.y.right = element_line(color = "#C0392B")
  )

ggsave(file.path(safeplots, "fig_theta_excess_stringency_v2.pdf"),
       p_theta_traj, width = 10, height = 5, device = pdf)
cat("Saved: fig_theta_excess_stringency_v2.pdf\n")


# =============================================================================
#  3. PLOT 2 (NEW): Observed Lethality + Stringency
#     Same structure as Plot 1 but replaces theta with death-to-case ratio
#     Left axis:  excess deaths / confirmed cases (proxy for observed IFR)
#     Blue area:  stringency S (scaled to left axis, like Plot 1)
# =============================================================================

# Compute country-level rolling ratio, then aggregate to OECD mean
ratio_agg <- panel_w %>%
  filter(date >= as.Date("2020-01-06"), date <= as.Date("2022-06-30")) %>%
  filter(!is.na(excess) & !is.na(cases_w) & !is.na(S_mean)) %>%
  group_by(Country) %>%
  arrange(date) %>%
  mutate(
    # 4-week rolling sums to smooth weekly noise
    excess_4w = zoo::rollsum(pmax(excess, 0), k = 4, fill = NA, align = "right"),
    cases_4w  = zoo::rollsum(pmax(cases_w, 0), k = 4, fill = NA, align = "right"),
    # Ratio: only compute when cases are meaningful (>50 per 4 weeks)
    death_case_ratio = ifelse(cases_4w > 50, excess_4w / cases_4w, NA_real_)
  ) %>%
  ungroup() %>%
  group_by(date) %>%
  summarise(
    mean_ratio  = mean(death_case_ratio, na.rm = TRUE),
    p25_ratio   = quantile(death_case_ratio, 0.25, na.rm = TRUE),
    p75_ratio   = quantile(death_case_ratio, 0.75, na.rm = TRUE),
    p10_ratio   = quantile(death_case_ratio, 0.10, na.rm = TRUE),
    p90_ratio   = quantile(death_case_ratio, 0.90, na.rm = TRUE),
    mean_S      = mean(S_mean, na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  # Cap extreme ratios (early weeks with few cases inflate the ratio)
  mutate(across(c(mean_ratio, p25_ratio, p75_ratio, p10_ratio, p90_ratio),
                ~ pmin(.x, 0.30)))

# Scale S to left axis (same approach as Plot 1: S=1 maps to max of P90)
scale_S_ratio <- max(ratio_agg$p90_ratio, na.rm = TRUE)

# Wave labels — same as Plot 1
wave_labels_df2 <- data.frame(
  x     = as.Date(c("2020-04-15", "2020-12-01", "2021-08-15", "2022-03-01")),
  label = c("Wave 1\n(Original)", "Wave 2\n(Wt/Alpha)",
            "Wave 3\n(Delta)", "Wave 4\n(Omicron)")
)

# Label positions inside plot
last_date2   <- max(ratio_agg$date, na.rm = TRUE)
last_row2    <- ratio_agg %>% filter(date == last_date2)
ref_ratio    <- last_row2$mean_ratio
ref_S2       <- last_row2$mean_S * scale_S_ratio
label_date2  <- last_date2 + 6

p_ifr_proxy <- ggplot(ratio_agg, aes(x = date)) +

  # Stringency background area (drawn first, behind everything — same as Plot 1)
  geom_area(aes(y = mean_S * scale_S_ratio),
            fill = "#2980B9", alpha = 0.12) +
  geom_line(aes(y = mean_S * scale_S_ratio),
            color = "#2980B9", linewidth = 0.5, linetype = "solid") +

  # Ratio uncertainty bands (analogous to theta bands in Plot 1)
  geom_ribbon(aes(ymin = p10_ratio, ymax = p90_ratio),
              fill = "#EAACAC", alpha = 0.5) +
  geom_ribbon(aes(ymin = p25_ratio, ymax = p75_ratio),
              fill = "#D46A6A", alpha = 0.4) +

  # Mean death-to-case ratio (main line, like theta in Plot 1)
  geom_line(aes(y = mean_ratio), color = "#C0392B", linewidth = 0.7) +

  # Wave boundary lines (identical to Plot 1)
  geom_vline(xintercept = as.numeric(as.Date(c("2020-06-15", "2020-09-15",
                                                "2021-03-01", "2021-07-01",
                                                "2022-01-01"))),
             linetype = "dotted", color = "grey50", linewidth = 0.3) +

  # Wave labels (identical position to Plot 1)
  geom_text(data = wave_labels_df2,
            aes(x = x, y = max(ratio_agg$p90_ratio) * 0.95, label = label),
            size = 2.3, family = "serif", color = "grey40", lineheight = 0.85) +

  # Direct line labels inside plot (same style as Plot 1)
  annotate("text",
           x = label_date2, y = ref_ratio * 1.4,
           label = "Excess deaths\nper case", hjust = 0, size = 2.3,
           family = "serif", color = "#C0392B", lineheight = 0.85,
           fontface = "bold") +
  annotate("text",
           x = label_date2, y = ref_S2 * 1.3,
           label = "S (scaled)", hjust = 0, size = 2.3,
           family = "serif", color = "#2980B9", lineheight = 0.85) +

  # Axes
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y",
               expand = expansion(mult = c(0.01, 0.08))) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . / scale_S_ratio,
                        name = "Containment Intensity S (0\u20131)",
                        labels = scales::number_format(accuracy = 0.1))
  ) +

  # Labels
  labs(
    y        = "Excess Deaths per Confirmed Case (4-week rolling)",
    x        = NULL,
    title    = "Pandemic Dynamics: Observed Lethality and Containment (OECD Mean)",
    subtitle = paste0(
      "Red: OECD mean excess-death-to-case ratio. ",
      "Bands: IQR (dark) and P10\u2013P90 (light).\n",
      "Blue area: mean stringency S \u2208 [0,1] (scaled to left axis).\n",
      "High ratio signals the necessity to act (\u03b4\u03b8 > 0); ",
      "collapse of ratio with vaccination dissolves the trilemma."
    ),
    caption  = paste0(
      "Notes: 35 OECD economies, weekly. Excess mortality from OWID (Karlinsky & Kobak 2021).\n",
      "Confirmed cases: Oxford CGRT/JHU. Stringency: Oxford CGRT (C1\u2013C8), pop-weighted.\n",
      "Ratio computed on 4-week rolling sums; country-weeks with <50 cases excluded.\n",
      "Vertical dotted lines: wave boundaries (OECD-median variant dominance >50%).\n",
      "S scaled to left axis for comparability."
    )
  ) +

  # Theme (identical to Plot 1)
  theme_aer +
  theme(
    axis.title.y       = element_text(color = "#C0392B", angle = 90, vjust = 0.5,
                                       margin = margin(r = 10)),
    axis.title.y.right = element_text(color = "#2980B9", angle = 90, vjust = 0.5,
                                       margin = margin(l = 10)),
    axis.text.y        = element_text(color = "#C0392B"),
    axis.text.y.right  = element_text(color = "#2980B9"),
    axis.ticks.y       = element_line(color = "#C0392B"),
    axis.ticks.y.right = element_line(color = "#2980B9")
  )

ggsave(file.path(safeplots, "fig_ifr_proxy_stringency_v2.pdf"),
       p_ifr_proxy, width = 10, height = 5, device = pdf)
cat("Saved: fig_ifr_proxy_stringency_v2.pdf\n")

cat("\nDone.\n")
