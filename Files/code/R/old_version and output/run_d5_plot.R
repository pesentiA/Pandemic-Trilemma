suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(lubridate)
  library(readxl); library(ggplot2); library(data.table)
})

conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflicts_prefer(dplyr::lag)
conflicted::conflicts_prefer(dplyr::first)
conflicted::conflicts_prefer(dplyr::coalesce)
conflicted::conflicts_prefer(dplyr::recode)
conflicted::conflicts_prefer(lubridate::quarter)
conflicted::conflicts_prefer(lubridate::year)
conflicted::conflicts_prefer(dplyr::last)
conflicted::conflicts_prefer(data.table::isoweek)
conflicted::conflicts_prefer(data.table::month)
conflicted::conflicts_prefer(dplyr::lead)
conflicted::conflicts_prefer(lubridate::union)

safedata  <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/processed"
safeplots <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/output/figures"

load(file.path(safedata, "dataforanalysis.RData"))

fm1 <- read_excel("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/raw/fiscal measures/fiscal_classified_v1_7.xlsx")
fm1 <- fm1 %>%
  mutate(YQ = paste0("Q", Quarter, ".", Year),
         YQ_ord = factor(YQ, levels = sort(unique(paste0("Q", Quarter, ".", Year))), ordered = TRUE)) %>%
  filter(!PolicyCode %in% c(5, 6, 11, 12, 15, 16))

pandemic_qs <- c("Q1.2019","Q2.2019","Q3.2019","Q4.2019",
                 "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                 "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
                 "Q1.2022","Q2.2022","Q3.2022","Q4.2022")

# --- Minimal data build (same as analysis.R) ---
df_qdata <- qdata %>% filter(Quarter %in% pandemic_qs) %>%
  select(Country, Quarter, y_t_pct, DebtR_share2019, p_proj_all_ages, Qpopulation_th)
pop_2019 <- df_qdata[df_qdata$Quarter=="Q4.2019", c("Country","Qpopulation_th")]
names(pop_2019)[2] <- "pop_2019"
df_qdata <- merge(df_qdata, pop_2019, by="Country")

df_theta <- theta_quarterly_full %>%
  mutate(Quarter = as.character(YQ)) %>%
  filter(Quarter %in% pandemic_qs) %>%
  select(Country, Quarter, theta_mean, S_mean, S_max)

df_fiscal <- fm1 %>% filter(broad_fiscal == 1) %>%
  mutate(Quarter = as.character(YQ_ord)) %>%
  filter(Quarter %in% pandemic_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(F_CP = sum(broad_fiscal_gdp[transmission_channel == "CP"], na.rm=TRUE),
            F_DI = sum(broad_fiscal_gdp[transmission_channel == "DI"], na.rm=TRUE),
            .groups="drop")

df_stringency <- panel_w %>% filter(!is.na(S_mean)) %>%
  mutate(year = year(date), quarter = quarter(date),
         Quarter = paste0("Q", quarter, ".", year)) %>%
  filter(Quarter %in% pandemic_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(S_mean_pw = mean(S_mean, na.rm=TRUE), .groups="drop")

df <- df_qdata %>%
  left_join(df_theta, by=c("Country","Quarter")) %>%
  left_join(df_fiscal, by=c("Country","Quarter")) %>%
  left_join(df_stringency, by=c("Country","Quarter")) %>%
  mutate(Quarter = factor(Quarter, levels = pandemic_qs, ordered = TRUE),
         F_CP = replace_na(F_CP, 0), F_DI = replace_na(F_DI, 0),
         S_mean_pw = replace_na(S_mean_pw, 0),
         p_proj_all_ages = replace_na(p_proj_all_ages, 0)) %>%
  arrange(Country, Quarter)

# Scale
pdata <- df %>%
  filter(Quarter %in% c("Q4.2019","Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                         "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
                         "Q1.2022","Q2.2022")) %>%
  mutate(S_mean_tw = S_mean_pw * 100, F_CP = F_CP * 100, F_DI = F_DI * 100)

# t_idx
pdata$t_idx <- match(as.character(pdata$Quarter), pandemic_qs)

# debt_dR (chronological)
pdataY <- pdata %>%
  arrange(Country, t_idx) %>%
  group_by(Country) %>%
  mutate(debt_dR = DebtR_share2019 - lag(DebtR_share2019, 1)) %>%
  ungroup()

cat(sprintf("pdataY: %d obs, %d countries\n", nrow(pdataY), n_distinct(pdataY$Country)))

# --- D5 PLOT ---
ts_data <- pdataY %>%
  filter(t_idx >= 5 & t_idx <= 14) %>%
  mutate(q_num = t_idx - 4L) %>%
  group_by(q_num, Quarter) %>%
  summarise(
    y_med  = median(y_t_pct, na.rm = TRUE),
    y_p25  = quantile(y_t_pct, 0.25, na.rm = TRUE),
    y_p75  = quantile(y_t_pct, 0.75, na.rm = TRUE),
    S_med  = median(S_mean_tw, na.rm = TRUE),
    S_p25  = quantile(S_mean_tw, 0.25, na.rm = TRUE),
    S_p75  = quantile(S_mean_tw, 0.75, na.rm = TRUE),
    CP_med = median(F_CP, na.rm = TRUE),
    CP_p25 = quantile(F_CP, 0.25, na.rm = TRUE),
    CP_p75 = quantile(F_CP, 0.75, na.rm = TRUE),
    DI_med = median(F_DI, na.rm = TRUE),
    DI_p25 = quantile(F_DI, 0.25, na.rm = TRUE),
    DI_p75 = quantile(F_DI, 0.75, na.rm = TRUE),
    d_med  = median(p_proj_all_ages, na.rm = TRUE),
    d_p25  = quantile(p_proj_all_ages, 0.25, na.rm = TRUE),
    d_p75  = quantile(p_proj_all_ages, 0.75, na.rm = TRUE),
    db_med = median(debt_dR, na.rm = TRUE),
    db_p25 = quantile(debt_dR, 0.25, na.rm = TRUE),
    db_p75 = quantile(debt_dR, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

q_labels <- c("Q1.20","Q2.20","Q3.20","Q4.20","Q1.21",
              "Q2.21","Q3.21","Q4.21","Q1.22","Q2.22")

panel_labels <- c(
  "Output gap (pp of potential GDP)",
  "Containment stringency (0\u2013100)",
  "Capacity preservation (% of 2019 GDP)",
  "Demand injection (% of 2019 GDP)",
  "Excess mortality (P-score, %)",
  "Change in public debt (pp of 2019 GDP)"
)

ts_long <- bind_rows(
  ts_data %>% transmute(q_num, variable = panel_labels[1], med = y_med,  p25 = y_p25,  p75 = y_p75),
  ts_data %>% transmute(q_num, variable = panel_labels[2], med = S_med,  p25 = S_p25,  p75 = S_p75),
  ts_data %>% transmute(q_num, variable = panel_labels[3], med = CP_med, p25 = CP_p25, p75 = CP_p75),
  ts_data %>% transmute(q_num, variable = panel_labels[4], med = DI_med, p25 = DI_p25, p75 = DI_p75),
  ts_data %>% transmute(q_num, variable = panel_labels[5], med = d_med,  p25 = d_p25,  p75 = d_p75),
  ts_data %>% transmute(q_num, variable = panel_labels[6], med = db_med, p25 = db_p25, p75 = db_p75)
) %>%
  mutate(variable = factor(variable, levels = panel_labels))

fig_ts_all <- ggplot(ts_long, aes(x = q_num)) +
  geom_ribbon(aes(ymin = p25, ymax = p75),
              fill = "grey80", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.3) +
  geom_line(aes(y = med), color = "black", linewidth = 0.7) +
  geom_point(aes(y = med), color = "black", size = 1.3, shape = 16) +
  facet_wrap(~ variable, scales = "free_y", ncol = 3) +
  scale_x_continuous(breaks = 1:10, labels = q_labels) +
  labs(x = NULL, y = NULL) +
  theme_classic(base_size = 10) +
  theme(
    text              = element_text(family = "serif"),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.minor  = element_blank(),
    axis.text.x       = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y       = element_text(size = 8),
    axis.ticks        = element_line(color = "grey70", linewidth = 0.3),
    strip.background  = element_blank(),
    strip.text        = element_text(face = "italic", size = 9, hjust = 0),
    plot.margin       = margin(t = 5, r = 10, b = 5, l = 5)
  )

ggsave(file.path(safeplots, "fig_ts_key_variables_v2.pdf"),
       fig_ts_all, width = 11, height = 6.5)
ggsave(file.path(safeplots, "fig_ts_key_variables_v2.png"),
       fig_ts_all, width = 11, height = 6.5, dpi = 300)
cat(sprintf("Saved to: %s\n", safeplots))
