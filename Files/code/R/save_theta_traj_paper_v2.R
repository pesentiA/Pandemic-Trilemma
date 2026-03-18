library(ggplot2)
library(dplyr)
library(lubridate)
library(patchwork)

safedata  <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/processed"
safeplots <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/output/figures"

load(file.path(safedata, "dataforanalysis.RData"))

# ── AER theme ─────────────────────────────────────────────────────────────────
theme_aer <- theme_classic(base_size = 11) +
  theme(
    text               = element_text(family = "serif"),
    plot.title         = element_text(face = "bold", size = 11, hjust = 0),
    axis.title         = element_text(size = 9.5),
    axis.text          = element_text(size = 8.5),
    legend.position    = "bottom",
    legend.title       = element_blank(),
    legend.text        = element_text(size = 8.5),
    legend.key.size    = unit(0.45, "cm"),
    legend.key.width   = unit(1.3, "cm"),
    legend.spacing.x   = unit(0.4, "cm"),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.25),
    plot.caption       = element_text(size = 7.5, color = "grey40", hjust = 0,
                                      lineheight = 1.3)
  )

# ── Data ──────────────────────────────────────────────────────────────────────
pw <- panel_w %>%
  filter(date >= as.Date("2020-01-01"),
         date <= as.Date("2022-12-31"))

# theta: 35 countries with weekly excess mortality (CRI, JPN, TUR excluded)
agg_theta <- pw %>%
  filter(!is.na(theta_hat)) %>%
  group_by(date) %>%
  summarise(
    mean_theta = mean(theta_hat, na.rm = TRUE) * 1e6,
    p25_theta  = quantile(theta_hat, 0.25, na.rm = TRUE) * 1e6,
    p75_theta  = quantile(theta_hat, 0.75, na.rm = TRUE) * 1e6,
    p10_theta  = quantile(theta_hat, 0.10, na.rm = TRUE) * 1e6,
    p90_theta  = quantile(theta_hat, 0.90, na.rm = TRUE) * 1e6,
    .groups    = "drop"
  )

# S and excess: all 38 OECD countries
agg_se <- pw %>%
  group_by(date) %>%
  summarise(
    mean_excess = mean(excess / (pop / 1e6), na.rm = TRUE),
    mean_S      = mean(S_mean, na.rm = TRUE) * 100,   # → [0, 100]
    .groups     = "drop"
  )

theta_agg <- left_join(agg_theta, agg_se, by = "date")

# ── Scale factor: excess deaths mapped onto theta axis ─────────────────────────
scale_excess <- max(theta_agg$mean_theta, na.rm = TRUE) /
                max(theta_agg$mean_excess, na.rm = TRUE)

# ── Shared x limits & wave boundaries ─────────────────────────────────────────
x_lim       <- c(as.Date("2020-01-01"), as.Date("2022-12-31"))
wave_bounds <- as.Date(c("2020-06-15", "2020-09-15",
                          "2021-03-01", "2021-07-01", "2022-01-01"))

wave_labels_df <- data.frame(
  x     = as.Date(c("2020-04-10", "2020-11-20", "2021-08-15", "2022-02-05")),
  y     = max(theta_agg$p90_theta, na.rm = TRUE) * 0.96,
  label = c("Wave 1\n(Original)", "Wave 2\n(Alpha)",
            "Wave 3\n(Delta)", "Wave 4\n(Omicron)")
)

# IFR collapse annotation position
omicron_peak_date <- theta_agg$date[which.max(theta_agg$mean_theta)]
omicron_peak_y    <- max(theta_agg$mean_theta, na.rm = TRUE)

# ── MAIN PANEL ────────────────────────────────────────────────────────────────
p_main <- ggplot(theta_agg, aes(x = date)) +

  # Wave boundaries
  geom_vline(xintercept = wave_bounds,
             linetype = "dotted", color = "grey55", linewidth = 0.35) +

  # theta P10–P90 band
  geom_ribbon(aes(ymin = p10_theta, ymax = p90_theta),
              fill = "grey85", alpha = 0.6) +
  # theta IQR band
  geom_ribbon(aes(ymin = p25_theta, ymax = p75_theta),
              fill = "grey65", alpha = 0.6) +

  # Excess mortality (right axis, red dashed)
  geom_line(aes(y    = mean_excess * scale_excess,
                color    = "Excess deaths per million (right axis)",
                linetype = "Excess deaths per million (right axis)"),
            linewidth = 0.75) +

  # theta mean (black solid)
  geom_line(aes(y    = mean_theta,
                color    = "Infection prevalence \u03b8\u0302 (left axis)",
                linetype = "Infection prevalence \u03b8\u0302 (left axis)"),
            linewidth = 0.85) +

  # Wave labels
  geom_text(data = wave_labels_df,
            aes(x = x, y = y, label = label),
            size = 2.6, family = "serif", color = "grey35",
            lineheight = 0.88, inherit.aes = FALSE) +

  # IFR collapse annotation
  annotate("text",
           x = omicron_peak_date + 14, y = omicron_peak_y * 0.63,
           label = "IFR collapse:\nhigh \u03b8\u0302, low mortality",
           hjust = 0, size = 2.7, family = "serif",
           color = "grey25", lineheight = 0.9) +
  annotate("segment",
           x    = omicron_peak_date + 12,
           xend = omicron_peak_date,
           y    = omicron_peak_y * 0.63,
           yend = omicron_peak_y * 0.84,
           arrow = arrow(length = unit(0.13, "cm"), type = "closed"),
           color = "grey35", linewidth = 0.4) +

  # Colors and linetypes for legend
  scale_color_manual(
    values = c("Infection prevalence \u03b8\u0302 (left axis)" = "black",
               "Excess deaths per million (right axis)"        = "#C0392B"),
    breaks = c("Infection prevalence \u03b8\u0302 (left axis)",
               "Excess deaths per million (right axis)")
  ) +
  scale_linetype_manual(
    values = c("Infection prevalence \u03b8\u0302 (left axis)" = "solid",
               "Excess deaths per million (right axis)"        = "dashed"),
    breaks = c("Infection prevalence \u03b8\u0302 (left axis)",
               "Excess deaths per million (right axis)")
  ) +

  # Axes
  scale_x_date(limits     = x_lim,
               date_breaks = "3 months", date_labels = "%b '%y",
               expand     = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(
    name     = "Implied infections per million (weekly)",
    expand   = expansion(mult = c(0, 0.08)),
    sec.axis = sec_axis(~ . / scale_excess,
                        name = "Excess deaths per million per week")
  ) +

  labs(x = NULL,
       title = "Pandemic Dynamics: Infection Prevalence, Excess Mortality, and Containment") +

  guides(color    = guide_legend(nrow = 1),
         linetype = guide_legend(nrow = 1)) +

  theme_aer +
  theme(
    axis.title.y.right = element_text(color = "#C0392B", angle = 90,
                                      vjust = 0.5, margin = margin(l = 8)),
    axis.text.y.right  = element_text(color = "#C0392B"),
    axis.ticks.y.right = element_line(color = "#C0392B"),
    axis.text.x        = element_blank(),
    axis.ticks.x       = element_blank(),
    axis.line.x        = element_blank(),
    plot.margin        = margin(8, 5, 0, 5)
  )


# ── STRINGENCY PANEL (bottom strip) ───────────────────────────────────────────
p_str <- ggplot(theta_agg, aes(x = date)) +

  geom_vline(xintercept = wave_bounds,
             linetype = "dotted", color = "grey55", linewidth = 0.35) +

  geom_area(aes(y = mean_S), fill = "#2980B9", alpha = 0.55) +
  geom_line(aes(y = mean_S), color = "#2980B9", linewidth = 0.6) +

  scale_x_date(limits     = x_lim,
               date_breaks = "3 months", date_labels = "%b '%y",
               expand     = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(
    name   = "Stringency\nIndex (0\u2013100)",
    limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100)
  ) +

  labs(x = NULL,
       caption = paste0(
         "Notes: 38 OECD economies. Infection prevalence \u03b8\u0302: 35 countries with weekly excess mortality",
         " (CRI, JPN, TUR excluded).\n",
         "Stringency and excess deaths: all 38 countries. Excess mortality: OWID (Karlinsky & Kobak 2021),",
         " projected baseline. IFR calibrated by wave.\n",
         "Bands: IQR (dark grey) and P10\u2013P90 (light grey) across 35 countries.",
         " Stringency: Oxford CGRT (C1\u2013C8), population-weighted."
       )) +

  theme_aer +
  theme(
    legend.position  = "none",
    axis.text.x      = element_text(angle = 30, hjust = 1),
    plot.margin      = margin(0, 5, 5, 5),
    axis.title.y     = element_text(size = 8.5)
  )


# ── Combine ───────────────────────────────────────────────────────────────────
p_final <- p_main / p_str +
  plot_layout(heights = c(4, 1.2)) &
  theme(plot.background = element_rect(fill = "white", color = NA))

ggsave(
  filename = file.path(safeplots, "fig_pandemic_dynamics_paper_v2.pdf"),
  plot     = p_final,
  width = 18, height = 12, units = "cm"
)
ggsave(
  filename = file.path(safeplots, "fig_pandemic_dynamics_paper_v2.png"),
  plot     = p_final,
  width = 18, height = 12, units = "cm", dpi = 300
)

cat("Saved: fig_pandemic_dynamics_paper_v2.pdf/.png\n")
