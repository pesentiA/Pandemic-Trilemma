library(ggplot2)
library(dplyr)
library(lubridate)

safedata  <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/processed"
safeplots <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/output/figures"

load(file.path(safedata, "dataforanalysis.RData"))

# ── Data (identical to original) ──────────────────────────────────────────────
theta_stats <- panel_w %>%
  filter(date >= as.Date("2020-01-01"), date <= as.Date("2022-12-31"),
         !is.na(theta_hat))

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
scale_S      <- max(theta_agg$p90_theta, na.rm = TRUE)

wave_labels_df <- data.frame(
  x     = as.Date(c("2020-04-15", "2020-12-01", "2021-08-15", "2022-02-01")),
  label = c("Wave 1\n(Original)", "Wave 2\n(Wt/Alpha)",
            "Wave 3\n(Delta)", "Wave 4\n(Omicron)")
)

# ── Dummy rows to drive the legend ────────────────────────────────────────────
lvls <- c("Infection prevalence (left axis)",
          "Excess deaths per million (right axis)",
          "Mean stringency S (left axis, scaled)")
legend_df <- data.frame(
  date   = rep(as.Date("2020-01-01"), 3),
  y      = rep(0, 3),
  series = factor(lvls, levels = lvls)
)

# ── Plot ──────────────────────────────────────────────────────────────────────
p_theta_traj <- ggplot(theta_agg, aes(x = date)) +

  # Stringency background (unchanged)
  geom_area(aes(y = mean_S * scale_S),
            fill = "#2980B9", alpha = 0.12) +
  geom_line(aes(y = mean_S * scale_S),
            color = "#2980B9", linewidth = 0.5, linetype = "solid") +

  # theta bands (unchanged)
  geom_ribbon(aes(ymin = p10_theta, ymax = p90_theta),
              fill = "grey85", alpha = 0.6) +
  geom_ribbon(aes(ymin = p25_theta, ymax = p75_theta),
              fill = "grey65", alpha = 0.6) +

  # theta mean (unchanged)
  geom_line(aes(y = mean_theta), color = "black", linewidth = 0.7) +

  # Excess mortality (unchanged)
  geom_line(aes(y = mean_excess * scale_excess),
            color = "#C0392B", linewidth = 0.7, linetype = "dashed") +

  # Wave boundaries (unchanged)
  geom_vline(xintercept = as.numeric(as.Date(c("2020-06-15", "2020-09-15",
                                               "2021-03-01", "2021-07-01",
                                               "2022-01-01"))),
             linetype = "dotted", color = "grey50", linewidth = 0.3) +

  # Wave labels (unchanged)
  geom_text(data = wave_labels_df,
            aes(x = x, y = max(theta_agg$p90_theta) * 0.95, label = label),
            size = 2.3, family = "serif", color = "grey40",
            lineheight = 0.85, inherit.aes = FALSE) +

  # Dummy lines → legend only, not visible in plot
  geom_line(data = legend_df,
            aes(x = date, y = y, color = series, linetype = series),
            linewidth = 0.7) +

  scale_color_manual(
    name   = NULL,
    values = c("Infection prevalence (left axis)"              = "black",
               "Excess deaths per million (right axis)"        = "#C0392B",
               "Mean stringency S (left axis, scaled)"         = "#2980B9")
  ) +
  scale_linetype_manual(
    name   = NULL,
    values = c("Infection prevalence (left axis)"              = "solid",
               "Excess deaths per million (right axis)"        = "dashed",
               "Mean stringency S (left axis, scaled)"         = "solid")
  ) +

  # Axes (tighten right expansion — no longer need space for labels)
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y",
               expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(
    expand   = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . / scale_excess,
                        name = "Excess deaths per million per week")
  ) +

  labs(
    y       = expression(hat(theta) ~ "(implied infections per million, weekly)"),
    title   = expression(paste("Imputed Infection Prevalence ",
                               hat(theta), ", Excess Mortality, and Stringency (OECD Mean)")),
    x       = NULL,
    caption = paste0(
      "Notes: 35 OECD economies, weekly. Excess mortality: OWID (Karlinsky & Kobak 2021), projected baseline. ",
      "IFR calibrated by wave (Levin et al. 2020; Marziano et al. 2023).\n",
      "Bands: IQR (dark grey) and P10\u2013P90 (light grey). ",
      "Stringency: Oxford CGRT (C1\u2013C8), population-weighted, scaled to left axis."
    )
  ) +

  guides(color    = guide_legend(nrow = 1),
         linetype = guide_legend(nrow = 1)) +

  theme_aer +
  theme(
    legend.position    = "bottom",
    legend.text        = element_text(size = 8.5, family = "serif"),
    legend.key.size    = unit(0.45, "cm"),
    legend.key.width   = unit(1.2, "cm"),
    axis.title.y = element_text(
      color  = "black", angle = 90, vjust = 0.5,
      margin = margin(t = 0, r = 10, b = 0, l = 0)
    ),
    axis.title.y.right = element_text(
      color  = "#C0392B", angle = 90, vjust = 0.5,
      margin = margin(t = 0, r = 0, b = 0, l = 10)
    ),
    axis.text.y.right  = element_text(color = "#C0392B"),
    axis.ticks.y.right = element_line(color = "#C0392B")
  )

ggsave(
  filename = file.path(safeplots, "fig_pandemic_dynamics_paper_v3.pdf"),
  plot     = p_theta_traj,
  width = 18, height = 11, units = "cm"
)
ggsave(
  filename = file.path(safeplots, "fig_pandemic_dynamics_paper_v3.png"),
  plot     = p_theta_traj,
  width = 18, height = 11, units = "cm", dpi = 300
)

cat("Saved: fig_pandemic_dynamics_paper_v3.pdf/.png\n")
