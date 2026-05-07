library(ggplot2)
library(dplyr)
library(lubridate)

safedata  <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/processed"
safeplots <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/output/figures"

load(file.path(safedata, "dataforanalysis.RData"))

# AER theme (replicate from descriptives.R)
theme_aer <- theme_classic(base_size = 11) +
  theme(
    text               = element_text(family = "serif"),
    plot.title         = element_text(face = "bold", size = 11, hjust = 0),
    axis.title         = element_text(size = 10),
    axis.text          = element_text(size = 9),
    legend.position    = "bottom",
    legend.title       = element_blank(),
    legend.text        = element_text(size = 9),
    legend.key.size    = unit(0.5, "cm"),
    legend.key.width   = unit(1.2, "cm"),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.25),
    plot.caption       = element_text(size = 7.5, color = "grey45", hjust = 0),
    plot.margin        = margin(8, 30, 5, 5)
  )

# ── Data ──────────────────────────────────────────────────────────────────────
theta_stats <- panel_w %>%
  filter(date >= as.Date("2020-01-01"),
         date <= as.Date("2022-12-31"),
         !is.na(theta_hat))

theta_agg <- theta_stats %>%
  group_by(date) %>%
  summarise(
    mean_theta  = mean(theta_hat, na.rm = TRUE) * 1e6,
    p25_theta   = quantile(theta_hat, 0.25, na.rm = TRUE) * 1e6,
    p75_theta   = quantile(theta_hat, 0.75, na.rm = TRUE) * 1e6,
    mean_excess = mean(excess / (pop / 1e6), na.rm = TRUE),
    mean_S      = mean(S_mean, na.rm = TRUE),   # already in [0,1]
    .groups     = "drop"
  )

# ── Scaling ───────────────────────────────────────────────────────────────────
scale_excess <- max(theta_agg$mean_theta, na.rm = TRUE) /
                max(theta_agg$mean_excess, na.rm = TRUE)
scale_S      <- max(theta_agg$p75_theta, na.rm = TRUE)   # S=1 → P75 of theta

# ── Key dates ─────────────────────────────────────────────────────────────────
omicron_date  <- as.Date("2021-12-01")   # onset of Omicron / endemic transition
sample_end    <- as.Date("2022-12-31")

# ── Era labels ────────────────────────────────────────────────────────────────
era_labels <- data.frame(
  x     = c(as.Date("2020-10-01"), as.Date("2022-04-01")),
  y     = max(theta_agg$p75_theta, na.rm = TRUE) * 0.97,
  label = c("Pandemic control period", "Endemic\ntransition"),
  hjust = c(0.5, 0.5)
)

# ── Legend data (dummy lines for ggplot legend) ───────────────────────────────
dummy <- data.frame(
  date  = rep(as.Date("2020-01-01"), 3),
  y     = c(0, 0, 0),
  group = factor(c("Infection prevalence (left)",
                   "Excess deaths/million (right)",
                   "Mean stringency (left, scaled)"),
                 levels = c("Infection prevalence (left)",
                            "Excess deaths/million (right)",
                            "Mean stringency (left, scaled)"))
)

# ── Plot ──────────────────────────────────────────────────────────────────────
p_paper <- ggplot(theta_agg, aes(x = date)) +

  # Shaded regions: pandemic vs endemic
  annotate("rect",
           xmin = as.Date("2020-01-01"), xmax = omicron_date,
           ymin = -Inf, ymax = Inf,
           fill = "grey50", alpha = 0.05) +
  annotate("rect",
           xmin = omicron_date, xmax = sample_end + 10,
           ymin = -Inf, ymax = Inf,
           fill = "grey50", alpha = 0.02) +

  # Transition line
  geom_vline(xintercept = omicron_date,
             linetype = "dashed", color = "grey40", linewidth = 0.5) +

  # Stringency (subtle, background)
  geom_area(aes(y = mean_S * scale_S),
            fill = "grey60", alpha = 0.18) +
  geom_line(aes(y = mean_S * scale_S),
            color = "grey50", linewidth = 0.45, linetype = "solid") +

  # theta IQR band
  geom_ribbon(aes(ymin = p25_theta, ymax = p75_theta),
              fill = "grey70", alpha = 0.55) +

  # theta mean
  geom_line(aes(y = mean_theta), color = "black", linewidth = 0.85) +

  # Excess mortality (right axis)
  geom_line(aes(y = mean_excess * scale_excess),
            color = "grey20", linewidth = 0.75, linetype = "dashed") +

  # Era labels
  geom_text(data = era_labels,
            aes(x = x, y = y, label = label, hjust = hjust),
            size = 3, family = "serif", color = "grey30",
            fontface = "italic", lineheight = 0.9) +

  # IFR collapse annotation at Omicron peak
  annotate("text",
           x = as.Date("2022-01-20"), y = max(theta_agg$mean_theta) * 0.72,
           label = "IFR collapse:\nhigh infections,\nlow mortality",
           hjust = 0, size = 2.7, family = "serif",
           color = "grey25", lineheight = 0.9) +
  annotate("segment",
           x    = as.Date("2022-01-18"), xend = as.Date("2022-01-01"),
           y    = max(theta_agg$mean_theta) * 0.72,
           yend = max(theta_agg$mean_theta) * 0.90,
           arrow = arrow(length = unit(0.12, "cm"), type = "closed"),
           color = "grey35", linewidth = 0.35) +

  # Dummy lines for legend
  geom_line(data = dummy %>% filter(group == "Infection prevalence (left)"),
            aes(y = y, linetype = group, color = group), linewidth = 0.85) +
  geom_line(data = dummy %>% filter(group == "Excess deaths/million (right)"),
            aes(y = y, linetype = group, color = group), linewidth = 0.75) +
  geom_line(data = dummy %>% filter(group == "Mean stringency (left, scaled)"),
            aes(y = y, linetype = group, color = group), linewidth = 0.45) +

  scale_color_manual(
    values = c("Infection prevalence (left)"       = "black",
               "Excess deaths/million (right)"     = "grey20",
               "Mean stringency (left, scaled)"    = "grey50")
  ) +
  scale_linetype_manual(
    values = c("Infection prevalence (left)"       = "solid",
               "Excess deaths/million (right)"     = "dashed",
               "Mean stringency (left, scaled)"    = "solid")
  ) +

  # Axes
  scale_x_date(date_breaks = "3 months", date_labels = "%b '%y",
               expand = expansion(mult = c(0.01, 0.02)),
               limits = c(as.Date("2020-01-01"), sample_end + 10)) +
  scale_y_continuous(
    name     = "Infection prevalence (implied, per million)",
    expand   = expansion(mult = c(0, 0.06)),
    sec.axis = sec_axis(~ . / scale_excess,
                        name = "Excess deaths per million per week")
  ) +

  labs(
    x       = NULL,
    title   = "Pandemic Dynamics: Infection Prevalence, Excess Mortality, and Containment",
    caption = paste0(
      "Notes: OECD mean across 35 economies, weekly frequency. Shaded band: IQR of infection prevalence across countries.\n",
      "Excess mortality: OWID (Karlinsky & Kobak 2021), projected baseline. ",
      "Stringency: Oxford CGRT, population-weighted, scaled to left axis.\n",
      "Dashed vertical line: onset of Omicron wave (Dec 2021). ",
      "IFR calibrated by wave (Levin et al. 2020; Marziano et al. 2023)."
    )
  ) +

  guides(
    color    = guide_legend(nrow = 1),
    linetype = guide_legend(nrow = 1)
  ) +

  theme_aer +
  theme(
    axis.title.y.right = element_text(color = "grey35",  angle = 90, vjust = 0.5,
                                      margin = margin(l = 8)),
    axis.text.y.right  = element_text(color = "grey35"),
    axis.ticks.y.right = element_line(color = "grey35"),
    axis.text.x        = element_text(angle = 30, hjust = 1)
  )

ggsave(
  filename = file.path(safeplots, "fig_pandemic_dynamics_paper.pdf"),
  plot     = p_paper,
  width = 16, height = 9, units = "cm"
)
ggsave(
  filename = file.path(safeplots, "fig_pandemic_dynamics_paper.png"),
  plot     = p_paper,
  width = 16, height = 9, units = "cm", dpi = 300
)

cat("Saved: fig_pandemic_dynamics_paper.pdf/.png\n")
