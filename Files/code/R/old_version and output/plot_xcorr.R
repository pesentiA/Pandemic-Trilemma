suppressPackageStartupMessages({ library(ggplot2); library(patchwork) })

safeplots <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/output/figures"

lag <- -6:6
r_cp <- c(-0.041,-0.045,-0.043,-0.020,0.011,0.117,0.194,0.171,0.079,0.094,0.085,0.060,0.061)
r_di <- c(-0.007,-0.018,-0.014,-0.004,0.010,0.030,0.093,0.074,0.031,0.027,0.019,0.002,0.011)

df_cp <- data.frame(lag = lag, r = r_cp, leads = ifelse(lag >= 0, "S leads", "F leads"))
df_di <- data.frame(lag = lag, r = r_di, leads = ifelse(lag >= 0, "S leads", "F leads"))

theme_pub <- theme_bw(base_size = 11) +
  theme(
    text             = element_text(family = "serif"),
    plot.title       = element_text(face = "bold", size = 11, hjust = 0.5),
    plot.subtitle    = element_text(size = 8, color = "grey40", hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position  = "none"
  )

p_cp <- ggplot(df_cp, aes(x = lag, y = r, fill = leads)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c("S leads" = "#2166AC", "F leads" = "grey65")) +
  annotate("text", x = 0, y = 0.194 + 0.012, label = "0.194",
           size = 3.2, fontface = "bold", family = "serif") +
  scale_x_continuous(breaks = -6:6) +
  scale_y_continuous(limits = c(-0.10, 0.25), breaks = seq(-0.10, 0.25, 0.05)) +
  labs(title = expression(Delta*S %->% F^{CP}),
       x = "Lag (weeks)", y = "Cross-correlation") +
  theme_pub

p_di <- ggplot(df_di, aes(x = lag, y = r, fill = leads)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c("S leads" = "#B2182B", "F leads" = "grey65")) +
  annotate("text", x = 0, y = 0.093 + 0.012, label = "0.093",
           size = 3.2, fontface = "bold", family = "serif") +
  scale_x_continuous(breaks = -6:6) +
  scale_y_continuous(limits = c(-0.10, 0.25), breaks = seq(-0.10, 0.25, 0.05)) +
  labs(title = expression(Delta*S %->% F^{DI}),
       x = "Lag (weeks)", y = NULL) +
  theme_pub

fig <- p_cp + p_di +
  plot_annotation(
    subtitle = "Positive lag: containment change leads fiscal deployment",
    theme = theme(
      plot.subtitle = element_text(size = 9, color = "grey35", hjust = 0.5, family = "serif")
    )
  )

ggsave(file.path(safeplots, "fig_crosscorr_weekly.pdf"), fig, width = 10, height = 4)
cat("Saved:", file.path(safeplots, "fig_crosscorr_weekly.pdf"), "\n")
