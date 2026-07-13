# ==============================================================================
#  descriptives_superseded.R — superseded code moved out of descriptives.R
# ==============================================================================
#  Moved out of the replication package on 2026-07-12 during cleaning.
#  These blocks were superseded by other code in descriptives.R and are NOT
#  used by anything in the replication pipeline. They are preserved verbatim
#  for reference only. This file is NOT meant to be sourced.
# ==============================================================================


# ------------------------------------------------------------------------------
#  Block 1 — ccf()-based cross-correlation S vs. d (from descriptives.R,
#  Excess Deaths Section 8 "Lag structure S -> theta -> d", originally
#  located immediately BEFORE the definition of agg_sd).
#
#  Why superseded: it references agg_sd before agg_sd is created, so it cannot
#  run in a clean top-to-bottom session. The manual lag-correlation computation
#  (lag_corrs) retained in descriptives.R replaces it and produces the
#  cross-correlogram figure p_ccf used for the analysis.
# ------------------------------------------------------------------------------

# Berechnet alle Lags von -10 bis +10 automatisch (plot = FALSE unterdrückt den direkten Plot)
cross_corr <- ccf(agg_sd$S, agg_sd$d, lag.max = 10, na.action = na.pass, plot = FALSE)

# Falls du es als Tibble brauchst, kannst du es direkt umwandeln:
lag_corrs_ccf <- tibble(
  lag = as.vector(cross_corr$lag),
  r   = as.vector(cross_corr$acf)
) %>%
  filter(lag >= -8) # Filtert es auf deine gewünschte Range (-8 bis 10)

# ggplot initialisieren
ggplot(lag_corrs_ccf, aes(x = lag, y = r)) +
  # Vertikale Linien von 0 bis zum Korrelationswert (r)
  geom_segment(aes(xend = lag, yend = 0), color = "steelblue", linewidth = 1) +
  # Punkte an der Spitze der Linien
  geom_point(color = "steelblue", size = 3) +
  # Horizontale Nulllinie zur besseren Orientierung
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  # Das Aussehen etwas aufgeräumter machen
  theme_minimal() +
  # Beschriftungen hinzufügen
  labs(
    title = "Kreuzkorrelation zwischen S und d",
    subtitle = "Lags von -8 bis +10",
    x = "Lag",
    y = "Korrelation (r)"
  ) +
  # Die x-Achse so einstellen, dass jeder Lag-Schritt angezeigt wird
  scale_x_continuous(breaks = -8:10)
