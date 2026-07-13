

# ==============================================================================
#  PART B: BACK-CALCULATE S^eff AND COMPARE TO S^oxford
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  PART B: S^eff vs. S^oxford\n")
cat(strrep("=", 70), "\n\n")

# --- B1. Logic -----------------------------------------------------------------
#
# The θ-equation is:  θ_{w+1} = ρ_θ · (1 - φ_S · S^eff_w) · θ_w
#
# S^eff is the EFFECTIVE suppression: the total behavioral reduction in
# transmission, including both mandated NPI (S^oxford) and voluntary
# fear-driven behavioral change.
#
# Solving for S^eff:
#
#   S^eff_w = (1 - θ_{w+1} / (ρ_θ · θ_w)) / φ_S
#
# If S^eff ≈ S^oxford:  The Oxford Stringency Index captures essentially
#   all of the behavioral response. The fear channel operates through S,
#   not independently of it. → Option 2 (no separate α_θ term) defensible.
#
# If S^eff > S^oxford systematically:  There is a substantial voluntary
#   behavioral component beyond mandates. → Option 1 (α_θ term) required.
#
# IMPORTANT: This test uses WEEKLY data where the θ-dynamics are most
# informative. Not applicable to the three monthly countries.

cat("--- B1. Back-Calculating S^eff from θ-Equation ---\n\n")
cat("  Formula: S^eff_w = (1 - θ̂_{w+1} / (ρ_θ · θ̂_w)) / φ_S\n")
cat("  Interpretation:\n")
cat("    S^eff > S^oxford  →  voluntary behavior adds suppression beyond mandates\n")
cat("    S^eff ≈ S^oxford  →  mandates capture total behavioral response\n")
cat("    S^eff < S^oxford  →  mandates not fully effective (compliance < 100%)\n\n")

# --- B2. Compute S^eff for weekly countries ------------------------------------

seff_data <- panel_w %>%
  filter(
    date >= as.Date("2020-03-01"), date <= as.Date("2021-12-31"),
    !is.na(theta_hat), theta_hat > 0,
    !is.na(rho_w), !is.na(S_mean)
  ) %>%
  arrange(Country, date) %>%
  group_by(Country) %>%
  mutate(
    theta_next = lead(theta_hat, 1)
  ) %>%
  ungroup() %>%
  filter(!is.na(theta_next), theta_next > 0) %>%
  mutate(
    # Back out S^eff from θ-equation
    growth_ratio = theta_next / (rho_w * theta_hat),
    S_eff        = (1 - growth_ratio) / phi_S_central,
    # Oxford stringency (already in [0,1])
    S_oxford     = S_mean,
    # Difference: voluntary behavioral component
    S_voluntary  = S_eff - S_oxford,
    # Wave label for stratification
    wave_label = factor(wave_label,
                        levels = c("Wave 1 (Original)","Interwave 1",
                                   "Wave 2 (Alpha)","Interwave 2",
                                   "Wave 3 (Delta)","Wave 4 (Omicron)"))
  )

cat(sprintf("  Sample: %d country-weeks (35 weekly countries)\n\n",
            nrow(seff_data)))

# --- B3. Winsorize extreme values ----------------------------------------------
# S^eff can be noisy when θ̂ is near zero (division by small number).
# Winsorize at 1st/99th percentile for robust statistics.

p01 <- quantile(seff_data$S_eff, 0.01, na.rm = TRUE)
p99 <- quantile(seff_data$S_eff, 0.99, na.rm = TRUE)

seff_data <- seff_data %>%
  mutate(
    S_eff_w   = pmin(pmax(S_eff, p01), p99),
    S_vol_w   = S_eff_w - S_oxford
  )

cat(sprintf("  Winsorized S^eff: [%.3f, %.3f] (P1/P99)\n\n", p01, p99))

# --- B4. Overall comparison statistics -----------------------------------------

cat("--- B4. S^eff vs. S^oxford: Summary Statistics ---\n\n")

overall_stats <- seff_data %>%
  summarise(
    mean_S_eff    = mean(S_eff_w, na.rm = TRUE),
    mean_S_oxford = mean(S_oxford, na.rm = TRUE),
    mean_diff     = mean(S_vol_w, na.rm = TRUE),
    median_diff   = median(S_vol_w, na.rm = TRUE),
    sd_diff       = sd(S_vol_w, na.rm = TRUE),
    corr          = cor(S_eff_w, S_oxford, use = "complete.obs"),
    pct_seff_gt   = mean(S_vol_w > 0, na.rm = TRUE) * 100
  )

cat(sprintf("  Mean S^eff:     %.3f\n", overall_stats$mean_S_eff))
cat(sprintf("  Mean S^oxford:  %.3f\n", overall_stats$mean_S_oxford))
cat(sprintf("  Mean gap:       %.3f  (S^eff - S^oxford)\n", overall_stats$mean_diff))
cat(sprintf("  Median gap:     %.3f\n", overall_stats$median_diff))
cat(sprintf("  SD of gap:      %.3f\n", overall_stats$sd_diff))
cat(sprintf("  Correlation:    %.3f\n", overall_stats$corr))
cat(sprintf("  S^eff > S^oxf:  %.1f%% of obs\n\n", overall_stats$pct_seff_gt))

cat("  Decision rule:\n")
cat("    corr > 0.8 AND |mean gap| < 0.1  →  S^eff ≈ S^oxford (Option 2 defensible)\n")
cat("    corr < 0.6 OR  mean gap > 0.15   →  Substantial fear channel (Option 1 needed)\n\n")

# --- B5. By-wave comparison ----------------------------------------------------

wave_seff <- seff_data %>%
  group_by(wave, wave_label) %>%
  summarise(
    N             = n(),
    mean_S_eff    = mean(S_eff_w, na.rm = TRUE),
    mean_S_oxford = mean(S_oxford, na.rm = TRUE),
    mean_gap      = mean(S_vol_w, na.rm = TRUE),
    median_gap    = median(S_vol_w, na.rm = TRUE),
    corr          = cor(S_eff_w, S_oxford, use = "complete.obs"),
    pct_seff_gt   = mean(S_vol_w > 0, na.rm = TRUE) * 100,
    .groups       = "drop"
  )

cat("--- B5. S^eff vs. S^oxford by Wave ---\n")
print(kable(wave_seff %>% select(wave_label, N, mean_S_eff, mean_S_oxford,
                                 mean_gap, corr, pct_seff_gt),
            digits = 3,
            col.names = c("Wave","N","S^eff","S^oxf","Gap","Corr","% S^eff>S^oxf")))

cat("\n  Key pattern to look for:\n")
cat("  • Wave 1: Gap likely POSITIVE and LARGE — peak fear, S^oxford alone\n")
cat("    cannot explain the observed transmission decline (Goolsbee & Syverson 2021).\n")
cat("  • Wave 4 (Omicron): Gap likely SMALLER — pandemic fatigue, less\n")
cat("    voluntary behavioral response despite high θ.\n")
cat("  • If gap is NEGATIVE: mandates not fully complied with (S^eff < S^oxford).\n\n")

# --- B6. Cross-country heterogeneity -------------------------------------------

country_seff <- seff_data %>%
  group_by(Country) %>%
  summarise(
    mean_gap    = mean(S_vol_w, na.rm = TRUE),
    median_gap  = median(S_vol_w, na.rm = TRUE),
    corr        = cor(S_eff_w, S_oxford, use = "complete.obs"),
    .groups     = "drop"
  ) %>%
  arrange(desc(mean_gap))

cat("--- B6. Cross-Country Heterogeneity (Top/Bottom 5 by Gap) ---\n\n")
cat("  Largest voluntary component (S^eff >> S^oxford):\n")
print(kable(head(country_seff, 5), digits = 3,
            col.names = c("Country","Mean Gap","Median Gap","Corr")))
cat("\n  Smallest gap / most mandate-driven:\n")
print(kable(tail(country_seff, 5), digits = 3,
            col.names = c("Country","Mean Gap","Median Gap","Corr")))

# --- B7. Formal regression test ------------------------------------------------
# Regress S^eff on S^oxford. If β ≈ 1 and intercept ≈ 0: perfect alignment.
# β > 1: mandates amplify voluntary behavior. β < 1: partial compliance.
# Intercept > 0: baseline voluntary caution even at S^oxford = 0.

cat("\n--- B7. Formal Regression: S^eff = α + β · S^oxford + ε ---\n\n")

reg_pooled <- lm(S_eff_w ~ S_oxford, data = seff_data)
cat("  Pooled OLS:\n")
cat(sprintf("    Intercept (α): %.3f (SE: %.3f) — baseline voluntary suppression\n",
            coef(reg_pooled)[1], summary(reg_pooled)$coefficients[1, 2]))
cat(sprintf("    Slope (β):     %.3f (SE: %.3f) — mandate amplification\n",
            coef(reg_pooled)[2], summary(reg_pooled)$coefficients[2, 2]))
cat(sprintf("    R²:            %.3f\n", summary(reg_pooled)$r.squared))

cat("\n  Interpretation:\n")
cat("    α > 0:   Voluntary suppression even without mandates (fear channel)\n")
cat("    β ≈ 1:   Each unit of mandate generates one unit of effective suppression\n")
cat("    β > 1:   Mandates amplify voluntary behavior (mandate signals danger)\n")
cat("    β < 1:   Imperfect compliance (not everyone follows mandates)\n")
cat("    R² high: S^oxford explains most of S^eff variation\n\n")

# With country FE (absorb cross-country baseline differences)
reg_fe <- lm(S_eff_w ~ S_oxford + factor(Country), data = seff_data)
cat(sprintf("  With country FE:\n"))
cat(sprintf("    Slope (β):     %.3f (SE: %.3f)\n",
            coef(reg_fe)["S_oxford"],
            summary(reg_fe)$coefficients["S_oxford", 2]))
cat(sprintf("    R²:            %.3f\n\n", summary(reg_fe)$r.squared))

# --- B8. Visualization ---------------------------------------------------------

# B8a: Scatter S^eff vs S^oxford
p_scatter <- seff_data %>%
  ggplot(aes(x = S_oxford, y = S_eff_w)) +
  geom_point(aes(color = wave_label), alpha = 0.15, size = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#C0392B",
              linewidth = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.7,
              fill = "grey80", alpha = 0.3) +
  scale_color_manual(
    values = c("grey20","grey55","grey30","grey60","grey25","grey45"),
    name = NULL
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(quantile(seff_data$S_eff_w, 0.01),
                                           quantile(seff_data$S_eff_w, 0.99))) +
  labs(
    title    = expression(paste("Figure X: ", S^eff, " vs. ", S^oxford)),
    subtitle = expression(paste(
      "Red dashed: 45° line (", S^eff, " = ", S^oxford, "). ",
      "Black: OLS fit. Points above line: voluntary suppression beyond mandates."
    )),
    x = expression(S^oxford ~ "(Oxford Stringency Index, rescaled [0,1])"),
    y = expression(S^eff ~ "(back-calculated from " * hat(theta) * "-dynamics)"),
    caption = paste0("Notes: S^eff = (1 − θ̂_{w+1}/(ρ_θ·θ̂_w)) / φ_S. ",
                     "Winsorized at P1/P99. 35 OECD countries, weekly.")
  ) +
  theme_aer +
  guides(color = guide_legend(nrow = 1, override.aes = list(alpha = 1, size = 2)))

print(p_scatter)

# B8b: Time series of gap (OECD mean)
gap_time <- seff_data %>%
  group_by(date) %>%
  summarise(
    mean_gap = mean(S_vol_w, na.rm = TRUE),
    p25_gap  = quantile(S_vol_w, 0.25, na.rm = TRUE),
    p75_gap  = quantile(S_vol_w, 0.75, na.rm = TRUE),
    .groups  = "drop"
  )

p_gap_time <- ggplot(gap_time, aes(x = date)) +
  geom_ribbon(aes(ymin = p25_gap, ymax = p75_gap), fill = "grey80", alpha = 0.5) +
  geom_line(aes(y = mean_gap), linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#C0392B", linewidth = 0.5) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(
    title    = expression(paste("Figure X: ", S^eff - S^oxford, " Over Time (OECD Mean)")),
    subtitle = "Positive: voluntary suppression exceeds mandated. Negative: imperfect compliance.",
    x = NULL,
    y = expression(S^eff - S^oxford),
    caption = paste0("Notes: Band = IQR across 35 countries. Gap > 0 indicates fear-driven\n",
                     "behavioral response beyond mandated restrictions.")
  ) + theme_aer

print(p_gap_time)

# B8c: By-wave boxplot of the gap
p_box_wave <- seff_data %>%
  filter(!is.na(wave_label)) %>%
  ggplot(aes(x = wave_label, y = S_vol_w)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#C0392B", linewidth = 0.5) +
  geom_boxplot(fill = "grey85", outlier.size = 0.3, outlier.alpha = 0.2) +
  labs(
    title    = expression(paste("Figure X: ", S^eff - S^oxford, " by Pandemic Wave")),
    subtitle = "Box: IQR. Line: median. Points: outliers. Red dashed: zero (mandates = behavior).",
    x = NULL,
    y = expression(S^eff - S^oxford),
    caption = "Notes: Positive gap indicates voluntary behavioral suppression beyond mandated NPIs."
  ) +
  theme_aer +
  theme(axis.text.x = element_text(angle = 25, hjust = 1, size = 7))

print(p_box_wave)

# --- B9. Summary assessment ----------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("  PART B: ASSESSMENT\n")
cat(strrep("=", 70), "\n\n")

cat("  Key statistics for paper:\n")
cat(sprintf("    Overall correlation(S^eff, S^oxford):  %.3f\n", overall_stats$corr))
cat(sprintf("    Overall mean gap (S^eff - S^oxford):   %.3f\n", overall_stats$mean_diff))
cat(sprintf("    Regression slope β:                    %.3f\n", coef(reg_pooled)[2]))
cat(sprintf("    Regression intercept α:                %.3f\n", coef(reg_pooled)[1]))
cat(sprintf("    Regression R²:                         %.3f\n\n",
            summary(reg_pooled)$r.squared))

cat("  DECISION FRAMEWORK:\n\n")

if (overall_stats$corr > 0.7 & abs(overall_stats$mean_diff) < 0.15) {
  cat("  → RESULT: S^eff ≈ S^oxford (high correlation, small gap)\n")
  cat("    Implication: Option 2 is defensible. S^oxford captures most of the\n")
  cat("    behavioral response. A separate α_θ term in the model is empirically\n")
  cat("    hard to identify. HOWEVER: this does NOT mean the fear channel is\n")
  cat("    absent—it means it runs THROUGH S (mandates and fear are collinear).\n")
  cat("    Model recommendation: Include α_θ in the specification, let the\n")
  cat("    data decide. Report that S^eff ≈ S^oxford as motivation for the\n")
  cat("    restricted specification (α_θ = 0) if Stage 2 confirms.\n")
} else if (overall_stats$mean_diff > 0.15) {
  cat("  → RESULT: S^eff > S^oxford (substantial voluntary component)\n")
  cat("    Implication: Option 1 is required. There is a fear channel that\n")
  cat("    operates INDEPENDENTLY of mandated S. The model needs α_θ · θ_k\n")
  cat("    in the output equation, and the empirics must control for θ̂_k\n")
  cat("    as a separate regressor.\n")
} else {
  cat("  → RESULT: Ambiguous. Moderate correlation or moderate gap.\n")
  cat("    Implication: Include α_θ in the model, let Stage 2 decide.\n")
}

cat("\n")
cat("  NOTE: Regardless of this test, the recommendation remains to include\n")
cat("  α_θ in the model and set α_θ = 0 as a data-driven restriction if the\n")
cat("  evidence supports it. This is strictly superior to assuming Option 2\n")
cat("  a priori, because the restriction is then TESTABLE rather than assumed.\n")
cat(strrep("=", 70), "\n")



# ==============================================================================
#  04c_theta_equation_quarterly_validation.R
#  Validate θ-equation on quarterly frequency
#
#  Run AFTER 04b (requires theta_quarterly_full, phi_S_central, theme_aer)
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  θ-EQUATION VALIDATION: QUARTERLY FREQUENCY\n")
cat(strrep("=", 70), "\n\n")

# ==============================================================================
#  1. PREPARE QUARTERLY θ AND S
# ==============================================================================

# Quarterly ρ_θ: convert weekly to quarterly
# If weekly growth factor is ρ_w, quarterly ≈ ρ_w^13 (13 weeks per quarter).
# BUT: this produces astronomical numbers (1.8^13 ≈ 3,900).
# The reduced-form quarterly ρ_θ must be MUCH smaller because susceptible
# depletion, behavioral adaptation, and within-wave peaks/troughs occur
# WITHIN the quarter. We need to calibrate ρ_θ^Q directly.
#
# Strategy: estimate ρ_θ^Q from the data rather than exponentiating weekly values.

val_q <- theta_quarterly_full %>%
  arrange(Country, YQ) %>%
  group_by(Country) %>%
  mutate(
    theta_next = lead(theta_mean, 1),
    S_next     = lead(S_mean, 1),
    YQ_next    = lead(YQ, 1)
  ) %>%
  ungroup() %>%
  filter(
    !is.na(theta_next), !is.na(theta_mean), !is.na(S_mean),
    theta_mean > 0, theta_next > 0
  )

cat(sprintf("  Validation sample: %d country-quarters (%d countries)\n\n",
            nrow(val_q), n_distinct(val_q$Country)))

# --- Observed quarterly growth ratio ------------------------------------------
val_q <- val_q %>%
  mutate(
    growth_ratio = theta_next / theta_mean,
    log_growth   = log(theta_next) - log(theta_mean)
  )

cat("--- Observed θ_{k+1}/θ_k Distribution ---\n")
cat(sprintf("  Mean:   %.2f\n", mean(val_q$growth_ratio)))
cat(sprintf("  Median: %.2f\n", median(val_q$growth_ratio)))
cat(sprintf("  SD:     %.2f\n", sd(val_q$growth_ratio)))
cat(sprintf("  Range:  [%.2f, %.2f]\n\n", min(val_q$growth_ratio),
            max(val_q$growth_ratio)))


# ==============================================================================
#  2. ESTIMATE ρ_θ^Q AND φ_S FROM QUARTERLY DATA
# ==============================================================================

cat("--- Estimating ρ_θ^Q and φ_S from Quarterly Data ---\n\n")

# The θ-equation in logs:
#   log(θ_{k+1}) = log(ρ_θ) + log(1 - φ_S · S_k) + log(θ_k)
#
# Rearranging:
#   log(θ_{k+1}/θ_k) = log(ρ_θ) + log(1 - φ_S · S_k)
#
# For small φ_S · S_k, log(1 - φ_S·S) ≈ -φ_S·S, giving the linear approx:
#   log(θ_{k+1}/θ_k) ≈ log(ρ_θ) - φ_S · S_k
#
# This is a simple regression of log growth on S.

# --- 2a. Linear approximation (pooled OLS) ------------------------------------

reg_pooled <- lm(log_growth ~ S_mean, data = val_q)

rho_Q_hat  <- exp(coef(reg_pooled)[1])
phi_S_hat  <- -coef(reg_pooled)[2]

cat("  Pooled OLS: log(θ_{k+1}/θ_k) = a + b·S_k\n")
cat(sprintf("    Intercept (log ρ_θ^Q):  %.3f  →  ρ_θ^Q = %.3f\n",
            coef(reg_pooled)[1], rho_Q_hat))
cat(sprintf("    Slope (-φ_S):           %.3f  →  φ_S   = %.3f\n",
            coef(reg_pooled)[2], phi_S_hat))
cat(sprintf("    R²:                     %.3f\n", summary(reg_pooled)$r.squared))
cat(sprintf("    N:                      %d\n\n", nrow(val_q)))

# --- 2b. With country fixed effects -------------------------------------------

reg_fe <- lm(log_growth ~ S_mean + factor(Country), data = val_q)

phi_S_fe <- -coef(reg_fe)["S_mean"]

cat("  Country FE: log(θ_{k+1}/θ_k) = a_i + b·S_k\n")
cat(sprintf("    Slope (-φ_S):           %.3f  →  φ_S   = %.3f\n",
            coef(reg_fe)["S_mean"], phi_S_fe))
cat(sprintf("    R²:                     %.3f\n\n", summary(reg_fe)$r.squared))

# --- 2c. With country AND time fixed effects ----------------------------------

val_q <- val_q %>%
  mutate(time_idx = as.numeric(YQ))

reg_twfe <- lm(log_growth ~ S_mean + factor(Country) + factor(YQ), data = val_q)

phi_S_twfe <- -coef(reg_twfe)["S_mean"]
se_twfe    <- summary(reg_twfe)$coefficients["S_mean", 2]

cat("  Two-way FE: log(θ_{k+1}/θ_k) = a_i + τ_k + b·S_k\n")
cat(sprintf("    Slope (-φ_S):           %.3f (SE: %.3f)  →  φ_S = %.3f\n",
            coef(reg_twfe)["S_mean"], se_twfe, phi_S_twfe))
cat(sprintf("    R²:                     %.3f\n\n", summary(reg_twfe)$r.squared))

cat("  NOTE: Time FE absorb ρ_θ variation across quarters (variant shifts,\n")
cat("  immunity buildup). The φ_S coefficient then identifies the WITHIN-\n")
cat("  quarter, CROSS-COUNTRY effect of S on transmission — the cleanest\n")
cat("  identification available.\n\n")

# --- 2d. By-wave estimation ---------------------------------------------------

cat("--- Wave-Specific Estimates ---\n\n")

wave_ests <- val_q %>%
  group_by(wave_dominant) %>%
  filter(n() >= 10) %>%
  summarise(
    N          = n(),
    mean_S     = mean(S_mean, na.rm = TRUE),
    mean_growth = mean(growth_ratio, na.rm = TRUE),
    # Regression within wave
    rho_Q  = tryCatch({
      m <- lm(log_growth ~ S_mean)
      exp(coef(m)[1])
    }, error = function(e) NA_real_),
    phi_S  = tryCatch({
      m <- lm(log_growth ~ S_mean)
      -coef(m)[2]
    }, error = function(e) NA_real_),
    R2     = tryCatch({
      m <- lm(log_growth ~ S_mean)
      summary(m)$r.squared
    }, error = function(e) NA_real_),
    .groups = "drop"
  )

print(kable(wave_ests, digits = 3,
            col.names = c("Wave","N","Mean S","Mean θ ratio",
                          "ρ_θ^Q","φ_S","R²")))


# ==============================================================================
#  3. PREDICTIVE FIT: θ̂_{k+1}^pred vs. θ̂_{k+1}^obs
# ==============================================================================

cat("\n--- Predictive Fit Using Estimated Parameters ---\n\n")

# Use pooled estimates for prediction
val_q <- val_q %>%
  mutate(
    # Predicted with estimated parameters
    theta_pred_est = rho_Q_hat * (1 - phi_S_hat * S_mean) * theta_mean,
    # Predicted with literature parameters (for comparison)
    theta_pred_lit = 3.0 * (1 - phi_S_central * S_mean) * theta_mean
  )

# Fit statistics — estimated parameters
r2_est      <- cor(log(val_q$theta_next), log(val_q$theta_pred_est),
                   use = "complete.obs")^2
rmse_est    <- sqrt(mean((val_q$theta_next - val_q$theta_pred_est)^2))
rmse_log_est <- sqrt(mean((log(val_q$theta_next) - log(val_q$theta_pred_est))^2))

cat(sprintf("  Estimated parameters (ρ_θ^Q = %.2f, φ_S = %.2f):\n",
            rho_Q_hat, phi_S_hat))
cat(sprintf("    R² (log):      %.3f\n", r2_est))
cat(sprintf("    RMSE (level):  %.6f\n", rmse_est))
cat(sprintf("    RMSE (log):    %.3f\n\n", rmse_log_est))

# Fit statistics — literature parameters
val_q_lit <- val_q %>% filter(theta_pred_lit > 0)
r2_lit    <- cor(log(val_q_lit$theta_next), log(val_q_lit$theta_pred_lit),
                 use = "complete.obs")^2

cat(sprintf("  Literature parameters (ρ_θ^Q = 3.0, φ_S = 0.70):\n"))
cat(sprintf("    R² (log):      %.3f\n\n", r2_lit))


# ==============================================================================
#  4. VISUALIZATIONS
# ==============================================================================

# --- 4a. Scatter: predicted vs observed θ_{k+1} (log scale) -------------------

p_fit_q <- val_q %>%
  ggplot(aes(x = log(theta_pred_est), y = log(theta_next))) +
  geom_point(aes(color = wave_dominant), alpha = 0.4, size = 1.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#C0392B",
              linewidth = 0.6) +
  labs(
    title    = expression(paste("Figure X: Quarterly ", theta, "-Equation Fit")),
    subtitle = bquote(paste(
      "log(", hat(theta)[k+1]^obs, ") vs. log(", hat(rho)[theta]^Q,
      "(1 - ", hat(phi)[S], " · ", S[k], ") · ", hat(theta)[k], "). ",
      R^2, " = ", .(round(r2_est, 3))
    )),
    x     = expression(paste("log(", theta[k+1]^predicted, ")")),
    y     = expression(paste("log(", hat(theta)[k+1]^observed, ")")),
    color = "Dominant wave",
    caption = paste0("Notes: Each point = country-quarter. Red dashed: 45° line.\n",
                     "Parameters estimated from quarterly data via log-linear regression.")
  ) + theme_aer

print(p_fit_q)

# --- 4b. log(θ growth) vs S: the identifying regression -----------------------

p_growth_S <- val_q %>%
  ggplot(aes(x = S_mean, y = log_growth)) +
  geom_point(aes(color = wave_dominant), alpha = 0.4, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.7,
              fill = "grey80", alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    title    = expression(paste("Figure X: Quarterly ", theta, "-Growth vs. Stringency")),
    subtitle = bquote(paste(
      "log(", theta[k+1], "/", theta[k], ") = log(", rho[theta]^Q,
      ") - ", phi[S], " · ", S[k],
      ".  Slope = -", .(round(phi_S_hat, 2)),
      ", intercept = log(", .(round(rho_Q_hat, 2)), ")"
    )),
    x     = expression(S[k]^oxford ~ "(quarterly mean stringency)"),
    y     = expression(paste("log(", theta[k+1], " / ", theta[k], ")")),
    color = "Dominant wave",
    caption = paste0("Notes: Negative slope = containment reduces infection growth.\n",
                     "Intercept = uncontrolled quarterly growth rate.\n",
                     "Points above y=0: infections growing. Below: declining.")
  ) + theme_aer

print(p_growth_S)

# --- 4c. Residual over time ---------------------------------------------------

val_q <- val_q %>%
  mutate(
    resid_log = log(theta_next) - log(theta_pred_est)
  )

resid_q_time <- val_q %>%
  group_by(YQ) %>%
  summarise(
    mean_resid = mean(resid_log, na.rm = TRUE),
    p25_resid  = quantile(resid_log, 0.25, na.rm = TRUE),
    p75_resid  = quantile(resid_log, 0.75, na.rm = TRUE),
    .groups    = "drop"
  )

p_resid_q <- ggplot(resid_q_time, aes(x = YQ, group = 1)) +
  geom_ribbon(aes(ymin = p25_resid, ymax = p75_resid), fill = "grey80", alpha = 0.5) +
  geom_line(aes(y = mean_resid), linewidth = 0.6) +
  geom_point(aes(y = mean_resid), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#C0392B") +
  labs(
    title = expression(paste("Figure X: Quarterly ", theta, "-Equation Residuals")),
    subtitle = "log(θ̂ observed) − log(θ predicted). Positive: model underpredicts growth.",
    x = NULL, y = "Log residual",
    caption = paste0("Notes: Band = IQR across countries. Systematic patterns indicate\n",
                     "time-varying ρ_θ (variant shifts) or φ_S (behavioral adaptation).")
  ) + theme_aer

print(p_resid_q)


# ==============================================================================
#  5. SUMMARY & IMPLICATIONS
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SUMMARY: QUARTERLY θ-EQUATION VALIDATION\n")
cat(strrep("=", 70), "\n\n")

cat("  ESTIMATED PARAMETERS:\n")
cat(sprintf("    ρ_θ^Q (pooled):     %.3f  (uncontrolled quarterly growth)\n", rho_Q_hat))
cat(sprintf("    φ_S   (pooled):     %.3f  (suppression effectiveness)\n", phi_S_hat))
cat(sprintf("    φ_S   (country FE): %.3f\n", phi_S_fe))
cat(sprintf("    φ_S   (two-way FE): %.3f\n\n", phi_S_twfe))

cat("  FIT:\n")
cat(sprintf("    R² (log, est. params):   %.3f\n", r2_est))
cat(sprintf("    R² (log, lit. params):   %.3f\n", r2_lit))
cat(sprintf("    RMSE (log):              %.3f\n\n", rmse_log_est))

cat("  INTERPRETATION:\n\n")

if (r2_est > 0.5) {
  cat("  → GOOD FIT: The quarterly θ-equation captures the dominant dynamics.\n")
  cat("    The calibrated parameters are usable for the iLQR optimization.\n")
  cat("    Residual variation reflects within-quarter noise and variant\n")
  cat("    transitions, absorbed by ε_k under certainty equivalence.\n\n")
} else if (r2_est > 0.2) {
  cat("  → MODERATE FIT: The θ-equation captures the direction but not the\n")
  cat("    magnitude of quarterly infection dynamics. Consider:\n")
  cat("    (a) Using wave-specific ρ_θ^Q (time-varying parameter)\n")
  cat("    (b) Adding immunity/vaccination as a separate state or modifier\n")
  cat("    (c) Treating ρ_θ as a free parameter estimated in Stage 2\n\n")
} else {
  cat("  → POOR FIT: The deterministic θ-equation does not capture quarterly\n")
  cat("    dynamics well. This does NOT invalidate the imputation (θ̂ comes\n")
  cat("    from mortality, not from the θ-equation). But it means:\n")
  cat("    (a) The θ-equation in the iLQR is a rough reduced form\n")
  cat("    (b) The optimal S trajectory from the model should be interpreted\n")
  cat("        as directional guidance, not precise prescription\n")
  cat("    (c) In Stage 2, θ̂_k enters as a CONTROL variable (absorbing\n")
  cat("        infection-driven endogeneity) — this role is unaffected by\n")
  cat("        the θ-equation fit, because identification comes from the\n")
  cat("        output and debt equations, not the infection equation\n\n")
}

cat("  KEY INSIGHT: The θ-equation serves TWO distinct roles:\n")
cat("    1. In the MODEL: governs the planner's infection forecast → S choice\n")
cat("    2. In the EMPIRICS: θ̂ is a CONTROL VARIABLE in Stage 2\n")
cat("  Role 2 is INDEPENDENT of the θ-equation fit. Even if R² is low,\n")
cat("  θ̂_k absorbs infection-driven variation in the output regression.\n")
cat("  Role 1 determines how precisely the iLQR optimum maps to reality.\n\n")

cat("  COMPARISON: WEEKLY vs. QUARTERLY FIT\n")
cat(sprintf("    Weekly S^eff backout:  R² = 0.003 (uninformative)\n"))
cat(sprintf("    Quarterly regression:  R² = %.3f (substantially better)\n", r2_est))
cat("    This confirms: quarterly aggregation smooths the noise that\n")
cat("    contaminated the weekly analysis. The model operates correctly\n")
cat("    at the frequency for which it was designed.\n")
cat(strrep("=", 70), "\n")





























# ============================================
# DEBT
# ============================================

##PLOT DES BESTANDES AN SCHULDEN
##Plots von unterschiedlichen Metriken-> anpassen

data_plot <- qdata %>%
  mutate(
    date = ymd(paste0(
      year_only, "-",
      dplyr::case_when(
        quarter_only == "Q1" ~ "01-01",
        quarter_only == "Q2" ~ "04-01",
        quarter_only == "Q3" ~ "07-01",
        quarter_only == "Q4" ~ "10-01"
      )
    ))
  )

# Plot der kombinierten Schulden-Metrik ab 2016 mit allen Quartalen
highlight_countries <- c("USA", "DEU", "CHN", "GBR", "FRA", "IRL")

ggplot(data_plot %>% filter(year_only >= 2016), aes(x = date, y = DebtN_combined)) +
  geom_line(aes(group = Country), alpha = 0.2, color = "gray70") +
  geom_line(
    data = data_plot %>% filter(Country %in% highlight_countries, year_only >= 2016),
    aes(color = Country),
    linewidth = 1.2
  ) +
  geom_vline(xintercept = as.Date("2020-01-01"), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  annotate("text", x = as.Date("2017-07-01"), y = 8,
           label = "Quartal-over-Quartal\nWachstum", color = "darkgreen", size = 3) +
  annotate("text", x = as.Date("2021-01-01"), y = 8,
           label = "Vergleich mit\nQuartal 2019", color = "darkred", size = 3) +
  labs(
    title = "Reale Staatsschulden (LC) – Kombinierte Metrik",
    subtitle = "Vor 2019: qoq | Ab 2019: Abweichung ggü. entsprechendem Quartal 2019",
    x = "Quartal",
    y = "Änderung (%)",
    color = "Land"
  ) +
  theme_minimal() +
  scale_x_date(
    date_breaks = "3 months",
    labels = function(x) paste0(lubridate::year(x), "-Q", lubridate::quarter(x))
  ) +
  scale_color_brewer(palette = "Set1") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##Plot Debt qoq
data_plot <- qdata %>%
  mutate(
    date = ymd(paste0(
      year_only, "-",
      dplyr::case_when(
        quarter_only == "Q1" ~ "01-01",
        quarter_only == "Q2" ~ "04-01",
        quarter_only == "Q3" ~ "07-01",
        quarter_only == "Q4" ~ "10-01"
      )
    ))
  )

# Plot der kombinierten Schulden-Metrik ab 2016 mit allen Quartalen
highlight_countries <- c("USA", "DEU", "CHN", "GBR", "FRA", "IRL")

ggplot(data_plot %>% filter(year_only >= 2016), aes(x = date, y = Debt_qoq)) +
  geom_line(aes(group = Country), alpha = 0.2, color = "gray70") +
  geom_line(
    data = data_plot %>% filter(Country %in% highlight_countries, year_only >= 2016),
    aes(color = Country),
    linewidth = 1.2
  ) +
  geom_vline(xintercept = as.Date("2020-01-01"), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  annotate("text", x = as.Date("2017-07-01"), y = 8,
           label = "Quartal-over-Quartal\nWachstum", color = "darkgreen", size = 3) +
  annotate("text", x = as.Date("2021-01-01"), y = 8,
           label = "Vergleich mit\nQuartal 2019", color = "darkred", size = 3) +
  labs(
    title = "Reale Staatsschulden (LC) – Kombinierte Metrik",
    subtitle = "Vor 2019: qoq | Ab 2019: Abweichung ggü. entsprechendem Quartal 2019",
    x = "Quartal",
    y = "Änderung (%)",
    color = "Land"
  ) +
  theme_minimal() +
  scale_x_date(
    date_breaks = "3 months",
    labels = function(x) paste0(lubridate::year(x), "-Q", lubridate::quarter(x))
  ) +
  scale_color_brewer(palette = "Set1") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##Plot Debt_combined
# Optional: Durchschnitt + 95%-KI für die kombinierte Metrik
avg_debt_combined <- data_plot %>%
  filter(year_only >= 2016) %>%
  group_by(date) %>%
  summarise(
    avg = mean(DebtN_combined, na.rm = TRUE),
    se  = sd(DebtN_combined, na.rm = TRUE) / sqrt(sum(!is.na(DebtN_combined))),
    upper = avg + 1.96 * se,
    lower = avg - 1.96 * se,
    .groups = "drop"
  )


ggplot(avg_debt_combined, aes(x = date, y = avg)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = as.Date("2019-10-01"), linetype = "dotted") +
  labs(
    title = "Durchschnittliche kombinierte Schuldenquote-Metrik",
    subtitle = "95%-Konfidenzintervall, ab 2016",
    x = "Quartal", y = "Änderung (%)"
  ) +
  theme_minimal() +
  scale_x_date(
    date_breaks = "3 months",
    labels = function(x) paste0(lubridate::year(x), "-Q", lubridate::quarter(x))
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##Plot average qoq
# Optional: Durchschnitt + 95%-KI für die kombinierte Metrik
avg_debt_combined <- data_plot %>%
  filter(year_only >= 2016) %>%
  group_by(date) %>%
  summarise(
    avg = mean(Debt_qoq, na.rm = TRUE),
    se  = sd(Debt_qoq, na.rm = TRUE) / sqrt(sum(!is.na(Debt_qoq))),
    upper = avg + 1.96 * se,
    lower = avg - 1.96 * se,
    .groups = "drop"
  )


ggplot(avg_debt_combined, aes(x = date, y = avg)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = as.Date("2019-10-01"), linetype = "dotted") +
  labs(
    title = "Durchschnittliche kombinierte Schuldenquote-Metrik",
    subtitle = "95%-Konfidenzintervall, ab 2016",
    x = "Quartal", y = "Änderung (%)"
  ) +
  theme_minimal() +
  scale_x_date(
    date_breaks = "3 months",
    labels = function(x) paste0(lubridate::year(x), "-Q", lubridate::quarter(x))
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



##STAATSVERSCHULDUNGQUOTE PLOTEN
# Datum aus TimeIndex
data_plot <- qdata %>%
  mutate(
    year_only   = 2015L + ((TimeIndex - 1L) %/% 4L),
    quarter_num = ((TimeIndex - 1L) %% 4L) + 1L,
    date = ymd(paste0(year_only, "-", c("01-01","04-01","07-01","10-01")[quarter_num]))
  )



##OVERALL MOTIVATION OF THE DIFFERENCES BETWEEN GROUPS
# -----------------------------------------------------------------------------
# 1. Setup & Libraries
# -----------------------------------------------------------------------------
# Falls nicht installiert: install.packages(c("tidyverse", "fixest", "modelsummary", "lubridate"))

library(tidyverse)
library(fixest)       # Goldstandard für Panel/DiD Regressionen
library(modelsummary) # Für schöne Output-Tabellen
library(lubridate)

# Daten laden (Angenommen qdata2 ist bereits im Environment)
df <- qdata

# -----------------------------------------------------------------------------
# 2. Data Cleaning & Definition der "Shock Groups" (IV Strategie)
# -----------------------------------------------------------------------------



# Schritt A: Identifikation des "Initial Shock" (Q1 2020)
# Wir normalisieren Fälle durch Bevölkerung, um vergleichbar zu sein
shock_base <- df %>%
  filter(Quarter == "Q1.2020") %>%
  mutate(cases_per_capita = ConfirmedCases.a / Qpopulation_th) %>%
  select(Country, cases_per_capita)

# Schritt B: Median Split (High vs. Low Shock Group)
median_shock <- median(shock_base$cases_per_capita, na.rm = TRUE)

shock_base <- shock_base %>%
  mutate(
    # 1 = Land hatte "Pech" (früh hart getroffen), 0 = "Glück"
    HighShock_Group = ifelse(cases_per_capita > median_shock, 1, 0)
  )

# Schritt C: Zurück zum Panel mergen
df <- left_join(df, select(shock_base, Country, HighShock_Group), by = "Country")

# Behandlung der NAs (falls Länder keine Q1 Daten hatten, fallen sie raus)
df <- df %>% filter(!is.na(HighShock_Group))

df %>%
  filter(HighShock_Group == 1) %>%
  distinct(Country) %>%
  arrange(Country)

# Label für Grafiken
df$GroupLabel <- ifelse(df$HighShock_Group == 1, "High Initial Shock", "Low Initial Shock")

# -----------------------------------------------------------------------------
# 3. Deskriptive Analyse: Das Trilemma Visualisieren (Event Study Plots)
# -----------------------------------------------------------------------------
# Hier zeigen wir die 3 Kurven (y, d, b) im Vergleich

# Theme für Plots
my_theme <- theme_minimal() + theme(legend.position = "bottom")

# A. Gesundheit (Excess Deaths) - Variable: cum_excess_per_million_proj_all_ages
p1 <- ggplot(df, aes(x = TimeIndex, y = cum_excess_per_million_proj_all_ages, color = GroupLabel)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  labs(title = "Health Damage (d)", y = "Cumulative Excess Deaths per Million", x = "") +
  my_theme

print(p1)

# B. Wirtschaft (Output Gap) - Variable: y_t (Achtung: Interpretation prüfen!)
# Annahme: y_t ist Output Gap (negativ = schlecht). Dein Modell sagt y = Schaden (positiv).
# Wir plotten hier den Output Gap direkt.
p2 <- ggplot(df, aes(x = TimeIndex, y = y_t, color = GroupLabel)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  labs(title = "Economic Activity (y)", y = "Output Gap (y_t)", x = "") +
  my_theme

print(p2)
# C. Fiskal (Schuldenwachstum) - Variable: Debt_growth_vs_2019_n
p3 <- ggplot(df, aes(x = TimeIndex, y = d_t, color = GroupLabel)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  labs(title = "Fiscal Damage (b)", y = "Debt Growth vs 2019", x = "") +
  my_theme

# Anzeigen (Trilemma sollte sichtbar sein: High Shock -> Low GDP -> High Debt)

print(p3)

#-------------------------------------------------------------------------------
##Identification Strategy - Policy Reaction Functions
#-------------------------------------------------------------------------------


# ==============================================================================
# Policy-Reaktionsfunktion auf TAGESBASIS
# Datensatz: oxd_d (Oxford COVID-19 Government Response Tracker)
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Datenaufbereitung
# ------------------------------------------------------------------------------
##Wann war der erste Case pro Land?
first_case_day <- oxd_d %>%
  filter(ConfirmedCases > 0 & !is.na(ConfirmedCases)) %>%
  group_by(Country) %>%
  summarise(
    first_case_date = min(Date),
    first_case_count = first(ConfirmedCases[Date == min(Date)]),
    .groups = "drop"
  ) %>%
  arrange(first_case_date)

print(first_case_day, n = 40)

#Decision-> Start where the first case war minus 7 Tage
class(df)
# Population pro Land (muss ggf. aus anderem Datensatz kommen)
# Falls nicht vorhanden, aus Quartalsdaten extrahieren:
pop_data <- df %>%
  filter(Quarter == "Q1.2020") %>%
  select(Country, population = Qpopulation_th) %>%
  mutate(population = population * 1000)  # in Personen, nicht Tausend

##make real new daily data aus den cummulativ values

df_daily <- oxd_d %>%
  left_join(pop_data, by = "Country") %>%
  filter(Date >= as.Date("2020-01-15") & Date <= as.Date("2022-01-01")) %>%
  group_by(Country) %>%
  arrange(Date) %>%
  mutate(
    # 1) daily new aus kumulativ
    new_cases  = pmax(ConfirmedCases  - lag(ConfirmedCases),  0),
    new_deaths = pmax(ConfirmedDeaths - lag(ConfirmedDeaths), 0),
    
    # 2) 7d rolling mean auf daily new (nicht kumulativ!)
    cases_7d  = rollmean(new_cases,  7, fill = NA, align = "right"),
    deaths_7d = rollmean(new_deaths, 7, fill = NA, align = "right"),
    
    # 3) pro 100k
    cases_7d_pc  = (cases_7d  / population) * 1e5,
    deaths_7d_pc = (deaths_7d / population) * 1e5,
    cases_pc     = (new_cases / population) * 1e5,
    deaths_pc    = (new_deaths / population) * 1e5,
    
    # 4) logs
    log_cases_7d  = log(cases_7d_pc  + 1),
    log_deaths_7d = log(deaths_7d_pc + 1),
    
    # 5) lags
    lag7_log_cases   = lag(log_cases_7d, 7),
    lag7_log_deaths  = lag(log_deaths_7d, 7),
    lag14_log_cases  = lag(log_cases_7d, 14),
    lag14_log_deaths = lag(log_deaths_7d, 14),
    lag21_log_cases  = lag(log_cases_7d, 21),
    lag21_log_deaths = lag(log_deaths_7d, 21),
    
    # 6) week-over-week changes
    delta_log_cases_7d   = log_cases_7d - lag(log_cases_7d, 7),
    delta_log_deaths_7d  = log_deaths_7d - lag(log_deaths_7d, 7),
    delta_log_cases_14d  = log_cases_7d - lag(log_cases_7d, 14),
    
    # 7) stringency dynamics
    lag1_stringency  = lag(StringencyIndex_PopWeighted, 1),
    lag7_stringency  = lag(StringencyIndex_PopWeighted, 7),
    lag14_stringency = lag(StringencyIndex_PopWeighted, 14),
    lag21_stringency = lag(StringencyIndex_PopWeighted, 21),
    
    delta_stringency    = StringencyIndex_PopWeighted - lag(StringencyIndex_PopWeighted, 1),
    delta_stringency_7d = StringencyIndex_PopWeighted - lag(StringencyIndex_PopWeighted, 7)
  ) %>%
  ungroup() %>%
  mutate(
    country_id = as.factor(Country),
    week_id    = as.factor(floor_date(Date, "week")),
    month_id   = as.factor(floor_date(Date, "month")),
    year_week  = paste0(year(Date), "-W", sprintf("%02d", isoweek(Date)))
  )

# ------------------------------------------------------------------------------
# 2. Deskriptive Checks
# ------------------------------------------------------------------------------

cat("============================================================\n")
cat("DATENÜBERSICHT\n")
cat("============================================================\n")
cat("Beobachtungen:", nrow(df_daily), "\n")
cat("Länder:", n_distinct(df_daily$Country), "\n")
cat("Zeitraum:", as.character(min(df_daily$Date)), "bis", 
    as.character(max(df_daily$Date)), "\n")
cat("Tage pro Land (Median):", 
    median(table(df_daily$Country)), "\n\n")

# Fehlende Werte
cat("Fehlende Werte:\n")
df_daily %>%
  summarise(
    cases_na = sum(is.na(ConfirmedCases)),
    deaths_na = sum(is.na(ConfirmedDeaths)),
    stringency_na = sum(is.na(StringencyIndex_PopWeighted)),
    lag7_cases_na = sum(is.na(lag7_log_cases))
  ) %>%
  print()

# ==============================================================================
# IDENTIFICATION STRATEGY: Policy-Reaktionsfunktion: Verschiedene Spezifikationen
# ------------------------------------------------------------------------------

##OHNE WEEK UND YEAR FE-> MULITKOLLINEARITÄT
cat("\n============================================================\n")
cat("POLICY-REAKTIONSFUNKTION: STRINGENCY (Tagesbasis)\n")
cat("============================================================\n\n")

# ---- Modell D1: Nur Gesundheitsvariablen, 7-Tage Lag ----
prf_d1 <- feols(
  StringencyIndex_PopWeighted ~ 
    lag7_log_cases + lag7_log_deaths |
    country_id + week_id,
  data = df_daily,
  cluster = ~country_id
)

# ---- Modell D2: 14-Tage Lag ----
prf_d2 <- feols(
  StringencyIndex_PopWeighted ~ 
    lag14_log_cases + lag14_log_deaths |
    country_id + week_id,
  data = df_daily,
  cluster = ~country_id
)

# ---- Modell D3: 7-Tage Lag + Änderungsdynamik ----
prf_d3 <- feols(
  StringencyIndex_PopWeighted ~ 
    lag7_log_cases + lag7_log_deaths +
    delta_log_cases_7d + delta_log_deaths_7d |
    country_id + week_id,
  data = df_daily,
  cluster = ~country_id
)

# ---- Modell D4: Mit AR(1) ----
prf_d4 <- feols(
  StringencyIndex_PopWeighted ~ 
    lag1_stringency +
    lag7_log_cases + lag7_log_deaths |
    country_id + week_id,
  data = df_daily,
  cluster = ~country_id
)
##HIGHEST WITHIN ABER AUTOKORRELATION

# ---- Modell D5: Mit AR(7) ----
prf_d5 <- feols(
  StringencyIndex_PopWeighted ~ 
    lag7_stringency +
    lag7_log_cases + lag7_log_deaths |
    country_id + week_id,
  data = df_daily,
  cluster = ~country_id
)

# ---- Modell D6: APersistenz 7 Tage + Dynamik (bevorzugte Spezifikation) ----
prf_d6 <- feols(
  StringencyIndex_PopWeighted ~ 
    lag7_stringency +
    lag14_log_cases + lag7_log_deaths +
    delta_log_cases_14d +delta_log_deaths_7d |
    country_id + week_id,
  data = df_daily,
  cluster = ~country_id
)

##GEFAHR KOLLINEARITÄT
df_daily |> 
  dplyr::select(lag14_log_cases, lag7_log_deaths, delta_log_cases_14d, delta_log_deaths_7d, lag7_stringency) |>
  dplyr::mutate(dplyr::across(everything(), as.numeric)) |>
  stats::cor(use = "pairwise.complete.obs")
#TEST 1: ONLY LEVELS

prf_d6_R1 <- feols(
  StringencyIndex_PopWeighted ~ 
    lag7_stringency +
    lag7_log_cases + lag7_log_deaths|
    country_id + week_id,
  data = df_daily,
  cluster = ~country_id
)

etable(
  prf_d6, prf_d6_R1,
  dict = c(
    lag7_stringency   = "persistenz Stringency (t-7)",
    lag7_log_cases    = "log cases (t-7)",
    lag7_log_deaths   = "log deaths (t-7)",
    delta_log_cases_7d = "Δ log cases (t,t-7)"
  ),
  fitstat = ~ n + r2 + wr2
)

#DAS LEVEL AN DEATHS BESTIMMT DIE REAKTION NICHT DIE VERÄNDERUNG (WIR HABEN SCHON SOVIELE TOTE...)-> WITHIN PRAKTISCH GLEICH
#cases drive policy responses; deaths add limited incremental explanatory power once dynamics are controlled for.”

#TEST 2: NUR DELTA

prf_d6_R2 <- feols(
  StringencyIndex_PopWeighted ~ 
    delta_log_cases_7d + delta_log_deaths_7d|
    country_id + week_id,
  data = df_daily,
  cluster = ~country_id
)

etable(
  prf_d6, prf_d6_R2,
  dict = c(
    lag7_stringency   = "Stringency (t-7)",
    lag7_log_cases    = "log cases (t-7)",
    lag7_log_deaths   = "log deaths (t-7)",
    delta_log_deaths_7d = "Δ log deaths (t,t-7)",
    delta_log_cases_7d = "Δ log cases (t,t-7)"
  ),
  fitstat = ~ n + r2 + wr2
)

#Level-/Zustands-Phänomen (Niveau der Lage + Trägheit)-> STEIGUNG ERKLÄRT FAST NICHTS (TIEFES WITHIN)

#D6 it is
#Kontrolliert für:
#Persistenz (AR-Term) → institutionelle Trägheit raus
#Infektionsdynamik (delta_cases) → Reaktion auf "Kurve steigt" erfasst
#Niveau (log_cases, log_deaths) → Kontextvariablen
#lag7_Stringency am besten-> RObustness check zeigen wie es sich verhält-> Within R2 sinkt auch
# Geschätzte Halbwertszeit berechnen
ar_coef <- 0.858  # Dein AR(7)-Koeffizient
half_life_weeks <- log(0.5) / log(ar_coef)
half_life_days <- half_life_weeks * 7

cat("Halbwertszeit der Stringency:", round(half_life_days, 1), "Tage\n")

# ---- Modell D7: Distributed Lags (alle Horizonte) ----
prf_d7 <- feols(
  StringencyIndex_PopWeighted ~ 
    lag7_log_cases + lag14_log_cases + lag21_log_cases +
    lag7_log_deaths + lag14_log_deaths + lag21_log_deaths |
    country_id + week_id,
  data = df_daily,
  cluster = ~country_id
)

# Ergebnistabelle
modelsummary(
  list(
    "D1: 7d Lag" = prf_d1,
    "D2: 14d Lag" = prf_d2,
    "D3: 7d + Δ" = prf_d3,
    "D4: AR(1)" = prf_d4,
    "D5: AR(7)" = prf_d5,
    "D6: AR(7)+Δ" = prf_d6,
    "D7: Alle lags" = prf_d7
  ),
  stars = TRUE,
  gof_map = c("nobs", "r.squared", "within.r.squared"),
  title = "Policy-Reaktionsfunktion: Stringency (Tagesbasis)"
)


##Distributed Lag Modell separat-> WITH ROLLING AVERAGE DATA
prf_dl2 <- feols(
  StringencyIndex_PopWeighted ~ l(log_cases_7d, 0:21) + l(log_deaths_7d, 0:42) |
    country_id + week_id,
  data = df_daily,
  panel.id = ~country_id + Date,
  cluster = ~country_id
)


cat("Zeigt Reaktionshorizont der Regierungen:\n\n")
summary(prf_dl2)

# Cases-Lags 0:21
coefplot(prf_dl2,
         keep = "l\\(log_cases_7d,",
         ci_level = 0.95,
         main = "Distributed lags: log cases (0–21) → Stringency",
         xlab = "Lag term",
         ylab = "Coefficient")

# Deaths-Lags 0:21
coefplot(prf_dl2,
         keep = "l\\(log_deaths_7d,",
         ci_level = 0.95,
         main = "Distributed lags: log deaths (0–21) → Stringency",
         xlab = "Lag term",
         ylab = "Coefficient")


##Alle R2 within
# Within R² für alle Modelle
cat("\n---- Within R² Vergleich ----\n")
tibble(
  Modell = c("D1: 7d Lag", "D2: 14d Lag", "D3: 7d + Δ", 
             "D4: AR(1)", "D5: AR(7)", "D6: AR(7)+Δ", "D7: Distributed"),
  Within_R2 = c(
    fitstat(prf_d1, "wr2")[[1]],
    fitstat(prf_d2, "wr2")[[1]],
    fitstat(prf_d3, "wr2")[[1]],
    fitstat(prf_d4, "wr2")[[1]],
    fitstat(prf_d5, "wr2")[[1]],
    fitstat(prf_d6, "wr2")[[1]],
    fitstat(prf_d7, "wr2")[[1]]
  )
) %>%
  mutate(Within_R2 = round(Within_R2, 4)) %>%
  print()

##D6 hier das beste Modell! Within R2 von
##wenn nur bis Ende 2020 Modell bleibt stabil->within sinkt sogar-> Mehr chaotisch, weniger persistent


#MISST ETWAS ANDERES-< POLICY ENTSCHEIDUNGEN DIE NICHT VON DER STÄRKE VON DER PANDEMIE DETERMINIERT WURDE UND SICH SOMIT UNTERSCHEIDEN
##SPANNEND ABER SPEZIFIKATION NOCH ANSCHAUEN; EVTL ANPASSEN, VERSTEHEN. Was kann ich damit zeigen?? 
#Wie sich die POlitik untershciedlich verhalten hat unabhängig von Druck? Ist das bereinigt? LOGIK
##Nur 2020 da nachher sehr stabil, welche lags und wie, auch basierend auf der Literatur


# =============================================================================
##NEXT: MINIMIZING OMITTED VARIABLE BIAS
# ==============================================================================
# FIRST: Google Mobility Daten aufbereiten und integrieren
# ==============================================================================
# ------------------------------------------------------------------------------
# 1. Von Long zu Wide pivotieren
# ------------------------------------------------------------------------------

mobility_wide <- google_mobility %>%
  # Kategorie-Namen bereinigen für Spaltennamen
  mutate(
    place_clean = case_when(
      place == "Grocery and pharmacy" ~ "grocery",
      place == "Parks" ~ "parks",
      place == "Residential" ~ "residential",
      place == "Retail and recreation" ~ "retail",
      place == "Transit stations" ~ "transit",
      place == "Workplaces" ~ "workplaces"
    )
  ) %>%
  select(Country = entity, Date = date, place_clean, trend) %>%
  pivot_wider(
    names_from = place_clean,
    values_from = trend,
    names_prefix = "mob_"
  )

# Check
head(mobility_wide)

# ------------------------------------------------------------------------------
# 2. Mit df_daily mergen
# ------------------------------------------------------------------------------

df_daily <- df_daily %>%
  left_join(mobility_wide, by = c("Country", "Date"))

# Check Merge-Qualität
cat("Beobachtungen mit Mobility-Daten:", 
    sum(!is.na(df_daily$mob_workplaces)), "\n")
cat("Beobachtungen ohne Mobility-Daten:", 
    sum(is.na(df_daily$mob_workplaces)), "\n")

# ------------------------------------------------------------------------------
# 3. Mobility-Variablen aufbereiten
# ------------------------------------------------------------------------------

df_daily <- df_daily %>%
  group_by(Country) %>%
  arrange(Country, Date) %>%
  mutate(
    # Composite Index: Durchschnitt der "Aktivitäts"-Kategorien
    # (Residential ist invers - mehr zu Hause = weniger Aktivität)
    #mob_workplaces = (mob_workplaces),
    mob_activity = (mob_workplaces + mob_retail + mob_transit + mob_grocery) / 4,
    mob_relevant = (mob_workplaces + mob_grocery + mob_transit)/3, ##relevant für personen ohne homeoffice
    #mob_retail = (mob_retail),
    #mob_grocery = (mob_grocery),
    #mob_transit = (mob_transit),
    # Alternativ: Gewichteter Index (Workplaces am wichtigsten für BIP)
    mob_economic = 0.5 * mob_workplaces + 0.3 * mob_retail + 0.2 * mob_transit,
    #mob_residential = mob_residential, ##ACHTUNG: ANDERE INTERPRETATION, NICHT BESUCHER SONDERN DAUER
    
    
    # Lags
    lag7_mob_workplaces = lag(mob_workplaces, 7),
    lag7_mob_activity = lag(mob_activity, 7),
    lag7_mob_relevant = lag(mob_relevant,7),
    lag7_mob_retail = lag(mob_retail,7),
    lag7_mob_grocery = lag(mob_grocery, 7),
    lag7_mob_transit = lag(mob_transit, 7),
    lag7_mob_economic = lag(mob_economic, 7),
    lag7_mob_residential= lag(mob_residential,7),
    
    # Änderungsraten
    delta_mob_workplaces = mob_workplaces - lag(mob_workplaces, 7),
    delta_mob_activity = mob_activity - lag(mob_activity, 7),
    delta_mob_relevant = mob_relevant - lag(mob_relevant, 7),
    delta_mob_retail = mob_retail - lag(mob_retail, 7),
    delta_mob_grocery = mob_grocery - lag(mob_grocery, 7),
    delta_mob_transit = mob_transit - lag(mob_transit, 7),
    delta_mob_economic = mob_economic - lag(mob_economic,7),
    delta_mob_residential = mob_residential - lag(mob_residential, 7)
  ) %>%
  ungroup()

# ------------------------------------------------------------------------------
# 4. Deskriptive Statistik
# ------------------------------------------------------------------------------
# ==============================================================================
# Loop über alle Mobility-Indikatoren
# ==============================================================================

# Liste der Mobility-Variablen
mobility_vars <- c(
  "mob_workplaces",
  "mob_activity",
  "mob_relevant",
  "mob_retail", 
  "mob_grocery",
  "mob_transit",
  "mob_residential",
  "mob_economic"
)

# Beschreibungen für Titel
mobility_labels <- c(
  "mob_workplaces" = "Workplaces (Arbeitsplätze)",
  "mob_activity" = "Activity Index (Durchschnitt ohne Residential)",
  "mob_relevant" = "Relevant Index (Workplaces + Grocery + Transit)",
  "mob_retail" = "Retail & Recreation (Einzelhandel & Freizeit)",
  "mob_grocery" = "Grocery & Pharmacy (Supermärkte & Apotheken)",
  "mob_transit" = "Transit Stations (Öffentlicher Verkehr)",
  "mob_residential" = "Residential (Verweildauer zu Hause)",
  "mob_economic" = "Economic Index (gewichtet: 50% Work, 30% Retail, 20% Transit)"
)

# Plots speichern
plot_list <- list()

for (var in mobility_vars) {
  
  # Check ob Variable existiert
  
  if (!var %in% names(df_daily)) {
    message(paste("Variable", var, "nicht gefunden - überspringe"))
    next
  }
  
  # Daten aggregieren
  plot_data <- df_daily %>%
    group_by(Date) %>%
    summarise(
      stringency = mean(StringencyIndex_PopWeighted, na.rm = TRUE),
      mobility = mean(.data[[var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = c(stringency, mobility), 
      names_to = "variable", 
      values_to = "value"
    ) %>%
    mutate(
      variable = ifelse(variable == "mobility", var, variable)
    )
  
  # Plot erstellen
  p <- ggplot(plot_data, aes(x = Date, y = value, color = variable)) +
    geom_line(linewidth = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    scale_color_manual(
      values = c("stringency" = "red", setNames("steelblue", var)),
      labels = c("stringency" = "Stringency Index", setNames(mobility_labels[var], var))
    ) +
    labs(
      title = paste("Stringency vs.", mobility_labels[var]),
      subtitle = "Durchschnitt über alle Länder",
      x = "",
      y = "Index / % Veränderung zur Baseline",
      color = ""
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 12, face = "bold")
    )
  
  # Plot speichern und anzeigen
  plot_list[[var]] <- p
  print(p)
  
}


# ------------------------------------------------------------------------------
# Alle Plots in einem Grid
# ------------------------------------------------------------------------------


# Kombiniere ausgewählte Plots
combined_plot <- (plot_list$mob_activity) /
  (plot_list$mob_residential+ plot_list$mob_transit +plot_list$mob_workplaces) /
  (plot_list$mob_grocery + plot_list$mob_retail + plot_list$mob_relevant)  
plot_annotation(
  title = "Google Mobility Indikatoren vs. Stringency",
  subtitle = "Durchschnitt über alle OECD-Länder, 2020-2021"
)

print(combined_plot)

##SAME BUT FOR OUTPUT
cutoff_date <- as.Date("2021-12-20")
start_date <- as.Date("2020-01-15")

combined_plot <- (
  plot_list$mob_activity + 
    scale_x_date(limits = c(start_date, cutoff_date), date_labels = "%b %Y")
) / (
  plot_list$mob_residential + scale_x_date(limits = c(start_date, cutoff_date), date_labels = "%b %Y") + 
    plot_list$mob_transit + scale_x_date(limits = c(start_date, cutoff_date), date_labels = "%b %Y") + 
    plot_list$mob_workplaces + scale_x_date(limits = c(start_date, cutoff_date), date_labels = "%b %Y")
) / (
  plot_list$mob_grocery + scale_x_date(limits = c(start_date, cutoff_date), date_labels = "%b %Y") + 
    plot_list$mob_retail + scale_x_date(limits = c(start_date, cutoff_date), date_labels = "%b %Y") + 
    plot_list$mob_relevant + scale_x_date(limits = c(start_date, cutoff_date), date_labels = "%b %Y")
) +
  plot_annotation(
    title = "Google Mobility Indikatoren vs. Stringency",
    subtitle = "Durchschnitt über alle OECD-Länder, 2020-2021"
  )

print(combined_plot)

ggsave(
  filename = "mobility_combined.pdf",
  plot = combined_plot,
  device = cairo_pdf,   # beste Einbettung/Schrift (falls installiert)
  width = 18, height = 12, units = "in"
)

##DESPITE HAVING STILL STRINGENCY DURING MID 2021 THE INTERESTING THINGS STANDS OUT:
##1. AB JULI 2027 WIEDER AUF PRE-COVID LEVEL: ACTIVITY OVERALL (AND SUBINDEX RELEVANT), RETAIL AND RESIDENTAL
##2. WORKPLACES UND TRANSIT BLEIBEN WÄHREND DER UNTERSUCHUNGSDAUER UNTER DEM PRE-COVID LEVEL (VERHALTENSÄNDERUNG?-> KEINEN DIREKTEN EFFEKT AUF DIE OUTCOMES ERWARTET)
##3. GROCERY & PHARAMCY SINKEN AM GERINGSTEN UND SIND NACH DEM ERSTEN LOCKDOWN WIEDER BEI 0, AB 2021 STEIGT ES SOGAR-< ACHTUNG ZEITPUNKT IMPFSTOFF EINTRAGEN-< LEUTE HABEN SICH TESTEN UN DIMPFEN LASSEN DORT!
##FAZIT: STRINGENCY FUNKTIONIERT, DER EFFEKT DER BESCHRÄNKUNG AUF DIE AKTIVITÄT DER WIRTSCHAFT IST SPÄTESTENS AB JULI 2021 BEI 0, ES IST EINE VERHALTENSÄNDERUNG BEI WORKPLACE UND TRANSIT SICHTBAR, WELCHE ABER EKIENEN EINFLUSS HAT (WENN VERGLICHEN MIT DER AKTIVITÄT-> EVTL. ARBEITSLOSENQUOTE?)

# Korrelation
cor(df_daily$StringencyIndex_PopWeighted, df_daily$mob_grocery, use = "complete.obs")
cor(df_daily$StringencyIndex_PopWeighted, df_daily$mob_relevant, use = "complete.obs")
cor(df_daily$StringencyIndex_PopWeighted, df_daily$mob_transit, use = "complete.obs")
cor(df_daily$StringencyIndex_PopWeighted, df_daily$mob_activity, use = "complete.obs")
cor(df_daily$StringencyIndex_PopWeighted, df_daily$mob_retail, use = "complete.obs")
cor(df_daily$StringencyIndex_PopWeighted, df_daily$mob_economic, use = "complete.obs")
cor(df_daily$StringencyIndex_PopWeighted, df_daily$mob_residential, use = "complete.obs")


# ==============================================================================
# Test aller Mobility-Indices in der Reaktionsfunktion
# ==============================================================================

# Basis-Spezifikation ohne Mobility (Referenz)
prf_base <- feols(
  StringencyIndex_PopWeighted ~ 
    lag7_stringency +
    lag14_log_cases + lag7_log_deaths +
    delta_log_cases_14d + delta_log_deaths_7d |
    country_id + week_id,
  data = df_daily,
  cluster = ~country_id
)

# Liste für Ergebnisse
mobility_results <- list()

# Loop über alle Mobility-Variablen
for (var in mobility_vars) {
  
  lag_var <- paste0("lag7_", var)
  delta_var <- paste0("delta_", var)
  
  # Check ob Variablen existieren
  if (!lag_var %in% names(df_daily)) {
    message(paste(lag_var, "nicht gefunden - überspringe"))
    next
  }
  
  # Formel dynamisch bauen
  formula_lag <- as.formula(paste0(
    "StringencyIndex_PopWeighted ~ lag7_stringency + lag7_log_cases + lag7_log_deaths + delta_log_cases_7d + ",
    lag_var,
    " | country_id + week_id"
  ))
  
  formula_both <- as.formula(paste0(
    "StringencyIndex_PopWeighted ~ lag7_stringency + lag7_log_cases + lag7_log_deaths + delta_log_cases_7d + ",
    lag_var, " + ", delta_var,
    " | country_id + week_id"
  ))
  
  # Schätzen
  model_lag <- tryCatch(
    feols(formula_lag, data = df_daily, cluster = ~country_id),
    error = function(e) NULL
  )
  
  model_both <- tryCatch(
    feols(formula_both, data = df_daily, cluster = ~country_id),
    error = function(e) NULL
  )
  
  # Ergebnisse speichern
  if (!is.null(model_lag)) {
    mobility_results[[paste0(var, "_lag")]] <- model_lag
  }
  if (!is.null(model_both)) {
    mobility_results[[paste0(var, "_both")]] <- model_both
  }
}

# Ergebnistabelle
modelsummary(
  c(list("Baseline" = prf_base), mobility_results),
  stars = TRUE,
  gof_map = c("nobs", "within.r.squared"),
  coef_omit = "lag7_stringency|lag7_log_cases|lag7_log_deaths"  # Fokus auf Mobility
)

# Option 2: In Datei speichern
modelsummary(
  c(list("Baseline" = prf_base), mobility_results),
  stars = TRUE,
  gof_map = c("nobs", "within.r.squared"),
  coef_omit = "lag7_stringency|lag7_log_cases|lag7_log_deaths",
  output = "results_mobility.txt"
)

# ==============================================================================
# Kompakte Vergleichstabelle: Within-R² und Koeffizienten
# ==============================================================================

comparison_table <- tibble(
  model = names(mobility_results),
  within_r2 = map_dbl(mobility_results, ~ fitstat(.x, "wr2")[[1]]),
  n_obs = map_dbl(mobility_results, ~ .x$nobs)
) %>%
  arrange(desc(within_r2))

print(comparison_table)


###TEST
# D6a: + Mobility Level (reagieren Regierungen auf Verhalten?)
prf_d6a <- feols(mob_activity~ lag1_stringency + lag7_log_cases  |
                   country_id + week_id,
                 data = df_daily,
                 cluster = ~country_id
)

# Within R2 (und optional adjusted)
fixest::fitstat(prf_d6a, c("wr2", "war2"))


##EFFEKT VON STRINGENCY AUF MOBILITY
df_daily <- df_daily %>%
  mutate(Date = as.Date(Date)) %>%      # falls Date noch kein Date ist
  arrange(country_id, Date)

m_mob_dl <- feols(
  mob_activity ~
    l(StringencyIndex_PopWeighted, 0:21) +
    log_cases_7d + log_deaths_7d |
    country_id + week_id,
  data = df_daily,
  panel.id = ~country_id + Date,        # <- DAS ist der Fix
  cluster = ~country_id
)


coefplot(m_mob_dl, keep = "StringencyIndex_PopWeighted")
fixest::fitstat(m_mob_dl, c("wr2", "war2"))

##STÜTZT MEINE STORY-> WENN ZU WEIT-> REBOUND EFFEKT UND PERSISTENZ AM RAND; WIE HOCH IST DER MITTLERE VERFALL??

#alle LAGS über die Zeithorizonte

h <- 14

# Namen der Lag-Koeffizienten so bauen, wie fixest sie speichert
lag_terms <- paste0("l(StringencyIndex_PopWeighted, ", 0:h, ")")

# nur die behalten, die wirklich im Modell sind
lag_terms <- lag_terms[lag_terms %in% names(coef(m_mob_dl))]

b <- coef(m_mob_dl)[lag_terms]

# Varianz-Kovarianz-Matrix (nimmt i.d.R. die im Modell gesetzte Cluster-Vcov)
V <- vcov(m_mob_dl)

# Linear-Kombination: Summe der Koeffizienten
est <- sum(b)

# SE der Summe: sqrt(1' V 1)
one <- rep(1, length(lag_terms))
se  <- sqrt(as.numeric(t(one) %*% V[lag_terms, lag_terms] %*% one))

ci <- est + c(-1, 1) * qnorm(0.975) * se

data.frame(
  horizon = h,
  estimate = est,
  se = se,
  ci_low = ci[1],
  ci_high = ci[2],
  n_terms = length(lag_terms)
)


cum_effect <- function(m, h, var = "StringencyIndex_PopWeighted"){
  lag_terms <- paste0("l(", var, ", ", 0:h, ")")
  lag_terms <- lag_terms[lag_terms %in% names(coef(m))]
  
  b <- coef(m)[lag_terms]
  V <- vcov(m)
  
  est <- sum(b)
  one <- rep(1, length(lag_terms))
  se  <- sqrt(as.numeric(t(one) %*% V[lag_terms, lag_terms] %*% one))
  ci  <- est + c(-1, 1) * qnorm(0.975) * se
  
  data.frame(horizon = h, estimate = est, se = se, ci_low = ci[1], ci_high = ci[2])
}

cum_tbl <- do.call(rbind, lapply(0:21, \(h) cum_effect(m_mob_dl, h)))
cum_tbl

#Interetation:
#A 10-point increase in the Stringency Index is associated with a 4.9 percentage point decline in mobility over the subsequent two weeks (95% CI: −5.8 to −4.0), implying that a meaningful tightening of restrictions is followed by a clear and sizable reduction in measured movement relative to the Google baseline.

##NOT ADDING VALUE-> THE EFFECT IS MORE THE OTHER WAY AROUND, STAY WITH D6C
#USING THE MOBILITY DATA TO JUSTIFY THE STRINGENCYINDEX, THE CHOSEN DATARANGE AND THE CAUSALMECHANICS

# =============================================================================
##SECOND: ADDING R-RATE AS IT WAS A KEY FACTOR FOR DETERMINING THE INTENSITY
# =============================================================================
##ADDING R RATE

head(rrate_wide)

# ------------------------------------------------------------------------------
# 1. R-Rate mit df_daily mergen
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# 3) Join in df_daily
# ------------------------------------------------------------------------------
df_daily <- df_daily %>%
  dplyr::select(-dplyr::any_of(c(
    "R", "ci_95_u", "ci_95_l",
    "R_di7", "ci_95_u_di7", "ci_95_l_di7",
    "R_di7.x", "ci_95_u_di7.x", "ci_95_l_di7.x",
    "R_di7.y", "ci_95_u_di7.y", "ci_95_l_di7.y"
  )))


df_daily <- df_daily %>%
  left_join(
    rrate_wide %>% dplyr::select(Country, Date, R_di7, ci_95_u_di7, ci_95_l_di7),
    by = c("Country", "Date")
  ) %>%
  dplyr::rename(
    R = R_di7,
    ci_95_u = ci_95_u_di7,
    ci_95_l = ci_95_l_di7
  )

# Beispiel: du hast dann z.B.
# R_di5, ci_95_u_di5, ci_95_l_di5, ... bis di10
# plus lag7_R_di5, lag14_R_di5, d7_R_di5, etc.



# Check
cat("Beobachtungen mit R-Rate:", sum(!is.na(df_daily$R)), "\n")
cat("Beobachtungen ohne R-Rate:", sum(is.na(df_daily$R)), "\n")


# ------------------------------------------------------------------------------
# 2. R-Rate Variablen aufbereiten
# ------------------------------------------------------------------------------

# Check welche Spalten noch da sind
names(df_daily) %>% sort()

df_daily <- df_daily %>%
  group_by(Country) %>%
  arrange(Country, Date) %>%
  mutate(
    # R über/unter 1 (kritischer Schwellenwert)
    R_above_1 = as.numeric(R > 1),
    
    # Log R (für Elastizitäts-Interpretation)
    log_R = log(R),
    
    # Lags
    lag7_R = lag(R, 7),
    lag14_R = lag(R, 14),
    lag7_log_R = lag(log_R, 7),
    
    # Änderung in R
    delta_R = R - lag(R, 7),
    delta_log_R = log_R - lag(log_R, 7),
    
    # Unsicherheit (Breite des Konfidenzintervalls)
    R_uncertainty = ci_95_u - ci_95_l,
    lag7_R_uncertainty = lag(R_uncertainty, 7)
  ) %>%
  ungroup()

# ------------------------------------------------------------------------------
# 3. Deskriptive Analyse
# ------------------------------------------------------------------------------

# Zeitreihe: R vs. Stringency
df_daily %>%
  filter(!is.na(R)) %>%
  group_by(Date) %>%
  summarise(
    R_mean = mean(R, na.rm = TRUE),
    stringency_mean = mean(StringencyIndex_PopWeighted, na.rm = TRUE) / 100,
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(R_mean, stringency_mean), names_to = "var", values_to = "value") %>%
  ggplot(aes(x = Date, y = value, color = var)) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(
    title = "R-Rate vs. Stringency (Durchschnitt alle Länder)",
    subtitle = "Rote Linie: R = 1 (kritischer Schwellenwert)",
    y = "R / Stringency (skaliert)"
  ) +
  theme_minimal()

# Korrelation R vs. delta_cases
cor(df_daily$R, df_daily$delta_log_cases_7d, use = "complete.obs")


##PLOT WITH CONFIDENCEINTERVAL

df_plot <- df_daily %>%
  filter(!is.na(R)) %>%
  group_by(Date) %>%
  summarise(
    R_mean = mean(R, na.rm = TRUE),
    R_ci_l_mean = mean(ci_95_l, na.rm = TRUE),
    R_ci_u_mean = mean(ci_95_u, na.rm = TRUE),
    stringency_mean = mean(StringencyIndex_PopWeighted, na.rm = TRUE) / 100,
    .groups = "drop"
  )

ggplot(df_plot, aes(x = Date)) +
  geom_ribbon(aes(ymin = R_ci_l_mean, ymax = R_ci_u_mean, fill = "R 95% CI"),
              alpha = 0.2) +
  geom_line(aes(y = R_mean, color = "R mean")) +
  geom_line(aes(y = stringency_mean, color = "Stringency/100")) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(
    title = "R-Rate vs. Stringency (Durchschnitt alle Länder)",
    subtitle = "Band: Ø der länderspezifischen 95%-Intervalle; rote Linie: R = 1",
    y = "R / Stringency (skaliert)",
    color = NULL, fill = NULL
  ) +
  theme_minimal()



##WITH CONFIDENCEINTERVALL FOR BOTH
df_plot <- df_daily %>%
  filter(!is.na(R)) %>%
  group_by(Date) %>%
  summarise(
    # R
    R_mean = mean(R, na.rm = TRUE),
    R_ci_l_mean = mean(ci_95_l, na.rm = TRUE),
    R_ci_u_mean = mean(ci_95_u, na.rm = TRUE),
    
    # Stringency (auf 0–1 skaliert)
    stringency_vals = StringencyIndex_PopWeighted / 100,
    n_str = sum(!is.na(stringency_vals)),
    stringency_mean = mean(stringency_vals, na.rm = TRUE),
    stringency_sd   = sd(stringency_vals, na.rm = TRUE),
    stringency_se   = stringency_sd / sqrt(n_str),
    stringency_ci_l = stringency_mean - 1.96 * stringency_se,
    stringency_ci_u = stringency_mean + 1.96 * stringency_se,
    .groups = "drop"
  )

ggplot(df_plot, aes(x = Date)) +
  geom_ribbon(aes(ymin = R_ci_l_mean, ymax = R_ci_u_mean, fill = "R 95% CI"),
              alpha = 0.20) +
  geom_ribbon(aes(ymin = stringency_ci_l, ymax = stringency_ci_u, fill = "Stringency 95% CI (mean)"),
              alpha = 0.15) +
  geom_line(aes(y = R_mean, color = "R mean")) +
  geom_line(aes(y = stringency_mean, color = "Stringency/100")) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(
    title = "R-Rate vs. Stringency (Durchschnitt alle Länder)",
    subtitle = "Bänder: R = Ø Länder-CIs; Stringency = 95%-CI des Länder-Mittelwerts (sd/√n)",
    y = "R / Stringency (skaliert)",
    color = NULL, fill = NULL
  ) +
  theme_minimal()




df_plot_f <- df_plot %>%
  dplyr::filter(Date >= as.Date("2020-02-28"))

ggplot(df_plot_f, aes(x = Date)) +
  geom_ribbon(aes(ymin = R_ci_l_mean, ymax = R_ci_u_mean, fill = "R 95% CI"),
              alpha = 0.20) +
  geom_ribbon(aes(ymin = stringency_ci_l, ymax = stringency_ci_u, fill = "Stringency 95% CI (mean)"),
              alpha = 0.15) +
  geom_line(aes(y = R_mean, color = "R mean")) +
  geom_line(aes(y = stringency_mean, color = "Stringency/100")) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(
    title = "R-Rate vs. Stringency (Durchschnitt alle Länder)",
    subtitle = "Bänder: R = Ø Länder-CIs; Stringency = 95%-CI des Länder-Mittelwerts (sd/√n)",
    y = "R / Stringency (skaliert)",
    color = NULL, fill = NULL
  ) +
  theme_minimal()


library(ggplot2)
library(dplyr)

df_plot_f <- df_plot %>%
  filter(Date >= as.Date("2020-03-01"))

y_top <- max(df_plot_f$R_ci_u_mean, df_plot_f$stringency_ci_u, na.rm = TRUE)
vax_date <- pandemic_events %>%
  dplyr::filter(event == "Vaccine\nRollout") %>%
  dplyr::pull(date) %>%
  .[1]

# falls nötig sicherstellen, dass date wirklich Date ist
pandemic_events <- pandemic_events %>%
  mutate(date = as.Date(date))

ggplot(df_plot_f, aes(x = Date)) +
  geom_ribbon(aes(ymin = R_ci_l_mean, ymax = R_ci_u_mean, fill = "R 95% CI"), alpha = 0.20) +
  geom_ribbon(aes(ymin = stringency_ci_l, ymax = stringency_ci_u, fill = "Stringency 95% CI (mean)"), alpha = 0.15) +
  geom_line(aes(y = R_mean, color = "R mean")) +
  geom_line(aes(y = stringency_mean, color = "Stringency/100")) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_vline(xintercept = vax_date, linetype = "dashed", linewidth = 0.6, alpha = 0.8) +
  annotate("text", x = vax_date, y = y_top,
           label = "Vaccine rollout starts (Dec 2020)", angle = 90, vjust = -0.4, hjust = 1) +
  # --- Events hinzufügen ---
  geom_vline(data = pandemic_events, aes(xintercept = date),
             linetype = "dashed", linewidth = 0.5, alpha = 0.6, inherit.aes = FALSE) +
  geom_text(data = pandemic_events, aes(x = date, y = y_pos, label = event),
            angle = 90, vjust = -0.4, hjust = 0, size = 3, inherit.aes = FALSE) +
  labs(
    title = "R-Rate vs. Stringency (Durchschnitt alle Länder)",
    subtitle = "Bänder: R = Ø Länder-CIs; Stringency = 95%-CI des Länder-Mittelwerts (sd/√n); gestrichelt: Impfstart + Events",
    y = "R / Stringency (skaliert)",
    color = NULL, fill = NULL
  ) +
  theme_minimal()


#IMPFSTOFF ZUGÄNGLICHKEIT VERIFIZIEREN-> Habe ich aber bereits in meiner Berechnung der Stringency erfasst

# ------------------------------------------------------------------------------
# Policy-Reaktionsfunktion mit R-Rate
# ------------------------------------------------------------------------------

# D6: Baseline (ohne R)
prf_d6 <- feols(
  StringencyIndex_PopWeighted ~ 
    lag7_stringency +
    lag14_log_cases + lag7_log_deaths +
    delta_log_cases_14d + delta_log_deaths_7d|
    country_id + week_id,
  data = df_daily,
  cluster = ~country_id
)

# D7a: + R Level
prf_d7a <- feols(
  StringencyIndex_PopWeighted ~ 
    lag7_stringency +
    lag14_log_cases + lag7_log_deaths +
    delta_log_cases_14d + delta_log_deaths_7d + 
    lag7_R |
    country_id + week_id,
  data = df_daily,
  cluster = ~country_id
)

# D7b: + R Änderung
prf_d7b <-feols(
  StringencyIndex_PopWeighted ~ 
    lag7_stringency +
    lag14_log_cases + lag7_log_deaths +
    delta_log_cases_14d + delta_log_deaths_7d + 
    delta_R |
    country_id + week_id,
  data = df_daily,
  cluster = ~country_id
)

# D7c: Beide (R Level + Änderung) 
prf_d7c <-feols(
  StringencyIndex_PopWeighted ~ 
    lag7_stringency +
    lag14_log_cases + lag7_log_deaths +
    delta_log_cases_14d + delta_log_deaths_7d + 
    lag7_R + delta_R |
    country_id + week_id,
  data = df_daily,
  cluster = ~country_id
)


# D7d: R statt delta_cases/deaths (ersetzt R die Falldynamik?)
prf_d7d <-feols(
  StringencyIndex_PopWeighted ~ 
    lag7_stringency +
    lag14_log_cases + lag7_log_deaths +
    lag7_R |
    country_id + week_id,
  data = df_daily,
  cluster = ~country_id
)


# Ergebnistabelle
modelsummary(
  list(
    "D6: Baseline" = prf_d6,
    "D7a: + R" = prf_d7a,
    "D7b: + ΔR" = prf_d7b,
    "D7c: R + ΔR" = prf_d7c,
    "D7d: R statt Δcases" = prf_d7d
  ),
  stars = TRUE,
  gof_map = c("nobs", "r.squared", "within.r.squared")
)

# Within R² Vergleich
cat("\n---- Within R² ----\n")
cat("D6 (Baseline):", round(fitstat(prf_d6, "wr2")[[1]], 4), "\n")
cat("D7a (+ R):", round(fitstat(prf_d7a, "wr2")[[1]], 4), "\n")
cat("D7b (+ ΔR):", round(fitstat(prf_d7b, "wr2")[[1]], 4), "\n")
cat("D7c (R statt Δcases):", round(fitstat(prf_d7c, "wr2")[[1]], 4), "\n")
cat("D7d (R + ΔR):", round(fitstat(prf_d7d, "wr2")[[1]], 4), "\n")


head(hosp_d)

##WENN R-RATE HINZUGEFÜGT WIRD WIRD CASES NEGATIV-> PROXY-> REGIERUNGEN HABEN AUF DIESE KENNZAHL REAGIERT (CASES WAR NUR DER PROXY)
##NIVEAU (DELTA) IST NICHT RELEVANT SONDERN DIE VERÄNDERUNG-> WIE VORHER

##LEVEL UND VERÄNDERUNG SORGFÄLLTIG DURCHEGEHEN UND AUFSCHREIBEN


#VORSCHLAG:
prf_final <- feols(
  StringencyIndex_PopWeighted ~ 
    lag7_stringency +
    lag14_log_cases + lag7_log_deaths +
    lag7_R |
    country_id + week_id,
  data = df_daily,
  cluster = ~country_id
)
#==============================================================================
#NEXT: ADD HOSPITAL DATA
# ==============================================================================
###MIT HOSPITAL DATEN (HAT MISSINGS)

# Check welche Länder Hospitalisierungsdaten haben
countries_with_hosp <- unique(hosp_d$Country)
countries_without_hosp <- setdiff(unique(df_daily$Country), countries_with_hosp)

cat("Länder ohne Hospitalisierungsdaten:", countries_without_hosp, "\n")
cat("Anzahl Länder mit Daten:", length(countries_with_hosp), "\n")

# ------------------------------------------------------------------------------
# Hospitalisierungsdaten aufbereiten und mergen
# ------------------------------------------------------------------------------

df_daily <- df_daily %>%
  left_join(
    hosp_d %>% 
      select(Country, Date = date, 
             hosp_occ = Daily.hospital.occupancy.per.million,
             icu_occ = Daily.ICU.occupancy.per.million),
    by = c("Country", "Date")
  )

# Variablen aufbereiten
df_daily <- df_daily %>%
  group_by(Country) %>%
  arrange(Country, Date) %>%
  mutate(
    # Logs (mit +1 für Nullen)
    log_hosp = log(hosp_occ + 1),
    log_icu = log(icu_occ + 1),
    
    # Lags
    lag7_log_hosp = lag(log_hosp, 7),
    lag7_log_icu = lag(log_icu, 7),
    
    # Änderungsraten
    delta_log_hosp = log_hosp - lag(log_hosp, 7),
    delta_log_icu = log_icu - lag(log_icu, 7)
  ) %>%
  ungroup()

# Check
cat("Beobachtungen mit Hosp-Daten:", sum(!is.na(df_daily$hosp_occ)), "\n")

# ------------------------------------------------------------------------------
# Vergleich: Mit vs. ohne Hospitalisierung
# ------------------------------------------------------------------------------

# D6c: Baseline (alle 38 Länder)
prf_baseline <- feols(
  StringencyIndex_PopWeighted ~ 
    lag7_stringency +
    lag14_log_cases + lag7_log_deaths +
    lag7_R |
    country_id + week_id,
  data = df_daily,
  cluster = ~country_id
)

# D8a: + Hospitalisierung (33 Länder)
prf_hosp_a <- feols(
  StringencyIndex_PopWeighted ~ 
    lag7_stringency +
    lag14_log_cases + lag7_log_deaths +
    lag7_R + lag7_log_hosp |
    country_id + week_id,
  data = df_daily,
  cluster = ~country_id
)

# D8b: + ICU (33 Länder)
prf_hosp_b <- feols(
  StringencyIndex_PopWeighted ~ 
    lag7_stringency +
    lag14_log_cases + lag7_log_deaths +
    lag7_R +lag7_log_icu |
    country_id + week_id,
  data = df_daily,
  cluster = ~country_id
)

# D8c: Hospitalisierung ersetzt Cases? (33 Länder)
prf_hosp_c <- feols(
  StringencyIndex_PopWeighted ~ 
    lag7_stringency +
    lag7_log_hosp + lag7_log_deaths + lag7_R +
    delta_log_hosp |
    country_id + week_id,
  data = df_daily,
  cluster = ~country_id
)

# D8d: Baseline auf gleichem Sample (33 Länder, für fairen Vergleich)
prf_baseline_33 <- feols(
  StringencyIndex_PopWeighted ~ 
    lag7_stringency +
    lag14_log_cases + lag7_log_deaths +
    lag7_R |
    country_id + week_id,
  data = df_daily %>% filter(!is.na(lag7_log_hosp)),
  cluster = ~country_id
)

modelsummary(
  list(
    "Baseline (N=38)" = prf_baseline,
    "Baseline (N=33)" = prf_baseline_33,
    "+ Hosp" = prf_hosp_a,
    "+ ICU" = prf_hosp_b,
    "Hosp statt Cases" = prf_hosp_c
  ),
  stars = TRUE,
  gof_map = c("nobs", "r.squared", "within.r.squared")
)

# Within R²
cat("\n---- Within R² ----\n")
cat("Baseline (38):", round(fitstat(prf_baseline, "wr2")[[1]], 4), "\n")
cat("Baseline (33):", round(fitstat(prf_baseline_33, "wr2")[[1]], 4), "\n")
cat("+ Hosp:", round(fitstat(prf_hosp_a, "wr2")[[1]], 4), "\n")
cat("+ ICU:", round(fitstat(prf_hosp_b, "wr2")[[1]], 4), "\n")
cat("Hosp statt Cases:", round(fitstat(prf_hosp_c, "wr2")[[1]], 4), "\n")



# Kompakte Vergleichstabelle für Paper
comparison_models <- modelsummary(
  list(
    "Alle Länder (N=38)" = prf_baseline,
    "Subsample (N=33)" = prf_baseline_33,
    "+ Hospitalisierung" = prf_hosp_a,
    "+ ICU" = prf_hosp_b
  ),
  stars = TRUE,
  coef_map = c(
    "lag7_stringency" = "Stringency (t-7)",
    "lag14_log_cases" = "Cases (t-14)",
    "lag7_log_deaths" = "Deaths (t-7)",
    "lag7_R" = "R-Rate (t-7)",
    "lag7_log_hosp" = "Hospitalisierung (t-7)",
    "lag7_log_icu" = "ICU-Belegung (t-7)"
  ),
  gof_map = c("nobs", "within.r.squared"),
  title = "Policy-Reaktionsfunktion: Robustheit gegenüber Gesundheitsindikatoren"
)


comparison_models

# =============================================================================
##DECISION: D7d

##Problem with SUTVA-> Stringency (captures cases, deaths, r rate and political ideaa) from the direct neighbours

#SPATIAL LAG MODELL



##FOR the oxd_d_spatial_spatial

#Spalten entfernen die nur NA haben-> vlt zuerst zusammenführen
oxd_d_spatial <- oxd_d_spatial |>
  dplyr::select(where(~ !all(is.na(.x)))) |>
  dplyr::select(-Jurisdiction, -CountryName)


#move the indexes in front
oxd_d_spatial <- oxd_d_spatial |>
  dplyr::relocate(
    # Stringency
    StringencyIndex_NonVaccinated,
    StringencyIndex_NonVaccinated_ForDisplay,
    StringencyIndex_Vaccinated,
    StringencyIndex_Vaccinated_ForDisplay,
    StringencyIndex_SimpleAverage,
    StringencyIndex_SimpleAverage_ForDisplay,
    StringencyIndex_WeightedAverage,
    StringencyIndex_WeightedAverage_ForDisplay,
    # Government response
    GovernmentResponseIndex_NonVaccinated,
    GovernmentResponseIndex_NonVaccinated_ForDisplay,
    GovernmentResponseIndex_Vaccinated,
    GovernmentResponseIndex_Vaccinated_ForDisplay,
    GovernmentResponseIndex_SimpleAverage,
    GovernmentResponseIndex_SimpleAverage_ForDisplay,
    GovernmentResponseIndex_WeightedAverage,
    GovernmentResponseIndex_WeightedAverage_ForDisplay,
    # Containment & health
    ContainmentHealthIndex_NonVaccinated,
    ContainmentHealthIndex_NonVaccinated_ForDisplay,
    ContainmentHealthIndex_Vaccinated,
    ContainmentHealthIndex_Vaccinated_ForDisplay,
    ContainmentHealthIndex_SimpleAverage,
    ContainmentHealthIndex_SimpleAverage_ForDisplay,
    ContainmentHealthIndex_WeightedAverage,
    ContainmentHealthIndex_WeightedAverage_ForDisplay,
    # Panemic stats
    ConfirmedCases,
    ConfirmedDeaths,
    PopulationVaccinated,
    .after = Date
  )


oxd_d_spatial <- oxd_d_spatial |>
  mutate(
    Date = as.Date(as.character(Date), format = "%Y%m%d")
  )


#overall descriptives
colnames(oxd_d_spatial)

vars <- c(
  "StringencyIndex_NonVaccinated_ForDisplay",
  "StringencyIndex_Vaccinated_ForDisplay",
  "StringencyIndex_SimpleAverage_ForDisplay",
  "StringencyIndex_WeightedAverage_ForDisplay",
  "GovernmentResponseIndex_NonVaccinated_ForDisplay",
  "GovernmentResponseIndex_Vaccinated_ForDisplay",
  "GovernmentResponseIndex_SimpleAverage_ForDisplay",
  "GovernmentResponseIndex_WeightedAverage_ForDisplay",
  "ContainmentHealthIndex_NonVaccinated_ForDisplay",
  "ContainmentHealthIndex_Vaccinated_ForDisplay",
  "ContainmentHealthIndex_SimpleAverage_ForDisplay",
  "ContainmentHealthIndex_WeightedAverage_ForDisplay"
)


oxd_d_spatial |> 
  dplyr::select(all_of(vars)) |>
  summary()




oxd_d_spatial <- oxd_d_spatial |>
  dplyr::rename(
    Country = CountryCode
  )


# Basic structure
cat("=== BASIC DATA STRUCTURE ===\n")
cat("Dimensions:", nrow(oxd_d_spatial), "rows ×", ncol(oxd_d_spatial), "columns\n")
cat("Date range:", min(oxd_d_spatial$Date, na.rm=TRUE), "to", max(oxd_d_spatial$Date, na.rm=TRUE), "\n")
cat("Number of countries:", n_distinct(oxd_d_spatial$Country), "\n\n")

# Check country codes
cat("=== COUNTRIES IN DATASET ===\n")
countries <- oxd_d_spatial %>%
  distinct(Country) %>%
  arrange(Country)
print(countries)



cat("=== AVAILABLE CONTAINMENT & HEALTH INDICES ===\n")

# Check for all possible variants
index_variants <- c(
  # Without vaccine differentiation (M-version)
  "ContainmentHealthIndex_WeightedAverage",
  "ContainmentHealthIndex_SimpleAverage",
  "ContainmentHealthIndex_WeightedAverage_ForDisplay",
  "ContainmentHealthIndex_SimpleAverage_ForDisplay",
  
  # With vaccine differentiation
  "ContainmentHealthIndex_NonVaccinated_WeightedAverage",
  "ContainmentHealthIndex_Vaccinated_WeightedAverage",
  "ContainmentHealthIndex_NonVaccinated_SimpleAverage",
  "ContainmentHealthIndex_Vaccinated_SimpleAverage",
  "ContainmentHealthIndex_NonVaccinated_WeightedAverage_ForDisplay",
  "ContainmentHealthIndex_Vaccinated_WeightedAverage_ForDisplay",
  
  # Stringency alternatives
  "StringencyIndex_WeightedAverage",
  "StringencyIndex_SimpleAverage",
  "StringencyIndex_WeightedAverage_ForDisplay")

available_indices <- index_variants[index_variants %in% names(oxd_d_spatial)]
cat("Found", length(available_indices), "index variants:\n")
print(available_indices)

# Identify primary variable
if ("ContainmentHealthIndex_WeightedAverage" %in% names(oxd_d_spatial)) {
  primary_var <- "ContainmentHealthIndex_WeightedAverage"
  cat("\n✅ PRIMARY VARIABLE IDENTIFIED:", primary_var, "\n")
} else if ("ContainmentHealthIndex_NonVaccinated_WeightedAverage" %in% names(oxd_d_spatial)) {
  primary_var <- "ContainmentHealthIndex_NonVaccinated_WeightedAverage"
  cat("\n⚠️ Only differentiated version available, using:", primary_var, "\n")
} else {
  stop("ERROR: No suitable Containment & Health Index found!")
}

# 3. MISSING VALUES ANALYSIS

cat("\n=== MISSING VALUES ANALYSIS ===\n")
##NO MISSING VALUES

# COMPARE WEIGHTED VS SIMPLE AVERAGE VALUES

cat("\n=== COMPARISON: WEIGHTED VS SIMPLE AVERAGE ===\n")

if (all(c("ContainmentHealthIndex_WeightedAverage", 
          "ContainmentHealthIndex_SimpleAverage") %in% names(oxd_d_spatial))) {
  
  # Calculate gap
  oxd_d_spatial <- oxd_d_spatial %>%
    mutate(
      Gap_Weighted_Simple = ContainmentHealthIndex_SimpleAverage - 
        ContainmentHealthIndex_WeightedAverage
    )
  
  # Gap statistics
  gap_stats <- oxd_d_spatial %>%
    summarize(
      Mean_Gap = mean(Gap_Weighted_Simple, na.rm = TRUE),
      SD_Gap = sd(Gap_Weighted_Simple, na.rm = TRUE),
      Max_Gap = max(Gap_Weighted_Simple, na.rm = TRUE),
      Correlation = cor(ContainmentHealthIndex_WeightedAverage,
                        ContainmentHealthIndex_SimpleAverage,
                        use = "complete.obs")
    )
  
  cat("\n📊 Weighted vs Simple Average:\n")
  print(gap_stats)
  
  # Gap by country
  gap_by_country <- oxd_d_spatial %>%
    group_by(Country) %>%
    summarize(
      Mean_Gap = mean(Gap_Weighted_Simple, na.rm = TRUE),
      Max_Gap = max(Gap_Weighted_Simple, na.rm = TRUE),
      Median_Gap = median(Gap_Weighted_Simple, na.rm = TRUE)
    ) %>%
    arrange(desc(Mean_Gap))
  
  cat("\n📊 Countries with Largest Weighted-Simple Gaps:\n")
  cat("(Large gaps indicate frequent targeted/regional policies)\n\n")
  print(kable(head(gap_by_country, 10), digits = 2))
  
  # Plot comparison for selected countries
  selected_countries <- c("USA", "DEU", "ITA", "SWE", "FRA", "GBR")
  
  p_comparison <- oxd_d_spatial %>%
    filter(Country %in% selected_countries) %>%
    select(Country, Date, 
           Weighted = ContainmentHealthIndex_WeightedAverage,
           Simple = ContainmentHealthIndex_SimpleAverage) %>%
    pivot_longer(cols = c(Weighted, Simple),
                 names_to = "Version",
                 values_to = "Value") %>%
    ggplot(aes(x = Date, y = Value, color = Version)) +
    geom_line(linewidth = 0.8) +
    facet_wrap(~Country, ncol = 2) +
    theme_minimal() +
    labs(
      title = "Weighted vs Simple Average by Country",
      subtitle = "Gap shows when policies were regionally targeted",
      x = "Date",
      y = "Containment & Health Index"
    ) +
    theme(legend.position = "bottom")
  
  print(p_comparison)
  
} else {
  cat("⚠️ Both Weighted and Simple versions not available for comparison\n")
}



# ALLE Länder in einem großen facet_wrap (wird mehrere Seiten scrollen)
p_all <- oxd_d_spatial %>%
  select(Country, Date, 
         Weighted = ContainmentHealthIndex_WeightedAverage,
         Simple = ContainmentHealthIndex_SimpleAverage) %>%
  pivot_longer(cols = c(Weighted, Simple),
               names_to = "Version",
               values_to = "Value") %>%
  ggplot(aes(x = Date, y = Value, color = Version)) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~Country, ncol = 4, scales = "free_y") +  # scales="free_y" optional
  theme_minimal() +
  labs(
    title = "Weighted vs Simple Average - All OECD Countries",
    subtitle = "Gap shows when policies were regionally targeted",
    x = "Date",
    y = "Containment & Health Index"
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 8),
    axis.text = element_text(size = 6)
  )

print(p_all)

##ERST AB Q2.2021 HAT ES EINE VARIATION IN DNE ZWEI MESSUNGEN->VARIATION AUCH SEHR KLEIN
##Problem Index unterscheidet nicht nach Target Vax and non Vax

##For vax and non-vax

p_all <- oxd_d_spatial %>%
  select(Country, Date, 
         Weighted = ContainmentHealthIndex_NonVaccinated,
         Simple = ContainmentHealthIndex_Vaccinated) %>%
  pivot_longer(cols = c(Weighted, Simple),
               names_to = "Version",
               values_to = "Value") %>%
  ggplot(aes(x = Date, y = Value, color = Version)) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~Country, ncol = 4, scales = "free_y") +  # scales="free_y" optional
  theme_minimal() +
  labs(
    title = "Non-Vax vs Vax - All OECD Countries",
    subtitle = "Gap shows when policies were targeted based on vax status",
    x = "Date",
    y = "Containment & Health Index"
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 8),
    axis.text = element_text(size = 6)
  )

print(p_all)

# Speichere als große PDF
# ggsave("comparison_all_countries.pdf", p_all, 
#        width = 16, height = 20)

#Deutlich höherer differenz, problem, eigenen Index bauen der das berücksichtigt-> Wie stark wird die Bevölkerung getroffen?

# 1. CREATE POPULATION-WEIGHTED CONTAINMENT INDEX


cat("\n", rep("=", 80), "\n", sep = "")
cat("CREATING POPULATION-WEIGHTED CONTAINMENT INDEX\n")
cat(rep("=", 80), "\n\n", sep = "")

# 1. Create the population-weighted index
oxd_d_spatial <- oxd_d_spatial %>%
  mutate(
    # Normalize vaccination rate to 0-1 (from percentage)
    vax_rate = PopulationVaccinated / 100,
    
    # Create population-weighted index
    ContainmentHealthIndex_PopWeighted = 
      (1 - vax_rate) * ContainmentHealthIndex_NonVaccinated + 
      vax_rate * ContainmentHealthIndex_Vaccinated,
    
    # Additional useful metrics
    Vax_Discount = ContainmentHealthIndex_NonVaccinated - 
      ContainmentHealthIndex_Vaccinated,
    Effective_Discount = vax_rate * Vax_Discount
  )

# 2. Move new variable to front of dataset (after key identifiers)
oxd_d_spatial <- oxd_d_spatial %>%
  select(
    # Key identifiers first
    Country, Date,
    
    # Our new PRIMARY variable
    ContainmentHealthIndex_PopWeighted,
    
    # Then the component variables
    ContainmentHealthIndex_NonVaccinated,
    ContainmentHealthIndex_Vaccinated,
    PopulationVaccinated,
    vax_rate,
    Vax_Discount,
    Effective_Discount,
    
    # Everything else
    everything()
  )



cat("✅ Population-Weighted Index created and moved to front\n")
cat("   Variable name:", primary_var, "\n")
cat("   Based on: ContainmentHealthIndex_NonVaccinated & _Vaccinated\n")
cat("   Weighted by: PopulationVaccinated (%)\n\n")


# DESCRIPTIVE STATISTICS

cat("=== DESCRIPTIVE STATISTICS ===\n\n")

desc_stats <- oxd_d_spatial %>%
  summarize(
    Mean_PopWeighted = mean(ContainmentHealthIndex_PopWeighted, na.rm = TRUE),
    Mean_NonVax = mean(ContainmentHealthIndex_NonVaccinated, na.rm = TRUE),
    Mean_Vax = mean(ContainmentHealthIndex_Vaccinated, na.rm = TRUE),
    SD_PopWeighted = sd(ContainmentHealthIndex_PopWeighted, na.rm = TRUE),
    SD_NonVax = sd(ContainmentHealthIndex_NonVaccinated, na.rm = TRUE),
    SD_Vax = sd(ContainmentHealthIndex_Vaccinated, na.rm = TRUE),
    Mean_VaxRate = mean(PopulationVaccinated, na.rm = TRUE),
    Mean_Discount = mean(Vax_Discount, na.rm = TRUE)
  )

print(kable(desc_stats, digits = 2, 
            caption = "Summary Statistics: Population-Weighted vs Component Indices"))

# Check for missing values
missing_check <- oxd_d_spatial %>%
  summarize(
    N_total = n(),
    N_PopWeighted = sum(!is.na(ContainmentHealthIndex_PopWeighted)),
    N_NonVax = sum(!is.na(ContainmentHealthIndex_NonVaccinated)),
    N_Vax = sum(!is.na(ContainmentHealthIndex_Vaccinated)),
    N_VaxRate = sum(!is.na(PopulationVaccinated)),
    Pct_Complete = round(N_PopWeighted / N_total * 100, 2)
  )

cat("\n=== DATA COMPLETENESS ===\n")
print(kable(missing_check))

# GRAPHICAL COMPARISON 1: TIME SERIES (Average across all countries)



# Overall time series
p_timeseries <- oxd_d_spatial %>%
  group_by(Date) %>%
  summarize(
    `Population-Weighted` = mean(ContainmentHealthIndex_PopWeighted, na.rm = TRUE),
    `Non-Vaccinated` = mean(ContainmentHealthIndex_NonVaccinated, na.rm = TRUE),
    `Vaccinated` = mean(ContainmentHealthIndex_Vaccinated, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(`Population-Weighted`, `Non-Vaccinated`, `Vaccinated`),
               names_to = "Index",
               values_to = "Value") %>%
  ggplot(aes(x = Date, y = Value, color = Index, linewidth = Index)) +
  geom_line() +
  scale_color_manual(
    values = c("Population-Weighted" = "black",
               "Non-Vaccinated" = "#E74C3C",
               "Vaccinated" = "#2ECC71")
  ) +
  scale_linewidth_manual(
    values = c("Population-Weighted" = 1.5,
               "Non-Vaccinated" = 1,
               "Vaccinated" = 1)
  ) +
  theme_minimal() +
  labs(
    title = "Comparison of Containment Indices Over Time",
    subtitle = "Average across all OECD countries",
    x = "Date",
    y = "Index Value (0-100)",
    color = "Index Type",
    linewidth = "Index Type",
    caption = "Population-Weighted accounts for share of population under each policy regime"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray40")
  )

print(p_timeseries)
##Top Plot, zeigt wie sich die Indexe anfangen zu unterscheiden und wie sich die weighted kurve nach unten angleicht


# GRAPHICAL COMPARISON 2: WITH VACCINATION RATE OVERLAY

# Create dual-axis plot with vaccination rate
p_with_vaxrate <- oxd_d_spatial %>%
  group_by(Date) %>%
  summarize(
    `Population-Weighted` = mean(ContainmentHealthIndex_PopWeighted, na.rm = TRUE),
    `Non-Vaccinated` = mean(ContainmentHealthIndex_NonVaccinated, na.rm = TRUE),
    `Vaccinated` = mean(ContainmentHealthIndex_Vaccinated, na.rm = TRUE),
    VaxRate = mean(PopulationVaccinated, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(`Population-Weighted`, `Non-Vaccinated`, `Vaccinated`),
               names_to = "Index",
               values_to = "Value") %>%
  ggplot(aes(x = Date)) +
  # Vaccination rate as area
  geom_area(aes(y = VaxRate), fill = "lightblue", alpha = 0.3) +
  # Index lines
  geom_line(aes(y = Value, color = Index, linewidth = Index)) +
  scale_color_manual(
    values = c("Population-Weighted" = "black",
               "Non-Vaccinated" = "#E74C3C",
               "Vaccinated" = "#2ECC71")
  ) +
  scale_linewidth_manual(
    values = c("Population-Weighted" = 1.5,
               "Non-Vaccinated" = 1,
               "Vaccinated" = 1)
  ) +
  theme_minimal() +
  labs(
    title = "Policy Indices with Vaccination Rate Context",
    subtitle = "Blue area shows vaccination rate (%). As vax rate rises, PopWeighted diverges from NonVax",
    x = "Date",
    y = "Index Value (0-100) / Vaccination Rate (%)",
    color = "Index Type",
    linewidth = "Index Type"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold")
  )

print(p_with_vaxrate)

##with vaccination rate

# GRAPHICAL COMPARISON 3: SELECTED COUNTRIES (Faceted)


# Select diverse countries
selected_countries <- c("USA", "GBR", "DEU", "FRA", "ITA", "ESP", 
                        "SWE", "AUS", "JPN", "KOR", "CAN", "CHE")

p_countries <- oxd_d_spatial %>%
  filter(Country %in% selected_countries) %>%
  select(Country, Date,
         `Population-Weighted` = ContainmentHealthIndex_PopWeighted,
         `Non-Vaccinated` = ContainmentHealthIndex_NonVaccinated,
         `Vaccinated` = ContainmentHealthIndex_Vaccinated) %>%
  pivot_longer(cols = c(`Population-Weighted`, `Non-Vaccinated`, `Vaccinated`),
               names_to = "Index",
               values_to = "Value") %>%
  ggplot(aes(x = Date, y = Value, color = Index, linewidth = Index)) +
  geom_line() +
  facet_wrap(~Country, ncol = 3, scales = "free_y") +
  scale_color_manual(
    values = c("Population-Weighted" = "black",
               "Non-Vaccinated" = "#E74C3C",
               "Vaccinated" = "#2ECC71")
  ) +
  scale_linewidth_manual(
    values = c("Population-Weighted" = 1.2,
               "Non-Vaccinated" = 0.8,
               "Vaccinated" = 0.8)
  ) +
  theme_minimal() +
  labs(
    title = "Index Comparison by Country",
    subtitle = "PopWeighted (black) falls between NonVax (red) and Vax (green) based on population shares",
    x = "Date",
    y = "Index Value",
    color = "Index Type",
    linewidth = "Index Type"
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
  )

print(p_countries)


# GRAPHICAL COMPARISON 4: DIVERGENCE ANALYSIS

# Show when and where indices diverge
p_divergence <- oxd_d_spatial %>%
  group_by(Date) %>%
  summarize(
    Gap_NV_Pop = mean(ContainmentHealthIndex_NonVaccinated - 
                        ContainmentHealthIndex_PopWeighted, na.rm = TRUE),
    Gap_Pop_V = mean(ContainmentHealthIndex_PopWeighted - 
                       ContainmentHealthIndex_Vaccinated, na.rm = TRUE),
    Gap_NV_V = mean(ContainmentHealthIndex_NonVaccinated - 
                      ContainmentHealthIndex_Vaccinated, na.rm = TRUE),
    VaxRate = mean(PopulationVaccinated, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(Gap_NV_Pop, Gap_Pop_V, Gap_NV_V),
               names_to = "Gap_Type",
               values_to = "Gap_Value") %>%
  mutate(
    Gap_Type = case_when(
      Gap_Type == "Gap_NV_Pop" ~ "NonVax - PopWeighted",
      Gap_Type == "Gap_Pop_V" ~ "PopWeighted - Vax",
      Gap_Type == "Gap_NV_V" ~ "NonVax - Vax"
    )
  ) %>%
  ggplot(aes(x = Date, y = Gap_Value, color = Gap_Type)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(
    values = c("NonVax - PopWeighted" = "#E74C3C",
               "PopWeighted - Vax" = "#2ECC71",
               "NonVax - Vax" = "#F39C12")
  ) +
  theme_minimal() +
  labs(
    title = "Index Divergence Over Time",
    subtitle = "Gap between different index specifications (average across countries)",
    x = "Date",
    y = "Gap (Index Points)",
    color = "Comparison",
    caption = "Rising gaps indicate increasing vaccine differentiation"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold")
  )

print(p_divergence)


# GRAPHICAL COMPARISON 5: SCATTER PLOT (PopWeighted vs NonVax)


p_scatter <- oxd_d_spatial %>%
  filter(!is.na(ContainmentHealthIndex_PopWeighted),
         !is.na(ContainmentHealthIndex_NonVaccinated)) %>%
  ggplot(aes(x = ContainmentHealthIndex_NonVaccinated, 
             y = ContainmentHealthIndex_PopWeighted,
             color = PopulationVaccinated)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  scale_color_viridis_c(name = "Vaccination\nRate (%)") +
  theme_minimal() +
  labs(
    title = "PopWeighted vs NonVax Index",
    subtitle = "Points below red line: PopWeighted < NonVax (due to high vaccination)",
    x = "Non-Vaccinated Index",
    y = "Population-Weighted Index",
    caption = "Color shows vaccination rate"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold")
  )

print(p_scatter)

# QUANTITATIVE COMPARISON


cat("\n", rep("=", 80), "\n", sep = "")
cat("QUANTITATIVE COMPARISON\n")
cat(rep("=", 80), "\n\n", sep = "")

# Correlation matrix
cat("=== CORRELATION MATRIX ===\n\n")

cor_matrix <- oxd_d_spatial %>%
  select(ContainmentHealthIndex_PopWeighted,
         ContainmentHealthIndex_NonVaccinated,
         ContainmentHealthIndex_Vaccinated) %>%
  cor(use = "complete.obs")

print(kable(round(cor_matrix, 4), 
            caption = "Correlations between Index Specifications"))

# When do indices diverge?
cat("\n=== DIVERGENCE TIMELINE ===\n\n")

divergence_check <- oxd_d_spatial %>%
  mutate(
    Divergence = abs(ContainmentHealthIndex_NonVaccinated - 
                       ContainmentHealthIndex_PopWeighted)
  ) %>%
  group_by(Date) %>%
  summarize(
    Mean_Divergence = mean(Divergence, na.rm = TRUE),
    Pct_Large_Divergence = mean(Divergence > 5, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  filter(Pct_Large_Divergence > 10)  # More than 10% of countries

if (nrow(divergence_check) > 0) {
  first_divergence <- min(divergence_check$Date)
  cat("First significant divergence (>10% countries with gap >5):", 
      as.character(first_divergence), "\n")
  cat("→ Before this date: Indices are essentially identical\n")
  cat("→ After this date: Population-weighting becomes important\n\n")
} else {
  cat("No significant divergence detected in sample period\n\n")
}

# Measurement bias by vaccination rate
cat("=== MEASUREMENT BIAS BY VACCINATION QUARTILE ===\n\n")


bias_by_vaxrate <- oxd_d_spatial %>%
  filter(
    !is.na(PopulationVaccinated),
    !is.na(ContainmentHealthIndex_PopWeighted)
  ) %>%
  mutate(
    # Quartile direkt aus Rängen
    Vax_Quartile = ntile(PopulationVaccinated, 4),
    Vax_Quartile = factor(
      Vax_Quartile,
      levels = 1:4,
      labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)")
    ),
    Bias = ContainmentHealthIndex_NonVaccinated -
      ContainmentHealthIndex_PopWeighted
  ) %>%
  group_by(Vax_Quartile) %>%
  summarize(
    Mean_VaxRate    = mean(PopulationVaccinated, na.rm = TRUE),
    Mean_NonVax     = mean(ContainmentHealthIndex_NonVaccinated, na.rm = TRUE),
    Mean_PopWeighted = mean(ContainmentHealthIndex_PopWeighted, na.rm = TRUE),
    Mean_Bias       = mean(Bias, na.rm = TRUE),
    N_obs           = n(),
    .groups = "drop"
  )


print(kable(bias_by_vaxrate, digits = 2,
            caption = "How NonVax Index Overestimates by Vaccination Level"))

cat("\n📊 Interpretation:\n")
cat("   Bias = NonVax - PopWeighted\n")
cat("   Higher vaccination → Larger bias from using NonVax alone\n")
cat("   Q4 shows the measurement problem: NonVax overstates stringency\n")

cat("\n", rep("=", 80), "\n", sep = "")
cat("✅ POPULATION-WEIGHTED INDEX CREATED AND VALIDATED\n")
cat(rep("=", 80), "\n\n", sep = "")

##Problem, containment index erfast nicht nur Beschränkungen sondern auch H Werte
#Lösung: Stringency PopWeighted machen plus noch einen eigenen für Pandemic Control (alle H) dann noch zusätzlich den Containment gemeinsam nehmen

# ==============================================================================
# 2. CREATE STRINGENCY INDEX - EXCLUDING H1 AS IT IS NOT MANDATORY AND HANDLING PRE AND POST VACCINE DIFFERENTIATION (Pop Weighted)
# ==============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("CREATING STRINGENCY INDEX (Handles pre/post vaccine differentiation)\n")
cat(rep("=", 80), "\n\n", sep = "")


# STEP 1: Create vax_rate and define maxima

oxd_d_spatial <- oxd_d_spatial %>%
  mutate(
    vax_rate = if_else(is.na(PopulationVaccinated) | PopulationVaccinated == 0, 
                       0, PopulationVaccinated / 100)
  )

max_values <- list(
  C1 = 3, C2 = 3, C3 = 2, C4 = 4, C5 = 2, C6 = 3, C7 = 2, C8 = 4,
  H1 = 2, H2 = 3, H3 = 2, H6 = 4, H7 = 5, H8 = 3
)

# STEP 2: Create unified variables (M before differentiation, NV/V after)

cat("=== Creating unified C-policy variables ===\n")

oxd_d_spatial <- oxd_d_spatial %>%
  mutate(
    # For each C-policy, use M (Majority) if NV is missing, otherwise use NV
    # This handles the transition from pre-differentiation to post-differentiation
    
    # C1 - Schools
    C1_unified_NV = case_when(
      !is.na(C1NV_School.closing) ~ C1NV_School.closing,
      !is.na(C1M_School.closing) ~ C1M_School.closing,
      !is.na(C1E_School.closing) ~ C1E_School.closing,
      TRUE ~ NA_real_
    ),
    C1_unified_V = case_when(
      !is.na(C1V_School.closing) ~ C1V_School.closing,
      !is.na(C1M_School.closing) ~ C1M_School.closing,
      !is.na(C1E_School.closing) ~ C1E_School.closing,
      TRUE ~ NA_real_
    ),
    C1_unified_Flag = case_when(
      !is.na(C1NV_Flag) ~ C1NV_Flag,
      !is.na(C1M_Flag) ~ C1M_Flag,
      !is.na(C1E_Flag) ~ C1E_Flag,
      TRUE ~ 1  # Default to national
    ),
    
    # C2 - Workplace
    C2_unified_NV = case_when(
      !is.na(C2NV_Workplace.closing) ~ C2NV_Workplace.closing,
      !is.na(C2M_Workplace.closing) ~ C2M_Workplace.closing,
      !is.na(C2E_Workplace.closing) ~ C2E_Workplace.closing,
      TRUE ~ NA_real_
    ),
    C2_unified_V = case_when(
      !is.na(C2V_Workplace.closing) ~ C2V_Workplace.closing,
      !is.na(C2M_Workplace.closing) ~ C2M_Workplace.closing,
      !is.na(C2E_Workplace.closing) ~ C2E_Workplace.closing,
      TRUE ~ NA_real_
    ),
    C2_unified_Flag = case_when(
      !is.na(C2NV_Flag) ~ C2NV_Flag,
      !is.na(C2M_Flag) ~ C2M_Flag,
      !is.na(C2E_Flag) ~ C2E_Flag,
      TRUE ~ 1
    ),
    
    # C3 - Events
    C3_unified_NV = case_when(
      !is.na(C3NV_Cancel.public.events) ~ C3NV_Cancel.public.events,
      !is.na(C3M_Cancel.public.events) ~ C3M_Cancel.public.events,
      !is.na(C3E_Cancel.public.events) ~ C3E_Cancel.public.events,
      TRUE ~ NA_real_
    ),
    C3_unified_V = case_when(
      !is.na(C3V_Cancel.public.events) ~ C3V_Cancel.public.events,
      !is.na(C3M_Cancel.public.events) ~ C3M_Cancel.public.events,
      !is.na(C3E_Cancel.public.events) ~ C3E_Cancel.public.events,
      TRUE ~ NA_real_
    ),
    C3_unified_Flag = case_when(
      !is.na(C3NV_Flag) ~ C3NV_Flag,
      !is.na(C3M_Flag) ~ C3M_Flag,
      !is.na(C3E_Flag) ~ C3E_Flag,
      TRUE ~ 1
    ),
    
    # C4 - Gatherings
    C4_unified_NV = case_when(
      !is.na(C4NV_Restrictions.on.gatherings) ~ C4NV_Restrictions.on.gatherings,
      !is.na(C4M_Restrictions.on.gatherings) ~ C4M_Restrictions.on.gatherings,
      !is.na(C4E_Restrictions.on.gatherings) ~ C4E_Restrictions.on.gatherings,
      TRUE ~ NA_real_
    ),
    C4_unified_V = case_when(
      !is.na(C4V_Restrictions.on.gatherings) ~ C4V_Restrictions.on.gatherings,
      !is.na(C4M_Restrictions.on.gatherings) ~ C4M_Restrictions.on.gatherings,
      !is.na(C4E_Restrictions.on.gatherings) ~ C4E_Restrictions.on.gatherings,
      TRUE ~ NA_real_
    ),
    C4_unified_Flag = case_when(
      !is.na(C4NV_Flag) ~ C4NV_Flag,
      !is.na(C4M_Flag) ~ C4M_Flag,
      !is.na(C4E_Flag) ~ C4E_Flag,
      TRUE ~ 1
    ),
    
    # C5 - Transport
    C5_unified_NV = case_when(
      !is.na(C5NV_Close.public.transport) ~ C5NV_Close.public.transport,
      !is.na(C5M_Close.public.transport) ~ C5M_Close.public.transport,
      !is.na(C5E_Close.public.transport) ~ C5E_Close.public.transport,
      TRUE ~ NA_real_
    ),
    C5_unified_V = case_when(
      !is.na(C5V_Close.public.transport) ~ C5V_Close.public.transport,
      !is.na(C5M_Close.public.transport) ~ C5M_Close.public.transport,
      !is.na(C5E_Close.public.transport) ~ C5E_Close.public.transport,
      TRUE ~ NA_real_
    ),
    C5_unified_Flag = case_when(
      !is.na(C5NV_Flag) ~ C5NV_Flag,
      !is.na(C5M_Flag) ~ C5M_Flag,
      !is.na(C5E_Flag) ~ C5E_Flag,
      TRUE ~ 1
    ),
    
    # C6 - Stay at home
    C6_unified_NV = case_when(
      !is.na(C6NV_Stay.at.home.requirements) ~ C6NV_Stay.at.home.requirements,
      !is.na(C6M_Stay.at.home.requirements) ~ C6M_Stay.at.home.requirements,
      !is.na(C6E_Stay.at.home.requirements) ~ C6E_Stay.at.home.requirements,
      TRUE ~ NA_real_
    ),
    C6_unified_V = case_when(
      !is.na(C6V_Stay.at.home.requirements) ~ C6V_Stay.at.home.requirements,
      !is.na(C6M_Stay.at.home.requirements) ~ C6M_Stay.at.home.requirements,
      !is.na(C6E_Stay.at.home.requirements) ~ C6E_Stay.at.home.requirements,
      TRUE ~ NA_real_
    ),
    C6_unified_Flag = case_when(
      !is.na(C6NV_Flag) ~ C6NV_Flag,
      !is.na(C6M_Flag) ~ C6M_Flag,
      !is.na(C6E_Flag) ~ C6E_Flag,
      TRUE ~ 1
    ),
    
    # C7 - Internal movement
    C7_unified_NV = case_when(
      !is.na(C7NV_Restrictions.on.internal.movement) ~ C7NV_Restrictions.on.internal.movement,
      !is.na(C7M_Restrictions.on.internal.movement) ~ C7M_Restrictions.on.internal.movement,
      !is.na(C7E_Restrictions.on.internal.movement) ~ C7E_Restrictions.on.internal.movement,
      TRUE ~ NA_real_
    ),
    C7_unified_V = case_when(
      !is.na(C7V_Restrictions.on.internal.movement) ~ C7V_Restrictions.on.internal.movement,
      !is.na(C7M_Restrictions.on.internal.movement) ~ C7M_Restrictions.on.internal.movement,
      !is.na(C7E_Restrictions.on.internal.movement) ~ C7E_Restrictions.on.internal.movement,
      TRUE ~ NA_real_
    ),
    C7_unified_Flag = case_when(
      !is.na(C7NV_Flag) ~ C7NV_Flag,
      !is.na(C7M_Flag) ~ C7M_Flag,
      !is.na(C7E_Flag) ~ C7E_Flag,
      TRUE ~ 1
    ),
    
    # C8 - International travel (no FLAG)
    C8_unified_NV = case_when(
      !is.na(C8NV_International.travel.controls) ~ C8NV_International.travel.controls,
      !is.na(C8EV_International.travel.controls) ~ C8EV_International.travel.controls,
      !is.na(C8E_International.travel.controls) ~ C8E_International.travel.controls,
      TRUE ~ NA_real_
    ),
    C8_unified_V = case_when(
      !is.na(C8V_International.travel.controls) ~ C8V_International.travel.controls,
      !is.na(C8EV_International.travel.controls) ~ C8EV_International.travel.controls,
      !is.na(C8E_International.travel.controls) ~ C8E_International.travel.controls,
      TRUE ~ NA_real_
    )
  )

cat("✅ Unified C-policy variables created\n\n")

# STEP 3: Normalize and adjust with FLAGS

cat("=== Normalizing and adjusting C-policies ===\n")

oxd_d_spatial <- oxd_d_spatial %>%
  mutate(
    # Normalize and FLAG-adjust for NV
    C1_NV_adj = (C1_unified_NV / max_values$C1) * 100 * (1 - 0.5 * (1 - C1_unified_Flag)),
    C2_NV_adj = (C2_unified_NV / max_values$C2) * 100 * (1 - 0.5 * (1 - C2_unified_Flag)),
    C3_NV_adj = (C3_unified_NV / max_values$C3) * 100 * (1 - 0.5 * (1 - C3_unified_Flag)),
    C4_NV_adj = (C4_unified_NV / max_values$C4) * 100 * (1 - 0.5 * (1 - C4_unified_Flag)),
    C5_NV_adj = (C5_unified_NV / max_values$C5) * 100 * (1 - 0.5 * (1 - C5_unified_Flag)),
    C6_NV_adj = (C6_unified_NV / max_values$C6) * 100 * (1 - 0.5 * (1 - C6_unified_Flag)),
    C7_NV_adj = (C7_unified_NV / max_values$C7) * 100 * (1 - 0.5 * (1 - C7_unified_Flag)),
    C8_NV_adj = (C8_unified_NV / max_values$C8) * 100,
    
    # Normalize and FLAG-adjust for V (same as NV before differentiation)
    C1_V_adj = (C1_unified_V / max_values$C1) * 100 * (1 - 0.5 * (1 - C1_unified_Flag)),
    C2_V_adj = (C2_unified_V / max_values$C2) * 100 * (1 - 0.5 * (1 - C2_unified_Flag)),
    C3_V_adj = (C3_unified_V / max_values$C3) * 100 * (1 - 0.5 * (1 - C3_unified_Flag)),
    C4_V_adj = (C4_unified_V / max_values$C4) * 100 * (1 - 0.5 * (1 - C4_unified_Flag)),
    C5_V_adj = (C5_unified_V / max_values$C5) * 100 * (1 - 0.5 * (1 - C5_unified_Flag)),
    C6_V_adj = (C6_unified_V / max_values$C6) * 100 * (1 - 0.5 * (1 - C6_unified_Flag)),
    C7_V_adj = (C7_unified_V / max_values$C7) * 100 * (1 - 0.5 * (1 - C7_unified_Flag)),
    C8_V_adj = (C8_unified_V / max_values$C8) * 100,
    
    # Replace NA with 0
    across(c(C1_NV_adj:C8_V_adj), ~if_else(is.na(.), 0, .))
  )

cat("✅ Normalized and FLAG-adjusted\n\n")

# STEP 4: Calculate Stringency Index

cat("=== Calculating Stringency Index ===\n")

oxd_d_spatial <- oxd_d_spatial %>%
  mutate(
    # Average of C-policies
    StringencyIndex_NV = (C1_NV_adj + C2_NV_adj + C3_NV_adj + C4_NV_adj +
                            C5_NV_adj + C6_NV_adj + C7_NV_adj + C8_NV_adj) / 8,
    
    StringencyIndex_V = (C1_V_adj + C2_V_adj + C3_V_adj + C4_V_adj +
                           C5_V_adj + C6_V_adj + C7_V_adj + C8_V_adj) / 8,
    
    # Population-weighted
    StringencyIndex_PopWeighted = (1 - vax_rate) * StringencyIndex_NV + 
      vax_rate * StringencyIndex_V
  )

cat("✅ Stringency Index created\n\n")

# Check
summary_stats <- oxd_d_spatial %>%
  summarize(
    Mean = mean(StringencyIndex_PopWeighted, na.rm = TRUE),
    SD = sd(StringencyIndex_PopWeighted, na.rm = TRUE),
    Min = min(StringencyIndex_PopWeighted, na.rm = TRUE),
    Max = max(StringencyIndex_PopWeighted, na.rm = TRUE),
    N_nonzero = sum(StringencyIndex_PopWeighted > 0, na.rm = TRUE)
  )

print(kable(summary_stats, digits = 2))

# Check by year
yearly_check <- oxd_d_spatial %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarize(
    Mean = mean(StringencyIndex_PopWeighted, na.rm = TRUE),
    N_nonzero = sum(StringencyIndex_PopWeighted > 0, na.rm = TRUE),
    N_total = n()
  )

cat("\nBy year:\n")
print(kable(yearly_check, digits = 2))

# Correlation with Oxford
cor_oxford <- cor(oxd_d_spatial$StringencyIndex_PopWeighted,
                  oxd_d_spatial$StringencyIndex_WeightedAverage,
                  use = "complete.obs")
cat("\nCorrelation with Oxford Stringency:", round(cor_oxford, 4), "\n")

cat("\n✅ DONE! StringencyIndex_PopWeighted should now have values from 2020 onwards.\n")
##Bereinigter STRINGENCY INDEX ERSTELLT-> Stimmt nicht ganz mit Oxford überein->ÜBERPRÜFEN INDEM DER OXFORD INDEX (MIT H1) REPLIZIERT WIRD

# 3. CREATING STRINGENCY INDEX WITH H1 (C1-C8 + H1)

cat("\n", rep("=", 80), "\n", sep = "")
cat("CREATING STRINGENCY INDEX WITH H1 (Matches Oxford definition)\n")
cat(rep("=", 80), "\n\n", sep = "")

# STEP 1: Create vax_rate and define maxima (already done, but repeated for clarity)

oxd_d_spatial <- oxd_d_spatial %>%
  mutate(
    vax_rate = if_else(is.na(PopulationVaccinated) | PopulationVaccinated == 0, 
                       0, PopulationVaccinated / 100)
  )

max_values <- list(
  C1 = 3, C2 = 3, C3 = 2, C4 = 4, C5 = 2, C6 = 3, C7 = 2, C8 = 4,
  H1 = 2, H2 = 3, H3 = 2, H6 = 4, H7 = 5, H8 = 3
)

# STEP 2: Create unified C-policy variables (same as before)

cat("=== Creating unified C-policy variables ===\n")

oxd_d_spatial <- oxd_d_spatial %>%
  mutate(
    # C1 - Schools
    C1_unified_NV = case_when(
      !is.na(C1NV_School.closing) ~ C1NV_School.closing,
      !is.na(C1M_School.closing) ~ C1M_School.closing,
      !is.na(C1E_School.closing) ~ C1E_School.closing,
      TRUE ~ NA_real_
    ),
    C1_unified_V = case_when(
      !is.na(C1V_School.closing) ~ C1V_School.closing,
      !is.na(C1M_School.closing) ~ C1M_School.closing,
      !is.na(C1E_School.closing) ~ C1E_School.closing,
      TRUE ~ NA_real_
    ),
    C1_unified_Flag = case_when(
      !is.na(C1NV_Flag) ~ C1NV_Flag,
      !is.na(C1M_Flag) ~ C1M_Flag,
      !is.na(C1E_Flag) ~ C1E_Flag,
      TRUE ~ 1
    ),
    
    # C2 - Workplace
    C2_unified_NV = case_when(
      !is.na(C2NV_Workplace.closing) ~ C2NV_Workplace.closing,
      !is.na(C2M_Workplace.closing) ~ C2M_Workplace.closing,
      !is.na(C2E_Workplace.closing) ~ C2E_Workplace.closing,
      TRUE ~ NA_real_
    ),
    C2_unified_V = case_when(
      !is.na(C2V_Workplace.closing) ~ C2V_Workplace.closing,
      !is.na(C2M_Workplace.closing) ~ C2M_Workplace.closing,
      !is.na(C2E_Workplace.closing) ~ C2E_Workplace.closing,
      TRUE ~ NA_real_
    ),
    C2_unified_Flag = case_when(
      !is.na(C2NV_Flag) ~ C2NV_Flag,
      !is.na(C2M_Flag) ~ C2M_Flag,
      !is.na(C2E_Flag) ~ C2E_Flag,
      TRUE ~ 1
    ),
    
    # C3 - Events
    C3_unified_NV = case_when(
      !is.na(C3NV_Cancel.public.events) ~ C3NV_Cancel.public.events,
      !is.na(C3M_Cancel.public.events) ~ C3M_Cancel.public.events,
      !is.na(C3E_Cancel.public.events) ~ C3E_Cancel.public.events,
      TRUE ~ NA_real_
    ),
    C3_unified_V = case_when(
      !is.na(C3V_Cancel.public.events) ~ C3V_Cancel.public.events,
      !is.na(C3M_Cancel.public.events) ~ C3M_Cancel.public.events,
      !is.na(C3E_Cancel.public.events) ~ C3E_Cancel.public.events,
      TRUE ~ NA_real_
    ),
    C3_unified_Flag = case_when(
      !is.na(C3NV_Flag) ~ C3NV_Flag,
      !is.na(C3M_Flag) ~ C3M_Flag,
      !is.na(C3E_Flag) ~ C3E_Flag,
      TRUE ~ 1
    ),
    
    # C4 - Gatherings
    C4_unified_NV = case_when(
      !is.na(C4NV_Restrictions.on.gatherings) ~ C4NV_Restrictions.on.gatherings,
      !is.na(C4M_Restrictions.on.gatherings) ~ C4M_Restrictions.on.gatherings,
      !is.na(C4E_Restrictions.on.gatherings) ~ C4E_Restrictions.on.gatherings,
      TRUE ~ NA_real_
    ),
    C4_unified_V = case_when(
      !is.na(C4V_Restrictions.on.gatherings) ~ C4V_Restrictions.on.gatherings,
      !is.na(C4M_Restrictions.on.gatherings) ~ C4M_Restrictions.on.gatherings,
      !is.na(C4E_Restrictions.on.gatherings) ~ C4E_Restrictions.on.gatherings,
      TRUE ~ NA_real_
    ),
    C4_unified_Flag = case_when(
      !is.na(C4NV_Flag) ~ C4NV_Flag,
      !is.na(C4M_Flag) ~ C4M_Flag,
      !is.na(C4E_Flag) ~ C4E_Flag,
      TRUE ~ 1
    ),
    
    # C5 - Transport
    C5_unified_NV = case_when(
      !is.na(C5NV_Close.public.transport) ~ C5NV_Close.public.transport,
      !is.na(C5M_Close.public.transport) ~ C5M_Close.public.transport,
      !is.na(C5E_Close.public.transport) ~ C5E_Close.public.transport,
      TRUE ~ NA_real_
    ),
    C5_unified_V = case_when(
      !is.na(C5V_Close.public.transport) ~ C5V_Close.public.transport,
      !is.na(C5M_Close.public.transport) ~ C5M_Close.public.transport,
      !is.na(C5E_Close.public.transport) ~ C5E_Close.public.transport,
      TRUE ~ NA_real_
    ),
    C5_unified_Flag = case_when(
      !is.na(C5NV_Flag) ~ C5NV_Flag,
      !is.na(C5M_Flag) ~ C5M_Flag,
      !is.na(C5E_Flag) ~ C5E_Flag,
      TRUE ~ 1
    ),
    
    # C6 - Stay at home
    C6_unified_NV = case_when(
      !is.na(C6NV_Stay.at.home.requirements) ~ C6NV_Stay.at.home.requirements,
      !is.na(C6M_Stay.at.home.requirements) ~ C6M_Stay.at.home.requirements,
      !is.na(C6E_Stay.at.home.requirements) ~ C6E_Stay.at.home.requirements,
      TRUE ~ NA_real_
    ),
    C6_unified_V = case_when(
      !is.na(C6V_Stay.at.home.requirements) ~ C6V_Stay.at.home.requirements,
      !is.na(C6M_Stay.at.home.requirements) ~ C6M_Stay.at.home.requirements,
      !is.na(C6E_Stay.at.home.requirements) ~ C6E_Stay.at.home.requirements,
      TRUE ~ NA_real_
    ),
    C6_unified_Flag = case_when(
      !is.na(C6NV_Flag) ~ C6NV_Flag,
      !is.na(C6M_Flag) ~ C6M_Flag,
      !is.na(C6E_Flag) ~ C6E_Flag,
      TRUE ~ 1
    ),
    
    # C7 - Internal movement
    C7_unified_NV = case_when(
      !is.na(C7NV_Restrictions.on.internal.movement) ~ C7NV_Restrictions.on.internal.movement,
      !is.na(C7M_Restrictions.on.internal.movement) ~ C7M_Restrictions.on.internal.movement,
      !is.na(C7E_Restrictions.on.internal.movement) ~ C7E_Restrictions.on.internal.movement,
      TRUE ~ NA_real_
    ),
    C7_unified_V = case_when(
      !is.na(C7V_Restrictions.on.internal.movement) ~ C7V_Restrictions.on.internal.movement,
      !is.na(C7M_Restrictions.on.internal.movement) ~ C7M_Restrictions.on.internal.movement,
      !is.na(C7E_Restrictions.on.internal.movement) ~ C7E_Restrictions.on.internal.movement,
      TRUE ~ NA_real_
    ),
    C7_unified_Flag = case_when(
      !is.na(C7NV_Flag) ~ C7NV_Flag,
      !is.na(C7M_Flag) ~ C7M_Flag,
      !is.na(C7E_Flag) ~ C7E_Flag,
      TRUE ~ 1
    ),
    
    # C8 - International travel (no FLAG)
    C8_unified_NV = case_when(
      !is.na(C8NV_International.travel.controls) ~ C8NV_International.travel.controls,
      !is.na(C8EV_International.travel.controls) ~ C8EV_International.travel.controls,
      !is.na(C8E_International.travel.controls) ~ C8E_International.travel.controls,
      TRUE ~ NA_real_
    ),
    C8_unified_V = case_when(
      !is.na(C8V_International.travel.controls) ~ C8V_International.travel.controls,
      !is.na(C8EV_International.travel.controls) ~ C8EV_International.travel.controls,
      !is.na(C8E_International.travel.controls) ~ C8E_International.travel.controls,
      TRUE ~ NA_real_
    ),
    
    # NEW: H1 - Public Information Campaigns
    # H1 has no NV/V differentiation, only E (Everyone)
    H1_unified = case_when(
      !is.na(H1E_Public.information.campaigns) ~ H1E_Public.information.campaigns,
      TRUE ~ NA_real_
    ),
    H1_unified_Flag = case_when(
      !is.na(H1E_Flag) ~ H1E_Flag,
      TRUE ~ 1  # Default to national
    )
  )

cat("✅ Unified C-policy and H1 variables created\n\n")

# STEP 3: Normalize and adjust with FLAGS

cat("=== Normalizing and adjusting C-policies and H1 ===\n")

oxd_d_spatial <- oxd_d_spatial %>%
  mutate(
    # Normalize and FLAG-adjust C-policies for NV
    C1_NV_adj = (C1_unified_NV / max_values$C1) * 100 * (1 - 0.5 * (1 - C1_unified_Flag)),
    C2_NV_adj = (C2_unified_NV / max_values$C2) * 100 * (1 - 0.5 * (1 - C2_unified_Flag)),
    C3_NV_adj = (C3_unified_NV / max_values$C3) * 100 * (1 - 0.5 * (1 - C3_unified_Flag)),
    C4_NV_adj = (C4_unified_NV / max_values$C4) * 100 * (1 - 0.5 * (1 - C4_unified_Flag)),
    C5_NV_adj = (C5_unified_NV / max_values$C5) * 100 * (1 - 0.5 * (1 - C5_unified_Flag)),
    C6_NV_adj = (C6_unified_NV / max_values$C6) * 100 * (1 - 0.5 * (1 - C6_unified_Flag)),
    C7_NV_adj = (C7_unified_NV / max_values$C7) * 100 * (1 - 0.5 * (1 - C7_unified_Flag)),
    C8_NV_adj = (C8_unified_NV / max_values$C8) * 100,
    
    # Normalize and FLAG-adjust C-policies for V
    C1_V_adj = (C1_unified_V / max_values$C1) * 100 * (1 - 0.5 * (1 - C1_unified_Flag)),
    C2_V_adj = (C2_unified_V / max_values$C2) * 100 * (1 - 0.5 * (1 - C2_unified_Flag)),
    C3_V_adj = (C3_unified_V / max_values$C3) * 100 * (1 - 0.5 * (1 - C3_unified_Flag)),
    C4_V_adj = (C4_unified_V / max_values$C4) * 100 * (1 - 0.5 * (1 - C4_unified_Flag)),
    C5_V_adj = (C5_unified_V / max_values$C5) * 100 * (1 - 0.5 * (1 - C5_unified_Flag)),
    C6_V_adj = (C6_unified_V / max_values$C6) * 100 * (1 - 0.5 * (1 - C6_unified_Flag)),
    C7_V_adj = (C7_unified_V / max_values$C7) * 100 * (1 - 0.5 * (1 - C7_unified_Flag)),
    C8_V_adj = (C8_unified_V / max_values$C8) * 100,
    
    # Normalize and FLAG-adjust H1 (no NV/V differentiation)
    H1_norm = (H1_unified / max_values$H1) * 100,
    H1_adj = H1_norm * (1 - 0.5 * (1 - H1_unified_Flag)),
    
    # Replace NA with 0
    across(c(C1_NV_adj:C8_V_adj, H1_norm, H1_adj), ~if_else(is.na(.), 0, .))
  )

cat("✅ Normalized and FLAG-adjusted (including H1)\n\n")

# STEP 4: Calculate BOTH Stringency Indices

cat("=== Calculating Stringency Indices (with and without H1) ===\n")

oxd_d_spatial <- oxd_d_spatial %>%
  mutate(
    
    # Index 1: C1-C8 ONLY (our preferred measure)
    StringencyIndex_NV = (C1_NV_adj + C2_NV_adj + C3_NV_adj + C4_NV_adj +
                            C5_NV_adj + C6_NV_adj + C7_NV_adj + C8_NV_adj) / 8,
    
    StringencyIndex_V = (C1_V_adj + C2_V_adj + C3_V_adj + C4_V_adj +
                           C5_V_adj + C6_V_adj + C7_V_adj + C8_V_adj) / 8,
    
    StringencyIndex_PopWeighted = (1 - vax_rate) * StringencyIndex_NV + 
      vax_rate * StringencyIndex_V,
    
    # Index 2: C1-C8 + H1 (Oxford definition)
    StringencyH1_NV = (C1_NV_adj + C2_NV_adj + C3_NV_adj + C4_NV_adj +
                         C5_NV_adj + C6_NV_adj + C7_NV_adj + C8_NV_adj +
                         H1_adj) / 9,
    
    StringencyH1_V = (C1_V_adj + C2_V_adj + C3_V_adj + C4_V_adj +
                        C5_V_adj + C6_V_adj + C7_V_adj + C8_V_adj +
                        H1_adj) / 9,
    
    StringencyH1_PopWeighted = (1 - vax_rate) * StringencyH1_NV + 
      vax_rate * StringencyH1_V
  )

cat("✅ Both Stringency Indices created:\n")
cat("   - StringencyIndex_PopWeighted (C1-C8 only)\n")
cat("   - StringencyH1_PopWeighted (C1-C8 + H1, matches Oxford)\n\n")

# STEP 5: Validation and Comparison

cat("=== VALIDATION: Which index matches Oxford? ===\n\n")

# Summary statistics
summary_comparison <- oxd_d_spatial %>%
  summarize(
    `C1-C8 only` = mean(StringencyIndex_PopWeighted, na.rm = TRUE),
    `C1-C8+H1` = mean(StringencyH1_PopWeighted, na.rm = TRUE),
    `Oxford` = mean(StringencyIndex_WeightedAverage, na.rm = TRUE),
    `Diff (C1-C8)` = mean(StringencyIndex_PopWeighted - StringencyIndex_WeightedAverage, na.rm = TRUE),
    `Diff (C1-C8+H1)` = mean(StringencyH1_PopWeighted - StringencyIndex_WeightedAverage, na.rm = TRUE)
  )

print(kable(summary_comparison, digits = 2,
            caption = "Mean Values: Which matches Oxford?"))

# Correlations
cor_without_h1 <- cor(oxd_d_spatial$StringencyIndex_PopWeighted,
                      oxd_d_spatial$StringencyIndex_WeightedAverage,
                      use = "complete.obs")

cor_with_h1 <- cor(oxd_d_spatial$StringencyH1_PopWeighted,
                   oxd_d_spatial$StringencyIndex_WeightedAverage,
                   use = "complete.obs")

cat("\nCorrelations:\n")
cat("C1-C8 only vs Oxford:", round(cor_without_h1, 4), "\n")
cat("C1-C8+H1 vs Oxford:  ", round(cor_with_h1, 4), "\n\n")

# RMSE
rmse_without_h1 <- sqrt(mean((oxd_d_spatial$StringencyIndex_PopWeighted - 
                                oxd_d_spatial$StringencyIndex_WeightedAverage)^2, 
                             na.rm = TRUE))

rmse_with_h1 <- sqrt(mean((oxd_d_spatial$StringencyH1_PopWeighted - 
                             oxd_d_spatial$StringencyIndex_WeightedAverage)^2, 
                          na.rm = TRUE))

cat("RMSE (lower is better):\n")
cat("C1-C8 only vs Oxford:", round(rmse_without_h1, 2), "\n")
cat("C1-C8+H1 vs Oxford:  ", round(rmse_with_h1, 2), "\n\n")

# By year
yearly_validation <- oxd_d_spatial %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarize(
    `C1-C8` = mean(StringencyIndex_PopWeighted, na.rm = TRUE),
    `C1-C8+H1` = mean(StringencyH1_PopWeighted, na.rm = TRUE),
    `Oxford` = mean(StringencyIndex_WeightedAverage, na.rm = TRUE),
    `Diff_C1C8` = `C1-C8` - Oxford,
    `Diff_C1C8H1` = `C1-C8+H1` - Oxford,
    .groups = "drop"
  )

cat("By Year:\n")
print(kable(yearly_validation, digits = 2))

# Test on specific observation
cat("\n=== TEST CASE: Germany, April 1, 2020 ===\n\n")

test_obs <- oxd_d_spatial %>%
  filter(Country == "DEU", Date == as.Date("2020-04-01")) %>%
  select(
    Country, Date,
    H1_adj,
    StringencyIndex_PopWeighted,
    StringencyH1_PopWeighted,
    StringencyIndex_WeightedAverage
  )

if (nrow(test_obs) > 0) {
  print(kable(t(test_obs), col.names = "Value", digits = 2))
  
  diff_without <- test_obs$StringencyIndex_PopWeighted - 
    test_obs$StringencyIndex_WeightedAverage
  diff_with <- test_obs$StringencyH1_PopWeighted - 
    test_obs$StringencyIndex_WeightedAverage
  
  cat("\nDifference from Oxford:\n")
  cat("Without H1:", round(diff_without, 2), "points\n")
  cat("With H1:   ", round(diff_with, 2), "points\n\n")
}
##Fertig erstellt, beide PopWeighted Stringency Index, ohne und mit H1
##Problem: Neben Beschränkungen gab es auch Bemühung die Pandemie anders zu kontrollieren-> Tests usw.-> Deshalb eigenen Control Index bauen

# ==============================================================================
# 4. CREATE POPULATION-WEIGHTED CONTROL INDEX
# ==============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("CREATING CONTROL INDEX (Population-Weighted)\n")
cat("Components: C1-C8 + H1, H2, H3, H6, H7, H8\n")
cat(rep("=", 80), "\n\n", sep = "")

# PREREQUISITE: Ensure C-policies and vax_rate already exist
# (Assumes you already ran the stringency index code which created C1-C8_adj)

# STEP 1: Create unified H-policy variables

cat("=== Creating unified H-policy variables ===\n")

oxd_d_spatial <- oxd_d_spatial %>%
  mutate(
    # H1 - Public Information Campaigns (E only, has FLAG)
    H1_unified = case_when(
      !is.na(H1E_Public.information.campaigns) ~ H1E_Public.information.campaigns,
      TRUE ~ NA_real_
    ),
    H1_unified_Flag = case_when(
      !is.na(H1E_Flag) ~ H1E_Flag,
      TRUE ~ 1  # Default to national
    ),
    
    # H2 - Testing Policy (E only, NO FLAG)
    H2_unified = case_when(
      !is.na(H2E_Testing.policy) ~ H2E_Testing.policy,
      TRUE ~ NA_real_
    ),
    
    # H3 - Contact Tracing (E only, NO FLAG)
    H3_unified = case_when(
      !is.na(H3E_Contact.tracing) ~ H3E_Contact.tracing,
      TRUE ~ NA_real_
    ),
    
    # H6 - Facial Coverings (HAS NV/V differentiation, has FLAG)
    H6_unified_NV = case_when(
      !is.na(H6NV_Facial.Coverings) ~ H6NV_Facial.Coverings,
      !is.na(H6M_Facial.Coverings) ~ H6M_Facial.Coverings,
      !is.na(H6E_Facial.Coverings) ~ H6E_Facial.Coverings,
      TRUE ~ NA_real_
    ),
    H6_unified_V = case_when(
      !is.na(H6V_Facial.Coverings) ~ H6V_Facial.Coverings,
      !is.na(H6M_Facial.Coverings) ~ H6M_Facial.Coverings,
      !is.na(H6E_Facial.Coverings) ~ H6E_Facial.Coverings,
      TRUE ~ NA_real_
    ),
    H6_unified_Flag = case_when(
      !is.na(H6NV_Flag) ~ H6NV_Flag,
      !is.na(H6M_Flag) ~ H6M_Flag,
      !is.na(H6E_Flag) ~ H6E_Flag,
      TRUE ~ 1
    ),
    
    # H7 - Vaccination Policy (E only, has FLAG)
    H7_unified = case_when(
      !is.na(H7E_Vaccination.policy) ~ H7E_Vaccination.policy,
      TRUE ~ NA_real_
    ),
    H7_unified_Flag = case_when(
      !is.na(H7E_Flag) ~ H7E_Flag,
      TRUE ~ 1
    ),
    
    # H8 - Protection of Elderly (HAS NV/V differentiation, has FLAG)
    H8_unified_NV = case_when(
      !is.na(H8NV_Protection.of.elderly.people) ~ H8NV_Protection.of.elderly.people,
      !is.na(H8M_Protection.of.elderly.people) ~ H8M_Protection.of.elderly.people,
      !is.na(H8E_Protection.of.elderly.people) ~ H8E_Protection.of.elderly.people,
      TRUE ~ NA_real_
    ),
    H8_unified_V = case_when(
      !is.na(H8V_Protection.of.elderly.people) ~ H8V_Protection.of.elderly.people,
      !is.na(H8M_Protection.of.elderly.people) ~ H8M_Protection.of.elderly.people,
      !is.na(H8E_Protection.of.elderly.people) ~ H8E_Protection.of.elderly.people,
      TRUE ~ NA_real_
    ),
    H8_unified_Flag = case_when(
      !is.na(H8NV_Flag) ~ H8NV_Flag,
      !is.na(H8M_Flag) ~ H8M_Flag,
      !is.na(H8E_Flag) ~ H8E_Flag,
      TRUE ~ 1
    )
  )

cat("✅ Unified H-policy variables created\n\n")

# STEP 2: Normalize and FLAG-adjust H-policies

cat("=== Normalizing and adjusting H-policies ===\n")

oxd_d_spatial <- oxd_d_spatial %>%
  mutate(
    # H1 - Public Info (normalize + FLAG adjust)
    H1_norm = (H1_unified / max_values$H1) * 100,
    H1_adj = H1_norm * (1 - 0.5 * (1 - H1_unified_Flag)),
    # H2 - Testing (normalize, NO FLAG)
    H2_norm = (H2_unified / max_values$H2) * 100,
    H2_adj = H2_norm,  # No FLAG adjustment
    # H3 - Contact Tracing (normalize, NO FLAG)
    H3_norm = (H3_unified / max_values$H3) * 100,
    H3_adj = H3_norm,  # No FLAG adjustment
    # H6 - Masks (normalize + FLAG adjust, NV/V differentiation)
    H6_NV_norm = (H6_unified_NV / max_values$H6) * 100,
    H6_V_norm = (H6_unified_V / max_values$H6) * 100,
    H6_NV_adj = H6_NV_norm * (1 - 0.5 * (1 - H6_unified_Flag)),
    H6_V_adj = H6_V_norm * (1 - 0.5 * (1 - H6_unified_Flag)),
    # Population-weighted H6
    H6_PopWeighted = (1 - vax_rate) * H6_NV_adj + vax_rate * H6_V_adj,
    # H7 - Vaccination (normalize + FLAG adjust)
    H7_norm = (H7_unified / max_values$H7) * 100,
    H7_adj = H7_norm * (1 - 0.5 * (1 - H7_unified_Flag)),
    # H8 - Elderly Protection (normalize + FLAG adjust, NV/V differentiation)
    H8_NV_norm = (H8_unified_NV / max_values$H8) * 100,
    H8_V_norm = (H8_unified_V / max_values$H8) * 100,
    H8_NV_adj = H8_NV_norm * (1 - 0.5 * (1 - H8_unified_Flag)),
    H8_V_adj = H8_V_norm * (1 - 0.5 * (1 - H8_unified_Flag)),
    # Population-weighted H8
    H8_PopWeighted = (1 - vax_rate) * H8_NV_adj + vax_rate * H8_V_adj
  ) %>%
  # Replace NA with 0 ONLY for numeric H-policy columns (FIXED!)
  mutate(
    across(c(H1_norm, H1_adj, 
             H2_norm, H2_adj, 
             H3_norm, H3_adj,
             H6_NV_norm, H6_V_norm, H6_NV_adj, H6_V_adj, H6_PopWeighted,
             H7_norm, H7_adj,
             H8_NV_norm, H8_V_norm, H8_NV_adj, H8_V_adj, H8_PopWeighted), 
           ~if_else(is.na(.), 0, .))
  )

cat("✅ H-policies normalized and adjusted\n\n")

# STEP 3: Calculate Control Index

cat("=== Calculating Control Index ===\n")

oxd_d_spatial <- oxd_d_spatial %>%
  mutate(
    # For NonVaccinated: C1-C8 (NV) + H1, H2, H3 + H6 (NV) + H7 + H8 (NV)
    ControlIndex_NV = (
      C1_NV_adj + C2_NV_adj + C3_NV_adj + C4_NV_adj +
        C5_NV_adj + C6_NV_adj + C7_NV_adj + C8_NV_adj +
        H1_adj + H2_adj + H3_adj + H6_NV_adj + H7_adj + H8_NV_adj
    ) / 14,
    
    # For Vaccinated: C1-C8 (V) + H1, H2, H3 + H6 (V) + H7 + H8 (V)
    ControlIndex_V = (
      C1_V_adj + C2_V_adj + C3_V_adj + C4_V_adj +
        C5_V_adj + C6_V_adj + C7_V_adj + C8_V_adj +
        H1_adj + H2_adj + H3_adj + H6_V_adj + H7_adj + H8_V_adj
    ) / 14,
    
    # Population-weighted Control Index
    ControlIndex_PopWeighted = (1 - vax_rate) * ControlIndex_NV + 
      vax_rate * ControlIndex_V
  )

cat("✅ Control Index created\n\n")

# STEP 4: Validation

cat("=== VALIDATION ===\n\n")

# Summary statistics
ch_summary <- oxd_d_spatial %>%
  summarize(
    Mean = mean(ControlIndex_PopWeighted, na.rm = TRUE),
    SD = sd(ControlIndex_PopWeighted, na.rm = TRUE),
    Min = min(ControlIndex_PopWeighted, na.rm = TRUE),
    Max = max(ControlIndex_PopWeighted, na.rm = TRUE),
    N_complete = sum(!is.na(ControlIndex_PopWeighted))
  )

cat("Our Control Index:\n")
print(kable(ch_summary, digits = 2))

# Compare with Oxford's version
comparison <- oxd_d_spatial %>%
  summarize(
    Our_Control = mean(ControlIndex_PopWeighted, na.rm = TRUE),
    Oxford_CH = mean(ContainmentHealthIndex_WeightedAverage, na.rm = TRUE),
    Difference = Our_Control - Oxford_CH,
    Correlation = cor(ControlIndex_PopWeighted,
                      ContainmentHealthIndex_WeightedAverage,
                      use = "complete.obs"),
    RMSE = sqrt(mean((ControlIndex_PopWeighted - 
                        ContainmentHealthIndex_WeightedAverage)^2,
                     na.rm = TRUE))
  )

cat("\n=== COMPARISON WITH OXFORD ===\n\n")
print(kable(comparison, digits = 3))

if (comparison$Correlation > 0.98) {
  cat("\n✅ EXCELLENT: High correlation with Oxford's index!\n")
} else if (comparison$Correlation > 0.95) {
  cat("\n✅ GOOD: Strong correlation with Oxford's index\n")
} else {
  cat("\n⚠️  Moderate correlation - may need investigation\n")
}

# Component contribution analysis
cat("\n=== COMPONENT CONTRIBUTION ===\n\n")

component_means <- oxd_d_spatial %>%
  summarize(
    C_policies = mean((C1_NV_adj + C2_NV_adj + C3_NV_adj + C4_NV_adj +
                         C5_NV_adj + C6_NV_adj + C7_NV_adj + C8_NV_adj) / 8, 
                      na.rm = TRUE),
    H_policies = mean((H1_adj + H2_adj + H3_adj + H6_PopWeighted + 
                         H7_adj + H8_PopWeighted) / 6, 
                      na.rm = TRUE),
    Full_Index = mean(ControlIndex_PopWeighted, na.rm = TRUE)
  )

cat("Average contribution:\n")
print(kable(component_means, digits = 2))

# By year
yearly_ch <- oxd_d_spatial %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarize(
    Our_Control = mean(ControlIndex_PopWeighted, na.rm = TRUE),
    Oxford_CH = mean(ContainmentHealthIndex_WeightedAverage, na.rm = TRUE),
    Difference = Our_Control - Oxford_CH,
    .groups = "drop"
  )

cat("\n=== BY YEAR ===\n\n")
print(kable(yearly_ch, digits = 2))

# STEP 5: Visualization

cat("\n=== Creating Comparison Plot ===\n\n")

p_ch_comparison <- oxd_d_spatial %>%
  group_by(Date) %>%
  summarize(
    `Our Control` = mean(ControlIndex_PopWeighted, na.rm = TRUE),
    `Oxford C&H` = mean(ContainmentHealthIndex_WeightedAverage, na.rm = TRUE),
    `Our Stringency` = mean(StringencyIndex_PopWeighted, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = -Date, names_to = "Index", values_to = "Value") %>%
  ggplot(aes(x = Date, y = Value, color = Index, linetype = Index, linewidth = Index)) +
  geom_line() +
  scale_color_manual(
    values = c("Our Control" = "#3498DB",
               "Oxford C&H" = "black",
               "Our Stringency" = "#E74C3C")
  ) +
  scale_linetype_manual(
    values = c("Our Control" = "solid",
               "Oxford C&H" = "dashed",
               "Our Stringency" = "solid")
  ) +
  scale_linewidth_manual(
    values = c("Our Control" = 1.2,
               "Oxford C&H" = 1.5,
               "Our Stringency" = 1)
  ) +
  theme_minimal() +
  labs(
    title = "Control Index vs Stringency Index",
    subtitle = "Blue: Our Control (C1-C8 + H1-H8) | Black dashed: Oxford C&H | Red: Stringency (C1-C8)",
    x = "Date",
    y = "Index Value (0-100)",
    color = "Index",
    linetype = "Index",
    linewidth = "Index"
  ) +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold"))

print(p_ch_comparison)

# Scatter plot
p_scatter_ch <- oxd_d_spatial %>%
  filter(!is.na(ControlIndex_PopWeighted),
         !is.na(ContainmentHealthIndex_WeightedAverage)) %>%
  ggplot(aes(x = ContainmentHealthIndex_WeightedAverage, 
             y = ControlIndex_PopWeighted)) +
  geom_point(alpha = 0.3, size = 1, color = "#3498DB") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
  coord_fixed() +
  theme_minimal() +
  labs(
    title = "Our Control Index vs Oxford's C&H Index",
    subtitle = paste("Correlation:", round(comparison$Correlation, 3)),
    x = "Oxford's Containment & Health Index",
    y = "Our Control Index (PopWeighted)"
  )

print(p_scatter_ch)

# STEP 6: Final Summary

cat("\n", rep("=", 80), "\n", sep = "")
cat("SUMMARY: CONTROL INDEX\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("✅ ControlIndex_PopWeighted created\n\n")

cat("Components (14 total):\n")
cat("  C-policies (8): C1-C8 (containment measures)\n")
cat("  H-policies (6): H1, H2, H3, H6, H7, H8 (health responses)\n\n")

cat("Population-weighted:\n")
cat("  • H6 (Masks): Weighted by vaccination status\n")
cat("  • H8 (Elderly): Weighted by vaccination status\n")
cat("  • H1, H2, H3, H7: Apply to everyone (no differentiation)\n\n")

cat("Use cases:\n")
cat("  • Comprehensive measure of government response\n")
cat("  • Includes both restrictive and health policies\n")
cat("  • Good for: Overall policy intensity\n")
cat("  • Less good for: Isolating economic costs (mixes types)\n\n")

cat("Correlation with Oxford:", round(comparison$Correlation, 3), "\n")
cat("Mean difference:", round(comparison$Difference, 2), "points\n\n")

cat(rep("=", 80), "\n\n", sep = "")

cat("Dataset now contains:\n")
cat("  • StringencyIndex_PopWeighted (C1-C8)\n")
cat("  • StringencyH1_PopWeighted (C1-C8 + H1)\n")
cat("  • ControlIndex_PopWeighted (C1-C8 + H1-H8)\n\n")

cat("All indices are population-weighted by vaccination status! ✅\n\n")

##All Variables erstellt

# REORGANIZE DATASET: Move key indices to front

cat("\n", rep("=", 80), "\n", sep = "")
cat("REORGANIZING DATASET: Moving indices to front\n")
cat(rep("=", 80), "\n\n", sep = "")

oxd_d_spatial <- oxd_d_spatial %>%
  select(
    # KEY IDENTIFIERS
    Country, Date,
    
    # OUR CUSTOM POPULATION-WEIGHTED INDICES (Main variables)
    StringencyIndex_PopWeighted,          # C1-C8 only (main measure)
    StringencyH1_PopWeighted,             # C1-C8 + H1 (Oxford definition but weighted with population)
    ControlIndex_PopWeighted,             # H1-H8 (comprehensive)
    #ContainmentHealthIndex_PopWeighted,   # C1-C8+H1-H8
    
    
    # Component indices for reference
    StringencyIndex_NV,                   # C1-C8 NonVaccinated
    StringencyIndex_V,                    # C1-C8 Vaccinated
    StringencyH1_NV,                      # C1-C8+H1 NonVaccinated
    StringencyH1_V,                       # C1-C8+H1 Vaccinated
    ControlIndex_NV,                      # C1-C8+H1-H8 NonVaccinated
    ControlIndex_V,                       # C1-C8+H1-H8 Vaccinated
    
    # OXFORD'S ORIGINAL STRINGENCY INDICES (for comparison)
    StringencyIndex_NonVaccinated,
    StringencyIndex_Vaccinated,
    StringencyIndex_SimpleAverage,
    StringencyIndex_WeightedAverage,
    
    # OXFORD'S OTHER INDICES
    ContainmentHealthIndex_NonVaccinated,
    ContainmentHealthIndex_Vaccinated,
    ContainmentHealthIndex_SimpleAverage,
    ContainmentHealthIndex_WeightedAverage,
    
    GovernmentResponseIndex_NonVaccinated,
    GovernmentResponseIndex_Vaccinated,
    GovernmentResponseIndex_SimpleAverage,
    GovernmentResponseIndex_WeightedAverage,
    
    # VACCINATION DATA
    PopulationVaccinated,
    vax_rate,
    
    # KEY OUTCOME VARIABLES
    ConfirmedCases,
    ConfirmedDeaths,
    
    # H-POLICIES for mechanism analysis
    H1_adj,                               # Our calculated H1
    H2_adj,                               # Our calculated H2
    H3_adj,                               # Our calculated H3
    H6_PopWeighted,                       # Our calculated H6 (pop-weighted)
    H7_adj,                               # Our calculated H7
    H8_PopWeighted,                       # Our calculated H8 (pop-weighted)
    H1E_Public.information.campaigns,     # Raw H1 from Oxford
    
    # EVERYTHING ELSE
    everything()
  )

colnames(oxd_d_spatial)

cat("✅ Dataset reorganized\n\n")


# SHOW NEW STRUCTURE

cat("=== NEW DATASET STRUCTURE (First 30 columns) ===\n\n")
cat(paste(names(oxd_d_spatial)[1:30], collapse = "\n"), "\n\n")

cat("First 3 observations of key variables:\n\n")
print(head(select(oxd_d_spatial, 
                  Country, Date,
                  StringencyIndex_PopWeighted,
                  StringencyH1_PopWeighted,
                  ControlIndex_PopWeighted,
                  #ContainmentHealthIndex_PopWeighted,
                  StringencyIndex_WeightedAverage,
                  PopulationVaccinated), 3))

# SUMMARY OF AVAILABLE INDICES

cat("\n", rep("=", 80), "\n", sep = "")
cat("SUMMARY: AVAILABLE INDICES FOR ANALYSIS\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("📊 PRIMARY INDICES (Use these!):\n\n")

cat("1️⃣  StringencyIndex_PopWeighted\n")
cat("   • Components: C1-C8 (containment only)\n")
cat("   • Population-weighted by vaccination status\n")
cat("   • Mean:", round(mean(oxd_d_spatial$StringencyIndex_PopWeighted, na.rm=TRUE), 2), "\n")
cat("   • Use for: MAIN ANALYSIS\n")
cat("   • Interpretation: Restrictive policies only\n\n")

cat("2️⃣  StringencyH1_PopWeighted\n")
cat("   • Components: C1-C8 + H1 (containment + public info)\n")
cat("   • Population-weighted by vaccination status\n")
cat("   • Mean:", round(mean(oxd_d_spatial$StringencyH1_PopWeighted, na.rm=TRUE), 2), "\n")
cat("   • Use for: ROBUSTNESS CHECK\n")
cat("   • Interpretation: Matches Oxford's definition\n\n")

cat("3️⃣  ControlIndex_PopWeighted\n")
cat("   • Components: H1, H2, H3, H6, H7, H8 (comprehensive)\n")
cat("   • Population-weighted by vaccination status\n")
cat("   • Mean:", round(mean(oxd_d_spatial$ControlIndex_PopWeighted, na.rm=TRUE), 2), "\n")
cat("   • Use for: ROBUSTNESS CHECK / ALTERNATIVE SPECIFICATION\n")
cat("   • Interpretation: All health policies\n\n")

cat("3️⃣ ContainmentHealthIndex_PopWeighted\n")
cat("   • Components: C1-C8 + H1, H2, H3, H6, H7, H8 (comprehensive)\n")
cat("   • Population-weighted by vaccination status\n")
cat("   • Mean:", round(mean(oxd_d_spatial$ContainmentHealthIndex_PopWeighted, na.rm=TRUE), 2), "\n")
cat("   • Use for: ROBUSTNESS CHECK / ALTERNATIVE SPECIFICATION\n")
cat("   • Interpretation: All containment + health policies\n\n")

cat("📋 OXFORD'S ORIGINAL INDICES (For comparison):\n\n")

cat("4️⃣  StringencyIndex_WeightedAverage\n")
cat("   • Oxford's official Stringency Index\n")
cat("   • Includes FLAG weighting\n")
cat("   • Mean:", round(mean(oxd_d_spatial$StringencyIndex_WeightedAverage, na.rm=TRUE), 2), "\n")
cat("   • Use for: Validation/comparison\n\n")

cat("5️⃣  ContainmentHealthIndex_WeightedAverage\n")
cat("   • Oxford's Containment & Health Index\n")
cat("   • Components: C1-C8 + H1, H2, H3, H6, H7, H8\n")
cat("   • Mean:", round(mean(oxd_d_spatial$ContainmentHealthIndex_WeightedAverage, na.rm=TRUE), 2), "\n")
cat("   • Use for: Alternative measure (includes health policies)\n\n")

# CORRELATION MATRIX

cat("=== CORRELATION MATRIX: Key Indices ===\n\n")

cor_matrix <- oxd_d_spatial %>%
  select(
    `Stringency (C1-C8) (Ours)` = StringencyIndex_PopWeighted,
    `C1-C8+H1 (Ours)` = StringencyH1_PopWeighted,
    `Control (Ours)` = ControlIndex_PopWeighted,
    `Oxford Stringency` = StringencyIndex_WeightedAverage,
  ) %>%
  cor(use = "complete.obs")

print(kable(round(cor_matrix, 3), 
            caption = "Correlations between different index specifications"))

# QUICK VALIDATION TABLE

cat("\n=== VALIDATION: Summary Statistics ===\n\n")

validation_summary <- data.frame(
  Index = c("StringencyIndex_PopWeighted (C1-C8)",
            "StringencyH1_PopWeighted (C1-C8+H1)",
            "ControlIndex_PopWeighted (H1-H8)",
            "ContainmentHealthIndex_PopWeighted (C1-C8+H1-H8)",
            "Oxford StringencyIndex",
            "Oxford ContainmentHealthIndex"),
  Mean = c(
    mean(oxd_d_spatial$StringencyIndex_PopWeighted, na.rm = TRUE),
    mean(oxd_d_spatial$StringencyH1_PopWeighted, na.rm = TRUE),
    mean(oxd_d_spatial$ControlIndex_PopWeighted, na.rm = TRUE),
    mean(oxd_d_spatial$ContainmentHealthIndex_PopWeighted, na.rm = TRUE),
    mean(oxd_d_spatial$StringencyIndex_WeightedAverage, na.rm = TRUE),
    mean(oxd_d_spatial$ContainmentHealthIndex_WeightedAverage, na.rm = TRUE)
  ),
  SD = c(
    sd(oxd_d_spatial$StringencyIndex_PopWeighted, na.rm = TRUE),
    sd(oxd_d_spatial$StringencyH1_PopWeighted, na.rm = TRUE),
    sd(oxd_d_spatial$ControlIndex_PopWeighted, na.rm = TRUE),
    sd(oxd_d_spatial$ContainmentHealthIndex_PopWeighted, na.rm = TRUE),
    sd(oxd_d_spatial$StringencyIndex_WeightedAverage, na.rm = TRUE),
    sd(oxd_d_spatial$ContainmentHealthIndex_WeightedAverage, na.rm = TRUE)
  ),
  Min = c(
    min(oxd_d_spatial$StringencyIndex_PopWeighted, na.rm = TRUE),
    min(oxd_d_spatial$StringencyH1_PopWeighted, na.rm = TRUE),
    min(oxd_d_spatial$ControlIndex_PopWeighted, na.rm = TRUE),
    min(oxd_d_spatial$ContainmentHealthIndex_PopWeighted, na.rm = TRUE),
    min(oxd_d_spatial$StringencyIndex_WeightedAverage, na.rm = TRUE),
    min(oxd_d_spatial$ContainmentHealthIndex_WeightedAverage, na.rm = TRUE)
  ),
  Max = c(
    max(oxd_d_spatial$StringencyIndex_PopWeighted, na.rm = TRUE),
    max(oxd_d_spatial$StringencyH1_PopWeighted, na.rm = TRUE),
    max(oxd_d_spatial$ControlIndex_PopWeighted, na.rm = TRUE),
    max(oxd_d_spatial$ContainmentHealthIndex_PopWeighted, na.rm = TRUE),
    max(oxd_d_spatial$StringencyIndex_WeightedAverage, na.rm = TRUE),
    max(oxd_d_spatial$ContainmentHealthIndex_WeightedAverage, na.rm = TRUE)
  )
)

print(kable(validation_summary, digits = 2,
            caption = "Summary Statistics: All Available Indices"))

# COMPARISON: Our Control vs Oxford's C&H

cat("\n=== COMPARISON: ControlIndex vs Oxford C&H Index ===\n\n")

control_comparison <- oxd_d_spatial %>%
  summarize(
    Our_Control = mean(ControlIndex_PopWeighted, na.rm = TRUE),
    Oxford_CH = mean(ContainmentHealthIndex_WeightedAverage, na.rm = TRUE),
    Difference = Our_Control - Oxford_CH,
    Correlation = cor(ControlIndex_PopWeighted,
                      ContainmentHealthIndex_WeightedAverage,
                      use = "complete.obs"),
    RMSE = sqrt(mean((ControlIndex_PopWeighted - 
                        ContainmentHealthIndex_WeightedAverage)^2,
                     na.rm = TRUE))
  )

print(kable(control_comparison, digits = 3,
            caption = "Our Control Index vs Oxford's C&H Index"))

##ALLE INDEXE BERECHNET AUF TAGESDATEN, oxd_d_spatial bleibt bestehen, jetzt aber in gleiche Form wie die anderen Variablen bringen

# ==============================================================================
#AGGREGATE INDICES UP TO QUARTERLY DATA
# AGGREGATE INDICES TO QUARTERLY DATA (WITH C1-C8 POLICIES)
# ==============================================================================
cat("\n", rep("=", 80), "\n", sep = "")
cat("AGGREGATING INDICES TO QUARTERLY DATA\n")
cat("Including C1-C8 policies for mechanism analysis\n")
cat(rep("=", 80), "\n\n", sep = "")

# STEP 1: Select ONLY index variables + identifiers + C1-C8 policies

cat("=== Selecting index variables and C1-C8 policies ===\n\n")

oxd_d_spatial <- oxd_d_spatial |>
  dplyr::rename(
    Country = CountryCode
  )


oxd_d_spatial_indices <- oxd_d_spatial %>%
  select(
    # Identifiers
    Country, Date,
    
    # OUR MAIN INDICES (Population-weighted)
    StringencyIndex_PopWeighted,          # C1-C8
    StringencyH1_PopWeighted,             # C1-C8 + H1
    ControlIndex_PopWeighted,             # C1-C8 + H1-H8
    #ContainmentHealthIndex_PopWeighted,   # Also C1-C8 + H1-H8
    
    # Component indices (NV/V)
    StringencyIndex_NV,
    StringencyIndex_V,
    StringencyH1_NV,
    StringencyH1_V,
    ControlIndex_NV,
    ControlIndex_V,
    
    # OXFORD'S STRINGENCY INDICES
    StringencyIndex_NonVaccinated,
    StringencyIndex_Vaccinated,
    StringencyIndex_SimpleAverage,
    StringencyIndex_WeightedAverage,
    
    # OXFORD'S CONTAINMENT & HEALTH INDICES
    ContainmentHealthIndex_NonVaccinated,
    ContainmentHealthIndex_Vaccinated,
    ContainmentHealthIndex_SimpleAverage,
    ContainmentHealthIndex_WeightedAverage,
    
    # OXFORD'S GOVERNMENT RESPONSE INDICES
    GovernmentResponseIndex_NonVaccinated,
    GovernmentResponseIndex_Vaccinated,
    GovernmentResponseIndex_SimpleAverage,
    GovernmentResponseIndex_WeightedAverage,
    
    # VACCINATION DATA
    PopulationVaccinated,
    vax_rate,
    
    # C1-C8 POLICIES (for mechanism analysis)
    C1_NV_adj,  # School closing
    C2_NV_adj,  # Workplace closing
    C3_NV_adj,  # Cancel public events
    C4_NV_adj,  # Restrictions on gatherings
    C5_NV_adj,  # Close public transport
    C6_NV_adj,  # Stay at home requirements
    C7_NV_adj,  # Restrictions on internal movement
    C8_NV_adj,  # International travel controls
    
    C1_V_adj,   # School closing (vaccinated)
    C2_V_adj,   # Workplace closing (vaccinated)
    C3_V_adj,   # Cancel public events (vaccinated)
    C4_V_adj,   # Restrictions on gatherings (vaccinated)
    C5_V_adj,   # Close public transport (vaccinated)
    C6_V_adj,   # Stay at home requirements (vaccinated)
    C7_V_adj,   # Restrictions on internal movement (vaccinated)
    C8_V_adj,   # International travel controls (vaccinated)
    
    # H-POLICIES (for mechanism analysis)
    H1_adj,
    H2_adj,
    H3_adj,
    H6_PopWeighted,
    H7_adj,
    H8_PopWeighted
  )

cat("✅ Selected", ncol(oxd_d_spatial_indices) - 2, "variables (", nrow(oxd_d_spatial_indices), "observations)\n\n")

# STEP 2: Create Quarter variable (Format: Q1.2020)

cat("=== Creating Quarter variable ===\n\n")
oxd_d_spatial_indices <- oxd_d_spatial_indices %>%
  mutate(
    # FIX: Das Datum ist aktuell eine Zahl (20200101). Wir wandeln es in ein Datum um.
    Date_Parsed = ymd(Date), 
    
    # Jetzt basieren Jahr und Quartal auf dem echten Datum
    Year = year(Date_Parsed),
    Quarter_Num = quarter(Date_Parsed),
    Quarter = paste0("Q", Quarter_Num, ".", Year)
  )
# Show unique quarters
unique_quarters <- sort(unique(oxd_d_spatial_indices$Quarter))
cat("Available quarters:\n")
cat(paste(head(unique_quarters, 12), collapse = ", "), "\n")
cat("...\n")
cat(paste(tail(unique_quarters, 12), collapse = ", "), "\n")
cat("Total quarters:", length(unique_quarters), "\n\n")

# STEP 3: Aggregate to quarterly level

cat("=== Aggregating to quarterly level ===\n\n")

oxd_d_spatial_quarterly <- oxd_d_spatial_indices %>%
  group_by(Country, Quarter, Year, Quarter_Num) %>%
  summarize(
    # OUR MAIN INDICES (average over quarter)
    StringencyIndex_PopWeighted = mean(StringencyIndex_PopWeighted, na.rm = TRUE),
    StringencyH1_PopWeighted = mean(StringencyH1_PopWeighted, na.rm = TRUE),
    ControlIndex_PopWeighted = mean(ControlIndex_PopWeighted, na.rm = TRUE),
    
    # Component indices
    StringencyIndex_NV = mean(StringencyIndex_NV, na.rm = TRUE),
    StringencyIndex_V = mean(StringencyIndex_V, na.rm = TRUE),
    StringencyH1_NV = mean(StringencyH1_NV, na.rm = TRUE),
    StringencyH1_V = mean(StringencyH1_V, na.rm = TRUE),
    ControlIndex_NV = mean(ControlIndex_NV, na.rm = TRUE),
    ControlIndex_V = mean(ControlIndex_V, na.rm = TRUE),
    
    # OXFORD'S STRINGENCY INDICES
    StringencyIndex_NonVaccinated = mean(StringencyIndex_NonVaccinated, na.rm = TRUE),
    StringencyIndex_Vaccinated = mean(StringencyIndex_Vaccinated, na.rm = TRUE),
    StringencyIndex_SimpleAverage = mean(StringencyIndex_SimpleAverage, na.rm = TRUE),
    StringencyIndex_WeightedAverage = mean(StringencyIndex_WeightedAverage, na.rm = TRUE),
    
    # OXFORD'S CONTAINMENT & HEALTH INDICES
    ContainmentHealthIndex_NonVaccinated = mean(ContainmentHealthIndex_NonVaccinated, na.rm = TRUE),
    ContainmentHealthIndex_Vaccinated = mean(ContainmentHealthIndex_Vaccinated, na.rm = TRUE),
    ContainmentHealthIndex_SimpleAverage = mean(ContainmentHealthIndex_SimpleAverage, na.rm = TRUE),
    ContainmentHealthIndex_WeightedAverage = mean(ContainmentHealthIndex_WeightedAverage, na.rm = TRUE),
    
    # OXFORD'S GOVERNMENT RESPONSE INDICES
    GovernmentResponseIndex_NonVaccinated = mean(GovernmentResponseIndex_NonVaccinated, na.rm = TRUE),
    GovernmentResponseIndex_Vaccinated = mean(GovernmentResponseIndex_Vaccinated, na.rm = TRUE),
    GovernmentResponseIndex_SimpleAverage = mean(GovernmentResponseIndex_SimpleAverage, na.rm = TRUE),
    GovernmentResponseIndex_WeightedAverage = mean(GovernmentResponseIndex_WeightedAverage, na.rm = TRUE),
    
    # VACCINATION (end-of-quarter value)
    PopulationVaccinated = last(PopulationVaccinated, na_rm = TRUE),
    vax_rate = last(vax_rate, na_rm = TRUE),
    
    # C1-C8 POLICIES (average over quarter)
    C1_NV_adj = mean(C1_NV_adj, na.rm = TRUE),
    C2_NV_adj = mean(C2_NV_adj, na.rm = TRUE),
    C3_NV_adj = mean(C3_NV_adj, na.rm = TRUE),
    C4_NV_adj = mean(C4_NV_adj, na.rm = TRUE),
    C5_NV_adj = mean(C5_NV_adj, na.rm = TRUE),
    C6_NV_adj = mean(C6_NV_adj, na.rm = TRUE),
    C7_NV_adj = mean(C7_NV_adj, na.rm = TRUE),
    C8_NV_adj = mean(C8_NV_adj, na.rm = TRUE),
    
    C1_V_adj = mean(C1_V_adj, na.rm = TRUE),
    C2_V_adj = mean(C2_V_adj, na.rm = TRUE),
    C3_V_adj = mean(C3_V_adj, na.rm = TRUE),
    C4_V_adj = mean(C4_V_adj, na.rm = TRUE),
    C5_V_adj = mean(C5_V_adj, na.rm = TRUE),
    C6_V_adj = mean(C6_V_adj, na.rm = TRUE),
    C7_V_adj = mean(C7_V_adj, na.rm = TRUE),
    C8_V_adj = mean(C8_V_adj, na.rm = TRUE),
    
    # Population-weighted C-policies
    C1_PopWeighted = (1 - last(vax_rate, na_rm = TRUE)) * mean(C1_NV_adj, na.rm = TRUE) + 
      last(vax_rate, na_rm = TRUE) * mean(C1_V_adj, na.rm = TRUE),
    C2_PopWeighted = (1 - last(vax_rate, na_rm = TRUE)) * mean(C2_NV_adj, na.rm = TRUE) + 
      last(vax_rate, na_rm = TRUE) * mean(C2_V_adj, na.rm = TRUE),
    C3_PopWeighted = (1 - last(vax_rate, na_rm = TRUE)) * mean(C3_NV_adj, na.rm = TRUE) + 
      last(vax_rate, na_rm = TRUE) * mean(C3_V_adj, na.rm = TRUE),
    C4_PopWeighted = (1 - last(vax_rate, na_rm = TRUE)) * mean(C4_NV_adj, na.rm = TRUE) + 
      last(vax_rate, na_rm = TRUE) * mean(C4_V_adj, na.rm = TRUE),
    C5_PopWeighted = (1 - last(vax_rate, na_rm = TRUE)) * mean(C5_NV_adj, na.rm = TRUE) + 
      last(vax_rate, na_rm = TRUE) * mean(C5_V_adj, na.rm = TRUE),
    C6_PopWeighted = (1 - last(vax_rate, na_rm = TRUE)) * mean(C6_NV_adj, na.rm = TRUE) + 
      last(vax_rate, na_rm = TRUE) * mean(C6_V_adj, na.rm = TRUE),
    C7_PopWeighted = (1 - last(vax_rate, na_rm = TRUE)) * mean(C7_NV_adj, na.rm = TRUE) + 
      last(vax_rate, na_rm = TRUE) * mean(C7_V_adj, na.rm = TRUE),
    C8_PopWeighted = (1 - last(vax_rate, na_rm = TRUE)) * mean(C8_NV_adj, na.rm = TRUE) + 
      last(vax_rate, na_rm = TRUE) * mean(C8_V_adj, na.rm = TRUE),
    
    # H-POLICIES (average over quarter)
    H1_adj = mean(H1_adj, na.rm = TRUE),
    H2_adj = mean(H2_adj, na.rm = TRUE),
    H3_adj = mean(H3_adj, na.rm = TRUE),
    H6_PopWeighted = mean(H6_PopWeighted, na.rm = TRUE),
    H7_adj = mean(H7_adj, na.rm = TRUE),
    H8_PopWeighted = mean(H8_PopWeighted, na.rm = TRUE),
    
    # Metadata
    N_days = n(),  # Number of days in quarter with data
    
    .groups = "drop"
  ) %>%
  # Sort by Country and Quarter
  arrange(Country, Year, Quarter_Num) %>%
  # Reorganize: Put main indices first, then C-policies, then H-policies
  select(
    # Identifiers
    Country, Quarter, Year, Quarter_Num,
    
    # Our 4 main indices
    StringencyIndex_PopWeighted,
    StringencyH1_PopWeighted,
    ControlIndex_PopWeighted,
    
    # C1-C8 policies (population-weighted - for mechanism analysis)
    C1_PopWeighted,  # School closing
    C2_PopWeighted,  # Workplace closing
    C3_PopWeighted,  # Cancel public events
    C4_PopWeighted,  # Restrictions on gatherings
    C5_PopWeighted,  # Close public transport
    C6_PopWeighted,  # Stay at home
    C7_PopWeighted,  # Internal movement
    C8_PopWeighted,  # International travel
    
    # H-policies (for mechanism analysis)
    H1_adj,
    H2_adj,
    H3_adj,
    H6_PopWeighted,
    H7_adj,
    H8_PopWeighted,
    
    # Vaccination
    PopulationVaccinated,
    vax_rate,
    
    # Everything else
    everything()
  )

cat("✅ Aggregated to", nrow(oxd_d_spatial_quarterly), "country-quarter observations\n\n")

# STEP 4: Summary and validation

cat("=== SUMMARY ===\n\n")

summary_quarterly <- oxd_d_spatial_quarterly %>%
  summarize(
    N_countries = n_distinct(Country),
    N_quarters = n_distinct(Quarter),
    N_observations = n(),
    First_quarter = min(Quarter),
    Last_quarter = max(Quarter),
    Mean_days_per_quarter = mean(N_days, na.rm = TRUE)
  )

print(kable(summary_quarterly, 
            caption = "Quarterly Dataset Summary"))
head(oxd_d_spatial_indices)
# Summary of C-policies
cat("\n\n=== C-POLICIES: Summary Statistics ===\n\n")

c_policies_summary <- oxd_d_spatial_quarterly %>%
  summarize(
    across(c(C1_PopWeighted, C2_PopWeighted, C3_PopWeighted, C4_PopWeighted,
             C5_PopWeighted, C6_PopWeighted, C7_PopWeighted, C8_PopWeighted),
           list(Mean = ~mean(., na.rm = TRUE),
                SD = ~sd(., na.rm = TRUE),
                Min = ~min(., na.rm = TRUE),
                Max = ~max(., na.rm = TRUE)),
           .names = "{.col}_{.fn}")
  ) %>%
  pivot_longer(everything(),
               names_to = c("Policy", ".value"),
               names_pattern = "(.+)_(Mean|SD|Min|Max)")

print(kable(c_policies_summary, digits = 2))

# Show first observations with C-policies
cat("\n\n=== First 5 observations (with C-policies) ===\n\n")
print(kable(head(select(oxd_d_spatial_quarterly, 
                        Country, Quarter,
                        StringencyIndex_PopWeighted,
                        C1_PopWeighted, C2_PopWeighted, C6_PopWeighted,
                        H1_adj, H6_PopWeighted), 5), 
            digits = 2))

# Check Germany
cat("\n\n=== Germany: Quarterly data with C-policies ===\n\n")
germany_quarterly <- oxd_d_spatial_quarterly %>%
  filter(Country == "DEU") %>%
  select(Quarter, 
         StringencyIndex_PopWeighted,
         C1_PopWeighted, C2_PopWeighted, C3_PopWeighted,
         C4_PopWeighted, C5_PopWeighted, C6_PopWeighted,
         C7_PopWeighted, C8_PopWeighted,
         PopulationVaccinated)

print(kable(head(germany_quarterly, 8), digits = 2))

# STEP 5: Which C-policies varied most?

cat("\n\n=== WHICH C-POLICIES VARIED MOST? ===\n\n")

c_variation <- oxd_d_spatial_quarterly %>%
  summarize(
    across(c(C1_PopWeighted:C8_PopWeighted),
           list(SD = ~sd(., na.rm = TRUE),
                CV = ~sd(., na.rm = TRUE) / mean(., na.rm = TRUE)),
           .names = "{.col}_{.fn}")
  ) %>%
  pivot_longer(everything(),
               names_to = c("Policy", "Metric"),
               names_pattern = "(.+)_(SD|CV)",
               values_to = "Value") %>%
  pivot_wider(names_from = Metric, values_from = Value) %>%
  arrange(desc(CV))

policy_labels <- c(
  "C1_PopWeighted" = "C1: School closing",
  "C2_PopWeighted" = "C2: Workplace closing",
  "C3_PopWeighted" = "C3: Cancel events",
  "C4_PopWeighted" = "C4: Gatherings",
  "C5_PopWeighted" = "C5: Public transport",
  "C6_PopWeighted" = "C6: Stay at home",
  "C7_PopWeighted" = "C7: Internal movement",
  "C8_PopWeighted" = "C8: International travel"
)

c_variation <- c_variation %>%
  mutate(Policy_Label = policy_labels[Policy])

print(kable(c_variation, digits = 3,
            caption = "C-policies ranked by variation (CV = Coefficient of Variation)"))

cat("\nInterpretation:\n")
cat("  Higher CV = More variation across countries/time\n")
cat("  → These policies are most useful for identification\n\n")

# STEP 6: Visualization - C-policies over time

cat("=== Creating C-policies visualization ===\n\n")

# Average C-policies over time
p_c_policies <- oxd_d_spatial_quarterly %>%
  group_by(Quarter, Year, Quarter_Num) %>%
  summarize(
    across(C1_PopWeighted:C8_PopWeighted,
           ~mean(., na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  arrange(Year, Quarter_Num) %>%
  mutate(Quarter_Seq = row_number()) %>%
  pivot_longer(cols = C1_PopWeighted:C8_PopWeighted,
               names_to = "Policy",
               values_to = "Value") %>%
  mutate(Policy_Label = policy_labels[Policy]) %>%
  ggplot(aes(x = Quarter_Seq, y = Value, color = Policy_Label)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  labs(
    title = "C-Policies Over Time (Quarterly Average)",
    subtitle = "Which containment policies were used when?",
    x = "Quarter",
    y = "Policy Stringency (0-100)",
    color = "Policy"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

print(p_c_policies)



oxd_d_spatial_q <- oxd_d_spatial_quarterly

##Stringency korrigiert für alle erstellt

# ==============================================================================
# Spatial Lag für alle erstellt
# ==============================================================================
# VOLLSTÄNDIGE NACHBARSCHAFTSMATRIX FÜR 38 OECD-LÄNDER
# Alle geografischen Nachbarn aus dem 185-Länder-Sample

oxd_d_all<-oxd_d_spatial

neighbors_list <- list(
  # === EUROPA ===
  "AUT" = c("DEU", "CHE", "ITA", "SVN", "HUN", "SVK", "CZE", "LIE"),
  "BEL" = c("FRA", "DEU", "NLD", "LUX"),
  "CHE" = c("DEU", "FRA", "ITA", "AUT", "LIE"),
  "CZE" = c("DEU", "AUT", "POL", "SVK"),
  "DEU" = c("AUT", "BEL", "CHE", "CZE", "DNK", "FRA", "LUX", "NLD", "POL"),
  "DNK" = c("DEU", "SWE"),
  "ESP" = c("FRA", "PRT", "AND", "MAR"),
  "EST" = c("LVA", "RUS"),
  "FIN" = c("SWE", "NOR", "RUS"),
  "FRA" = c("BEL", "DEU", "CHE", "ITA", "ESP", "LUX", "AND", "MCO"),
  "GBR" = c("IRL"),
  "GRC" = c("ALB", "BGR", "TUR"),
  "HUN" = c("AUT", "SVK", "UKR", "ROU", "SRB", "HRV", "SVN"),
  "IRL" = c("GBR"),
  "ISL" = c("NOR", "GBR", "FRO"),
  "ITA" = c("AUT", "CHE", "FRA", "SVN", "SMR"),
  "LTU" = c("LVA", "POL", "BLR", "RUS"),
  "LUX" = c("BEL", "DEU", "FRA"),
  "LVA" = c("EST", "LTU", "BLR", "RUS"),
  "NLD" = c("BEL", "DEU"),
  "NOR" = c("SWE", "FIN", "RUS"),
  "POL" = c("DEU", "CZE", "SVK", "UKR", "BLR", "LTU", "RUS"),
  "PRT" = c("ESP"),
  "SVK" = c("AUT", "CZE", "HUN", "POL", "UKR"),
  "SVN" = c("AUT", "ITA", "HUN", "HRV"),
  "SWE" = c("NOR", "FIN", "DNK"),
  "TUR" = c("BGR", "GRC", "GEO", "IRN", "IRQ", "SYR"),
  
  # === AMERIKA ===
  "CAN" = c("USA"),
  "CHL" = c("ARG", "PER", "BOL"),
  "COL" = c("VEN", "BRA", "PER", "ECU", "PAN"),
  "CRI" = c("NIC", "PAN"),
  "MEX" = c("USA", "GTM", "BLZ"),
  "USA" = c("CAN", "MEX"),
  
  # === NAHER OSTEN ===
  "ISR" = c("EGY", "JOR", "LBN", "SYR", "PSE"),
  
  # === ASIEN-PAZIFIK (Meeresnachbarn) ===
  "AUS" = c("NZL", "IDN", "PNG"),
  "JPN" = c("KOR", "CHN", "RUS"),
  "KOR" = c("JPN", "CHN"),
  "NZL" = c("AUS")
)


calculate_neighbor_stringency_fast <- function(df_daily, oxd_d_all, neighbors_list) {
  
  
  # 1. Datenvorbereitung & Datums-Sicherheit
  oxd_d_clean <- oxd_d_all |>
    dplyr::mutate(Date = as.Date(Date))
  
  df_daily <- df_daily |>
    dplyr::mutate(Date = as.Date(Date))
  
  # 2. Nachbarschafts-Liste flachklopfen
  neighbors_df <- tibble::enframe(neighbors_list, name = "Country", value = "neighbor") |>
    tidyr::unnest_longer(neighbor)
  
  # 3. Lookup-Tabelle erstellen (KRITISCH: Duplikate entfernen!)
  # Wir gruppieren vorher, falls es doch mehrere Einträge pro Land/Tag gibt (z.B. Regionen),
  # und nehmen den Durchschnitt oder den ersten Wert. Hier: distinct + mean sicherheitshalber.
  stringency_lookup <- oxd_d_clean |>
    dplyr::select(Country, Date, StringencyIndex_PopWeighted) |>
    # Sicherheitsnetz: Falls oxd_d_all Duplikate hat -> bereinigen
    dplyr::group_by(Country, Date) |>
    dplyr::summarise(StringencyIndex_PopWeighted = mean(StringencyIndex_PopWeighted, na.rm = TRUE), .groups = "drop") |>
    dplyr::rename(
      neighbor     = Country,
      neighbor_str = StringencyIndex_PopWeighted
    )
  
  # 4. Die eigentliche Berechnung
  # Wir starten mit den einzigartigen Land/Datum Kombinationen aus df_daily,
  # um Rechenzeit zu sparen (statt den ganzen df_daily zu nutzen)
  neighbor_stringency_calc <- df_daily |>
    dplyr::distinct(Country, Date) |>
    # A) Nachbarn anfügen (Zeilenvermehrung: 1 Land -> X Nachbarn)
    dplyr::left_join(neighbors_df, by = "Country", relationship = "many-to-many") |>
    # B) Stringency der Nachbarn anfügen
    dplyr::left_join(stringency_lookup, by = c("neighbor", "Date")) |>
    # C) Aggregieren
    dplyr::group_by(Country, Date) |>
    dplyr::summarise(
      neighbor_stringency = mean(neighbor_str, na.rm = TRUE),
      n_neighbors_data    = sum(!is.na(neighbor_str)), # Wieviele Nachbarn hatten Daten?
      .groups = "drop"
    ) |>
    # D) NaN zu NA konvertieren (passiert, wenn alle Nachbarn NA sind)
    dplyr::mutate(neighbor_stringency = ifelse(is.nan(neighbor_stringency), NA, neighbor_stringency))
  
  # 5. Ergebnis an den originalen Dataframe spielen
  result <- df_daily |>
    dplyr::left_join(neighbor_stringency_calc, by = c("Country", "Date"))
  
  return(result)
}

# Ausführung
df_daily <- calculate_neighbor_stringency_fast(df_daily, oxd_d_all, neighbors_list)


# kurz checken:
names(df_daily)
# hier solltest du "neighbor_stringency" und "n_neighbors" sehen


df_daily <- df_daily %>%
  dplyr::group_by(Country) %>%
  dplyr::arrange(Country, Date) %>%
  dplyr::mutate(
    lag7_neighbor_stringency  = dplyr::lag(neighbor_stringency, 7),
    lag14_neighbor_stringency = dplyr::lag(neighbor_stringency, 14),
    delta_neighbor_stringency = neighbor_stringency - dplyr::lag(neighbor_stringency, 7)
  ) %>%
  dplyr::ungroup()


# D7d + vollständige Nachbar-Stringency
prf_d7d_spatial_full <- feols(
  StringencyIndex_PopWeighted ~ 
    lag7_stringency +
    lag14_log_cases + lag7_log_deaths +
    lag7_R +
    lag7_neighbor_stringency +
    delta_neighbor_stringency |
    country_id + week_id,
  data = df_daily,
  cluster = ~country_id
)

summary(prf_d7d_spatial_full)

##WHAT IS IF THE NEIGHBOURS DO NOT REACT-> NO STRINGENCY BUT HAD A STRONG PRESSURE
##ADDING R-RATE FROM THE NEIGHBOURS

rrate_all<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/r_rate.csv", header=TRUE, sep=",")

drop_these <- c("Summer Olympics 2020")

rrate_all <- rrate_all %>%
  pivot_wider(
    names_from  = days_infectious,
    values_from = c(R, ci_95_u, ci_95_l),
    names_glue  = "{.value}_di{days_infectious}"
  )

head(rrate_all)
unique(rrate_all$Country.Region)
# ==============================================================================
# 1. COUNTRY NAME TO ISO3 MAPPING
# ==============================================================================

country_to_iso3 <- c(
  "Afghanistan" = "AFG",
  "Albania" = "ALB",
  "Algeria" = "DZA",
  "Andorra" = "AND",
  "Angola" = "AGO",
  "Argentina" = "ARG",
  "Aruba" = "ABW",
  "Australia" = "AUS",
  "Austria" = "AUT",
  "Azerbaijan" = "AZE",
  "Bahamas" = "BHS",
  "Bahrain" = "BHR",
  "Bangladesh" = "BGD",
  "Barbados" = "BRB",
  "Belarus" = "BLR",
  "Belgium" = "BEL",
  "Belize" = "BLZ",
  "Benin" = "BEN",
  "Bermuda" = "BMU",
  "Bhutan" = "BTN",
  "Bolivia" = "BOL",
  "Bosnia and Herzegovina" = "BIH",
  "Botswana" = "BWA",
  "Brazil" = "BRA",
  "Brunei" = "BRN",
  "Bulgaria" = "BGR",
  "Burkina Faso" = "BFA",
  "Burundi" = "BDI",
  "Cambodia" = "KHM",
  "Cameroon" = "CMR",
  "Canada" = "CAN",
  "Cape Verde" = "CPV",
  "Central African Republic" = "CAF",
  "Chad" = "TCD",
  "Chile" = "CHL",
  "China" = "CHN",
  "Colombia" = "COL",
  "Congo" = "COG",
  "Costa Rica" = "CRI",
  "Cote d'Ivoire" = "CIV",
  "Croatia" = "HRV",
  "Cuba" = "CUB",
  "Cyprus" = "CYP",
  "Czech Republic" = "CZE",
  "Czechia" = "CZE",
  "Democratic Republic of Congo" = "COD",
  "Denmark" = "DNK",
  "Djibouti" = "DJI",
  "Dominica" = "DMA",
  "Dominican Republic" = "DOM",
  "Ecuador" = "ECU",
  "Egypt" = "EGY",
  "El Salvador" = "SLV",
  "Eritrea" = "ERI",
  "Estonia" = "EST",
  "Eswatini" = "SWZ",
  "Ethiopia" = "ETH",
  "Faroe Islands" = "FRO",
  "Fiji" = "FJI",
  "Finland" = "FIN",
  "France" = "FRA",
  "Gabon" = "GAB",
  "Gambia" = "GMB",
  "Georgia" = "GEO",
  "Germany" = "DEU",
  "Ghana" = "GHA",
  "Greece" = "GRC",
  "Greenland" = "GRL",
  "Guam" = "GUM",
  "Guatemala" = "GTM",
  "Guinea" = "GIN",
  "Guyana" = "GUY",
  "Haiti" = "HTI",
  "Honduras" = "HND",
  "Hong Kong" = "HKG",
  "Hungary" = "HUN",
  "Iceland" = "ISL",
  "India" = "IND",
  "Indonesia" = "IDN",
  "Iran" = "IRN",
  "Iraq" = "IRQ",
  "Ireland" = "IRL",
  "Israel" = "ISR",
  "Italy" = "ITA",
  "Jamaica" = "JAM",
  "Japan" = "JPN",
  "Jordan" = "JOR",
  "Kazakhstan" = "KAZ",
  "Kenya" = "KEN",
  "Kiribati" = "KIR",
  "Kosovo" = "RKS",
  "Kuwait" = "KWT",
  "Kyrgyzstan" = "KGZ",
  "Laos" = "LAO",
  "Latvia" = "LVA",
  "Lebanon" = "LBN",
  "Lesotho" = "LSO",
  "Liberia" = "LBR",
  "Libya" = "LBY",
  "Liechtenstein" = "LIE",
  "Lithuania" = "LTU",
  "Luxembourg" = "LUX",
  "Macao" = "MAC",
  "Madagascar" = "MDG",
  "Malawi" = "MWI",
  "Malaysia" = "MYS",
  "Mali" = "MLI",
  "Malta" = "MLT",
  "Mauritania" = "MRT",
  "Mauritius" = "MUS",
  "Mexico" = "MEX",
  "Moldova" = "MDA",
  "Monaco" = "MCO",
  "Mongolia" = "MNG",
  "Morocco" = "MAR",
  "Mozambique" = "MOZ",
  "Myanmar" = "MMR",
  "Namibia" = "NAM",
  "Nepal" = "NPL",
  "Netherlands" = "NLD",
  "New Zealand" = "NZL",
  "Nicaragua" = "NIC",
  "Niger" = "NER",
  "Nigeria" = "NGA",
  "North Korea" = "PRK",
  "Norway" = "NOR",
  "Oman" = "OMN",
  "Pakistan" = "PAK",
  "Palestine" = "PSE",
  "Panama" = "PAN",
  "Papua New Guinea" = "PNG",
  "Paraguay" = "PRY",
  "Peru" = "PER",
  "Philippines" = "PHL",
  "Poland" = "POL",
  "Portugal" = "PRT",
  "Puerto Rico" = "PRI",
  "Qatar" = "QAT",
  "Romania" = "ROU",
  "Russia" = "RUS",
  "Rwanda" = "RWA",
  "San Marino" = "SMR",
  "Saudi Arabia" = "SAU",
  "Senegal" = "SEN",
  "Serbia" = "SRB",
  "Seychelles" = "SYC",
  "Sierra Leone" = "SLE",
  "Singapore" = "SGP",
  "Slovakia" = "SVK",
  "Slovenia" = "SVN",
  "Solomon Islands" = "SLB",
  "Somalia" = "SOM",
  "South Africa" = "ZAF",
  "South Korea" = "KOR",
  "South Sudan" = "SSD",
  "Spain" = "ESP",
  "Sri Lanka" = "LKA",
  "Sudan" = "SDN",
  "Suriname" = "SUR",
  "Sweden" = "SWE",
  "Switzerland" = "CHE",
  "Syria" = "SYR",
  "Taiwan" = "TWN",
  "Tajikistan" = "TJK",
  "Tanzania" = "TZA",
  "Thailand" = "THA",
  "Timor-Leste" = "TLS",
  "Togo" = "TGO",
  "Tonga" = "TON",
  "Trinidad and Tobago" = "TTO",
  "Tunisia" = "TUN",
  "Turkey" = "TUR",
  "Turkmenistan" = "TKM",
  "Uganda" = "UGA",
  "Ukraine" = "UKR",
  "United Arab Emirates" = "ARE",
  "United Kingdom" = "GBR",
  "United States" = "USA",
  "United States Virgin Islands" = "VIR",
  "Uruguay" = "URY",
  "Uzbekistan" = "UZB",
  "Venezuela" = "VEN",
  "Vietnam" = "VNM",
  "Vanuatu" = "VUT",
  "Yemen" = "YEM",
  "Zambia" = "ZMB",
  "Zimbabwe" = "ZWE"
)

# ==============================================================================
# 2. R-RATE DATEN AUFBEREITEN
# ==============================================================================

# Annahme: deine R-Rate Daten heißen rrate_all
rrate_all <- rrate_all %>%
  mutate(
    Country = country_to_iso3[Country.Region],
    Date = as.Date(Date),
    R = R_di7  # Gleiche Variable wie im OECD-Sample (7-Tage Schätzung)
  ) %>%
  filter(!is.na(Country)) %>%  # Länder ohne Mapping entfernen
  select(Country, Date, R)

# Check
rrate_all %>%
  distinct(Country) %>%
  nrow()  # Sollte ~150+ Länder sein

# ==============================================================================
# 3. NACHBAR-R-RATE BERECHNEN
# ==============================================================================

calculate_neighbor_R_fast <- function(df_daily, rrate_all, neighbors_list) {
  
  rrate_all <- rrate_all %>%
    mutate(Date = as.Date(Date))
  
  df_daily <- df_daily %>%
    mutate(Date = as.Date(Date))
  
  neighbors_df <- tibble::enframe(neighbors_list, name = "Country", value = "neighbor") %>%
    tidyr::unnest_longer(neighbor)
  
  R_lookup <- rrate_all %>%
    select(Country, Date, R) %>%
    rename(
      neighbor = Country,
      neighbor_R = R
    )
  
  neighbor_R <- df_daily %>%
    distinct(Country, Date) %>%
    left_join(neighbors_df, by = "Country") %>%
    left_join(R_lookup, by = c("neighbor", "Date")) %>%
    group_by(Country, Date) %>%
    summarise(
      neighbor_R = mean(neighbor_R, na.rm = TRUE),
      n_neighbors_R = sum(!is.na(neighbor_R)),
      .groups = "drop"
    )
  
  df_daily %>%
    left_join(neighbor_R, by = c("Country", "Date"))
}

# Anwenden
df_daily <- calculate_neighbor_R_fast(df_daily, rrate_all, neighbors_list)

# Lags erstellen
df_daily <- df_daily %>%
  group_by(Country) %>%
  arrange(Country, Date) %>%
  mutate(
    lag7_neighbor_R = lag(neighbor_R, 7),
    lag14_neighbor_R = lag(neighbor_R, 14),
    delta_neighbor_R = neighbor_R - lag(neighbor_R, 7)
  ) %>%
  ungroup()

# Check
df_daily %>%
  filter(Country == "DEU") %>%
  select(Date, Country, lag7_R, lag7_neighbor_R, lag7_neighbor_stringency) %>%
  head(20)

# ==============================================================================
# 4. ERWEITERTE MODELLE
# ==============================================================================

# D7d + Nachbar-Stringency + Nachbar-R
prf_d7d_spatial_R <- feols(
  StringencyIndex_PopWeighted ~ 
    lag7_stringency +
    lag14_log_cases + lag7_log_deaths +
    lag7_R +
    lag7_neighbor_stringency +
    delta_neighbor_stringency +
    lag7_neighbor_R |
    country_id + week_id,
  data = df_daily,
  cluster = ~country_id
)

summary(prf_d7d_spatial_R)


# Vergleich
modelsummary(
  list(
    "D7d Baseline" = prf_d7d,
    "D7d + Nachbar-Str" = prf_d7d_spatial_full,
    "D7d + Nachbar-Str + R" = prf_d7d_spatial_R
  ),
  stars = TRUE,
  gof_map = c("nobs", "within.r.squared")
)

modelsummary(
  list(
    "D7d Baseline" = prf_d7d,
    "D7d + Nachbar-Str" = prf_d7d_spatial_full,
    "D7d + Nachbar-Str + R" = prf_d7d_spatial_R
  ),
  stars = TRUE,
  gof_map = c("nobs", "within.r.squared"),
  output = "markdown"  # Gibt Markdown-Tabelle aus
)

fitstat(prf_d7d, ~ r2 + wr2)
fitstat(prf_d7d_spatial_full, ~ r2 + wr2)
fitstat(prf_d7d_spatial_R, ~ r2 + wr2)

# ==============================================================================
# RESIDUEN AUS VERSCHIEDENEN SPEZIFIKATIONEN
# ==============================================================================


# Wahrscheinlich sind es keine numerischen Zeilenindizes
# Besser: Über augment oder manuell mit dem Modell-Sample

# ==============================================================================
# METHODE 1: Über das Modell-Sample
# ==============================================================================

# Die Modelle nutzen nur complete cases - wir müssen das Sample rekonstruieren

# Baseline: Welche Variablen?
# StringencyIndex_PopWeighted ~ lag7_stringency + lag14_log_cases + lag7_log_deaths + lag7_R

df_daily <- df_daily %>%
  mutate(
    # Sample-Indikator für Baseline
    in_sample_baseline = !is.na(StringencyIndex_PopWeighted) & 
      !is.na(lag7_stringency) & 
      !is.na(lag14_log_cases) & 
      !is.na(lag7_log_deaths) & 
      !is.na(lag7_R),
    
    # Sample-Indikator für Nachbar-Str
    in_sample_neighbor_str = in_sample_baseline & 
      !is.na(lag7_neighbor_stringency) & 
      !is.na(delta_neighbor_stringency),
    
    # Sample-Indikator für Nachbar-Full
    in_sample_neighbor_full = in_sample_neighbor_str & 
      !is.na(lag7_neighbor_R)
  )

# Check Sample-Größen
df_daily %>%
  summarise(
    n_baseline = sum(in_sample_baseline),
    n_neighbor_str = sum(in_sample_neighbor_str),
    n_neighbor_full = sum(in_sample_neighbor_full)
  )

# Sollte ungefähr matchen:
# Baseline: 24873, Neighbor_str: 24873, Neighbor_full: 24834

# ==============================================================================
# METHODE 2: Residuen direkt zuweisen
# ==============================================================================

# Residuen als Vektor
resid_baseline_vec <- residuals(prf_d7d)
resid_neighbor_str_vec <- residuals(prf_d7d_spatial_full)
resid_neighbor_full_vec <- residuals(prf_d7d_spatial_R)

# Längen checken
length(resid_baseline_vec)      # sollte 24873 sein
length(resid_neighbor_str_vec)  # sollte 24873 sein
length(resid_neighbor_full_vec) # sollte 24834 sein

# Zuweisen über Sample-Filter
df_daily$resid_baseline <- NA_real_
df_daily$resid_baseline[df_daily$in_sample_baseline] <- resid_baseline_vec

df_daily$resid_neighbor_str <- NA_real_
df_daily$resid_neighbor_str[df_daily$in_sample_neighbor_str] <- resid_neighbor_str_vec

df_daily$resid_neighbor_full <- NA_real_
df_daily$resid_neighbor_full[df_daily$in_sample_neighbor_full] <- resid_neighbor_full_vec

# ==============================================================================
# CHECK
# ==============================================================================

df_daily %>%
  summarise(
    n_resid_baseline = sum(!is.na(resid_baseline)),
    n_resid_neighbor_str = sum(!is.na(resid_neighbor_str)),
    n_resid_neighbor_full = sum(!is.na(resid_neighbor_full))
  )

# Korrelation
df_daily %>%
  filter(!is.na(resid_baseline) & !is.na(resid_neighbor_full)) %>%
  summarise(
    cor_base_str = cor(resid_baseline, resid_neighbor_str),
    cor_base_full = cor(resid_baseline, resid_neighbor_full),
    cor_str_full = cor(resid_neighbor_str, resid_neighbor_full)
  )


# ==============================================================================
# WELTKARTE: DURCHSCHNITTLICHE R-RATE PRO LAND
# ==============================================================================

# Pakete laden
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(viridis)

# ==============================================================================
# 1. DURCHSCHNITTLICHE R-RATE PRO LAND BERECHNEN
# ==============================================================================

# Aus deinem rrate_all Datensatz (alle 185 Länder)
avg_R_by_country <- rrate_all %>%
  filter(Date >= as.Date("2020-03-01") & Date <= as.Date("2021-12-31")) %>%
  group_by(Country) %>%
  summarise(
    avg_R = mean(R, na.rm = TRUE),
    median_R = median(R, na.rm = TRUE),
    sd_R = sd(R, na.rm = TRUE),
    n_days = sum(!is.na(R)),
    .groups = "drop"
  )

# Check
avg_R_by_country %>%
  arrange(desc(avg_R)) %>%
  head(20)

# ==============================================================================
# 2. WELTKARTE LADEN
# ==============================================================================

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(iso_a3, name, geometry) %>%
  rename(Country = iso_a3)

# ==============================================================================
# 3. DATEN MERGEN
# ==============================================================================
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(
    Country = case_when(
      name == "France" ~ "FRA",
      name == "Norway" ~ "NOR",
      name == "Kosovo" ~ "RKS",
      iso_a3 == "-99" ~ iso_a3_eh,
      TRUE ~ iso_a3
    )
  ) %>%
  select(Country, name, geometry)
world_R <- world %>%
  left_join(avg_R_by_country, by = "Country")

# ==============================================================================
# 4. KARTE: GESAMTER ZEITRAUM
# ==============================================================================

ggplot(world_R) +
  geom_sf(aes(fill = avg_R), color = "white", size = 0.1) +
  scale_fill_viridis(
    option = "inferno",
    name = "Durchschn. R",
    na.value = "grey80",
    limits = c(0.8, 1.5),
    oob = scales::squish
  ) +
  labs(
    title = "Durchschnittliche Reproduktionszahl R nach Land",
    subtitle = "März 2020 – Dezember 2021",
    caption = "Quelle: COVID-19 R-Rate Estimates"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

ggsave("R_rate_world_map.png", width = 12, height = 7, dpi = 300)

# ==============================================================================
# 5. KARTE NACH ZEITPERIODEN (Optional: Cluster über Zeit)
# ==============================================================================

# R-Rate nach Periode
avg_R_by_period <- rrate_all %>%
  mutate(
    Period = case_when(
      Date >= as.Date("2020-03-01") & Date <= as.Date("2020-06-30") ~ "2020-Q1/Q2 (Erste Welle)",
      Date >= as.Date("2020-10-01") & Date <= as.Date("2021-02-28") ~ "2020-Q4/2021-Q1 (Zweite Welle)",
      Date >= as.Date("2021-07-01") & Date <= as.Date("2021-12-31") ~ "2021-Q3/Q4 (Delta)",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Period)) %>%
  group_by(Country, Period) %>%
  summarise(avg_R = mean(R, na.rm = TRUE), .groups = "drop")

world_R_period <- world %>%
  left_join(avg_R_by_period, by = "Country")

ggplot(world_R_period) +
  geom_sf(aes(fill = avg_R), color = "white", size = 0.1) +
  scale_fill_viridis(
    option = "inferno",
    name = "Durchschn. R",
    na.value = "grey80",
    limits = c(0.7, 1.6),
    oob = scales::squish
  ) +
  facet_wrap(~Period, ncol = 1) +
  labs(
    title = "Regionale R-Rate Cluster nach COVID-19 Welle",
    caption = "Quelle: COVID-19 R-Rate Estimates"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

ggsave("R_rate_world_map_periods.png", width = 10, height = 12, dpi = 300)

# ==============================================================================
# 6. NUR EUROPA (für bessere Sichtbarkeit der OECD-Cluster)
# ==============================================================================

europe_bbox <- c(xmin = -25, xmax = 45, ymin = 35, ymax = 72)

ggplot(world_R) +
  geom_sf(aes(fill = avg_R), color = "white", size = 0.2) +
  scale_fill_viridis(
    option = "inferno",
    name = "Durchschn. R",
    na.value = "grey80",
    limits = c(0.9, 1.3),
    oob = scales::squish
  ) +
  coord_sf(xlim = c(europe_bbox["xmin"], europe_bbox["xmax"]),
           ylim = c(europe_bbox["ymin"], europe_bbox["ymax"])) +
  labs(
    title = "Durchschnittliche R-Rate in Europa",
    subtitle = "März 2020 – Dezember 2021"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid = element_blank()
  )

ggsave("R_rate_europe_map.png", width = 10, height = 8, dpi = 300)

# ==============================================================================
# 7. KORRELATION MIT NACHBARN (Cluster-Nachweis)
# ==============================================================================

# Zeige, dass Nachbar-R mit eigener R korreliert
neighbor_R_correlation <- df_daily %>%
  filter(!is.na(lag7_R) & !is.na(lag7_neighbor_R)) %>%
  group_by(Country) %>%
  summarise(
    cor_own_neighbor_R = cor(lag7_R, lag7_neighbor_R, use = "complete.obs"),
    .groups = "drop"
  ) %>%
  arrange(desc(cor_own_neighbor_R))

print(neighbor_R_correlation, n = 38)

# Durchschnittliche Korrelation
mean(neighbor_R_correlation$cor_own_neighbor_R, na.rm = TRUE)

##WELLEN SIND SICHTBAR

# Korrelation auf Europakarte
world_cor <- world %>%
  left_join(neighbor_R_correlation, by = "Country")

# Europa
ggplot(world_cor) +
  geom_sf(aes(fill = cor_own_neighbor_R), color = "white", size = 0.2) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0.5,
    name = "Korrelation\neigene R ~ Nachbar-R",
    na.value = "grey80",
    limits = c(-0.5, 1)
  ) +
  coord_sf(xlim = c(-25, 45), ylim = c(35, 72)) +
  labs(
    title = "Regionale Epidemie-Synchronisation in Europa",
    subtitle = "Korrelation zwischen nationaler und Nachbar-R-Rate"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid = element_blank()
  )

ggsave("R_correlation_europe.png", width = 10, height = 8, dpi = 300)


##FURTHER TESTS:

# Quadratischer Term für R
prf_nonlin <- feols(
  StringencyIndex_PopWeighted ~ 
    lag7_stringency +
    lag14_log_cases + lag7_log_deaths +
    lag7_R + I(lag7_R^2) |
    country_id + week_id,
  data = df_daily, cluster = ~country_id
)

prf_nonlin

#NICHT SIGIFIKANT

# Schwelleneffekt R > 1
df_daily <- df_daily %>%
  mutate(R_above_1 = as.numeric(lag7_R > 1))

prf_threshold <- feols(
  StringencyIndex_PopWeighted ~ 
    lag7_stringency +
    lag14_log_cases + lag7_log_deaths +
    lag7_R * R_above_1 |
    country_id + week_id,
  data = df_daily, cluster = ~country_id
)

prf_threshold

##NICHT SIGNIFIKANT

##INTERACTIONS MIT PERIODEN???
# Periode definieren
df_daily <- df_daily %>%
  mutate(
    period = case_when(
      Date < as.Date("2020-09-01") ~ "Wave1",
      Date < as.Date("2021-03-01") ~ "Wave2_PreVax",
      TRUE ~ "PostVax"
    )
  )

# Interaktion mit Periode
prf_structural <- feols(
  StringencyIndex_PopWeighted ~ 
    lag7_stringency +
    lag14_log_cases * period +
    lag7_log_deaths * period +
    lag7_R * period |
    country_id + week_id,
  data = df_daily, cluster = ~country_id
)

prf_structural

###OK


# R-Rate Unsicherheit als Gewicht
df_daily <- df_daily %>%
  mutate(R_precision = 1 / (ci_95_u - ci_95_l + 0.01))

prf_weighted <- feols(
  StringencyIndex_PopWeighted ~ 
    lag7_stringency + lag14_log_cases + lag7_log_deaths + lag7_R |
    country_id + week_id,
  data = df_daily, 
  cluster = ~country_id,
  weights = ~R_precision
)

prf_weighted


# Heterogenität nach Region
df_daily <- df_daily %>%
  mutate(
    region = case_when(
      Country %in% c("DEU","FRA","ITA","ESP","GBR","NLD","BEL","AUT","CHE") ~ "Western_Europe",
      Country %in% c("POL","CZE","HUN","SVK","SVN","EST","LVA","LTU") ~ "Eastern_Europe",
      Country %in% c("SWE","NOR","FIN","DNK","ISL") ~ "Scandinavia",
      Country %in% c("USA","CAN","MEX") ~ "North_America",
      Country %in% c("AUS","NZL","JPN","KOR") ~ "Asia_Pacific",
      TRUE ~ "Other"
    )
  )

prf_heterog <- feols(
  StringencyIndex_PopWeighted ~ 
    lag7_stringency +
    lag14_log_cases * region +
    lag7_R * region |
    country_id + week_id,
  data = df_daily, cluster = ~country_id
)

prf_heterog

##ACHTUNG: SIGNIFIKANT!!

##WIE FEST TREIBEN DIE AUSREISER DAS ERGEBNIS
# Residuen-Analyse
df_daily <- df_daily %>%
  mutate(
    resid_abs = abs(resid_baseline),
    outlier = resid_abs > 3 * sd(resid_abs, na.rm = TRUE)
  )

# Wie viele Ausreißer?
sum(df_daily$outlier, na.rm = TRUE)

# Modell ohne Ausreißer
prf_no_outliers <- feols(
  StringencyIndex_PopWeighted ~ 
    lag7_stringency + lag14_log_cases + lag7_log_deaths + lag7_R |
    country_id + week_id,
  data = df_daily %>% filter(!outlier),
  cluster = ~country_id
)

prf_no_outliers
##LOGISCHES ERGEBNISS DAS SO DAS WITHIN R BESSER WIRD

# Residuen-Autokorrelation pro Land
acf_by_country <- df_daily %>%
  filter(!is.na(resid_baseline)) %>%
  group_by(Country) %>%
  summarise(
    acf_lag1 = cor(resid_baseline, lag(resid_baseline, 1), use = "complete.obs"),
    acf_lag7 = cor(resid_baseline, lag(resid_baseline, 7), use = "complete.obs"),
    .groups = "drop"
  )

summary(acf_by_country$acf_lag1)
summary(acf_by_country$acf_lag7)

acf_long <- acf_by_country %>%
  pivot_longer(
    cols = c(acf_lag1, acf_lag7),
    names_to = "lag",
    values_to = "acf"
  )

ggplot(acf_long, aes(x = acf)) +
  geom_histogram(bins = 15) +
  facet_wrap(~ lag, scales = "free_x") +
  labs(
    title = "Verteilung der Residuen-Autokorrelation nach Lag",
    x = "ACF",
    y = "Anzahl Länder"
  )

##AUF DAY 1 STARK SERIAL CORRELATION-> BUT MUCH LESS WITH 7 DAYS!!
##ANNAHME AR(1)??


# Newey-West SE als Robustheit
prf_nw <- feols(
  StringencyIndex_PopWeighted ~ 
    lag7_stringency + lag14_log_cases + lag7_log_deaths + lag7_R |
    country_id + week_id,
  data     = df_daily,
  panel.id = ~ country_id + Date,   # oder panel.id = c("country_id", "Date")
  vcov     = "NW"                   # Newey-West
)


prf_nw

#Dass alles trotz Newey-West noch hochsignifikant ist, heisst: Die Zusammenhänge sind robust gegenüber serieller Korrelation der Fehler (die du ja vorher im ACF gesehen hast).


#Entscheid: 

#prf_d7d  
#prf_d7d_spatial_full

#VORSCHLAG:
prf_final <- feols(
  StringencyIndex_PopWeighted ~ 
    lag7_stringency +
    lag14_log_cases + lag7_log_deaths +
    lag7_R|
    country_id + week_id,
  data = df_daily,
  cluster = ~country_id
)


# D7d + Nachbar-Stringency + Nachbar-R
prf_final_spatial <- feols(
  StringencyIndex_PopWeighted ~ 
    lag7_stringency +
    lag14_log_cases + lag7_log_deaths +
    lag7_R +
    lag7_neighbor_stringency +
    delta_neighbor_stringency +
    lag7_neighbor_R |
    country_id + week_id,
  data = df_daily,
  cluster = ~country_id
)

summary(prf_d7d_spatial_R)



modelsummary(
  list("D7d Baseline" = prf_final),
  output = "markdown"
)


# Vergleich
modelsummary(
  list(
    "D7d Baseline" = prf_final,
    "D7d + Nachbar" = prf_final_spatial
  ),
  stars   = TRUE,
  gof_map = gof_map,
  output  = "markdown"
)


# alle wichtigen Fit-Stats inkl. Within-R²
fitstat(prf_final,        c("n", "wr2", "war2", "r2", "ar2", "rmse"))
fitstat(prf_final_spatial,c("n", "wr2", "war2", "r2", "ar2", "rmse"))

etable(
  prf_final, prf_final_spatial,
  se = "cluster",
  cluster = ~ country_id,
  fitstat = ~ n + wr2 + war2 + r2 + ar2 + rmse
)



# ==============================================================================
# 1. RESIDUEN EXTRAHIEREN
# ==============================================================================

df_daily <- df_daily %>%
  mutate(
    fitted_baseline = predict(prf_final, newdata = df_daily),
    resid_baseline = StringencyIndex_PopWeighted - fitted_baseline,
    
    fitted_neighbor = predict(prf_final_spatial, newdata = df_daily),
    resid_neighbor = StringencyIndex_PopWeighted - fitted_neighbor
  )

# Check
df_daily %>%
  summarise(
    n_resid_baseline = sum(!is.na(resid_baseline)),
    n_resid_neighbor = sum(!is.na(resid_neighbor))
  )

# ==============================================================================
# 2. AUF QUARTALSBASIS AGGREGIEREN
# ==============================================================================

df_quarterly <- df_daily %>%
  mutate(
    Year = year(Date),
    Quarter = quarter(Date),
    YQ = paste0(Year, "-Q", Quarter)
  ) %>%
  filter(Year %in% c(2020, 2021)) %>%
  group_by(Country, Year, Quarter, YQ) %>%
  summarise(
    # Residuen: Durchschnitt
    resid_baseline_mean = mean(resid_baseline, na.rm = TRUE),
    resid_neighbor_mean = mean(resid_neighbor, na.rm = TRUE),
    
    # Residuen: Summe (NEU)
    resid_baseline_sum = sum(resid_baseline, na.rm = TRUE),
    resid_neighbor_sum = sum(resid_neighbor, na.rm = TRUE),
    
    # Residuen: Standardabweichung (Volatilität)
    resid_baseline_sd = sd(resid_baseline, na.rm = TRUE),
    resid_neighbor_sd = sd(resid_neighbor, na.rm = TRUE),
    
    # Anzahl Beobachtungen
    n_days_baseline = sum(!is.na(resid_baseline)),
    n_days_neighbor = sum(!is.na(resid_neighbor)),
    
    # Stringency (für Kontext)
    stringency_mean = mean(StringencyIndex_PopWeighted, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  filter(n_days_baseline >= 45)

# Check: Verhältnis sum/mean ≈ n_days
df_quarterly %>%
  mutate(
    ratio_baseline = resid_baseline_sum / resid_baseline_mean,
    ratio_neighbor = resid_neighbor_sum / resid_neighbor_mean
  ) %>%
  select(Country, YQ, n_days_baseline, ratio_baseline, ratio_neighbor) %>%
  head(10)


# Gibt mir resid für baseline und spatial in den ausprägungen mean, sd and sum
# ==============================================================================
# 3. VISUALISIERUNG: RESIDUEN ÜBER ZEIT
# ==============================================================================

# 3a. Durchschnittliche Residuen pro Quartal (alle Länder)
avg_resid_by_quarter <- df_quarterly %>%
  group_by(YQ) %>%
  summarise(
    mean_baseline = mean(resid_baseline_mean, na.rm = TRUE),
    sd_baseline = sd(resid_baseline_mean, na.rm = TRUE),
    mean_neighbor = mean(resid_neighbor_mean, na.rm = TRUE),
    sd_neighbor = sd(resid_neighbor_mean, na.rm = TRUE),
    n_countries = n(),
    .groups = "drop"
  )

ggplot(avg_resid_by_quarter, aes(x = YQ)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(ymin = mean_baseline - sd_baseline, 
                    ymax = mean_baseline + sd_baseline),
                width = 0.2, color = "steelblue", alpha = 0.5) +
  geom_point(aes(y = mean_baseline), color = "steelblue", size = 3) +
  geom_line(aes(y = mean_baseline, group = 1), color = "steelblue") +
  labs(
    title = "Durchschnittliche Policy-Residuen über Zeit",
    subtitle = "Mittelwert ± 1 SD über alle 38 OECD-Länder",
    x = "Quartal",
    y = "Residuum (Stringency-Punkte)",
    caption = "Positive Werte = strenger als erwartet, Negative = lockerer als erwartet"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("resid_over_time.png", width = 10, height = 6, dpi = 300)

# ==============================================================================
# 3b. Residuen-Verteilung pro Quartal (Boxplots)
# ==============================================================================

ggplot(df_quarterly, aes(x = YQ, y = resid_baseline_mean)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_boxplot(fill = "steelblue", alpha = 0.6, outlier.alpha = 0.3) +
  labs(
    title = "Verteilung der Policy-Residuen nach Quartal",
    subtitle = "Variation der 'exogenen' Policy-Entscheidungen",
    x = "Quartal",
    y = "Durchschnittliches Residuum pro Land"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("resid_boxplot_quarter.png", width = 10, height = 6, dpi = 300)

# ==============================================================================
# 3c. Länder-Heatmap: Residuen über Zeit
# ==============================================================================

ggplot(df_quarterly, aes(x = YQ, y = reorder(Country, resid_baseline_mean), 
                         fill = resid_baseline_mean)) +
  geom_tile(color = "white", size = 0.1) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0,
    name = "Residuum",
    limits = c(-15, 15),
    oob = scales::squish
  ) +
  labs(
    title = "Policy-Residuen: Länder × Quartale",
    subtitle = "Rot = strenger als erwartet, Blau = lockerer als erwartet",
    x = "Quartal",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 7)
  )

ggsave("resid_heatmap.png", width = 10, height = 12, dpi = 300)

# ==============================================================================
# 3d. Vergleich Baseline vs. Nachbar-Residuen
# ==============================================================================

ggplot(df_quarterly, aes(x = resid_baseline_mean, y = resid_neighbor_mean)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_point(alpha = 0.5, color = "steelblue") +
  labs(
    title = "Vergleich der Residuen-Definitionen",
    subtitle = paste0("Korrelation: r = ", 
                      round(cor(df_quarterly$resid_baseline_mean, 
                                df_quarterly$resid_neighbor_mean, 
                                use = "complete.obs"), 3)),
    x = "Residuum (Baseline)",
    y = "Residuum (+ Nachbar)"
  ) +
  theme_minimal()

ggsave("resid_comparison.png", width = 8, height = 8, dpi = 300)

# ==============================================================================
# 3e. Länder-Profile: Wer war strenger/lockerer als erwartet?
# ==============================================================================

country_avg_resid <- df_quarterly %>%
  group_by(Country) %>%
  summarise(
    avg_resid = mean(resid_baseline_mean, na.rm = TRUE),
    sd_resid = sd(resid_baseline_mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_resid))

ggplot(country_avg_resid, aes(x = reorder(Country, avg_resid), y = avg_resid)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_col(aes(fill = avg_resid > 0), show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "coral", "FALSE" = "steelblue")) +
  coord_flip() +
  labs(
    title = "Durchschnittliche Policy-Residuen nach Land",
    subtitle = "Rot = durchschnittlich strenger, Blau = durchschnittlich lockerer als erwartet",
    x = "",
    y = "Durchschnittliches Residuum (2020-2021)"
  ) +
  theme_minimal()

ggsave("resid_by_country.png", width = 8, height = 10, dpi = 300)

# ==============================================================================
# 3f. Zeitreihen für ausgewählte Länder
# ==============================================================================

selected_countries <- c("DEU", "FRA", "GBR", "USA", "SWE", "ITA", "ESP", "NZL")

df_quarterly %>%
  filter(Country %in% selected_countries) %>%
  ggplot(aes(x = YQ, y = resid_baseline_mean, group = Country, color = Country)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~Country, ncol = 2, scales = "free_y") +
  labs(
    title = "Policy-Residuen: Ausgewählte Länder",
    subtitle = "Abweichung von erwarteter Stringency basierend auf epidemiologischer Lage",
    x = "Quartal",
    y = "Residuum"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    legend.position = "none"
  )

ggsave("resid_selected_countries.png", width = 10, height = 10, dpi = 300)

# ==============================================================================
# 4. DESKRIPTIVE STATISTIKEN
# ==============================================================================

# Zusammenfassung der Residuen
cat("\n=== RESIDUEN: DESKRIPTIVE STATISTIKEN ===\n\n")

cat("Tägliche Residuen:\n")
df_daily %>%
  filter(!is.na(resid_baseline)) %>%
  summarise(
    Mean = mean(resid_baseline),
    SD = sd(resid_baseline),
    Min = min(resid_baseline),
    Q25 = quantile(resid_baseline, 0.25),
    Median = median(resid_baseline),
    Q75 = quantile(resid_baseline, 0.75),
    Max = max(resid_baseline)
  ) %>%
  print()

cat("\nQuartals-Residuen:\n")
df_quarterly %>%
  filter(!is.na(resid_baseline_mean)) %>%
  summarise(
    Mean = mean(resid_baseline_mean),
    SD = sd(resid_baseline_mean),
    Min = min(resid_baseline_mean),
    Q25 = quantile(resid_baseline_mean, 0.25),
    Median = median(resid_baseline_mean),
    Q75 = quantile(resid_baseline_mean, 0.75),
    Max = max(resid_baseline_mean)
  ) %>%
  print()

cat("\nKorrelation Baseline vs. Nachbar (Quartal):\n")
cor(df_quarterly$resid_baseline_mean, df_quarterly$resid_neighbor_mean, 
    use = "complete.obs") %>%
  round(3) %>%
  print()

# Top 5 "strenger als erwartet"
cat("\nTop 5 strenger als erwartet (Land-Quartal):\n")
df_quarterly %>%
  arrange(desc(resid_baseline_mean)) %>%
  select(Country, YQ, resid_baseline_mean, stringency_mean) %>%
  head(5) %>%
  print()

# Top 5 "lockerer als erwartet"
cat("\nTop 5 lockerer als erwartet (Land-Quartal):\n")
df_quarterly %>%
  arrange(resid_baseline_mean) %>%
  select(Country, YQ, resid_baseline_mean, stringency_mean) %>%
  head(5) %>%
  print()


# ==============================================================================
# DESKRIPTIVE STATISTIKEN PRO LAND
# ==============================================================================

# Tägliche Residuen pro Land
resid_stats_daily <- df_daily %>%
  filter(!is.na(resid_baseline)) %>%
  group_by(Country) %>%
  summarise(
    N = n(),
    Mean = mean(resid_baseline),
    SD = sd(resid_baseline),
    Min = min(resid_baseline),
    Q25 = quantile(resid_baseline, 0.25),
    Median = median(resid_baseline),
    Q75 = quantile(resid_baseline, 0.75),
    Max = max(resid_baseline),
    .groups = "drop"
  ) %>%
  arrange(desc(Mean))

# Anzeigen
print(resid_stats_daily, n = 38)


# Quartals-Residuen pro Land
resid_stats_quarterly <- df_quarterly %>%
  filter(!is.na(resid_baseline_mean)) %>%
  group_by(Country) %>%
  summarise(
    N_Quarters = n(),
    Mean = mean(resid_baseline_mean),
    SD = sd(resid_baseline_mean),
    Min = min(resid_baseline_mean),
    Q25 = quantile(resid_baseline_mean, 0.25),
    Median = median(resid_baseline_mean),
    Q75 = quantile(resid_baseline_mean, 0.75),
    Max = max(resid_baseline_mean),
    .groups = "drop"
  ) %>%
  arrange(desc(Mean))

print(resid_stats_quarterly, n = 38)



# ==============================================================================
# SCATTERPLOT: MEAN vs. SD DER QUARTALS-RESIDUEN
# ==============================================================================

# ==============================================================================
# ALTERNATIVE: MIT QUADRANTEN
# ==============================================================================

ggplot(resid_stats_quarterly, aes(x = Mean, y = SD)) +
  # Quadranten-Hintergrund
  annotate("rect", xmin = 0, xmax = Inf, ymin = median(resid_stats_quarterly$SD), ymax = Inf,
           fill = "coral", alpha = 0.1) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = median(resid_stats_quarterly$SD), ymax = Inf,
           fill = "steelblue", alpha = 0.1) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = median(resid_stats_quarterly$SD),
           fill = "coral", alpha = 0.05) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = median(resid_stats_quarterly$SD),
           fill = "steelblue", alpha = 0.05) +
  # Referenzlinien
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  geom_hline(yintercept = median(resid_stats_quarterly$SD), linetype = "dashed", color = "grey30") +
  # Punkte und Labels
  geom_point(size = 3) +
  geom_text(aes(label = Country), hjust = -0.15, vjust = 0.5, size = 2.8) +
  # Quadranten-Labels
  annotate("text", x = max(resid_stats_quarterly$Mean) * 0.7, 
           y = max(resid_stats_quarterly$SD) * 0.95,
           label = "Strenger & Volatil", fontface = "italic", size = 3, color = "grey40") +
  annotate("text", x = min(resid_stats_quarterly$Mean) * 0.7, 
           y = max(resid_stats_quarterly$SD) * 0.95,
           label = "Lockerer & Volatil", fontface = "italic", size = 3, color = "grey40") +
  annotate("text", x = max(resid_stats_quarterly$Mean) * 0.7, 
           y = min(resid_stats_quarterly$SD) * 1.5,
           label = "Strenger & Konsistent", fontface = "italic", size = 3, color = "grey40") +
  annotate("text", x = min(resid_stats_quarterly$Mean) * 0.7, 
           y = min(resid_stats_quarterly$SD) * 1.5,
           label = "Lockerer & Konsistent", fontface = "italic", size = 3, color = "grey40") +
  labs(
    title = "Policy-Residuen: Mittelwert vs. Volatilität",
    subtitle = "Quartals-Residuen pro Land (2020-2021)",
    x = "Durchschnittliches Residuum\n(← lockerer | strenger →)",
    y = "Standardabweichung\n(Volatilität)"
  ) +
  theme_minimal() +
  xlim(min(resid_stats_quarterly$Mean) - 1.5, max(resid_stats_quarterly$Mean) + 3)

ggsave("resid_mean_vs_sd_quadrants.png", width = 10, height = 8, dpi = 300)

###zwei definitionen rsid_baseline_mean und resid_neighbor_mean
# ==============================================================================
# ZUSAMMENFASSUNGS-TABELLE FÜR PAPER
# ==============================================================================

fwrite(qdata, "lpdata.csv")
fwrite(df_quarterly, "lpdata2.csv")
fwrite(fm1_q, "lpdata3.csv")


################################################################################
###########################EMPIRICAL APPLICATION################################
################################################################################

#Möglichkeit um Datensätze neu zu laden
qdata <- read_csv("lpdata.csv")
df_quarterly <- read_csv("lpdata2.csv")
fm1_q2<- read_csv("lpdata3.csv")


fm1_q2 <- fm1_q2 %>%
  mutate(above_gdp = if_else(above == 1, broad_fiscal_gdp, NA_real_))

fm1_q2 <- fm1_q2 %>%
  mutate(below_gdp = if_else(above == 0, broad_fiscal_gdp, NA_real_))




clean_outcomes <- qdata %>%
  # Format: "Q" (Text) + %q (Quartal 1-4) + "." (Punkt) + %Y (Jahr 2015)
  mutate(Date = as.Date(as.yearqtr(Quarter, format = "Q%q.%Y"))) %>% 
  
  select(Country, Quarter, TimeIndex, qtime, qcovid, log_gdp, log_gdp_trend, gdp_trend, y_t, y_t_pct, 
         rGDP_per_capita, QReal.GDP.Growth_gr, Debt_an, Debt_ar, DebtN_combined, 
         d_t, d_t_pct, d_t_r, d_t_pct_r, log_debt, excess.deaths_a, p_avg_all_ages)


df_quarterly <- df_quarterly %>%
  mutate(
    # 1. Wir extrahieren die erste Zahl (1-4) aus dem "Müll"-String
    # (Egal wie viele Qs davor stehen, wir holen uns nur die Ziffer)
    clean_q_num = str_extract(Quarter, "[1-4]"),
    
    # 2. Wir bauen das Format sauber neu auf: "Q" + Zahl + "." + Jahr
    Quarter = paste0("Q", clean_q_num, ".", Year)
  ) %>%
  # Aufräumen der Hilfsspalte
  select(-clean_q_num)

# Check
head(df_quarterly$Quarter)
clean_s <- df_quarterly %>%
  select(Country, Quarter, resid_baseline_mean, resid_baseline_sum, resid_baseline_sd, resid_neighbor_mean, resid_neighbor_sum, resid_neighbor_sd, stringency_mean)


clean_s
# Beispiel: Policy Daten (S und F)
clean_f <- fm1_q2 %>%
  mutate(Date = as.Date(as.yearqtr(Quarter, format = "Q%q.%Y"))) %>% 
  select(Country, Quarter, broad_fiscal_gdp, expenditure, revenue, above, above_gdp, below_gdp)

clean_f <- clean_f %>%
  # 1. Gruppieren nach Land und Quartal
  group_by(Country, Quarter) %>%
  
  # 2. Werte aufsummieren
  summarise(
    broad_fiscal_gdp = sum(broad_fiscal_gdp, na.rm = TRUE),
    expenditure = sum(expenditure, na.rm=TRUE),
    below_gdp = sum(below_gdp, na.rm=TRUE),
    above_gdp = sum(above_gdp, na.rm=TRUE),
    .groups = "drop" # Hebt die Gruppierung danach wieder auf
  )



clean_outcomes
clean_s
clean_f

# ------------------------------------------------------------------------------
# SCHRITT 2: DAS MERGEN (ZUSAMMENFÜHREN)
# ------------------------------------------------------------------------------
# Wir nutzen 'reduce' aus purrr. Das ist viel sauberer als 
# viele verschachtelte left_join(left_join(left_join...))


# 1. FIX: Quarter überall in Text umwandeln (damit der Join klappt)
clean_outcomes <- clean_outcomes %>% mutate(Quarter = as.character(Quarter))
clean_s        <- clean_s        %>% mutate(Quarter = as.character(Quarter))
clean_f        <- clean_f        %>% mutate(Quarter = as.character(Quarter))

# 2. Liste erstellen
list_of_dfs <- list(clean_outcomes, clean_s, clean_f)

# 3. Mergen und Fertigstellen
lpdata <- list_of_dfs %>%
  # Zusammenfügen (jetzt klappt es, da alle Quarter Text sind)
  reduce(full_join, by = c("Country", "Quarter")) %>%
  
  # Datum erstellen (damit filter() und Plots funktionieren)
  mutate(Date = as.Date(as.yearqtr(Quarter, format = "Q%q.%Y"))) %>%
  
  # Sortieren und Filtern
  arrange(Country, Date) %>%
  filter(year(Date) >= 2015) %>%
  
  # Spalten ordnen
  select(Country, Quarter, everything())

# Ergebnis prüfen
head(lpdata)

lpdata

print(head(lpdata), width=Inf)
##Data geladen

# -----------------------------------------------------------------------------
# Local Projection nach Jordà 2025
# -----------------------------------------------------------------------------
library(lpirfs)
# ------------------------------------------------------------------------------
# 2. DATEN VORBEREITUNG
# ------------------------------------------------------------------------------
# Annahme: Dein Dataframe heisst 'lpdata'
# Variablen: country, quarter (Date oder ID), rGDP_pc, debt, excess_deaths, S, F
lpdata[is.na(lpdata)] <- 0


##Resid Neighbor als Kontrolle, resid_sd variation/unsicherheit testen (sehr shacky, weglassen)

lpdata <- lpdata %>%
  rename(
    S_t = resid_baseline_mean,
    F_t = broad_fiscal_gdp ##Unterschiedliche Treatment definierbar
  )



##Creating log
lpdata2 <- lpdata %>%
  mutate(log_rGDP = log(rGDP_per_capita*100),
         log_Debt = log(Debt_ar))



# Transformiere alle Outcomes zu Wachstumsraten/Prozentänderungen
lpdata2 <- lpdata%>%
  group_by(Country) %>%
  mutate(
    # GDP: bereits Wachstumsrate ✓
    gdp_growth = (rGDP_per_capita - lag(rGDP_per_capita)) / lag(rGDP_per_capita) * 100,
    
    # Debt: Wachstumsrate berechnen
    debt_growth = (Debt_an - lag(Debt_an)) / lag(Debt_an) * 100,
    
    # Deaths: bereits in % ✓
    excess_mortality = p_avg_all_ages,
  ) %>%
  ungroup()



##Creating Cummulativ F_t Variable
lpdata2 <- lpdata2 %>%
  group_by(Country) %>%  # Anpassen an deinen Ländervariablen-Namen
  arrange(Country, Quarter) %>%  # Anpassen an deinen Zeitvariablen-Namen
  mutate(F_t_cumul = cumsum(F_t)) %>%
  ungroup()

##F auf gleiche Skala bringen
lpdata2$F_t<-lpdata2$F_t*100
lpdata2$F_t_cumul<-lpdata2$F_t_cumul*100
lpdata2$above_gdp<-lpdata2$above_gdp*100
lpdata2$below_gdp<-lpdata2$below_gdp*100


lpdata2 <- lpdata2 %>%
  select(Country, qtime, everything())


##Main Schätzung
results_lp<-lp_lin_panel(
  # Datenstruktur
  data_set = lpdata3,  ##achtung hier 3 da quartal raus
  data_sample = seq(-3,12),
  
  # Endogene Variable
  endog_data = "S_t",
  cumul_mult = TRUE,  # Kumulativer Effekt: y(t+h) - y(t-1)
  
  # Shock-Variable
  shock = "stringency_mean",
  diff_shock = FALSE,  # KORRIGIERT: F_t ist bereits Flow
  
  # Kein IV nötig, da F_t bereits exogenisiert
  iv_reg = FALSE,
  instrum = NULL,
  
  # Panel-Spezifikation
  panel_model = "within",
  panel_effect = "twoways",  # Country + Time FE
  
  # Robuste Standardfehler: Driscoll-Kraay
  robust_cov = "vcovSCC",
  robust_method = NULL,
  robust_type = NULL,  # Nicht relevant für vcovSCC
  robust_cluster = NULL,
  robust_maxlag = 4,  # Etwa T^(1/4) ≈ 40^0.25 ≈ 2.5, aufgerundet, robustness 2 und 6 verwenden
  
  # Kein GMM
  use_gmm = FALSE,
  gmm_model = "onestep",
  gmm_effect = "twoways",
  gmm_transformation = "d",
  
  # Kontrollvariablen
  c_exog_data = NULL,
  l_exog_data = NULL,
  lags_exog_data = Inf,  # KORRIGIERT: NaN statt Inf
  c_fd_exog_data = NULL,
  l_fd_exog_data = NULL,  ##controlls angeben
  lags_fd_exog_data = Inf,  # 2 Lags sollten reichen bei Quartalsdaten
  
  # Inferenz
  confint = 1.96,  # 95% Konfidenzintervall
  hor = 8  # KORRIGIERT: 8 Quartale (2 Jahre)
)


# Zeige die Ergebnisse für ein paar Horizonte
results_lp$reg_summaries[[1]]  # h=0
results_lp$reg_summaries[[4]]  # h=3 (Peak)
results_lp$reg_summaries[[8]]  # h=7

# Was enthält das Objekt?
names(results_lp)
# IRF-Plot anzeigen
plot(results_lp)

# Diagnostiken anzeigen
summary(results_lp)

# Punkt-Schätzer extrahieren
results_lp$irf_panel_mean

# Volle Regression für Horizont h=4 anschauen
summary(results_lp$reg_outputs[[7]])  # Index 5 = Horizont 4 (startet bei 0)

##For adding controlls, remove Quarter-> then chang datasource in the modell to "lpdata3"
##Quarter lsöchen sonst gibt es einen Konflikt
lpdata3 <- lpdata2 %>%
  select(-Quarter)

#quadratischer term als test
lpdata3$above_gdp_sq <- lpdata3$above_gdp^2

# 1) CHECK-> define optimal starting model-> based on my notes and the theory, notiere alle anforderungen und tradeoffs-> was erhalte ich?
# 1.5) correct S-> do I have to include lags again?
# 2) Get information out, do robustness, check nonlinear and co
# 3) what to say? what to do with the results or how to interpret?
# 4) excess mortaility on weekly base
## ONLY DO THE NEXT STEP IF YOU ARE FINISHED WITH THE FIRST ONE


#Robustheitschecks für deine LP-Schätzung**

#1. Alternative Standardfehler**
#`robust_maxlag = 2` (weniger konservativ)
#`robust_maxlag = 6` (konservativer)
#`robust_cov = "vcovHC"` statt Driscoll-Kraay

#2. Sample-Variationen**
#Ohne einzelne Länder (Leave-one-out)
#Nur EU-Länder vs. alle OECD
#Sample ab Q1.2020 statt Q1.2019

#3. Spezifikations-Variationen**
#-`panel_effect = "individual"` statt "twoways"
#- Mit/ohne S_t-Kontrolle
#- Alternative Lag-Längen für Kontrollen (1, 3, 4 statt 2)

#4. Variablen-Definitionen**
# Controlls und lags einführen (outcomes usw)
#- Nominal statt real (Debt_an statt Debt_ar)
#- Log-Levels statt Gaps
#- Alternative PRF-Residuen (falls vorhanden)

#5. Placebo-Tests**
#- Fake-Treatment 2018: F_t auf 2018 shiften → sollte Null sein
#- Pre-Trend-Test: Effekt von F_t auf Outcomes vor COVID

#6. Nicht-Linearität** ✓ (bereits gemacht)
#- Quadratischer Term → nicht signifikant

#7. Heterogenitäts-Analyse**
#- High vs. Low Debt-Länder (vor COVID)
#- Fiscal Space Interaktion
#- Eurozone vs. Nicht-Eurozone

#8. Bootstrap


## TEST if F is determined

prf_d7dF <-feols(
  broad_fiscal_gdp_daily ~ 
    lag7_stringency +
    lag14_log_cases + lag7_log_deaths +
    lag7_R |
    country_id + week_id,
  data = df_daily,
  cluster = ~country_id
)

summary(prf_d7dF)

# 1. Auf Tagesebene aggregieren (Summe aller Maßnahmen pro Land pro Tag)
fm1_daily <- fm1 %>%
  group_by(Country, date) %>%
  summarise(
    broad_fiscal_gdp_daily = sum(broad_fiscal_gdp, na.rm = TRUE),
    above_daily = sum(above * broad_fiscal_gdp, na.rm = TRUE),  # Gewichtet mit Größe
    n_measures = n(),
    .groups = "drop"
  )

# 2. Mit df_daily mergen
df_daily <- df_daily %>%
  left_join(
    fm1_daily,
    by = c("Country" = "Country", "Date" = "date")  # Prüfe ob Date oder date
  ) %>%
  # NAs auf 0 setzen (Tage ohne Maßnahmen)
  mutate(
    broad_fiscal_gdp_daily = replace_na(broad_fiscal_gdp_daily, 0),
    above_daily = replace_na(above_daily, 0),
    n_measures = replace_na(n_measures, 0)
  )



# 1. Fiskalische Daten auf Quartal aggregieren
fm1_quarterly <- fm1 %>%
  mutate(
    Year = as.numeric(as.character(Year)),
    Quarter = as.numeric(as.character(Quarter))
  ) %>%
  group_by(Country, Year, Quarter) %>%
  summarise(
    F_t = sum(broad_fiscal_gdp, na.rm = TRUE),
    above_gdp = sum(above * broad_fiscal_gdp, na.rm = TRUE),
    below_gdp = sum(loans_and_guar * broad_fiscal_gdp, na.rm = TRUE),
    n_measures = n(),
    .groups = "drop"
  ) %>%
  # Quarter-Format anpassen an qdata
  mutate(
    Quarter = paste0("Q", Quarter, ".", Year)
  )

# 2. Mit qdata mergen (nur noch Country und Quarter nötig)
prf_data <- qdata %>%
  left_join(fm1_quarterly, by = c("Country", "Quarter")) %>%
  mutate(
    F_t = replace_na(F_t, 0),
    above_gdp = replace_na(above_gdp, 0),
    below_gdp = replace_na(below_gdp, 0)
  ) %>%
  group_by(Country) %>%
  arrange(Country, Quarter) %>%
  mutate(
    lag_y_t = lag(y_t),
    lag_gdp_growth = lag(rGDP_per_capita),
    lag_S_t = lag(StringencyIndex_PopWeighted)
  ) %>%
  ungroup()

# Check ob Merge funktioniert hat
prf_data %>%
  filter(F_t > 0) %>%
  select(Country, Quarter, F_t, above_gdp) %>%
  head(10)
# 3. PRF schätzen
library(fixest)

prf_F_q1 <- feols(
  F_t ~ lag_y_t | Country + Quarter,
  data = prf_data,
  cluster = ~ Country
)

prf_F_q2 <- feols(
  F_t ~ lag_y_t + lag_S_t | Country + Quarter,
  data = prf_data,
  cluster = ~ Country
)

prf_F_q3 <- feols(
  F_t ~ lag_gdp_growth + lag_S_t | Country + Quarter,
  data = prf_data,
  cluster = ~ Country
)

# 4. Vergleichen
modelsummary(
  list(
    "Output Gap" = prf_F_q1,
    "+ Stringency" = prf_F_q2,
    "GDP Growth + S" = prf_F_q3
  ),
  stars = TRUE,
  gof_map = c("nobs", "within.r.squared"),
  output = "markdown"
)



## TEST

# -----------------------------------------------------------------------------
# DiD Continous nach Callaway 2025
# -----------------------------------------------------------------------------

# =============================================================================
# CONTDID ANALYSE: COVID-19 Policy Response Effects
# =============================================================================
# Forschungsfrage: Hatten Länder mit strengeren Maßnahmen (höhere PRF-Residuen)
#                  mehr Wirtschaftseinbruch, Verschuldung aber weniger Tote?
#
# WICHTIGE METHODISCHE EINSCHRÄNKUNG:
# Alle Länder wurden gleichzeitig in Q1.2020 treated. Es gibt keine 
# "never-treated" oder "not-yet-treated" Kontrollgruppe. Die Identifikation
# basiert auf dem Vergleich von Ländern mit UNTERSCHIEDLICHER DOSIS.
# 
# Gemäß Callaway, Goodman-Bacon & Sant'Anna (2024), Remark 3.1:
# Ohne unbehandelte Einheiten identifiziert man: ATT(d|d) - ATT(d_L|d_L)
# Dies erfordert STRONG PARALLEL TRENDS für eine kausale Interpretation.
# =============================================================================


# -------------------------------------------------------------------------------
# LP DiD
# ------------------------------------------------------------------------------

library(fixest)

## easy 2x2 setting


set.seed(757)

I <- 50; T <- 50; N <- I * T
data.frame(id = rep(1:I, each = T),
           t = rep(1:T, I),
           e = rnorm(N, 0, 4)) -> df
unit_FE <- rnorm(I, 0, 4)
time_FE <- rnorm(T, 0, 4)
df$treated <- ifelse(df$id < 21, 1, 0)
df$time_til <- ifelse(df$treated == 1, df$t - 30, -1)
df$treat_status <- ifelse(df$treated == 1 & df$t == 30, 1, 0)
df$y <- df$e + unit_FE[df$id] + time_FE[df$t] + ifelse(df$id < 21 & df$t > 30, df$t - 30, 0)
aggregate(df$y, list(df$t, df$treated), mean) -> tmp
par(mar = c(4.1, 4.1, .1, 4.1))
plot(tmp$Group.1, tmp$x, col = tmp$Group.2 + 1, pch = 19,
     xlab = "Time", ylab = "Outcome")
legend("topleft", legend = c("Treated", "Control"),
       bty = "n", pch = 19, col = c("tomato", "black"))

##lpdid

lpdid(df, window = c(-20, 20), y = "y",
      unit_index = "id", time_index = "t",
      treat_status = "treat_status") -> reg


lpdata_balanced$treat<-ifelse(lpdata_balanced$qtime==0,1,0)


test<-lpdid(lpdata_balanced, window = c(-10, 10), y = "log_gdp",
            unit_index = "id", time_index = "TimeIndex",
            treat_status = "treat") -> reg


feols<-feols(log_debt ~ i(qtime, G, -1) | id + TimeIndex, data = lpdata_balanced) -> twfe

summary(feols)

par(mar = c(4.1, 4.1, .1, 4.1))
plot_lpdid(reg, col = "tomato", cex = 1.3)
iplot(twfe, add = TRUE)
legend("topleft", legend = c("TWFE", "LP-DiD"),
       col = c("black", "tomato"), pch = c(20, 19), bty = "n")

#staggered exogenous treatment

df <- genr_sim_data(exogenous_timing = TRUE, 789)

twfe1 <- feols(y ~ i(rel_time, ever_treat, -1) | i + t, data = df)

lpdid1 <- lpdid(df, window = c(-5, 10), y = "y",
                unit_index = "i", time_index = "t",
                treat_status = "treat_status")

par(mar = c(4.1, 4.1, .1, 4.1))
plot_lpdid(lpdid1, col = "tomato", x.shift = 0.1)
iplot(twfe1, add = TRUE, x.shift = -0.1)
points(aggregate(df$beta[df$ever_treat == 1],
                 list(df$rel_time[df$ever_treat == 1]), mean))
legend("topleft", c("True Beta", "TWFE", "LP-DiD"), bty = "n",
       pch = c(1, 19, 19), col = c("black", "black", "tomato"))

##staggered endogenous treatment

df <- genr_sim_data(exogenous_timing = FALSE, 789)
# The returned "df" is a pdata.frame, so lag() works in a panel setting.
df$lag_y <- lag(df$y, 1)
twfe0 <- feols(y ~ i(rel_time, ever_treat, -1) | i + t, data = df)
twfe1 <- feols(y ~ lag_y + i(rel_time, ever_treat, -1) | i + t, data = df)
lpdid0 <- lpdid(df, window = c(-5, 10), y = "y", outcome_lags = 0,
                unit_index = "i", time_index = "t", treat_status = "treat_status")
lpdid1 <- lpdid(df, window = c(-5, 10), y = "y", outcome_lags = 1,
                unit_index = "i", time_index = "t", treat_status = "treat_status")
par(mar = c(4.1, 4.1, .1, 4.1))
plot_lpdid(lpdid0, col = "tomato", x.shift = 0.1)
plot_lpdid(lpdid1, col = "dodgerblue", x.shift = 0.1, add = TRUE)
iplot(twfe0, add = TRUE, x.shift = -0.1)
iplot(twfe1, add = TRUE, x.shift = -0.1, col = "gray")
points(aggregate(df$beta[df$ever_treat == 1], list(df$rel_time[df$ever_treat == 1]), mean))
legend("topleft", bty = "n",
       c("True Beta", "TWFE", "TWFE + Y_lag", "LP-DiD", "LP-DiD + Y_lag"),
       pch = c(1, 19, 19, 19, 19),
       col = c("black", "black", "gray", "tomato", "dodgerblue"))



##when treatment is unexpected negative, you can reweight it and get a ATT rather than a VWATT
##do I need that?

# --- CONTDID SCHÄTZUNG ----------------------------------------------------
# ACHTUNG: Da alle Länder gleichzeitig treated werden, nutzt contdid
# Länder mit NIEDRIGER Dosis als implizite Vergleichsgruppe.
# Die Interpretation erfordert Strong Parallel Trends!

# =============================================================================
# VOLLSTÄNDIGER CODE: Datensatz für contdid erstellen
# =============================================================================
summary(lpdata_balanced$resid_baseline_mean)
# SCHRITT 0: SCHWELLENWERT DEFINIEREN (hier anpassen!)
# -----------------------------------------------------------------------------
CUTOFF <- 0.0428 # Alternativen: 0.02 (Median), 0.21 (Mean), -1.14 (Q1)

# =============================================================================
# SCHRITT 1: Dosis pro Land berechnen
# =============================================================================
# =============================================================================
# VIER DOSIS-VARIANTEN BERECHNEN (alle zeitinvariant pro Land)
# =============================================================================

dose_per_country <- lpdata %>%
  filter(!is.na(resid_baseline_mean)) %>%
  group_by(Country) %>%
  summarise(
    # 1. D_avg: Mean über gesamte COVID-Periode (Q1.2020 - Q4.2021)
    D_avg = mean(resid_baseline_mean, na.rm = TRUE),
    
    # 2. D_first: Mean nur vor Impfung (Q1.2020 - Q2.2021 = TimeIndex 21-26)
    D_first = mean(resid_baseline_mean[TimeIndex >= 21 & TimeIndex <= 26], na.rm = TRUE),
    
    .groups = "drop"
  )

# 3. D_pos: Linear in positiven Bereich verschieben (Minimum wird 0)
min_D_avg <- min(dose_per_country$D_avg, na.rm = TRUE)
dose_per_country <- dose_per_country %>%
  mutate(
    D_pos = D_avg - min_D_avg  # Verschiebe so dass Minimum = 0
  )

# 4. D_zer: Negative Werte auf 0 setzen
dose_per_country <- dose_per_country %>%
  mutate(
    D_zer = ifelse(D_avg < 0, 0, D_avg)
  )

# =============================================================================
# AUSGABE UND VERIFIZIERUNG
# =============================================================================

cat("=== Dosis pro Land (4 Varianten) ===\n\n")
print(dose_per_country, n = Inf)

cat("\n=== Zusammenfassung der Varianten ===\n")
cat("\nD_avg (Mean gesamte Periode):\n")
print(summary(dose_per_country$D_avg))

cat("\nD_first (Mean vor Impfung Q1.2020-Q2.2021):\n")
print(summary(dose_per_country$D_first))

cat("\nD_pos (linear verschoben, min = 0):\n")
print(summary(dose_per_country$D_pos))
cat("Verschiebung um:", round(abs(min_D_avg), 3), "\n")

cat("\nD_zer (negative auf 0 gesetzt):\n")
print(summary(dose_per_country$D_zer))
cat("Anzahl Länder mit D_zer = 0:", sum(dose_per_country$D_zer == 0), "\n")

# =============================================================================
# ZUM HAUPTDATENSATZ HINZUFÜGEN
# =============================================================================

lpdata_contdid <- lpdata %>%
  left_join(
    dose_per_country %>% select(Country, D_avg, D_first, D_pos, D_zer),
    by = "Country"
  )

# Verifizierung: Zeitinvarianz prüfen
cat("\n=== Zeitinvarianz-Check ===\n")
invariance_check <- lpdata_contdid %>%
  group_by(Country) %>%
  summarise(
    D_avg_unique = n_distinct(D_avg),
    D_first_unique = n_distinct(D_first),
    D_pos_unique = n_distinct(D_pos),
    D_zer_unique = n_distinct(D_zer),
    .groups = "drop"
  )

if(all(invariance_check$D_avg_unique == 1) & 
   all(invariance_check$D_first_unique == 1) &
   all(invariance_check$D_pos_unique == 1) &
   all(invariance_check$D_zer_unique == 1)) {
  cat("Alle D-Varianten sind zeitinvariant pro Land ✓\n")
} else {
  cat("WARNUNG: Nicht alle Varianten sind zeitinvariant!\n")
}

# =============================================================================
# SCHRITT 2: Summary Statistic ansehen (Entscheidungshilfe)
# =============================================================================
cat("=== D_avg ===\n"); summary(dose_per_country$D_avg)
cat("\n=== D_first ===\n"); summary(dose_per_country$D_first)
cat("\n=== D_pos ===\n"); summary(dose_per_country$D_pos)
cat("\n=== D_zer ===\n"); summary(dose_per_country$D_zer)

# =============================================================================
# SCHRITT 3: Cutoff wählen & Gruppen zuordnen
# =============================================================================
# =============================================================================
# FLEXIBLE KONFIGURATION
# =============================================================================

# 1. Wähle D-Definition: "D_avg", "D_first", "D_pos", "D_zer"
D_VAR <- "D_first"

# 2. Wähle Cutoff (Wert oder Quantil)
MY_CUTOFF <- 0.2922  # Oder: quantile(dose_per_country[[D_VAR]], 0.25, na.rm = TRUE)

cat("D-Variable:", D_VAR, "\nCutoff:", MY_CUTOFF, "\n")

# =============================================================================
# DATENSATZ ERSTELLEN
# =============================================================================

lpdata_balanced <- lpdata %>%
  left_join(dose_per_country, by = "Country") %>%
  filter(!is.na(.data[[D_VAR]])) %>%
  group_by(Country) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  mutate(
    time_period = TimeIndex,
    D_selected = .data[[D_VAR]],
    G = ifelse(D_selected <= MY_CUTOFF, 0, 1),
    D = ifelse(D_selected <= MY_CUTOFF, 0, D_selected)
  )

# Verifizierung
cat("\nKontrolle (G=0):", n_distinct(lpdata_balanced$id[lpdata_balanced$G == 0]), "Länder")
cat("\nTreated (G=21):", n_distinct(lpdata_balanced$id[lpdata_balanced$G == 21]), "Länder\n")
# =============================================================================
# SCHRITT 4: Finaler Check
# =============================================================================

cat("\n=== Ergebnis ===\n")
cat("Anzahl Kontroll-Länder (G=0):", n_distinct(lpdata_balanced$id[lpdata_balanced$G == 0]), "\n")
cat("Anzahl Treated-Länder (G=21):", n_distinct(lpdata_balanced$id[lpdata_balanced$G == 21]), "\n")



##Korrekturen un berechnungen von Variablen

# Korrektur der Group-Variable G
lpdata_balanced <- lpdata_balanced %>% # oder lpdata_fixed vom vorherigen Schritt
  mutate(
    # Logik: Wenn die Dosis (D) 0 ist, dann setze G auf 0 (Kontrollgruppe).
    # Andernfalls behalte den Wert von G (hier 21).
    G = ifelse(D == 0, 0, G)
  )


lpdata_balanced <- lpdata_balanced %>%
  mutate(
    # Erstellt den natürlichen Logarithmus (ln)
    log_gdp = log(rGDP_per_capita)
  )
lpdata_balanced <- lpdata_balanced %>%
  mutate(
    # Erstellt den natürlichen Logarithmus (ln)
    log_debt = log(Debt_an)
  )
lpdata_balanced <- lpdata_balanced %>%
  mutate(
    # Erstellt den natürlichen Logarithmus (ln)
    log_deaths = log(excess.deaths_a)
  )


lpdata_balanced$p_avg_all_ages <- jitter(lpdata_balanced$p_avg_all_ages, amount = 0.00001)

##alle drei outcomes als wachstumsraten berechnen

# Transformiere alle Outcomes zu Wachstumsraten/Prozentänderungen
lpdata_balanced <- lpdata_balanced %>%
  group_by(id) %>%
  mutate(
    # GDP: bereits Wachstumsrate ✓
    gdp_growth = (rGDP_per_capita - lag(rGDP_per_capita)) / lag(rGDP_per_capita) * 100,
    
    # Debt: Wachstumsrate berechnen
    debt_growth = (Debt_an - lag(Debt_an)) / lag(Debt_an) * 100,
    
    # Deaths: bereits in % ✓
    excess_mortality = p_avg_all_ages,
  ) %>%
  ungroup()


lpdata_balanced$debt_growth[is.na(lpdata_balanced$debt_growth)] <- 0
lpdata_balanced$gdp_growth[is.na(lpdata_balanced$gdp_growth)] <- 0
lpdata_balanced$excess_mortality[is.na(lpdata_balanced$excess_mortality)] <- 0



################################################################################
##contdid package examples

# Simulate data
set.seed(1234)
df <- simulate_contdid_data(
  n = 5000,
  num_time_periods = 4,
  num_groups = 4,
  dose_linear_effect = 0,
  dose_quadratic_effect = 0
)


cd_res <- cont_did(
  yname = "Y",
  tname = "time_period",
  idname = "id",
  dname = "D",
  data = df,
  gname = "G",
  target_parameter = "slope",
  aggregation = "dose",
  treatment_type = "continuous",
  control_group = "notyettreated",
  biters = 100,
  cband = TRUE,
  num_knots = 1,
  degree = 3,
)

summary(cd_res)
ggcont_did(cd_res, type = "att")



##myexample ATT and ACRT
cd_res2 <- cont_did(
  yname = "gdp_growth",
  tname = "time_period",
  idname = "id",
  dname = "D_selected",
  data = lpdata_balanced,
  gname = "G",
  target_parameter = "slope",
  aggregation = "dose",
  treatment_type = "continuous",
  control_group = "notyettreated",
  biters = 100,
  cband = TRUE,
  num_knots = 1,
  degree = 3,
)

summary(cd_res2)
ggcont_did(cd_res, type = "att")


##eventstudy callaway
cd_res_es_level <- cont_did(
  yname = "Y",
  tname = "time_period",
  idname = "id",
  dname = "D",
  data = df,
  gname = "G",
  target_parameter = "level",
  aggregation = "eventstudy",
  treatment_type = "continuous",
  control_group = "nevertreated",
  biters = 100,
  cband = TRUE,
  num_knots = 1,
  degree = 3,
)

summary(cd_res_es_level)
ggcont_did(cd_res_es_level)


cd_res_es_level2 <- cont_did(
  yname = "log_gdp",
  tname = "time_period",
  idname = "id",
  dname = "D_pos",
  data = lpdata_balanced,
  gname = "G",
  target_parameter = "level",
  aggregation = "eventstudy",
  treatment_type = "continuous",
  control_group = "notyettreated",
  biters = 100,
  cband = TRUE,
  num_knots = 1,
  degree = 3,
)

summary(cd_res_es_level2)
ggcont_did(cd_res_es_level2)



##Still the same error there are countries with G=24 but D=0
##FIX it
##cut all before and after out-> Sample from 2018-2023

# =============================================================================
# G BASIEREND AUF ERSTEM POSITIVEM QUARTAL SETZEN
# =============================================================================
# =============================================================================
# G BASIEREND AUF ERSTEM POSITIVEM QUARTAL SETZEN
# =============================================================================

library(devtools)
library(lpdid)
library(fixest)

# =============================================================================
# G BASIEREND AUF ERSTEM POSITIVEM QUARTAL SETZEN
# =============================================================================
# =============================================================================
# FLEXIBLE KONFIGURATION
# =============================================================================

# HIER SCHWELLENWERT WÄHLEN:
CUTOFF <- 2  # Alternativen: 0.5, quantile(dose_per_country$D_avg, 0.25), etc.

cat("Gewählter Cutoff:", CUTOFF, "\n")

# =============================================================================
# G BASIEREND AUF ERSTEM QUARTAL ÜBER SCHWELLENWERT
# =============================================================================
# =============================================================================
# FLEXIBLE KONFIGURATION
# =============================================================================

CUTOFF <- 0

# =============================================================================
# G BASIEREND AUF ERSTEM QUARTAL ÜBER SCHWELLENWERT
# =============================================================================

# Schritt 1: Finde erstes Quartal mit resid_baseline_mean > CUTOFF
first_above_cutoff <- lpdata_balanced %>%
  filter(resid_baseline_mean > CUTOFF, TimeIndex >= 21 & TimeIndex <= 24) %>%
  group_by(Country) %>%
  summarise(G_timing = min(TimeIndex), .groups = "drop")

cat("=== G_timing Verteilung ===\n")
print(table(first_above_cutoff$G_timing))

# Schritt 2: Zuordnung basierend auf G_timing (NICHT D_selected!)
lpdata_balanced <- lpdata_balanced %>%
  select(-any_of("G_timing")) %>%
  left_join(first_above_cutoff, by = "Country") %>%
  mutate(
    # Treated = Land hat mindestens ein Quartal > CUTOFF
    G = ifelse(is.na(G_timing), 0, G_timing),
    D = ifelse(G == 0, 0, D_selected)
  ) %>%
  select(-G_timing)

# Verifizierung
cat("\n=== Finale G-Verteilung ===\n")
print(table(lpdata_balanced$G))

cat("\nLänder pro G:\n")
lpdata_balanced %>% distinct(Country, G) %>% count(G) %>% print()
################################################################################
##CONTDID EINZELN
library(contdid)
set.seed(1234)

lpdata_balanced <- lpdata_balanced %>%
  mutate(
    G = ifelse(D_selected <= MY_CUTOFF, 0, 21),
    D_selected = ifelse(D_selected <= MY_CUTOFF, 0, D_selected)
  )

# Level-Effekte: ATT(d)
cd_gdp_level2 <- cont_did(
  yname = "gdp_growth",
  target_parameter = "level",
  aggregation = "dose",
  tname ="time_period",
  required_pre_periods = 0,
  base_period = "varying",  #default is "varying"
  idname = "id",
  dname = "D_selected",
  gname = "G",
  data = lpdata_balanced,
  treatment_type = "continuous",
  control_group = "nevertreated",   #notyettreated','nevertreated','eventuallytreated'
  dose_est_method = "parametric",
  dvals = NULL,
  degree = 3,    # check for best fit value, 1 = linear, 2 = polynom und 3 = kubisch
  num_knots = 0,  #0 Eine Formel für ganzen Bereich, 1 darf eine Trennun gmachen-> wäre möglich nach der Impfung
  anticipation = 0,
  weightname = NULL, #Evtl. nach grösse gewichten
  boot_type = "empirical",
  bstrap = TRUE,
  biters = 500,
  cband = TRUE,
  alp = 0.05,
  cl= 1,
)

summary(cd_gdp_level2)

ggcont_did(cd_gdp_level2, type = "att")
ggcont_did(cd_gdp_level2, type = "acrt")


# Filtert nach exaktem Wortlaut
treated_list <- lpdata_balanced %>%
  filter(treatment_group == "Treated (strenger)") %>%
  distinct(Country) # Listet jedes Land nur einmal auf

print(treated_list)







# =============================================================================
# SENSITIVITÄTSANALYSE: Robustheit der Ergebnisse
# =============================================================================

# 1. Alternative Kontrollgruppen-Definition
# ------------------------------------------
# Teste ob Ergebnisse robust sind gegenüber verschiedenen Schwellenwerten

cat("=== Sensitivität: Alternative Kontrollgruppen ===\n")

# Ursprünglich: D < 0 als Kontrollgruppe
# Alternative 1: D < -0.5 (nur sehr lockere Länder)
# Alternative 2: D < median(D) als Kontrollgruppe

# 2. Placebo-Test mit Pre-Treatment Perioden
# ------------------------------------------
# Verwende einen "falschen" Treatment-Zeitpunkt vor COVID

placebo_data <- lpdata_balanced %>%
  filter(time_period <= 20) %>%  # Nur Pre-COVID
  mutate(
    # Falscher Treatment-Zeitpunkt: Q1.2018 (TimeIndex = 13)
    G_placebo = ifelse(G == 0, 0, 17)
  )

cat("\n=== Placebo-Test: Falscher Treatment-Zeitpunkt (Q1.2018) ===\n")

placebo_result <- tryCatch({
  cont_did(
    yname = "gdp_growth",
    target_parameter = "level",
    aggregation = "dose",
    tname = "time_period",
    idname = "id",
    dname = "D",
    gname = "G_placebo",
    data = placebo_data,
    treatment_type = "continuous",
    control_group = "nevertreated",
    degree = 3,
    num_knots = 0,
    bstrap = TRUE,
    biters = 500,
    cband = TRUE,
    alp = 0.05
  )
}, error = function(e) {
  cat("FEHLER:", e$message, "\n")
  return(NULL)
})

if(!is.null(placebo_result)) {
  cat("\nPlacebo-Ergebnis (sollte NICHT signifikant sein):\n")
  summary(placebo_result)
}


# Entferne alle Beobachtungen vor Q1.2018 (TimeIndex = 13)
lpdata_balanced <- lpdata_balanced %>%
  filter(time_period >= 13)

# Verifizieren
cat("Zeitraum nach Filter:\n")
cat("Min:", min(lpdata_balanced$time_period), "(sollte 13 = Q1.2018 sein)\n")
cat("Max:", max(lpdata_balanced$time_period), "\n")
cat("Anzahl Perioden:", n_distinct(lpdata_balanced$time_period), "\n")
cat("Beobachtungen:", nrow(lpdata_balanced), "\n")


# Neuer Placebo-Test mit kürzerem Pre-Treatment Fenster
placebo_data_v2 <- lpdata_balanced %>%
  filter(time_period <= 20) %>%
  mutate(
    G_placebo = ifelse(G == 0, 0, 19)  # Q1.2019 als falscher Treatment-Zeitpunkt
  )


placebo_debt_v2 <- cont_did(
  yname = "debt_growth",
  target_parameter = "level",
  aggregation = "dose",
  tname = "time_period",
  idname = "id",
  dname = "D",
  gname = "G_placebo",
  data = placebo_data_v2,
  treatment_type = "continuous",
  control_group = "nevertreated",
  degree = 3,
  num_knots = 0,
  bstrap = TRUE,
  biters = 500
)

summary(placebo_debt_v2)



#EventStudy


cd_gdp_level2 <- cont_did(
  yname = "QReal.GDP.Growth_gr",
  target_parameter = "slope",
  aggregation = "eventstudy",
  est_mode = "reg",
  tname ="time_period",
  idname = "id",
  dname = "D",
  gname = "G",
  data = lpdata_balanced,
  treatment_type = "continuous",
  control_group = "nevertreated",   #notyettreated','nevertreated','eventuallytreated'
  dose_est_method = "parametric",
  dvals = NULL,
  degree = 3,    # check for best fit value, 1 = linear, 2 = polynom und 3 = kubisch
  num_knots = 0,  #0 Eine Formel für ganzen Bereich, 1 darf eine Trennun gmachen-> wäre möglich nach der Impfung
  anticipation = 0,
  weightname = NULL, #Evtl. nach grösse gewichten
  boot_type = "empirical",
  bstrap = TRUE,
  biters = 50,
  cband = TRUE,
  alp = 0.05,
  cl= 1,
)

summary(cd_gdp_level2)

ggcont_did(cd_gdp_level2, type = "att")
ggcont_did(cd_gdp_level2, type = "acrt")



# =============================================================================
# PRE-TRENDS TEST: Standard Parallel Trends
# =============================================================================

# --- Manuelle Pre-Trends Analyse ---------------------------------------------

# Berechne Trends vor COVID (time_period < 21) nach Dosis-Gruppen
pre_trends <- lpdata_balanced %>%
  filter(time_period < 21) %>%  # Nur Pre-Treatment
  mutate(
    # Teile in Dosis-Gruppen
    dose_group = case_when(
      G == 0 ~ "Kontrollgruppe",
      D < median(D[G > 0]) ~ "Treated: Niedrige Dosis",
      TRUE ~ "Treated: Hohe Dosis"
    )
  ) %>%
  group_by(time_period, dose_group) %>%
  summarise(
    mean_gdp_growth = mean(QReal.GDP.Growth_gr, na.rm = TRUE),
    mean_debt_growth = mean(DebtN_combined, na.rm = TRUE),
    mean_mortality = mean(p_avg_all_ages, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Visualisierung: GDP Growth Pre-Trends
p_pretrends_gdp <- ggplot(pre_trends, aes(x = time_period, y = mean_gdp_growth, 
                                          color = dose_group, group = dose_group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 20.5, linetype = "dashed", color = "red") +
  annotate("text", x = 20.5, y = Inf, label = "Treatment\n(Q1.2020)", 
           vjust = 1.5, hjust = 1.1, color = "red", size = 3) +
  labs(
    title = "Pre-Trends Test: BIP-Wachstum",
    subtitle = "Parallele Trends vor COVID-19?",
    x = "Zeitperiode",
    y = "Durchschnittliches BIP-Wachstum (pp)",
    color = "Gruppe"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_pretrends_gdp)

# Wiederhole für Debt und Mortality
p_pretrends_debt <- ggplot(pre_trends, aes(x = time_period, y = mean_debt_growth, 
                                           color = dose_group, group = dose_group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 20.5, linetype = "dashed", color = "red") +
  labs(
    title = "Pre-Trends Test: Schulden-Wachstum",
    x = "Zeitperiode",
    y = "Durchschnittliches Schulden-Wachstum (pp)",
    color = "Gruppe"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_pretrends_debt)

# --- Formaler Test: Regression auf Pre-Trends --------------------------------

# Teste ob Dosis mit Pre-Treatment Trends korreliert
pretrend_test_data <- lpdata_balanced %>%
  filter(time_period < 21) %>%
  group_by(id) %>%
  mutate(
    # Berechne Trend (Änderung zum Vorquartal)
    gdp_change = QReal.GDP.Growth_gr - lag(QReal.GDP.Growth_gr),
    debt_change = DebtN_combined - lag(DebtN_combined)
  ) %>%
  ungroup()

# Regression: Ist D mit Pre-Trends korreliert?
pretrend_model_gdp <- lm(gdp_change ~ D + factor(time_period), 
                         data = pretrend_test_data)

cat("\n=== Formaler Pre-Trends Test: BIP ===\n")
cat("H0: Dosis (D) ist nicht mit Pre-Treatment Trends korreliert\n\n")
summary(pretrend_model_gdp)$coefficients["D", ]

pretrend_model_debt <- lm(debt_change ~ D + factor(time_period), 
                          data = pretrend_test_data)

cat("\n=== Formaler Pre-Trends Test: Schulden ===\n")
summary(pretrend_model_debt)$coefficients["D", ]

# --- Interpretation ---
cat("\n=== Interpretation ===\n")
cat("Wenn der Koeffizient auf D NICHT signifikant ist,\n")
cat("unterstützt dies die Standard Parallel Trends Annahme.\n")
















##LOOOOOPS

# --- 1. Basis-Parameter definieren -------------------------------------------

base_params <- list(
  tname = "time_period",
  idname = "id", 
  dname = "D",
  gname = "G",
  data = lpdata_balanced,
  treatment_type = "continuous",
  control_group = "notyettreated",  # WICHTIG: Jetzt "nevertreated" statt "notyettreated"
  
  # B-Spline Spezifikation
  degree = 3,
  num_knots = 0,
  
  # Inferenz
  bstrap = TRUE,
  biters = 500,
  cband = TRUE,
  alp = 0.05
)

# --- 2. OUTCOME 1: BIP pro Kopf ----------------------------------------------

cat("\n", strrep("=", 60), "\n")
cat("OUTCOME 1: BIP pro Kopf (rGDP_per_capita)\n")
cat(strrep("=", 60), "\n")

# Level-Effekte: ATT(d)
cd_gdp_level <- cont_did(
  yname = "QReal.GDP.Growth_gr",
  target_parameter = "level",
  aggregation = "dose",
  tname = base_params$tname,
  idname = base_params$idname,
  dname = base_params$dname,
  gname = base_params$gname,
  data = base_params$data,
  treatment_type = base_params$treatment_type,
  control_group = base_params$control_group,
  degree = base_params$degree,
  num_knots = base_params$num_knots,
  bstrap = base_params$bstrap,
  biters = base_params$biters,
  cband = base_params$cband,
  alp = base_params$alp
)

cat("\n--- ATT(d) für BIP ---\n")
summary(cd_gdp_level)

# Slope-Effekte: ACRT(d)
cd_gdp_slope <- cont_did(
  yname = "QReal.GDP.Growth_gr",
  target_parameter = "slope",
  aggregation = "dose",
  tname = base_params$tname,
  idname = base_params$idname,
  dname = base_params$dname,
  gname = base_params$gname,
  data = base_params$data,
  treatment_type = base_params$treatment_type,
  control_group = base_params$control_group,
  degree = base_params$degree,
  num_knots = base_params$num_knots,
  bstrap = base_params$bstrap,
  biters = base_params$biters,
  cband = base_params$cband,
  alp = base_params$alp
)

cat("\n--- ACRT(d) für BIP ---\n")
summary(cd_gdp_slope)

# Event-Study
cd_gdp_event <- cont_did(
  yname = "rGDP_per_capita",
  target_parameter = "level",
  aggregation = "eventstudy",
  tname = base_params$tname,
  idname = base_params$idname,
  dname = base_params$dname,
  gname = base_params$gname,
  data = base_params$data,
  treatment_type = base_params$treatment_type,
  control_group = base_params$control_group,
  degree = base_params$degree,
  num_knots = base_params$num_knots,
  bstrap = base_params$bstrap,
  biters = base_params$biters,
  cband = base_params$cband,
  alp = base_params$alp
)

cat("\n--- Event-Study für BIP ---\n")
summary(cd_gdp_event)

# --- 3. OUTCOME 2: Staatsverschuldung ----------------------------------------

cat("\n", strrep("=", 60), "\n")
cat("OUTCOME 2: Staatsverschuldung (Debt_an)\n")
cat(strrep("=", 60), "\n")

cd_debt_level <- cont_did(
  yname = "Debt_an",
  target_parameter = "level",
  aggregation = "dose",
  tname = base_params$tname,
  idname = base_params$idname,
  dname = base_params$dname,
  gname = base_params$gname,
  data = base_params$data,
  treatment_type = base_params$treatment_type,
  control_group = base_params$control_group,
  degree = base_params$degree,
  num_knots = base_params$num_knots,
  bstrap = base_params$bstrap,
  biters = base_params$biters,
  cband = base_params$cband,
  alp = base_params$alp
)

cat("\n--- ATT(d) für Verschuldung ---\n")
summary(cd_debt_level)

cd_debt_event <- cont_did(
  yname = "Debt_an",
  target_parameter = "level",
  aggregation = "eventstudy",
  tname = base_params$tname,
  idname = base_params$idname,
  dname = base_params$dname,
  gname = base_params$gname,
  data = base_params$data,
  treatment_type = base_params$treatment_type,
  control_group = base_params$control_group,
  degree = base_params$degree,
  num_knots = base_params$num_knots,
  bstrap = base_params$bstrap,
  biters = base_params$biters,
  cband = base_params$cband,
  alp = base_params$alp
)

# --- 4. OUTCOME 3: Übersterblichkeit -----------------------------------------

cat("\n", strrep("=", 60), "\n")
cat("OUTCOME 3: Übersterblichkeit (p_avg_all_ages)\n")
cat(strrep("=", 60), "\n")

# Prüfe Datenverfügbarkeit
n_complete_mortality <- lpdata_balanced %>%
  group_by(id) %>%
  filter(all(!is.na(p_avg_all_ages))) %>%
  ungroup() %>%
  distinct(id) %>%
  nrow()

cat("Länder mit vollständigen Mortalitätsdaten:", n_complete_mortality, "\n")

if(n_complete_mortality >= 10) {
  
  lpdata_mortality <- lpdata_balanced %>%
    group_by(id) %>%
    filter(all(!is.na(p_avg_all_ages))) %>%
    ungroup()
  
  cd_mortality_level <- cont_did(
    yname = "p_avg_all_ages",
    target_parameter = "level",
    aggregation = "dose",
    tname = "time_period",
    idname = "id",
    dname = "D",
    gname = "G",
    data = lpdata_mortality,
    treatment_type = "continuous",
    control_group = "nevertreated",
    degree = 3,
    num_knots = 0,
    bstrap = TRUE,
    biters = base_params$biters,
    cband = TRUE,
    alp = 0.05
  )
  
  cat("\n--- ATT(d) für Übersterblichkeit ---\n")
  summary(cd_mortality_level)
  
} else {
  cat("Zu wenige Länder - überspringe Mortalitätsanalyse.\n")
  cd_mortality_level <- NULL
}

# --- 5. VISUALISIERUNG -------------------------------------------------------

cat("\n", strrep("=", 60), "\n")
cat("VISUALISIERUNG\n")
cat(strrep("=", 60), "\n")

# ATT(d) Plots
p_gdp_att <- ggcont_did(cd_gdp_level, type = "att") +
  labs(
    title = "ATT(d): Effekt der Policy-Intensität auf BIP",
    x = "Policy-Intensität (PRF-Residuen)",
    y = "ATT: Effekt auf BIP pro Kopf"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal()

print(p_gdp_att)

p_debt_att <- ggcont_did(cd_debt_level, type = "att") +
  labs(
    title = "ATT(d): Effekt der Policy-Intensität auf Verschuldung",
    x = "Policy-Intensität (PRF-Residuen)",
    y = "ATT: Effekt auf Staatsverschuldung"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal()

print(p_debt_att)

# ACRT(d) Plot
p_gdp_acrt <- ggcont_did(cd_gdp_slope, type = "acrt") +
  labs(
    title = "ACRT(d): Marginaler Effekt auf BIP",
    x = "Policy-Intensität (PRF-Residuen)",
    y = "Marginaler Causal Response"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal()

print(p_gdp_acrt)

# Event-Study Plot
plot(cd_gdp_event)

# --- 6. ERGEBNISSE SPEICHERN -------------------------------------------------

results <- list(
  gdp = list(level = cd_gdp_level, slope = cd_gdp_slope, event = cd_gdp_event),
  debt = list(level = cd_debt_level, event = cd_debt_event),
  mortality = list(level = cd_mortality_level),
  meta = list(
    n_treated = 20,
    n_control = 18,
    control_definition = "Länder mit D_mean < 0 (lockerer als erwartet)"
  )
)

saveRDS(results, "contdid_results.rds")
cat("\nErgebnisse gespeichert.\n")


















##LTU FEHLT 1 BEOABCHTUNG Q1.2020

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&TEST&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#treatment_countries <- c("CAN", "DEU", "GRC", "IRL", "ISR", "ITA", "JPN", "KOR", "LVA", "USA") 
#control_countries<- c("Aus", "COL", "CRI", "ESP", "GRC", "IRL", "NLD", "PRT", "TUR", "USA")
#original
treatment_countries <- c("MEX", "COL", "NZL", "SVN", "BEL", "EST", "HUN", "NOR", "GRC", "NLD", "ESP", "IRL", "CHL", "GBR", "CHE", "AUS", "FRA", "FIN", "PRT", "LTU")

#create new colum for treatment indicator
qdata <- qdata %>%
  mutate(TreatmentGroup = ifelse(Country %in% treatment_countries, 1, 0))


cro(qdata$TreatmentGroup)

# Treatment period variable (after Q4.2019)
qdata$qcovid <- ifelse(qdata$TimeIndex >= 21 & qdata$TimeIndex <= 40, 1, 0)


qdata$qstrict<-qdata$TreatmentGroup
# Treatment variable (treated x post)
qdata$qtreatment <- (qdata$qstrict*qdata$qcovid)

cro(qdata$qtreatment)

control_countries <- qdata %>%
  filter(!Country %in% treatment_countries) %>%
  distinct(Country) %>%
  pull(Country)


print(treatment_countries)
print(control_countries)

qdata <- qdata %>%
  mutate(qrel_year = ifelse(TreatmentGroup == 0, Inf, qtime))


##NORMAL 2SDID




qdata$cum_excess_per_million_proj_all_ages

#####bootstrap
did3g<- did2s(qdata, yname= "DebtN_combined",
              treatment= "qtreatment",
              cluster_var="Country",
              first_stage = ~ 0| Quarter+ Country, 
              second_stage = ~ i(qtreatment, ref= FALSE),
              bootstrap = FALSE,
              n_bootstraps = 10000,
              return_bootstrap = TRUE,
              verbose = TRUE)

summary(did3g, vcov = ~ Country + Quarter)

##VCOV and Clustering think about it

did41 <- did2s(data = qdata,
               yname = "DebtN_combined", 
               treatment = "qtreatment",
               cluster_var = "Country",
               first_stage = ~ 0 | Quarter + Country, 
               second_stage = ~ i(qrel_year, ref = c(0, Inf)),
               bootstrap = FALSE,
               n_bootstraps = 10000,
               return_bootstrap = TRUE,
               verbose = FALSE)

summary(did41, vcov = ~ Country + Quarter)




##CONTINOUS TREATMENT
##CALLWAY & SUN LESEN


# Welche Variablen willst du aus df_analysis nach qdata übernehmen?
vars_add <- c("resid_baseline_mean", "resid_neighbor_mean", "stringency_mean")
# <- passe diese Liste an

# 1) qdata: Quarter "Q1.2020" -> YQ "2020-Q1" + Year/Quarter_num
qdata2 <- qdata %>%
  mutate(
    Year        = as.integer(str_extract(Quarter, "\\d{4}")),
    Quarter_num = as.integer(str_extract(Quarter, "(?<=Q)[1-4]")),
    YQ          = paste0(Year, "-Q", Quarter_num)
  )

# 2) df_analysis: YQ sicher als character, nur Keys + Variablen
df_add <- df_analysis %>%
  mutate(YQ = as.character(YQ)) %>%
  select(Country, YQ, all_of(vars_add))

# 3) Join + 4) vor 2020-Q1 = 0 (für die hinzugefügten Variablen)
qdata2 <- qdata2 %>%
  left_join(df_add, by = c("Country", "YQ")) %>%
  mutate(
    across(
      all_of(vars_add),
      ~ ifelse(Year < 2020, 0, .x)   # nur vor 2020 auf 0 setzen
    )
  )


qdata2$cum_excess_per_million_proj_all_ages

did_cont <- feols(
  y_t ~ i(qcovid, resid_baseline_mean, ref = 0) |
    Country+ Quarter,
  data = qdata2,
  cluster = ~Country
)

summary(did_cont)


es_cont <- feols(
  y_t ~ i(qrel_year, resid_baseline_mean, ref = 1) |
    Country + Quarter,         
  data = qdata2,
  cluster = ~Country
)

summary(es_cont)



##CALLWAY & SUN

qdata2$resid_baseline_mean

library(contdid)

?cont_did

cd_res <- cont_did(
  yname = "y_t",
  tname = "qtime",
  idname = "Country",
  dname = "resid_baseline_mean",
  data = qdata2,
  gname = "qcovid",
  target_parameter = "slope",
  aggregation = "dose",
  treatment_type = "continuous",
  control_group = "notyettreated",
  biters = 100,
  cband = TRUE,
  num_knots = 1,
  degree = 3,
)




summary(cd_res)
ggcont_did(cd_res, type = "att")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%TEST ENDE%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



# ==============================================================================

# ==============================================================================
# 1. FISKAL-DATEN VORBEREITEN (WOCHENBASIS)
# ==============================================================================

# Annahme: Deine Daten heißen fiscal_weekly
fiscal_weekly <- fm1_w2 %>%
  mutate(
    # Week-String parsen
    Year = as.numeric(str_sub(Week, 1, 4)),
    Week_num = as.numeric(str_sub(Week, 7, 8)),
    # Datum des Montags dieser Woche
    Date = as.Date(paste0(Year, "-01-04")) + weeks(Week_num - 1),
    # Korrektur: ISOweek zu Datum
    Quarter = quarter(Date),
    YQ = paste0(Year, "-Q", Quarter)
  )

# Check
fiscal_weekly %>%
  filter(Country == "DEU") %>%
  select(Country, Week, Date, Year, Week_num, broad_fiscal_gdp) %>%
  head(20)

# ==============================================================================
# 2. MIT STRINGENCY UND EPIDEMIE-DATEN ZUSAMMENFÜHREN (WOCHENBASIS)
# ==============================================================================

# Stringency und Epidemie auf Wochenbasis aggregieren
weekly_context <- df_daily %>%
  mutate(
    Year = year(Date),
    Week_num = isoweek(Date),
    Week = paste0(Year, "-W", sprintf("%02d", Week_num))
  ) %>%
  group_by(Country, Week, Year, Week_num) %>%
  summarise(
    stringency_mean = mean(StringencyIndex_PopWeighted, na.rm = TRUE),
    R_mean = mean(R, na.rm = TRUE),
    cases_mean = mean(ConfirmedCases, na.rm = TRUE),
    deaths_mean = mean(ConfirmedDeaths, na.rm = TRUE),
    log_cases_mean = mean(log(ConfirmedCases + 1), na.rm = TRUE),
    log_deaths_mean = mean(log(ConfirmedDeaths + 1), na.rm = TRUE),
    .groups = "drop"
  )

# Zusammenführen
fiscal_weekly <- fiscal_weekly %>%
  left_join(weekly_context, by = c("Country", "Week", "Year", "Week_num"))

# Check
fiscal_weekly %>%
  filter(Country == "DEU") %>%
  select(Country, Week, broad_fiscal_gdp, stringency_mean, R_mean) %>%
  head(20)

# ==============================================================================
# 3. LAGS ERSTELLEN (WOCHENBASIS)
# ==============================================================================

fiscal_weekly <- fiscal_weekly %>%
  arrange(Country, Year, Week_num) %>%
  group_by(Country) %>%
  mutate(
    # Fiskal-Lags
    lag1_fiscal = lag(broad_fiscal_gdp, 1),
    lag2_fiscal = lag(broad_fiscal_gdp, 2),
    lag4_fiscal = lag(broad_fiscal_gdp, 4),  # 1 Monat
    
    # Stringency-Lags
    lag1_stringency = lag(stringency_mean, 1),
    lag2_stringency = lag(stringency_mean, 2),
    
    # Epidemie-Lags
    lag1_R = lag(R_mean, 1),
    lag1_log_cases = lag(log_cases_mean, 1),
    lag1_log_deaths = lag(log_deaths_mean, 1),
    lag2_log_cases = lag(log_cases_mean, 2),
    
    # Änderungen
    delta_stringency = stringency_mean - lag(stringency_mean, 1),
    delta_fiscal = broad_fiscal_gdp - lag(broad_fiscal_gdp, 1)
  ) %>%
  ungroup()

# ==============================================================================
# 4. NACHBAR-FISKAL BERECHNEN (optional, für Spillover)
# ==============================================================================

# Analog zur Stringency: Durchschnitt der Nachbarn
calculate_neighbor_fiscal_fast <- function(fiscal_weekly, neighbors_list) {
  
  neighbors_df <- tibble::enframe(neighbors_list, name = "Country", value = "neighbor") %>%
    tidyr::unnest_longer(neighbor)
  
  fiscal_lookup <- fiscal_weekly %>%
    select(Country, Week, broad_fiscal_gdp) %>%
    rename(neighbor = Country, neighbor_fiscal = broad_fiscal_gdp)
  
  neighbor_fiscal <- fiscal_weekly %>%
    distinct(Country, Week) %>%
    left_join(neighbors_df, by = "Country") %>%
    left_join(fiscal_lookup, by = c("neighbor", "Week")) %>%
    group_by(Country, Week) %>%
    summarise(
      neighbor_fiscal = mean(neighbor_fiscal, na.rm = TRUE),
      n_neighbors_fiscal = sum(!is.na(neighbor_fiscal)),
      .groups = "drop"
    )
  
  fiscal_weekly %>%
    left_join(neighbor_fiscal, by = c("Country", "Week"))
}

fiscal_weekly <- calculate_neighbor_fiscal_fast(fiscal_weekly, neighbors_list)

# Lag der Nachbar-Fiskal
fiscal_weekly <- fiscal_weekly %>%
  group_by(Country) %>%
  mutate(
    lag1_neighbor_fiscal = lag(neighbor_fiscal, 1)
  ) %>%
  ungroup()

# ==============================================================================
# 5. FIXED EFFECTS ERSTELLEN
# ==============================================================================

fiscal_weekly <- fiscal_weekly %>%
  mutate(
    country_id = as.factor(Country),
    week_id = as.factor(Week)
  )

# ==============================================================================
# 6. FISKAL-REAKTIONSFUNKTION (WOCHENBASIS)
# ==============================================================================

# F1: Baseline - nur Persistenz
fiscal_rf_w1 <- feols(
  broad_fiscal_gdp ~ 
    lag1_fiscal |
    country_id + week_id,
  data = fiscal_weekly,
  cluster = ~country_id
)

# F2: + Reaktion auf Stringency
fiscal_rf_w2 <- feols(
  broad_fiscal_gdp ~ 
    lag1_fiscal +
    lag1_stringency |
    country_id + week_id,
  data = fiscal_weekly,
  cluster = ~country_id
)

# F3: + Stringency-Änderung (Reaktion auf neue Lockdowns)
fiscal_rf_w3 <- feols(
  broad_fiscal_gdp ~ 
    lag1_fiscal +
    lag1_stringency +
    delta_stringency |
    country_id + week_id,
  data = fiscal_weekly,
  cluster = ~country_id
)

# F4: + Epidemiologische Lage
fiscal_rf_w4 <- feols(
  broad_fiscal_gdp ~ 
    lag1_fiscal +
    lag1_stringency +
    delta_stringency +
    lag1_log_cases +
    lag1_log_deaths |
    country_id + week_id,
  data = fiscal_weekly,
  cluster = ~country_id
)

# F5: + R-Rate
fiscal_rf_w5 <- feols(
  broad_fiscal_gdp ~ 
    lag1_fiscal +
    lag1_stringency +
    delta_stringency +
    lag1_log_cases +
    lag1_log_deaths +
    lag1_R |
    country_id + week_id,
  data = fiscal_weekly,
  cluster = ~country_id
)

# F6: + Nachbar-Spillover
fiscal_rf_w6 <- feols(
  broad_fiscal_gdp ~ 
    lag1_fiscal +
    lag1_stringency +
    delta_stringency +
    lag1_log_cases +
    lag1_log_deaths +
    lag1_R +
    lag1_neighbor_fiscal |
    country_id + week_id,
  data = fiscal_weekly,
  cluster = ~country_id
)

# ==============================================================================
# 7. ERGEBNISSE VERGLEICHEN
# ==============================================================================

modelsummary(
  list(
    "F1: Persistenz" = fiscal_rf_w1,
    "F2: + Stringency" = fiscal_rf_w2,
    "F3: + ΔStringency" = fiscal_rf_w3,
    "F4: + Cases/Deaths" = fiscal_rf_w4,
    "F5: + R-Rate" = fiscal_rf_w5,
    "F6: + Nachbar" = fiscal_rf_w6
  ),
  stars = TRUE,
  gof_map = c("nobs", "within.r.squared")
)

# Within R²
cat("\n---- Within R² ----\n")
cat("F1 Persistenz:", round(fitstat(fiscal_rf_w1, "wr2")[[1]], 4), "\n")
cat("F2 + Stringency:", round(fitstat(fiscal_rf_w2, "wr2")[[1]], 4), "\n")
cat("F3 + ΔStringency:", round(fitstat(fiscal_rf_w3, "wr2")[[1]], 4), "\n")
cat("F4 + Cases/Deaths:", round(fitstat(fiscal_rf_w4, "wr2")[[1]], 4), "\n")
cat("F5 + R-Rate:", round(fitstat(fiscal_rf_w5, "wr2")[[1]], 4), "\n")
cat("F6 + Nachbar:", round(fitstat(fiscal_rf_w6, "wr2")[[1]], 4), "\n")

# ==============================================================================
# 8. RESIDUEN EXTRAHIEREN (WOCHENBASIS)
# ==============================================================================

# Beste Spezifikation wählen (z.B. F5 oder F6)
fiscal_weekly <- fiscal_weekly %>%
  mutate(
    in_sample_fiscal = !is.na(broad_fiscal_gdp) & 
      !is.na(lag1_fiscal) &
      !is.na(lag1_stringency) &
      !is.na(delta_stringency) &
      !is.na(lag1_log_cases) &
      !is.na(lag1_log_deaths) &
      !is.na(lag1_R)
  )

fiscal_weekly$resid_fiscal <- NA_real_
fiscal_weekly$resid_fiscal[fiscal_weekly$in_sample_fiscal] <- residuals(fiscal_rf_w5)

# Check
sum(!is.na(fiscal_weekly$resid_fiscal))

# ==============================================================================
# 9. AUF QUARTALSBASIS AGGREGIEREN
# ==============================================================================

fiscal_quarterly_resid <- fiscal_weekly %>%
  mutate(
    Quarter = quarter(Date),
    YQ = paste0(Year, "-Q", Quarter)
  ) %>%
  filter(Year %in% c(2020, 2021)) %>%
  group_by(Country, Year, Quarter, YQ) %>%
  summarise(
    # Fiskal-Residuen
    resid_fiscal_mean = mean(resid_fiscal, na.rm = TRUE),
    resid_fiscal_sd = sd(resid_fiscal, na.rm = TRUE),
    
    # Original-Fiskal (für Kontext)
    fiscal_total_sum = sum(broad_fiscal_gdp, na.rm = TRUE),
    fiscal_total_mean = mean(broad_fiscal_gdp, na.rm = TRUE),
    
    # Anzahl Wochen
    n_weeks_fiscal = sum(!is.na(resid_fiscal)),
    
    .groups = "drop"
  )

# Mit Hauptdaten zusammenführen
df_quarterly <- df_quarterly %>%
  left_join(
    fiscal_quarterly_resid %>% 
      select(Country, Year, Quarter, YQ, resid_fiscal_mean, fiscal_total_sum),
    by = c("Country", "Year", "Quarter", "YQ")
  )

# Check
df_quarterly %>%
  filter(Country == "DEU") %>%
  select(YQ, resid_baseline_mean, resid_fiscal_mean, fiscal_total_sum) %>%
  print(n = 8)



# ==============================================================================
# DIAGNOSE: SAMPLE-VERLUST
# ==============================================================================

# Wie viele Beobachtungen hast du in den Rohdaten?
nrow(fiscal_weekly)

# Wie viele pro Land?
fiscal_weekly %>%
  count(Country) %>%
  print(n = 40)

# Wie viele NAs pro Variable?
fiscal_weekly %>%
  summarise(
    n_total = n(),
    na_fiscal = sum(is.na(broad_fiscal_gdp)),
    na_lag1_fiscal = sum(is.na(lag1_fiscal)),
    na_stringency = sum(is.na(stringency_mean)),
    na_lag1_stringency = sum(is.na(lag1_stringency)),
    na_R = sum(is.na(R_mean)),
    na_lag1_R = sum(is.na(lag1_R)),
    na_log_cases = sum(is.na(log_cases_mean)),
    na_neighbor = sum(is.na(lag1_neighbor_fiscal))
  )

# Welche Länder fehlen komplett?
fiscal_weekly %>%
  group_by(Country) %>%
  summarise(
    n_weeks = n(),
    n_nonzero_fiscal = sum(broad_fiscal_gdp > 0, na.rm = TRUE),
    n_with_stringency = sum(!is.na(stringency_mean))
  ) %>%
  filter(n_nonzero_fiscal < 10) %>%
  print(n = 40)

# Zeitraum der Daten?
fiscal_weekly %>%
  summarise(
    min_week = min(Week),
    max_week = max(Week),
    n_distinct_weeks = n_distinct(Week)
  )





# ==============================================================================
# FISKAL-REAKTIONSFUNKTION AUF QUARTALSBASIS
# ==============================================================================

# 1. Auf Quartal aggregieren (Summe der Pakete)
fiscal_quarterly <- fiscal_weekly %>%
  mutate(
    Quarter = quarter(Date),
    YQ = paste0(Year, "-Q", Quarter)
  ) %>%
  filter(Year %in% c(2020, 2021)) %>%
  group_by(Country, Year, Quarter, YQ) %>%
  summarise(
    # Summe der Fiskal-Maßnahmen im Quartal
    fiscal_total = sum(broad_fiscal_gdp, na.rm = TRUE),
    fiscal_expenditure = sum(expenditure, na.rm = TRUE),
    fiscal_above = sum(above, na.rm = TRUE),
    
    # Kontext-Variablen (Durchschnitt)
    stringency_mean = mean(stringency_mean, na.rm = TRUE),
    R_mean = mean(R_mean, na.rm = TRUE),
    log_cases_mean = mean(log_cases_mean, na.rm = TRUE),
    log_deaths_mean = mean(log_deaths_mean, na.rm = TRUE),
    
    n_weeks = n(),
    n_packages = sum(broad_fiscal_gdp > 0, na.rm = TRUE),
    
    .groups = "drop"
  )

# Check
fiscal_quarterly %>%
  filter(Country == "DEU") %>%
  print(n = 8)

# 2. Lags erstellen
fiscal_quarterly <- fiscal_quarterly %>%
  arrange(Country, Year, Quarter) %>%
  group_by(Country) %>%
  mutate(
    lag1_fiscal = lag(fiscal_total, 1),
    lag1_stringency = lag(stringency_mean, 1),
    lag1_R = lag(R_mean, 1),
    lag1_log_cases = lag(log_cases_mean, 1),
    lag1_log_deaths = lag(log_deaths_mean, 1)
  ) %>%
  ungroup()

# 3. Fixed Effects
fiscal_quarterly <- fiscal_quarterly %>%
  mutate(
    country_id = as.factor(Country),
    yq_id = as.factor(YQ)
  )

# ==============================================================================
# FISKAL-REAKTIONSFUNKTION (QUARTALSBASIS)
# ==============================================================================

# FQ1: Baseline - nur Persistenz
fiscal_rf_q1 <- feols(
  fiscal_total ~ 
    lag1_fiscal |
    country_id + yq_id,
  data = fiscal_quarterly,
  cluster = ~country_id
)

# FQ2: + Stringency (Kompensation für Lockdowns)
fiscal_rf_q2 <- feols(
  fiscal_total ~ 
    lag1_fiscal +
    stringency_mean |
    country_id + yq_id,
  data = fiscal_quarterly,
  cluster = ~country_id
)

# FQ3: + Epidemiologische Lage
fiscal_rf_q3 <- feols(
  fiscal_total ~ 
    lag1_fiscal +
    stringency_mean +
    lag1_log_cases +
    lag1_log_deaths |
    country_id + yq_id,
  data = fiscal_quarterly,
  cluster = ~country_id
)

# FQ4: + R-Rate
fiscal_rf_q4 <- feols(
  fiscal_total ~ 
    lag1_fiscal +
    stringency_mean +
    lag1_log_cases +
    lag1_log_deaths +
    lag1_R |
    country_id + yq_id,
  data = fiscal_quarterly,
  cluster = ~country_id
)

# Vergleich
modelsummary(
  list(
    "FQ1: Persistenz" = fiscal_rf_q1,
    "FQ2: + Stringency" = fiscal_rf_q2,
    "FQ3: + Cases/Deaths" = fiscal_rf_q3,
    "FQ4: + R-Rate" = fiscal_rf_q4
  ),
  stars = TRUE,
  gof_map = c("nobs", "within.r.squared")
)

# Within R²
cat("\n---- Within R² (Quartalsbasis) ----\n")
cat("FQ1 Persistenz:", round(fitstat(fiscal_rf_q1, "wr2")[[1]], 4), "\n")
cat("FQ2 + Stringency:", round(fitstat(fiscal_rf_q2, "wr2")[[1]], 4), "\n")
cat("FQ3 + Cases/Deaths:", round(fitstat(fiscal_rf_q3, "wr2")[[1]], 4), "\n")
cat("FQ4 + R-Rate:", round(fitstat(fiscal_rf_q4, "wr2")[[1]], 4), "\n")


# ==============================================================================
# FISKAL-REAKTIONSFUNKTION MIT STRINGENCY-RESIDUEN
# ==============================================================================

# 1. Stringency-Residuen zu fiscal_quarterly hinzufügen
fiscal_quarterly <- fiscal_quarterly %>%
  left_join(
    df_quarterly %>% 
      select(Country, Year, Quarter, YQ, resid_baseline_mean, resid_neighbor_mean),
    by = c("Country", "Year", "Quarter", "YQ")
  )

# 2. Lags der Stringency-Residuen
fiscal_quarterly <- fiscal_quarterly %>%
  arrange(Country, Year, Quarter) %>%
  group_by(Country) %>%
  mutate(
    lag1_resid_stringency = lag(resid_baseline_mean, 1)
  ) %>%
  ungroup()

# ==============================================================================
# MODELLE MIT STRINGENCY-RESIDUEN
# ==============================================================================

# FQ5: Reaktion auf Stringency-Residuen (politische Komponente)
fiscal_rf_q5 <- feols(
  fiscal_total ~ 
    lag1_fiscal +
    resid_baseline_mean |
    country_id + yq_id,
  data = fiscal_quarterly,
  cluster = ~country_id
)

# FQ6: Beide - Original-Stringency UND Residuen
fiscal_rf_q6 <- feols(
  fiscal_total ~ 
    lag1_fiscal +
    stringency_mean +
    resid_baseline_mean |
    country_id + yq_id,
  data = fiscal_quarterly,
  cluster = ~country_id
)

# FQ7: Vollständiges Modell mit Residuen
fiscal_rf_q7 <- feols(
  fiscal_total ~ 
    lag1_fiscal +
    resid_baseline_mean +
    lag1_log_cases +
    lag1_log_deaths |
    country_id + yq_id,
  data = fiscal_quarterly,
  cluster = ~country_id
)

# Vergleich
modelsummary(
  list(
    "FQ2: Stringency" = fiscal_rf_q2,
    "FQ5: Resid-Stringency" = fiscal_rf_q5,
    "FQ6: Beide" = fiscal_rf_q6,
    "FQ7: Resid + Epi" = fiscal_rf_q7
  ),
  stars = TRUE,
  coef_rename = c(
    "lag1_fiscal" = "Fiscal (t-1)",
    "stringency_mean" = "Stringency (total)",
    "resid_baseline_mean" = "Stringency (exogen/politisch)",
    "lag1_log_cases" = "Cases (t-1)",
    "lag1_log_deaths" = "Deaths (t-1)"
  ),
  gof_map = c("nobs", "within.r.squared")
)

# Korrelation zwischen Stringency-Residuen und Fiskal
cor(fiscal_quarterly$resid_baseline_mean, fiscal_quarterly$fiscal_total, 
    use = "complete.obs")

# Wenn positiv: Länder mit "überraschend hoher" Stringency 
#               gaben auch mehr Fiscal Support



# ==============================================================================
# FINALE OUTCOME-REGRESSION: VEREINFACHT
# ==============================================================================

# Da Fiscal quasi-exogen ist, können wir es direkt verwenden:


# 1. Daten zusammenführen
df_final <- df_analysis %>%
  left_join(
    fiscal_quarterly %>% select(Country, YQ, fiscal_total, fiscal_above, fiscal_expenditure),
    by = c("Country", "YQ")
  ) %>%
  filter(YQ != "2020-Q1")  # Q1 2020 ausschließen (keine Residuen)

# 2. Outcome-Regressionen

# GESUNDHEIT
reg_health <- feols(
  cum_excess_per_million_proj_all_ages ~ 
    resid_baseline_mean |    # Stringency (exogen)
    #HIER evtl. FISCAL EINFÜGEN: Fiscal (quasi-exogen)
    Country + YQ,
  data = df_final,
  cluster = ~Country
)

# WIRTSCHAFT
reg_econ <- feols(
  y_t ~ 
    resid_baseline_mean +
    fiscal_expenditure |
    Country + YQ,
  data = df_final,
  cluster = ~Country
)

# WIRTSCHAFT mit Interaktion
reg_econ_interact <- feols(
  y_t~ 
    resid_baseline_mean * fiscal_expenditure |
    Country + YQ,
  data = df_final,
  cluster = ~Country
)

# 3. Ergebnisse
modelsummary(
  list(
    "Health" = reg_health,
    "GDP" = reg_econ,
    "GDP (Interaktion)" = reg_econ_interact
  ),
  stars = TRUE,
  coef_rename = c(
    "resid_baseline_mean" = "Stringency (exogen)",
    "fiscal_total" = "Fiscal Support",
    "resid_baseline_mean:fiscal_total" = "Stringency × Fiscal"
  )
)



##MIT WEITEREN VARIATION TESTEN->ZB TARGETS USW, LAGS, OHNE FE, LOCAL PROJECTIONS (LOKALER)

# ==============================================================================
# ROBUSTHEIT: VERSCHIEDENE FISCAL-MASSE
# ==============================================================================

##DOOO







































































# -----------------------------------------------------------------------------
# 1. Impulse Response Functions (IRFs) berechnen
# -----------------------------------------------------------------------------
# Wir schauen 6 Quartale in die Zukunft (n.ahead = 6)
# Wir nutzen 100 Bootstrap-Runden für die Konfidenzintervalle (n.boot)
# Hinweis: Das kann 1-2 Minuten dauern.
# Schritt A: Die einfache IRF berechnen (für die Linien/Punktschätzer)
# Entferne einfach n.boot hier.
irf_pvar <- oirf(model_pvar, n.ahead = 6)

# Schritt B: Den Bootstrap separat berechnen (für die Konfidenzintervalle/Schatten)
# Das kann 1-2 Minuten dauern.
set.seed(123) # Für reproduzierbare Ergebnisse

boot_pvar <- bootstrap_irf(
  model_pvar, 
  typeof_irf = "OIRF",   
  n.ahead = 6, 
  nof_Nstar_draws = 99, 
  confidence.band = 0.95,
  mc.cores = 1
)


# A. Lockdown -> Wirtschaft
plot(boot_pvar, impulse = "log_stringency", response = "output_gap")
title(main = "Effect of Lockdown (S) on Economy (y)")

# B. Fiscal -> Schulden
plot(boot_pvar, impulse = "fiscal", response = "debt_growth")
title(main = "Effect of Fiscal Support (F) on Debt (b)")

# C. Wirtschaft -> Schulden
plot(boot_pvar, impulse = "output_gap", response = "debt_growth")
title(main = "Effect of Economic Activity (y) on Debt (b)")

# D. Lockdown -> Fälle
plot(boot_pvar, impulse = "log_stringency", response = "log_cases")
title(main = "Effect of Lockdown (S) on Pandemic Spread")








# -----------------------------------------------------------------------------
# 2. Das Trilemma plotten: Die Trade-offs
# -----------------------------------------------------------------------------

# A. Trade-off: Gesundheitsschutz vs. Wirtschaft (S -> y)
# Theorie: Höhere Stringency (S) führt zu schlechterem Output Gap (y)
# Achte auf das Vorzeichen im Plot!
plot(irf_pvar, impulse = "stringency", response = "output_gap")
title(main = "Effect of Lockdown (S) on Economy (y)")

# B. Trade-off: Wirtschaftshilfen vs. Schulden (F -> b)
# Theorie: Mehr Fiskalhilfe (F) erhöht Schuldenwachstum (b)
plot(irf_pvar, impulse = "fiscal", response = "debt_growth")
title(main = "Effect of Fiscal Support (F) on Debt (b)")

# C. Feedback Loop: Wirtschaft vs. Schulden (y -> b)
# Theorie: Schlechter Output Gap (Rezession) treibt Schulden (Autom. Stabilisatoren)
plot(irf_pvar, impulse = "output_gap", response = "debt_growth")
title(main = "Effect of Economic Activity (y) on Debt (b)")

# D. Die Effektivität: Hilft Lockdown gegen die Pandemie? (S -> cases)
# Theorie: Stringency hoch -> Cases runter (negative Reaktion erwartet)
plot(irf_pvar, impulse = "stringency", response = "cases_pc")
title(main = "Effect of Lockdown (S) on Pandemic Spread")

# -----------------------------------------------------------------------------
# 3. Variance Decomposition (FEVD) - "Wer ist schuld?"
# -----------------------------------------------------------------------------
# Zeigt, wie viel Prozent der Varianz einer Variable durch andere erklärt wird.

fevd_pvar <- fevd(model_pvar, n.ahead = 10)

# Fokus auf Schulden: Was treibt den Schuldenanstieg?
# Ist es die Fiskalpolitik (fiscal) oder der Wirtschaftseinbruch (output_gap)?
print(fevd_pvar$debt_growth)

# Fokus auf Wirtschaft: Was treibt den Output Gap?
# Ist es der Lockdown (stringency) oder die Pandemie selbst (cases)?
print(fevd_pvar$output_gap)



























##Alt

# -----------------------------------------------------------------------------
# 3. Identifikation & Impulse Response Functions (IRFs)
# -----------------------------------------------------------------------------

# HIER passiert die Magie der Kausalität. 
# Wir definieren die Cholesky-Reihenfolge basierend auf deinem Theorie-Modell (A1 Timing).
# Reihenfolge: Schock -> Policy Reaktion -> Makro-Outcomes

# Theorie: 
# 1. Pandemic Pressure (cases_pc) ist exogen im Quartal
# 2. Stringency reagiert sofort auf Fälle
# 3. Fiscal reagiert auf Lockdown
# 4. Output Gap reagiert auf Policies
# 5. Debt reagiert als "Residual" am Ende

cholesky_order <- c("cases_pc", "stringency", "fiscal", "output_gap", "debt_growth")

# Berechnung der IRFs
# n.ahead = 6: Wir schauen 6 Quartale in die Zukunft
irf_pvar <- oirf(model_pvar, n.ahead = 6)

# -----------------------------------------------------------------------------
# 4. Visualisierung des Trilemmas (Die Plots)
# -----------------------------------------------------------------------------

# Das Paket hat eine Plot-Funktion, aber wir wollen spezifische "Stories" plotten.
# Wir schauen uns an: Was passiert bei einem Schock in "cases_pc" oder "stringency"?

# Szenario A: Der Preis des Lockdowns (Trilemma Teil 1)
# Schock: Stringency steigt -> Reaktion: Output Gap & Debt
plot(irf_pvar, impulse = "stringency", response = c("output_gap", "debt_growth"))

# Szenario B: Die Effektivität der Fiskalpolitik (Trilemma Teil 2)
# Schock: Fiscal steigt -> Reaktion: Output Gap (hoffentlich hoch) & Debt (hoch)
plot(irf_pvar, impulse = "fiscal", response = c("output_gap", "debt_growth"))

# Szenario C: Der Teufelskreis (Feedback Loop)
# Schock: Output Gap fällt (negativer Schock) -> Reaktion: Debt
plot(irf_pvar, impulse = "output_gap", response = "debt_growth")

# -----------------------------------------------------------------------------
# 5. Variance Decomposition (FEVD)
# -----------------------------------------------------------------------------
# Beantwortet: "Wieviel % der Varianz der Schulden wird durch Fiskalpakete erklärt?"

fevd_pvar <- fevd(model_pvar, n.ahead = 10)

# Zeige die Zerlegung für die Variable "debt_growth"
print(fevd_pvar$debt_growth)




























#-------------------------------------------------------------------------------
##Descriptives OUTCOMES COMBINED


start_q <- zoo::as.yearqtr("2020 Q1")
end_q   <- zoo::as.yearqtr("2022 Q4")

# Populations-Baseline (Q4.2019, in Tausend) für Todesraten
pop_base <- qdata %>%
  filter(Quarter == "Q4.2019") %>%
  select(Country, Qpopulation_th)

# Direkt rGDP_per_capita verwenden (keine nachträgliche Division durch Bevölkerung)
sum_outcomes_pc <- qdata %>%
  mutate(qtr = zoo::as.yearqtr(Quarter, format = "Q%q.%Y")) %>%
  filter(qtr >= start_q, qtr <= end_q) %>%
  group_by(Country) %>%
  summarise(
    gdp_per_capita = sum(rGDP_per_capita, na.rm = TRUE),   # Summe pro Kopf über 2020Q1–2022Q4
    deaths_sum     = sum(excess.deaths_a, na.rm = TRUE),   # Summe Excess-Deaths
    .groups = "drop"
  ) %>%
  left_join(pop_base, by = "Country") %>%
  mutate(
    excess_deaths_per_100k = deaths_sum * 100 / Qpopulation_th
  ) %>%
  select(Country, gdp_per_capita, deaths_sum, excess_deaths_per_100k)

sum_outcomes_pc



# ---------- Parameter ----------
start_q <- zoo::as.yearqtr("2020 Q1")
end_q   <- zoo::as.yearqtr("2022 Q4")

# ---------- Datenvorbereitung ----------
qdata_clean <- qdata %>%
  mutate(yq = zoo::as.yearqtr(Quarter, format = "Q%q.%Y"))

# ---------- GDP pro Kopf (direkt rGDP_per_capita verwenden), Q1 2020–Q4 2022 ----------
gdp_metrics <- qdata_clean %>%
  filter(yq >= start_q, yq <= end_q) %>%
  group_by(Country) %>%
  summarise(
    gdp_pc_sum = sum(rGDP_per_capita, na.rm = TRUE),           # kumuliertes reales BIP pro Kopf über den Zeitraum
    n_quarters = sum(!is.na(rGDP_per_capita)),                 # effektive Quartalsanzahl je Land
    gdp_pc_avg = if_else(n_quarters > 0, gdp_pc_sum / n_quarters, NA_real_), # durchschnittlich pro Quartal
    .groups = "drop"
  ) %>%
  arrange(desc(gdp_pc_sum))

# ---------- Ausgabe ----------
cat("\n=== GDP pro Kopf (kumuliert 2020Q1–2022Q4) ===\n")
print(gdp_metrics)

cat("\n=== KOR & CHL ===\n")
print(dplyr::filter(gdp_metrics, Country %in% c("KOR", "CHL")))

#-------------------------------------------------------------------------------

# ---------- Parameter ----------
start_q <- zoo::as.yearqtr("2020 Q1")
end_q   <- zoo::as.yearqtr("2022 Q4")

# ---------- Datenvorbereitung ----------
qdata_clean <- qdata %>%
  mutate(yq = zoo::as.yearqtr(Quarter, format = "Q%q.%Y"))

# ---------- 2019-Q4 Baseline je Land (level + Populationsbasis) ----------
debt_base_2019 <- qdata_clean %>%
  filter(Quarter == "Q4.2019") %>%
  transmute(
    Country,
    Debt_ar_base2019 = Debt_ar,
    pop_base_th      = Qpopulation_th
  )

# ---------- Debt_new = Debt_ar - Debt_ar(Q4.2019); optional pro Kopf mit Pop-Basis Q4.2019 ----------
debt_new_metrics <- qdata_clean %>%
  filter(yq >= start_q, yq <= end_q) %>%
  left_join(debt_base_2019, by = "Country") %>%
  mutate(
    Debt_new    = Debt_ar - Debt_ar_base2019,
    Debt_new_pc = dplyr::if_else(!is.na(pop_base_th) & pop_base_th > 0,
                                 Debt_new / (pop_base_th * 1000), NA_real_)
  ) %>%
  group_by(Country) %>%
  summarise(
    debt_new_sum    = sum(Debt_new, na.rm = TRUE),                 # kumulierte Neuschuld (LC)
    debt_new_avg    = if_else(sum(!is.na(Debt_new)) > 0,
                              mean(Debt_new, na.rm = TRUE), NA_real_),
    debt_new_pc_sum = sum(Debt_new_pc, na.rm = TRUE),              # kumuliert pro Kopf (LC pro Person)
    debt_new_pc_avg = if_else(sum(!is.na(Debt_new_pc)) > 0,
                              mean(Debt_new_pc, na.rm = TRUE), NA_real_),
    n_quarters      = sum(!is.na(Debt_new)),
    .groups = "drop"
  ) %>%
  arrange(desc(debt_new_sum))

# ---------- Ausgabe ----------
cat("\n=== Debt_new (LC) und pro Kopf, kumuliert 2020Q1–2022Q4 ===\n")
print(debt_new_metrics, n = Inf)

cat("\n=== KOR & CHL ===\n")
print(dplyr::filter(debt_new_metrics, Country %in% c("KOR", "CHL")))



# -------------------- 2. Excess Deaths Summe (Q1.2020 - Q4.2022) --------------------
excess_deaths_metrics <- qdata_clean %>%
  filter(yq >= as.yearqtr("2020 Q1") & yq <= as.yearqtr("2022 Q4")) %>%
  group_by(Country) %>%
  summarise(
    # Summe der Excess Deaths
    excess_deaths_total = sum(excess.deaths_a, na.rm = TRUE),
    # Durchschnittliche Population für Per-100k Berechnung
    avg_population_thousands = mean(Qpopulation_th, na.rm = TRUE),
    n_quarters = n(),
    .groups = "drop"
  ) %>%
  mutate(
    # Excess Deaths per 100k (Population ist in Tausend)
    excess_deaths_per_100k = (excess_deaths_total / avg_population_thousands) * 100
  )

# Ausgabe prüfen
cat("\n=== Excess Deaths Metriken für alle Länder ===\n")
print(excess_deaths_metrics)

cat("\n=== Excess Deaths Metriken für KOR & CHL ===\n")
print(excess_deaths_metrics %>% filter(Country %in% c("KOR", "CHL")))


# --- 3) Alles zusammenführen: GDP pro Kopf + Debt pro Kopf + Excess Deaths ---

# Eindeutige Namensgebung der Quartalszählungen
gdp_m  <- gdp_metrics      %>% rename(n_quarters_gdp  = n_quarters)
debt_m <- debt_pc_metrics  %>% rename(n_quarters_debt = n_quarters)

final_metrics <- gdp_m %>%
  left_join(debt_m %>% select(Country, debt_pc_sum, debt_pc_avg, n_quarters_debt), by = "Country") %>%
  left_join(excess_deaths_metrics %>% select(Country, excess_deaths_total, excess_deaths_per_100k), by = "Country") %>%
  select(
    Country,
    # GDP pro Kopf
    gdp_pc_sum, gdp_pc_avg,
    # Debt pro Kopf
    debt_pc_sum, debt_pc_avg,
    # Excess Deaths
    excess_deaths_total, excess_deaths_per_100k,
    # Meta
    n_quarters_gdp, n_quarters_debt
  ) %>%
  arrange(Country)

# --- Ausgabe ---
cat("\n=== FINALE METRIKEN FÜR ALLE LÄNDER ===\n")
print(final_metrics, n = Inf)

cat("\n=== FINALE METRIKEN FÜR KOR & CHL ===\n")
print(final_metrics %>% filter(Country %in% c("KOR", "CHL")))

cat("\n=== ZUSAMMENFASSUNG ===\n")
cat("Anzahl Länder:", nrow(final_metrics), "\n")
cat("Zeitraum: 2020 Q1 - 2022 Q4\n")
cat("\nStatistiken (kumuliertes reales BIP pro Kopf):\n")
print(summary(final_metrics$gdp_pc_sum))
cat("\nStatistiken (kumuliertes Debt pro Kopf):\n")
print(summary(final_metrics$debt_pc_sum))
cat("\nStatistiken (Excess Deaths pro 100k):\n")
print(summary(final_metrics$excess_deaths_per_100k))


###NEXT: F, S AND OUTCOMES CHECKING FOR RELATIONSHIP INTO THE GROUPS AND DESCRIPTIVES ZUSAMMEN
##BUT ONLY WHEN THE SINGLE STANDING DESCRIPTVES ARE FINISHED


##GIVE THE MAIN DESCRIPTIVES OUT IN THE CORRESPONDING GROUP BUT BE CAREFULL, THE GROUP HAS TO BE DETERMINED BEFORE
##THERE ALSO CODE ABOUT THE 3 OUTCOMES TOGETHER
####################################################################
#   PHASE 3: DESCRIPTIVE STATISTICS AND VALIDITY CHECKS
####################################################################

did_vars <- c("qstrict", "qcovid", "qtreatment")
#did_vars <- c("connected", "submarines", "treatment")
did_var<-as.numeric(did_vars)
# DiD variables, before (pre)treatment is introduced
desc_before <- dplyr::filter(qdata, qdata$qcovid==0) 
fBasics::basicStats(desc_before[did_vars]) %>% 
  t() %>% 
  as.data.frame() %>% 
  dplyr::select(Mean, Stdev, Minimum, Maximum, nobs)


# DiD variables, after treatment is introduced
desc_after <- dplyr::filter(qdata, qdata$qcovid==1) 
fBasics::basicStats(desc_after[did_vars]) %>% 
  t() %>% 
  as.data.frame() %>% 
  dplyr::select(Mean, Stdev, Minimum, Maximum, nobs)


# What periods we have - calendar

cro(qdata$Quarter)

# Normalized time periods (to use in regressions)
qdata$time<-qdata$TimeIndex

cro(qdata$time)

cro(qdata$qrel_year)
# Summary statistics for outcomes by treatment group, before and after treatment
# ----------------------------------------------------

outcomes <- dplyr::select(qdata, QReal.GDP.Growth, QPrivate.Consumption,
                          QGov.Cons,QGross.Cap.form,QX,QM, on_per,health_per,
                          nhealth_per, acc_per, of_per, 
                          be_per,gu_per, share_on_mo,	
                          share_nhealth_mo,share_health_mo,
                          share_acc_mo,share_nhealth_on,share_health_on,
                          share_of_mo,share_be_mo,
                          share_gu_mo,share_be_of,share_gu_of,total_intensity_per,
                          total_mo_per, broad_fiscal_gdp_q)


outcomes_names <- colnames(outcomes) 
attach(qdata)
# Define a function estimating the differences in variables across D and T
balance_check.model <- function(x, qdata){
  
  # Observations
  nobs<-nrow(x)
  
  # Conditional means
  mean_d0_before <- mean(x[qdata$qstrict==0 & qdata$qcovid==0], na.rm=TRUE) # filters out missings 
  mean_d1_before <- mean(x[qdata$qstrict==1 & qdata$qcovid==0], na.rm=TRUE)
  mean_d0_after <- mean(x[qdata$qstrict==0 & qdata$qcovid==1], na.rm=TRUE) 
  mean_d1_after <- mean(x[qdata$qstrict==1 & qdata$qcovid==1], na.rm=TRUE)
  
  # Difference in means before treatment
  diff_before <- lm(x[qcovid==0] ~ qdata$qstrict[qcovid==0])
  cov <- vcovHC(diff_before, type = "HC")
  robust.se_before <- sqrt(diag(cov))
  
  # Difference in means after treatment
  diff_after <- lm(x[qcovid==1] ~ qdata$qstrict[qcovid==1])
  cov <- vcovHC(diff_after, type = "HC")
  robust.se_after <- sqrt(diag(cov))
  
  list(mean_d0_before = mean_d0_before, 
       mean_d1_before = mean_d1_before,
       diff_before = diff_before$coefficients[2], 
       robust.se_before = robust.se_before[2], 
       pval_before = 2*pnorm(-abs(diff_before$coefficients[2]/robust.se_before[2])), 
       mean_d0_after = mean_d0_after, 
       mean_d1_after = mean_d1_after,
       diff_after = diff_after$coefficients[2], 
       robust.se_after = robust.se_after[2], 
       pval_after = 2*pnorm(-abs(diff_after$coefficients[2]/robust.se_after[2]))
  )             
}

diff_output <- apply(outcomes, 2,function(x) balance_check.model(x, qdata))

# convert list to table
diff_output<-rbindlist(diff_output)


# add a row with number of observations
n_d0_before <- nrow(qdata[qdata$qstrict==0 & qdata$qcovid==0,])
n_d1_before <- nrow(qdata[qdata$qstrict==1 & qdata$qcovid==0,])
n_d0_after <- nrow(qdata[qdata$qstrict==0 & qdata$qcovid==1,])
n_d1_after <- nrow(qdata[qdata$qstrict==1 & qdata$qcovid==1,])
obs <-c(n_d0_before, n_d1_before, NA, NA, NA, n_d0_after, n_d1_after, NA, NA, NA)

diff_output <- rbind(as.matrix(diff_output), obs)

rownames(diff_output)<- c(outcomes_names, "Observations")
colnames(diff_output)<- c("E(Y|D=0, T=0)", "E(Y|D=1, T=0)", 
                          "Difference", "s.e.", "p-value", 
                          "E(Y|D=0, T=1)", "E(Y|D=1, T=1)", 
                          "Difference", "s.e.", "p-value")

print("Average Outcomes before and after Treatment")
xtable(diff_output, digits=2)
print(diff_output)


latex_code <- xtable(diff_output, digits=2)
print(latex_code, type = "latex")  # f?r LaTeX-Code
sink("descriptivesq.tex")
print(latex_code, type = "latex")
sink()
#-------------------------------------------------------------------------------
#Plot Y,D,B over Time


##plot for Stringency, GDP, Spending (w/o debt und deahts)
# Mittelwerte: 2020–2022 für Stringency & GDP, aber 2020–2021 für total_intensity_per
colnames(fm)
combined_data_2020_2022 <- qdata %>%
  filter(year >= 2020, year <= 2022) %>%
  group_by(Country) %>%
  summarise(
    mean_stringency = mean(StringencyIndex_Average, na.rm = TRUE),
    mean_total_intensity_per = mean(ifelse(year <= 2021, total_intensity_per*100, NA_real_), na.rm = TRUE),
    mean_gdp_lvl_vs_2019q4 = mean(GDP_ln_lvl_vs_2019q4, na.rm = TRUE),
    .groups = "drop"
  )

combined_plot_2020_2022 <- ggplot(combined_data_2020_2022, aes(x = reorder(Country, -mean_stringency))) +
  geom_bar(aes(y = mean_stringency, fill = "Mean Stringency Index (2020–2022)"),
           stat = "identity", alpha = 0.7, width = 0.6) +
  geom_text(aes(y = mean_stringency, label = round(mean_stringency, 1)),
            vjust = -0.3, color = "black", size = 5.5) +
  geom_bar(aes(y = mean_total_intensity_per, fill = "Mean Total Intensity Per (%) (2020–2021)"),
           stat = "identity", alpha = 1, width = 0.3, position = position_nudge(x = -0.15)) +
  geom_bar(aes(y = mean_gdp_lvl_vs_2019q4, fill = "Mean GDP ln level vs 2019Q4 (2020–2022)"),
           stat = "identity", alpha = 1, width = 0.3, position = position_nudge(x = 0.15)) +
  geom_text(aes(y = mean_gdp_lvl_vs_2019q4, label = round(mean_gdp_lvl_vs_2019q4, 2)),
            vjust = 1.5, color = "black", size = 5.5, position = position_nudge(x = 0.30)) +
  scale_y_continuous(
    name = "Index / log units",
    sec.axis = sec_axis(~., name = "Percentage", breaks = seq(-20, 100, by = 20))
  ) +
  scale_fill_manual(
    name = "",
    values = c(
      "Mean Stringency Index (2020–2022)" = "darkgray",
      "Mean Total Intensity Per (%) (2020–2021)" = "#00008B",
      "Mean GDP ln level vs 2019Q4 (2020–2022)" = "orange"
    )
  ) +
  labs(x = "Country") +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

print(combined_plot_2020_2022)
ggsave("combined_plot_2020_2022_ti_2020_2021.pdf", plot = combined_plot_2020_2022, width = 20, height = 12)


#-------------------------------------------------------------------------------
##final plot outcomes

#plot with debt and spending
# --- Kennzahlen vorbereiten (nur: NewDebt und Total Intensity) ---
plot_data <- qdata %>%
  mutate(
    Country = toupper(trimws(Country)),
    # Quartalsordnung
    qnum = as.integer(sub("^Q(\\d)\\..*$", "\\1", Quarter)),
    y    = as.integer(sub("^Q\\d\\.(\\d{4})$", "\\1", Quarter)),
    ord  = y*4 + qnum
  ) %>%
  group_by(Country) %>%
  mutate(
    debt_base_2019q4 = {
      v <- DebtTot_CG_nom[Quarter == "Q4.2019"]
      if (length(v)) v[1] else NA_real_
    },
    ngdp2019_annual  = sum(nGDP[year == 2019], na.rm = TRUE),
    # Laufende Neuverschuldung (kumuliert ggü. Q4.2019) in % des fixen 2019-nGDP
    NewDebt_pct_2019nGDP = if_else(is.finite(ngdp2019_annual) & ngdp2019_annual > 0 &
                                     is.finite(debt_base_2019q4),
                                   100 * (DebtTot_CG_nom - debt_base_2019q4) / ngdp2019_annual,
                                   NA_real_),
    # Total Intensity nur 2020–2021
    TI_per100 = if_else(year %in% 2020:2021, total_intensity_per * 100, NA_real_)
  ) %>%
  ungroup() %>%
  arrange(Country, ord)

# --- Einzelplot-Funktion: nur Total Intensity (Bars) + New Debt (Linie) ---
plot_ti_newdebt <- function(d) {
  d <- d[order(d$ord), , drop = FALSE]
  d$Quarter_loc <- factor(d$Quarter, levels = unique(d$Quarter))
  
  # skalieren für Sekundärachse (pro Land)
  rng_debt <- suppressWarnings(max(abs(d$NewDebt_pct_2019nGDP), na.rm = TRUE))
  rng_ti   <- suppressWarnings(max(d$TI_per100, na.rm = TRUE))
  sf <- if (is.finite(rng_debt) && is.finite(rng_ti) && rng_ti > 0) rng_debt / rng_ti else 1
  
  ggplot(d, aes(x = Quarter_loc)) +
    geom_col(aes(y = TI_per100 * sf, fill = "Total intensity (%)"),
             width = 0.8, alpha = 0.35, na.rm = TRUE) +
    geom_line(aes(y = NewDebt_pct_2019nGDP, color = "New debt (% of 2019 nGDP)", group = 1),
              linewidth = 1.0, na.rm = TRUE) +
    scale_y_continuous(
      name = "New debt (% of 2019 nGDP)",
      sec.axis = sec_axis(~ . / sf, name = "Total intensity (%)")
    ) +
    scale_fill_manual(values = c("Total intensity (%)" = "#1f77b4"), name = "") +
    scale_color_manual(values = c("New debt (% of 2019 nGDP)" = "#d62728"), name = "") +
    labs(x = NULL, title = unique(d$Country)) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major = element_line(colour = "grey80", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5),
      legend.position = "bottom"
    )
}

# --- Plots je Land ---
plots_ti_debt <- lapply(split(plot_data, plot_data$Country), plot_ti_newdebt)

# Beispiel anzeigen
print(plots_ti_debt[[37]])     # oder: print(plots_ti_debt[["FRA"]])

# Optional speichern: ein PDF pro Land
dir.create("plots_ti_newdebt", showWarnings = FALSE)
for (nm in names(plots_ti_debt)) {
  ggsave(file.path("plots_ti_newdebt", paste0(nm, ".pdf")),
         plots_ti_debt[[nm]], width = 10, height = 6)
}

# Optional: ein mehrseitiges PDF mit allen Ländern
pdf("plots_ti_newdebt_all.pdf", width = 10, height = 6)
invisible(lapply(plots_ti_debt, print))
dev.off()



##PLOT of 3 OUTPUTS

# Basen + Endstände mit Todesfällen pro 100k (Bevölkerung = Qpopulation_th in Q4.2019; Einheit: Tausend)
q_end <- qdata %>%
  arrange(Country, TimeIndex) %>%
  group_by(Country) %>%
  mutate(
    nGDP_roll4 = rollapply(nGDP, 4, sum, align = "right", fill = NA_real_),
    rGDP_roll4 = rollapply(rGDP, 4, sum, align = "right", fill = NA_real_)
  ) %>%
  summarise(
    # Basen 2019
    debt_base_2019q4 = {v <- DebtTot_CG_nom[Quarter == "Q4.2019"]; if (length(v)) v[1] else NA_real_},
    ngdp2019_annual  = sum(nGDP[year == 2019], na.rm = TRUE),
    rgdp2019_annual  = sum(rGDP[year == 2019], na.rm = TRUE),
    pop_base_th_2019q4 = {
      vq4 <- Qpopulation_th[Quarter == "Q4.2019"]
      if (length(vq4) && is.finite(vq4[1])) vq4[1] else {
        v2019 <- Qpopulation_th[year == 2019 & !is.na(Qpopulation_th)]
        if (length(v2019)) tail(v2019, 1) else NA_real_
      }
    },
    
    # Debt-Ende in 2022 (Q4.2022 bevorzugt, sonst letztes 2022-Quartal)
    debt_end_2022 = {
      idx <- which(year == 2022 & !is.na(DebtTot_CG_nom))
      if (length(idx)) {
        idx_q4 <- which(Quarter == "Q4.2022")
        idx_use <- if (length(intersect(idx_q4, idx))) intersect(idx_q4, idx)[1] else tail(idx, 1)
        DebtTot_CG_nom[idx_use]
      } else NA_real_
    },
    
    # rGDP (Rolling-4Q) Ende 2022
    rgdp_roll4_end_2022 = {
      idx <- which(year == 2021 & !is.na(rGDP_roll4))
      if (length(idx)) {
        idx_q4 <- which(Quarter == "Q4.2021")
        idx_use <- if (length(intersect(idx_q4, idx))) intersect(idx_q4, idx)[1] else tail(idx, 1)
        rGDP_roll4[idx_use]
      } else NA_real_
    },
    
    # Summe Todesfälle ab 2020
    deaths_sum_2020p = sum(deaths_q[year >= 2020], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # NEW DEBT: Basis = nGDP 2019 (nominal)
    NewDebt_2020_2022_pct_2019nGDP = if_else(
      ngdp2019_annual > 0 & is.finite(debt_base_2019q4) & is.finite(debt_end_2022),
      100 * (debt_end_2022 - debt_base_2019q4) / ngdp2019_annual, NA_real_
    ),
    # NEW GDP: Basis = rGDP 2019 (real)
    NewGDP_2020_2022_pct_2019rGDP = if_else(
      rgdp2019_annual > 0 & is.finite(rgdp_roll4_end_2022),
      100 * (rgdp_roll4_end_2022 - rgdp2019_annual) / rgdp2019_annual, NA_real_
    ),
    # Todesfälle pro 100k, Bevölkerung aus Q4.2019 (Qpopulation_th * 1000 Personen)
    Deaths_per_100k_2020p = if_else(
      is.finite(pop_base_th_2019q4) & pop_base_th_2019q4 > 0,
      100000 * deaths_sum_2020p / (pop_base_th_2019q4 * 1000), NA_real_
    )
  )

# Plot (3 Panels; Todesfälle als pro 100k)
order_countries <- q_end %>% arrange(desc(NewDebt_2020_2022_pct_2019nGDP)) %>% pull(Country)

plot_df <- q_end %>%
  transmute(
    Country = factor(Country, levels = order_countries),
    `New debt 2020–2022 (% of 2019 nGDP)` = NewDebt_2020_2022_pct_2019nGDP,
    `New GDP 2020–2021 (% vs 2019 rGDP)`  = NewGDP_2020_2022_pct_2019rGDP,
    `Total deaths per 100k (since 2020)`  = Deaths_per_100k_2020p
  ) %>%
  pivot_longer(-Country, names_to = "metric", values_to = "value")

p <- ggplot(plot_df, aes(x = Country, y = value, fill = metric)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = ifelse(metric == "Total deaths per 100k (since 2020)",
                               round(value, 1),
                               round(value, 1))),
            vjust = -0.3, size = 3.8) +
  facet_wrap(~ metric, ncol = 1, scales = "free_y") +
  labs(x = "Country", y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_line(colour = "grey80", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p)

ggsave("plot.pdf", plot = p, width = 12, height = 8)

#########
# Zeitraum
start_q <- as.yearqtr("2020 Q1")
end_q   <- as.yearqtr("2022 Q4")

# 1) Baseline-Population Q4.2019
pop_base <- qdata %>%
  filter(Quarter == "Q4.2019") %>%
  select(Country, Qpopulation_th)

# 2) Country-Stats 2020Q1–2022Q4: Sum(deaths_q) und Ø Stringency
country_stats <- qdata %>%
  mutate(qtr = as.yearqtr(Quarter, format = "Q%q.%Y")) %>%
  filter(qtr >= start_q, qtr <= end_q) %>%
  group_by(Country) %>%
  summarise(
    deaths_sum = sum(deaths_q, na.rm = TRUE),
    stringency_avg = mean(StringencyIndex_Average, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(pop_base, by = "Country") %>%
  mutate(
    deaths_norm = deaths_sum / Qpopulation_th  # pro 1'000 (wenn QPopulation_th in Tausend)
  )

# 3) Tertile-Gruppen nach Stringency (Q1=low, Q2=mid, Q3=high)
country_stats <- country_stats %>%
  mutate(
    stringency_tercile_num = dplyr::ntile(stringency_avg, 2),
    stringency_tercile = factor(stringency_tercile_num,
                                levels = c(1,2),
                                labels = c("Q1 (low S)", "Q3 (high S)"))
  )

# 4) deaths_norm pro Gruppe anzeigen (Tabelle)
group_summary <- country_stats %>%
  group_by(stringency_tercile) %>%
  summarise(
    n_countries   = dplyr::n(),
    mean_deaths   = mean(deaths_norm, na.rm = TRUE),
    median_deaths = median(deaths_norm, na.rm = TRUE),
    sd_deaths     = sd(deaths_norm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(stringency_tercile)

group_summary

# 5) Visualisierung: Boxplot deaths_norm je Stringency-Tertile
ggplot(country_stats, aes(x = stringency_tercile, y = deaths_norm)) +
  geom_boxplot(outlier.alpha = 0.4, width = 0.7) +
  geom_jitter(width = 0.08, height = 0, alpha = 0.6, size = 1) +
  labs(x = "Stringency tertiles (country-level average, 2020Q1–2022Q4)",
       y = "Deaths (2020Q1–2022Q4) / Q4.2019 population (thousands)",
       title = "deaths_norm by stringency group") +
  theme_minimal(base_size = 11)

# Optional: Balken der Gruppenmittelwerte
group_summary %>%
  ggplot(aes(x = stringency_tercile, y = mean_deaths)) +
  geom_col() +
  labs(x = "Stringency tertiles", y = "Mean deaths_norm") +
  theme_minimal(base_size = 11)


######SPAAANNNEND



start_q <- as.yearqtr("2020 Q1")
end_q   <- as.yearqtr("2022 Q4")

# 1) Baseline-Population Q4.2019 (robust auf Schreibweise)
pop_base <- qdata %>%
  filter(Quarter == "Q4.2019") %>%
  mutate(Qpop_th = dplyr::coalesce(.data$Qpopulation_th, .data$Qpopulation_th)) %>%
  select(Country, Qpop_th)

# 2) Country-Stats 2020Q1–2022Q4
country_stats <- qdata %>%
  mutate(qtr = as.yearqtr(Quarter, format = "Q%q.%Y")) %>%
  filter(qtr >= start_q, qtr <= end_q) %>%
  group_by(Country) %>%
  summarise(
    deaths_sum              = sum(excess.deaths_a, na.rm = TRUE),
    stringency_avg          = mean(StringencyIndex_Average, na.rm = TRUE),
    fiscal_avg              = mean(broad_fiscal_gdp_sn, na.rm = TRUE),
    mean_QReal_GDP_Growth   = mean(`QReal.GDP.Growth_gr`, na.rm = TRUE),
    mean_debt_share_2019ngdp= mean(debt_ar_pct_gdp2019, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(pop_base, by = "Country") %>%
  mutate(
    deaths_norm = deaths_sum / Qpop_th
  )

# 3) Gruppenbildung (dein aktueller Median-Split)
country_stats <- country_stats %>%
  mutate(
    stringency_group_num = dplyr::ntile(stringency_avg, 2),
    stringency_group = factor(stringency_group_num,
                              levels = c(1,2),
                              labels = c("Low S (<= median)", "High S (> median)"))
  )

# 4) Gruppentabelle mit zusätzlichen Mitteln
group_summary <- country_stats %>%
  group_by(stringency_group) %>%
  summarise(
    n_countries                  = dplyr::n(),
    mean_deaths_norm             = mean(deaths_sum, na.rm = TRUE),
    median_deaths_norm           = median(deaths_norm, na.rm = TRUE),
    sd_deaths_norm               = sd(deaths_norm, na.rm = TRUE),
    mean_broad_fiscal_gdp_q      = mean(fiscal_avg, na.rm = TRUE),
    mean_debt_share_2019ngdp     = mean(mean_debt_share_2019ngdp, na.rm = TRUE),
    mean_QReal_GDP_Growth        = mean(mean_QReal_GDP_Growth, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(stringency_group)

print(group_summary, n = Inf, width = Inf)



# Voraussetzung: country_stats enthält je Land:
#   Country, deaths_norm, stringency_avg, fiscal_avg,
#   mean_debt_share_2019ngdp, mean_QReal_GDP_Growth

# 1) Median-Splits
m_stringency <- median(country_stats$stringency_avg, na.rm = TRUE)
m_fiscal     <- median(country_stats$fiscal_avg,     na.rm = TRUE)

cs2 <- country_stats %>%
  mutate(
    S_group = if_else(stringency_avg >  m_stringency, "High S",
                      if_else(stringency_avg <= m_stringency, "Low S", NA_character_)),
    F_group = if_else(fiscal_avg     >  m_fiscal,     "High F",
                      if_else(fiscal_avg     <= m_fiscal,     "Low F", NA_character_)),
    SF_group = factor(paste(S_group, "x", F_group),
                      levels = c("High S x High F","High S x Low F",
                                 "Low S x High F","Low S x Low F"))
  )

# 2) Gruppen-Tabelle (4 Felder)
group_summary_2x2 <- cs2 %>%
  group_by(SF_group) %>%
  summarise(
    n_countries              = n(),
    mean_deaths_norm         = mean(deaths_norm, na.rm = TRUE),
    median_deaths_norm       = median(deaths_norm, na.rm = TRUE),
    sd_deaths_norm           = sd(deaths_norm, na.rm = TRUE),
    mean_broad_fiscal_gdp_q  = mean(fiscal_avg, na.rm = TRUE),
    mean_debt_share_2019ngdp = mean(mean_debt_share_2019ngdp, na.rm = TRUE),
    mean_QReal_GDP_Growth    = mean(mean_QReal_GDP_Growth, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(SF_group)

print(group_summary_2x2, n = Inf, width = Inf)



# Datengrundlage: cs2 (eine Zeile je Land) mit:
# deaths_norm, SF_group ∈ {High S x High F, High S x Low F, Low S x High F, Low S x Low F}

# 1) Vergleich über 4 Gruppen: Boxplot + Punkte (empfohlen)
ggplot(cs2, aes(x = SF_group, y = deaths_norm)) +
  geom_boxplot(outlier.alpha = 0.35, width = 0.6) +
  geom_jitter(width = 0.08, height = 0, size = 1, alpha = 0.6) +
  labs(x = NULL, y = "Deaths (2020Q1–2022Q4) / Q4.2019 pop (thousands)",
       title = "deaths_norm by Stringency×Fiscal (2×2)") +
  theme_minimal(base_size = 11)

# 2) Alternative: Mittelwerte mit 95%-CI (klarer Gruppenvergleich)
sum_agg <- cs2 %>%
  group_by(SF_group) %>%
  summarise(
    n  = n(),
    mean = mean(deaths_norm, na.rm = TRUE),
    sd   = sd(deaths_norm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(se = sd / sqrt(n),
         lo = mean - 1.96 * se,
         hi = mean + 1.96 * se,
         SF_group = reorder(SF_group, mean))

ggplot(sum_agg, aes(x = SF_group, y = mean)) +
  geom_pointrange(aes(ymin = lo, ymax = hi)) +
  labs(x = NULL, y = "Mean deaths_norm ± 95% CI",
       title = "Mean comparison across 4 groups") +
  theme_minimal(base_size = 11)


# cs2: eine Zeile je Land mit Spalten
#   SF_group, deaths_norm, mean_debt_share_2019ngdp, mean_QReal_GDP_Growth


# 1) Scale deaths by 1/10 before pivot
metrics_long <- cs2 %>%
  mutate(deaths_norm = deaths_norm / 10) %>%        # <-- scaled
  select(SF_group,
         deaths_norm,
         mean_debt_share_2019ngdp,
         mean_QReal_GDP_Growth) %>%
  rename(
    `Deaths (norm.)`         = deaths_norm,
    `Debt share 2019 nGDP`   = mean_debt_share_2019ngdp,
    `Real GDP growth (avg.)` = mean_QReal_GDP_Growth
  ) %>%
  pivot_longer(-SF_group, names_to = "Metric", values_to = "Value")

# 2) Group means + 95% CI
sum_agg <- metrics_long %>%
  group_by(SF_group, Metric) %>%
  summarise(
    n    = n(),
    mean = mean(Value, na.rm = TRUE),
    sd   = sd(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    se = sd / sqrt(n),
    lo = mean - 1.96 * se,
    hi = mean + 1.96 * se,
    SF_group = factor(SF_group,
                      levels = c("High S x High F","High S x Low F",
                                 "Low S x High F","Low S x Low F"))
  )

# 3) Plot
ggplot(sum_agg, aes(x = SF_group, y = mean)) +
  geom_col(width = 0.65) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2) +
  facet_wrap(~ Metric, scales = "free_y") +
  coord_flip() +
  labs(x = NULL, y = "Group mean (scaled; deaths ÷ 100)",
       title = "Outcomes by Stringency × Fiscal (2×2 groups)") +
  theme_minimal(base_size = 11)

library(dplyr)

# Optional: feste Reihenfolge der 4 Gruppen
lvl <- c("High S x High F","High S x Low F","Low S x High F","Low S x Low F")
cs2 <- cs2 %>% mutate(SF_group = factor(SF_group, levels = lvl))

# 1) Tabelle: je Gruppe Länderzahl + kommagetrennte Länderliste
by_group_tbl <- cs2 %>%
  group_by(SF_group) %>%
  summarise(
    n_countries = n(),
    countries   = paste(sort(Country), collapse = ", "),
    .groups = "drop"
  )

print(by_group_tbl, n = Inf, width = Inf)

# 2) Als Named-List (Vektor je Gruppe)
by_group_list <- split(sort(cs2$Country), cs2$SF_group)
# str(by_group_list)  # optional inspizieren


cs2 <- cs2 %>% dplyr::filter(Country != "ISL")
qdata <- qdata %>% dplyr::filter(Country != "ISL")
# --- Libraries ---


# --- Parameters ---
start_q   <- as.yearqtr("2020 Q1")
end_q     <- as.yearqtr("2022 Q4")
ref_pop_q <- as.yearqtr("2019 Q4")

# ===================== 1) Harmonize =====================
qdata_std <- qdata %>%
  mutate(
    Country = as.character(Country),
    Quarter = str_replace_all(as.character(Quarter), "\\s+", ""),
    qtr     = as.yearqtr(Quarter, format = "Q%q.%Y")
  )

# ===================== 2) Baseline population Q4.2019 =====================
pop_base <- qdata_std %>%
  filter(qtr == ref_pop_q) %>%
  mutate(Qpop_th = dplyr::coalesce(.data$Qpopulation_th, .data$Qpopulation_th)) %>%
  select(Country, Qpop_th)

# ===================== 3) Aggregations 2020Q1–2022Q4 per country =====================
agg_2020_2022 <- qdata_std %>%
  filter(qtr >= start_q, qtr <= end_q) %>%
  group_by(Country) %>%
  summarise(
    deaths_sum      = sum(deaths_q, na.rm = TRUE),
    stringency_avg  = mean(StringencyIndex_Average, na.rm = TRUE),
    fiscal_avg      = mean(broad_fiscal_gdp_q, na.rm = TRUE),
    mean_debt_share_2019ngdp = mean(debt_share_2019gdp, na.rm = TRUE),
    .groups = "drop"
  )

# ===================== 4) rGDP: DIFF (2022Q4 − 2020Q1), then per capita =====================
rgdp_points <- qdata_std %>%
  filter(qtr %in% c(as.yearqtr("2020 Q1"), as.yearqtr("2022 Q4"))) %>%
  select(Country, qtr, rGDP) %>%
  mutate(q_label = ifelse(qtr == as.yearqtr("2020 Q1"), "rGDP_2020Q1", "rGDP_2022Q4")) %>%
  select(-qtr) %>%
  pivot_wider(names_from = q_label, values_from = rGDP) %>%
  mutate(rGDP_diff_2020Q1_2022Q4 = rGDP_2022Q4 - rGDP_2020Q1)

rgdp_diff_pc <- rgdp_points %>%
  left_join(pop_base, by = "Country") %>%
  mutate(
    rGDP_diff_pc_2020Q1_2022Q4 = rGDP_diff_2020Q1_2022Q4 / Qpop_th   # per 1,000 persons if Qpop_th is in thousands
  ) %>%
  select(Country, rGDP_diff_pc_2020Q1_2022Q4)

# ===================== 5) Country panel (cs2) =====================
cs2 <- agg_2020_2022 %>%
  left_join(pop_base, by = "Country") %>%
  mutate(deaths_norm = deaths_sum / Qpop_th) %>%         # per 1,000 persons
  left_join(rgdp_diff_pc, by = "Country")

# Median splits → 2×2 groups
m_stringency <- median(cs2$stringency_avg, na.rm = TRUE)
m_fiscal     <- median(cs2$fiscal_avg,     na.rm = TRUE)

cs2 <- cs2 %>%
  mutate(
    S_group  = if_else(stringency_avg > m_stringency, "High S", "Low S"),
    F_group  = if_else(fiscal_avg     > m_fiscal,     "High F", "Low F"),
    SF_group = factor(paste(S_group, "x", F_group),
                      levels = c("High S x High F","High S x Low F",
                                 "Low S x High F","Low S x Low F"))
  )

# ===================== 6) Metrics long, scaling, group means + CI =====================
metrics_long <- cs2 %>%
  mutate(deaths_norm = deaths_norm / 100) %>% 
  mutate(rGDP_diff_pc_2020Q1_2022Q4 = rGDP_diff_pc_2020Q1_2022Q4 / 10) %>%# scale deaths for comparability
  select(SF_group,
         deaths_norm,
         mean_debt_share_2019ngdp,
         rGDP_diff_pc_2020Q1_2022Q4) %>%
  rename(
    `Deaths (norm.)`                 = deaths_norm,
    `Debt share 2019 nGDP`           = mean_debt_share_2019ngdp,
    `Δ rGDP per capita (2020Q1→2022Q4)` = rGDP_diff_pc_2020Q1_2022Q4
  ) %>%
  pivot_longer(-SF_group, names_to = "Metric", values_to = "Value")

sum_agg <- metrics_long %>%
  group_by(SF_group, Metric) %>%
  summarise(
    n    = n(),
    mean = mean(Value, na.rm = TRUE),
    sd   = sd(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    se = sd / sqrt(n),
    lo = mean - 1.96 * se,
    hi = mean + 1.96 * se,
    SF_group = factor(SF_group,
                      levels = c("High S x High F","High S x Low F",
                                 "Low S x High F","Low S x Low F"))
  )

# ===================== 7) Numbers =====================
print(
  sum_agg %>%
    arrange(Metric, SF_group) %>%
    mutate(across(c(mean, lo, hi), ~round(.x, 3))),
  n = Inf, width = Inf
)

# Optional: countries per group
by_group_tbl <- cs2 %>%
  group_by(SF_group) %>%
  summarise(n_countries = n(),
            countries = paste(sort(Country), collapse = ", "),
            .groups = "drop")
print(by_group_tbl, n = Inf, width = Inf)

# ===================== 8) Plot with numbers =====================
sum_agg_lab <- sum_agg %>%
  mutate(
    mean_lab = ifelse(is.finite(mean), format(round(mean, 3), nsmall = 3), NA_character_),
    n_lab    = paste0("n=", n)
  )

ggplot(sum_agg_lab, aes(x = SF_group, y = mean)) +
  geom_col(width = 0.65) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2) +
  geom_text(aes(label = mean_lab), hjust = -1, vjust = 1.5, size = 3.2) +
  geom_text(aes(label = n_lab, y = 0), hjust = 0, vjust = 1.4, size = 2.8) +
  facet_wrap(~ Metric, scales = "free_y") +
  coord_flip() +
  expand_limits(y = max(sum_agg_lab$hi, na.rm = TRUE) * 1.08) +
  labs(
    x = NULL,
    y = "Group mean (±95% CI)\n[Deaths scaled by ÷100; Δ rGDP per capita uses Q4.2019 pop]",
    title = "Outcomes by Stringency × Fiscal (2×2 groups)"
  ) +
  theme_minimal(base_size = 11)


cs2 <- cs2 %>% left_join(ngdp_diff_pc, by = "Country")





#-------------------------------------------------------------------------------
##Get the Stringency Based on Target

colnames(qdata)

# ---- Annahmen an den Datensatz ----
# ox_q: data.frame im Quartalsformat mit Spalten:
# country_code (ISO3, z.B. "FRA","ESP"), quarter (z.B. "2020Q1"),
# sowie numerische C1,...,C8 (Quartalsmittel oder -werte).
# Falls deine Spaltentitel anders heißen (z.B. C1_School), passe die Vektoren unten an.

# 1) Mapping und Max-Level der OxCGRT C-Variablen
hh_vars    <- c("C1M_School.closing","C3M_Cancel.public.events","C4M_Restrictions.on.gatherings","C6M_Stay.at.home.requirements")
firm_vars  <- c("C2M_Workplace.closing","C5M_Close.public.transport","C7M_Restrictions.on.internal.movement","C8EV_International.travel.controls")

max_levels <- c(
  C1M_School.closing = 3,  # School closing
  C2M_Workplace.closing = 3,  # Workplace closing
  C3M_Cancel.public.events = 2,  # Cancel public events
  C4M_Restrictions.on.gatherings = 4,  # Restrictions on gatherings
  C5M_Close.public.transport = 2,  # Close public transport
  C6M_Stay.at.home.requirements = 3,  # Stay-at-home
  C7M_Restrictions.on.internal.movement = 2,  # Restrictions on internal movement
  C8EV_International.travel.controls = 4   # International travel controls
)

# 2) Auf [0,1] skalieren und arithmetische Mittel für HH & Firms bilden
S_targets <- qdata %>%
  filter(Country %in% c("FRA","ESP")) %>%
  # jede Ck durch ihr zulässiges Maximum teilen
  mutate(across(all_of(names(max_levels)),
                ~ .x / max_levels[cur_column()],
                .names = "{.col}_u")) %>%
  # Target-Indizes als einfaches arithm. Mittel (NA's ignorieren)
  mutate(
    S_HH    = rowMeans(across(all_of(paste0(hh_vars,   "_u"))), na.rm = TRUE),
    S_Firms = rowMeans(across(all_of(paste0(firm_vars, "_u"))), na.rm = TRUE),
    # optional zusätzlich in Prozentpunkten (0–100)
    S_HH_pct    = 100 * S_HH,
    S_Firms_pct = 100 * S_Firms
  ) %>%
  select(Country, Quarter, S_HH, S_Firms, S_HH_pct, S_Firms_pct)

print(S_targets, n="inf")

##Take out F and split it up into A, O^G, O^D and with Targets 




#------------------------------------------------------------------------------
#################################################################################

##Plot real GDP growth w/o distinction between control and treatment

# Berechnen des mittleren Real GDP Growth ohne Unterscheidung nach qstrict
common_trends <- qdata %>%
  filter(TimeIndex >= 1 & TimeIndex <= 40) %>%
  group_by(TimeIndex) %>%
  summarise(mean = mean(GDP_ln_yoy, na.rm = TRUE))

# Erstellen des Plots
qrealgdp <- ggplot(data = common_trends, 
                   aes(x = TimeIndex, y = mean)) + 
  geom_line(size = 1, color = "black") +
  geom_point(size = 2.5, color = "black") +
  geom_vline(xintercept = 20, linetype = "dashed", color = "black", size = 1) +
  annotate("text", x = 20, y = Inf, label = "Emergence of Covid", vjust = 2.5, hjust = 1.2, size=8) +
  scale_x_continuous(breaks = seq(1, 40, by = 1)) +
  labs(title = NULL,
       x = "Quarter (1= Q1.2017)",
       y = "Mean Real GDP Growth Rate in %",
       caption = NULL) +
  theme_bw(base_size = 10) +
  theme(panel.grid.major = element_line(colour = "#808080", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        legend.position = "top")

# Plot anzeigen
print(qrealgdp)

# Plot als PDF speichern
ggsave("qrealgdpwo.pdf", plot = qrealgdp, width = 10, height = 6)





filtered_data <- qdata %>%
  filter(Country %in% c("USA", "CHE", "DEU", "JPN", "CRI", "SWE", "AUS", "ISL") & TimeIndex >= 19)

# Create the line plot
stringency_plot <- ggplot(filtered_data, aes(x = TimeIndex, y = StringencyIndex_Average, color = Country)) + 
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(min(filtered_data$TimeIndex), max(filtered_data$TimeIndex), by = 1)) +
  labs(x = "TimeIndex (20=Q1.2020)",
       y = "Stringency Index Average",
       color = "Country") +
  theme_classic(base_size = 14) +
  theme(panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        legend.position = "right",
        panel.border = element_rect(colour = "black", fill = NA, size = 1))

# Show the plot
print(stringency_plot)

# Save the plot as PDF
ggsave("stringency_plot.pdf", plot = stringency_plot, width = 11, height = 7)



filtered_data <- qdata %>%
  filter(Country %in% c("USA", "CHE", "DEU", "JPN", "CRI", "SWE", "AUS", "ISL") & TimeIndex >= 19 & TimeIndex <= 40)

# Create the line plot
stringency_plot <- ggplot(filtered_data, aes(x = TimeIndex, y = total_intensity_per, color = Country)) + 
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(19, 40, by = 1)) +
  labs(x = "TimeIndex (12=Q1.2020)",
       y = "Spending and Forgone Revenues as % of pre-COVID GDP",
       color = "Country") +
  theme_classic(base_size = 14) +
  theme(panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        legend.position = "right",
        panel.border = element_rect(colour = "black", fill = NA, size = 1))

# Show the plot
print(stringency_plot)

# Save the plot as PDF
ggsave("spending_plot.pdf", plot = stringency_plot, width = 11, height = 7)




# Filter the data for the specified countries and TimeIndex >= 12
filtered_data1 <- qdata %>%
  filter(Country %in% c("USA", "CHE", "DEU", "JPN", "CRI", "SWE", "AUS", "ISL", "FRA", "CAN") & TimeIndex >= 19)

# Create the first line plot
stringency_plot <- ggplot(filtered_data1, aes(x = TimeIndex, y = StringencyIndex_Average, color = Country)) + 
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(min(filtered_data1$TimeIndex), max(filtered_data1$TimeIndex), by = 1)) +
  labs(x = "TimeIndex (20=Q1.2020)",
       y = "Stringency Index Average",
       color = "") +
  theme_classic(base_size = 14) +
  theme(panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        legend.position = "bottom",
        panel.border = element_rect(colour = "black", fill = NA, size = 1))

# Filter the data for the specified countries and TimeIndex between 12 and 20
filtered_data2 <- qdata %>%
  filter(Country %in% c("USA", "CHE", "DEU", "JPN", "CRI", "SWE", "AUS", "ISL", "FRA", "CAN") & TimeIndex >= 19 & TimeIndex <= 40)

# Create the second line plot
spending_plot <- ggplot(filtered_data2, aes(x = TimeIndex, y = total_intensity_per, color = Country)) + 
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(12, 20, by = 1)) +
  labs(x = "TimeIndex (20=Q1.2020)",
       y = "Spending and Forgone Revenues as % of pre-COVID GDP",
       color = "") +
  theme_classic(base_size = 14) +
  theme(panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        legend.position = "bottom",
        panel.border = element_rect(colour = "black", fill = NA, size = 1))

# Combine the plots and add a shared legend
combined_plot <- stringency_plot + spending_plot + 
  plot_layout(ncol = 2, guides = "collect") & theme(legend.position = 'bottom')

# Show the combined plot
print(combined_plot)

# Save the combined plot as PDF
ggsave("combined_plot.pdf", plot = combined_plot, width = 16, height = 8)



##with the distinction between treatment and control
table(subset(qdata, TimeIndex >= 4 & TimeIndex <= 40)$qstrict)

common_trends <- qdata %>% 
  filter(TimeIndex >= 5 & TimeIndex <= 40) %>%
  group_by(TimeIndex, qstrict) %>% 
  summarise(mean = mean(new_borrowing_pct2019_pos, na.rm = TRUE))

new_b

# Stellen Sie sicher, dass qstrict als Faktor vor der Plot-Erstellung definiert ist
common_trends$qstrict <- as.factor(common_trends$qstrict)

# Korrekter Aufruf von ggplot mit TimeIndex als x-Achse
ggplot(data = common_trends, 
       aes(x = TimeIndex, y = mean, 
           group = qstrict, color = qstrict)) + 
  geom_line() +
  scale_color_manual(values = c("0" = "blue", "1" = "red")) +
  geom_vline(xintercept = 12, linetype="dashed") +
  scale_x_continuous(breaks = seq(4, 40, by = 1)) +
  theme_bw(base_size = 20)



#  plot code
qrealgdp <- ggplot(data = common_trends, 
                   aes(x = TimeIndex, y = mean, 
                       group = qstrict, color = qstrict)) + 
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  geom_vline(xintercept = 20, linetype = "dashed", color = "black", size=0.5) +
  annotate("text", x = 12, y = Inf, label = "Covid", vjust = 2, hjust=1.2) +
  scale_x_continuous(breaks = seq(1, 40, by = 1)) +
  scale_color_manual(values = c("0" = "blue", "1" = "red"),
                     name = "",  # Leerer Name f?r die Legende
                     labels = c("Controlgroup", "Treatmentgroup")) +
  labs(title = NULL,
       x = "Quarter (Index)",
       y = "Mean Real GDP Growth Rate in %",
       caption = NULL) +
  theme_bw(base_size = 10) +
  theme(panel.grid.major = element_line(colour = "#808080", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        legend.position = "top")

# Print to view the plot
print(qrealgdp)

# Save the plot as a PDF
ggsave("qrealgdp.pdf", plot = qrealgdp, width = 10, height = 6)


print(control_countries)

##for each country einzeln
#all treatment
countries <- c("AUS","CAN","CHL","COL","CRI","DEU","ESP","GBR","GRC","ITA","NLD","PRT","TUR","USA", "IRL")

#all control
#countries<- c("AUT", "BEL", "CHE", "CZE", "DNK", "EST", "FIN", "FRA", "HUN", "ISL", "ISR", "JPN", "KOR", "LTU", "LUX", "LVA", "MEX", "NOR", "NZL", "POL", "SVK", "SVN", "SWE")

quarter_label <- function(d) {
  m <- as.integer(format(d, "%m"))
  q <- ((m - 1) %/% 3) + 1
  paste0("Q", q, ".", format(d, "%Y"))
}

plot_df <- qdata %>%
  # Wachstums-Spalte (unabhaengig von Gross-/Kleinschreibung) auf "growth" umbenennen
  rename_with(~"growth", matches("(?i)^QReal\\.GDP\\.growth(\\.x)?$")) %>%
  filter(Country %in% countries) %>%
  mutate(
    # Quarter wie "Q1.2015" nach yearqtr parsen (falls schon yearqtr, einfach uebernehmen)
    yq = if (inherits(Quarter, "yearqtr")) Quarter else zoo::as.yearqtr(Quarter, format = "Q%q.%Y"),
    date = as.Date(yq)  # Quartalsbeginn als Datum
  ) %>%
  filter(yq >= as.yearqtr("2015 Q1"), yq <= as.yearqtr("2024 Q4")) %>%
  arrange(Country, date)

# Variante 1: Alle Laender in einem Plot (Farben)
ggplot(plot_df, aes(x = date, y = growth, color = Country)) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  labs(x = NULL, y = "QReal GDP growth", title = "Reales BIP-Wachstum je Quartal") +
  scale_x_date(date_breaks = "1 year", labels = quarter_label, expand = expansion(mult = c(0.01, 0.02))) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

# Variante 2 (oft uebersichtlicher): Small Multiples je Land

ggplot(plot_df, aes(x = date, y = GDP_ln_yoy, group = Country)) +
  geom_line(na.rm = TRUE) +
  facet_wrap(~ Country, ncol = 5) +
  labs(x = NULL, y = "GDP",
       title = "Reales BIP-Wachstum je Quartal (Q1.2015–Q4.2024)") +
  scale_x_date(date_breaks = "1 year", labels = quarter_label) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(size = 6)     # << kleiner
  )
##remove IRL


##Plot for all
qdata$qstrict<-as.factor(qdata$qstrict)
attach(qdata)
# List of dependent variables
#See above
dependent_varsq<-c("GDP_ln_g_qoq_pc","GDP_ln_g_saar_pc","GDP_ln_g_4q_pc","GDP_ln_g_8q_pc","GDP_ln_yoy_pc",
                   "Gross.fc_ln_g_qoq_pc","Gross.fc_ln_g_saar_pc","Gross.fc_ln_g_4q_pc","Gross.fc_ln_g_8q_pc","Gross.fc_ln_yoy_pc",
                   "X_ln_g_qoq_pc","X_ln_g_saar_pc","X_ln_g_4q_pc","X_ln_g_8q_pc","X_ln_yoy_pc",
                   "M_ln_g_qoq_pc","M_ln_g_saar_pc","M_ln_g_4q_pc","M_ln_g_8q_pc","M_ln_yoy_pc",
                   "Private.c_ln_g_qoq_pc","Private.c_ln_g_saar_pc","Private.c_ln_g_4q_pc","Private.c_ln_g_8q_pc","Private.c_ln_yoy_pc",
                   "Gov.cons_ln_g_qoq_pc","Gov.cons_ln_g_saar_pc","Gov.cons_ln_g_4q_pc","Gov.cons_ln_g_8q_pc","Gov.cons_ln_yoy_pc")

dependent_varsq<-c(
  "GDP_idx_2019q4_100",
  "pop_idx_2019q4_100",
  "GDP_pc_idx_2019q4_100",
  "GDP_pc_ln",
  "GDP_pc_lvl_vs_2019q4"
)


dependent_varsq<-c("QReal.GDP.Growth", "QPrivate.Consumption", "QGov.Cons","QGross.Cap.form","QX","QM","dl_gdp","X_ln","Gov.cons_ln","GDP_ln","Gross.fc_ln","M_ln","Private.c_ln","X_ln_yoy","Gov.cons_ln_yoy","GDP_ln_yoy","Gross.fc_ln_yoy","M_ln_yoy","Private.c_ln_yoy","X","Gov.cons","GDP","Gross.fc","M","Private.c","QX","QM","QPrivate.Consumption","QGov.Cons","QGross.Cap.form","X_ln_g_qoq","X_ln_g_saar","X_ln_g_4q","X_ln_g_8q","Gov.cons_ln_g_qoq","Gov.cons_ln_g_saar","Gov.cons_ln_g_4q","Gov.cons_ln_g_8q",
                   "GDP_ln_g_qoq","GDP_ln_g_saar","GDP_ln_g_4q","GDP_ln_g_8q","Gross.fc_ln_g_qoq","Gross.fc_ln_g_saar","Gross.fc_ln_g_4q","Gross.fc_ln_g_8q","M_ln_g_qoq","M_ln_g_saar","M_ln_g_4q","M_ln_g_8q","Private.c_ln_g_qoq","Private.c_ln_g_saar","Private.c_ln_g_4q","Private.c_ln_g_8q","X_ln_lvl_vs_2019q4","Gov.cons_ln_lvl_vs_2019q4","GDP_ln_lvl_vs_2019q4","Gross.fc_ln_lvl_vs_2019q4","M_ln_lvl_vs_2019q4","Private.c_ln_lvl_vs_2019q4",
                   "X_ln_idx_2019q4_100","Gov.cons_ln_idx_2019q4_100","GDP_ln_idx_2019q4_100","Gross.fc_ln_idx_2019q4_100","M_ln_idx_2019q4_100","Private.c_ln_idx_2019q4_100")


dependent_var_qeco<-c("QReal.GDP.Growth", "QPrivate.Consumption", "QGov.Cons", "GDP_ln_yoy", "X_ln_yoy", "M_ln_yoy", "Gov.cons_ln_yoy", "Gross.fc_ln_yoy", "Private.c_ln_yoy") 
# Function to calculate means for a given variable
calculate_means <- function(var_name, qdata) {
  qdata %>%
    mutate(qstrict = factor(qstrict, levels = c(0,1),
                            labels = c("Kontrolle","Behandelt"))) %>%
    group_by(TimeIndex, qstrict) %>%
    summarise(mean_value = mean(.data[[var_name]], na.rm = TRUE),
              .groups = "drop")
}

create_plot <- function(data_means, var_name) {
  ggplot(data = data_means,
         aes(x = TimeIndex, y = mean_value, group = qstrict, color = qstrict)) +
    geom_line() +
    scale_color_manual(values = c("Kontrolle" = "blue", "Behandelt" = "red"),
                       name = NULL) +
    geom_vline(xintercept = 20, linetype = "dashed") +
    scale_x_continuous(breaks = seq(1, 40, by = 1)) +
    theme_bw(base_size = 15) +
    labs(title = paste("Plot for", var_name), x = "TimeIndex", y = "Mean")
}

plots_list <- lapply(dependent_varsq, function(var) {
  means_data <- calculate_means(var, qdata)
  create_plot(means_data, var)
})


print(plots_list)

# To view a specific plot, you can use:

print(plots_list[[6]])

# Display all plots
do.call(grid.arrange, c(plots_list, ncol = 6, nrow=2)) # Adjust ncol to set the number of columns in the layout



#------------------------------------------------------------------------------
###PLOT FOR THe overview
prep <- qdata %>%
  mutate(
    Year  = as.integer(str_extract(Quarter, "\\d{4}$")),
    Qnum  = as.integer(sub("^Q([1-4]).*", "\\1", Quarter)),
    qdate = as.Date(sprintf("%d-%02d-01", Year, (Qnum-1)*3 + 1)),
    Gross.fc = as.numeric(Gross.fc)
  ) %>%
  arrange(Country, qdate)





sel <- c("AUS","CAN","CHL","COL","CRI","DEU","ESP","GBR","GRC","ITA","NLD","PRT","TUR","USA", "IRL")  

ggplot(filter(prep, Country %in% sel),
       aes(x = qdate, y = Gov.cons_ln_yoy, colour = Country)) +
  geom_line() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = "GDP_ln_yoy", title = "Gross.fc (ausgewaehlte Laender)") +
  theme_minimal()



#-------------------------------------------------------------------------------
####################################################################
#   PHASE 4: DiD effect estimation 
####################################################################

#qdata <- qdata %>% filter(qtime <= 16)


##ESTIMATION GARDENER

dependent_varsq<-c("QReal.GDP.Growth", "QPrivate.Consumption", "QGov.Cons","QGross.Cap.form","QX","QM","GDP_ln_yoy", "X_ln_yoy", "M_ln_yoy", "Gov.cons_ln_yoy", "Gross.fc_ln_yoy", "Private.c_ln_yoy")    

dependent_var_qeco<-c("PopulationVaccinated","QReal.GDP.Growth", "QPrivate.Consumption", "QGov.Cons","QGross.Cap.form","QX","QM","GDP_ln_yoy", "X_ln_yoy", "M_ln_yoy", "Gov.cons_ln_yoy", "Gross.fc_ln_yoy", "Private.c_ln_yoy")  


dependent_var_qeco<-c("QReal.GDP.Growth", "QPrivate.Consumption", "QGov.Cons","QGross.Cap.form","QX","QM","dl_gdp","X_ln","Gov.cons_ln","GDP_ln","Gross.fc_ln","M_ln","Private.c_ln","X_ln_yoy","Gov.cons_ln_yoy","GDP_ln_yoy","Gross.fc_ln_yoy","M_ln_yoy","Private.c_ln_yoy","X","Gov.cons","GDP","Gross.fc","M","Private.c","QX","QM","QPrivate.Consumption","QGov.Cons","QGross.Cap.form","X_ln_g_qoq","X_ln_g_saar","X_ln_g_4q","X_ln_g_8q","Gov.cons_ln_g_qoq","Gov.cons_ln_g_saar","Gov.cons_ln_g_4q","Gov.cons_ln_g_8q",
                      "GDP_ln_g_qoq","GDP_ln_g_saar","GDP_ln_g_4q","GDP_ln_g_8q","Gross.fc_ln_g_qoq","Gross.fc_ln_g_saar","Gross.fc_ln_g_4q","Gross.fc_ln_g_8q","M_ln_g_qoq","M_ln_g_saar","M_ln_g_4q","M_ln_g_8q","Private.c_ln_g_qoq","Private.c_ln_g_saar","Private.c_ln_g_4q","Private.c_ln_g_8q","X_ln_lvl_vs_2019q4","Gov.cons_ln_lvl_vs_2019q4","GDP_ln_lvl_vs_2019q4","Gross.fc_ln_lvl_vs_2019q4","M_ln_lvl_vs_2019q4","Private.c_ln_lvl_vs_2019q4",
                      "X_ln_idx_2019q4_100","Gov.cons_ln_idx_2019q4_100","GDP_ln_idx_2019q4_100","Gross.fc_ln_idx_2019q4_100","M_ln_idx_2019q4_100","Private.c_ln_idx_2019q4_100")

#------------------------------------------------------------------------------
#TEST START

qdata2 <- qdata %>% filter(qtime <= 12)
qdata2 <- qdata2 %>% filter(qtime >= -8)
qdata3 <- filter(qdata2, Country != "TUR")
# Sample 2020–2022 (falls gewünscht)


# Schritt 2: Labels zuweisen (falls noch nicht geschehen)
attr(clean_variables$rGDP_per_capita, "label") <- "Reales BIP pro Kopf (US Dollar)"
attr(clean_variables$rGDP_pc_combined, "label") <- "Kombinierte Wachstumsrate: Vor 2019 qoq, Ab 2019 vs. Quartal 2019"
attr(clean_variables$rGDP_pc_corrected, "label") <- "Bereinigte Wachstumsrate (Irland korrigiert)"
attr(clean_variables$rGDP_pc_vs_2019, "label") <- "Wachstum vs. jeweiliges Quartal 2019 (%)"


#####bootstrap
did3g<- did2s(qdata2, yname= "QReal.GDP.Growth_gr",
              treatment= "qtreatment",
              cluster_var="Country",
              first_stage = ~ broad_fiscal_gdp_sn| Quarter+ Country, 
              second_stage = ~ i(qtreatment, ref= FALSE),
              bootstrap = FALSE,
              n_bootstraps = 10000,
              return_bootstrap = TRUE,
              verbose = TRUE)

summary(did3g, vcov = ~ Country + Quarter)

##VCOV and Clustering think about it

did41 <- did2s(data = qdata3,
               yname = "QReal.GDP.Growth_gr", 
               treatment = "qtreatment",
               cluster_var = "Country",
               first_stage = ~ broad_fiscal_gdp_sn | Quarter + Country, 
               second_stage = ~ i(qrel_year, ref = c(0, Inf)),
               bootstrap = FALSE,
               n_bootstraps = 10000,
               return_bootstrap = TRUE,
               verbose = FALSE)

summary(did41, vcov = ~ Country + Quarter)



fixest::iplot(
  did41, 
  main = "log(Private Consumption)", 
  xlab = "Relative time to treatment", 
  col = "black", ref.line = 0,
  cluster = "Country"
)



boot_mat <- did41  # oder did4q1$bootstrap_estimates, falls korrekt gespeichert

# 2. Namen der Zeitperioden extrahieren
time_labels <- colnames(boot_mat)

# 3. Mittelwerte & Standardabweichungen je Periode berechnen
boot_summary <- data.frame(
  qrel_year = time_labels,
  estimate = apply(boot_mat, 2, mean),
  std.error = apply(boot_mat, 2, sd)
)

# 4. t-Wert und p-Wert berechnen (zweiseitig)
boot_summary$t_value <- boot_summary$estimate / boot_summary$std.error
boot_summary$p_value <- 2 * (1 - pnorm(abs(boot_summary$t_value)))

# 5. Optional: Konfidenzintervalle
boot_summary$conf.low <- boot_summary$estimate - 1.96 * boot_summary$std.error
boot_summary$conf.high <- boot_summary$estimate + 1.96 * boot_summary$std.error

# 6. Ergebnis sortieren nach Zeit
boot_summary$qrel_year <- gsub("qrel_year::", "", boot_summary$qrel_year)
boot_summary$qrel_year <- as.numeric(boot_summary$qrel_year)
boot_summary <- boot_summary[order(boot_summary$qrel_year), ]


boot_summary




##test panel data
qdata_2020plus <- qdata %>%
  mutate(yq = as.yearqtr(as.character(Quarter), format = "Q%q.%Y")) %>%
  filter(
    yq >= as.yearqtr("2020 Q1"),
    yq <= as.yearqtr("2023 Q4")
  ) %>%
  select(-yq) %>%
  droplevels()



qdata$excess.deaths_a

m <- feols(Debt_combined~ StringencyIndex_Average+broad_fiscal_gdp_sn | Country + Quarter, data = qdata_2020plus,
           vcov = ~ Country + Quarter)
summary(m, cluster="Country")


#mit plm
dat <- qdata |> mutate(TimeIndex = as.integer(as.factor(Quarter)))



pm <- plm(debt_ar_pct_gdp2019 ~ StringencyIndex_Average + broad_fiscal_gdp_sn,
          data  = qdata_2020plus,
          index = c("Country", "TimeIndex"),
          model = "within", effect = "twoways")

# robuste (Arellano) SE, nach Country clustern:
coeftest(pm, vcov = vcovHC(pm, method = "arellano", type = "HC1", cluster = "group"))

#TEST END
#---------------------------------------------------------------------------





#####bootstrap
did3g<- did2s(qdata, yname= "DebtTot_CG_nom",
              treatment= "qtreatment",
              cluster_var="Country",
              first_stage = ~ 0 | Quarter+ Country, 
              second_stage = ~ i(qtreatment, ref= FALSE),
              bootstrap = FALSE,
              n_bootstraps = 10000,
              return_bootstrap = TRUE,
              verbose = TRUE)

summary(did3g, cluster="Country")

#-------------------------------------------------------------------------------
###AUSGABE BOOTSTRAP
# Falls Matrix, zuerst in DataFrame umwandeln
boot_df <- as.data.frame(did3g)

# Spaltennamen setzen (je nach Inhalt!)
colnames(boot_df) <- "qtreatment::1"


boot_summary <- boot_df %>%
  summarise(
    estimate = mean(`qtreatment::1`),
    std.error = sd(`qtreatment::1`),
    t_value = estimate / std.error,
    conf.low = quantile(`qtreatment::1`, 0.025),
    conf.high = quantile(`qtreatment::1`, 0.975),
    p_value = 2 * (1 - pnorm(abs(t_value)))
  )

boot_summary

# Zusammenfassung (z.???B. f?r qtreatment::1)

boot_summary <- boot_df %>%
  summarise(
    estimate = median(`qtreatment::1`),
    std.error = sd(`qtreatment::1`),
    t_value = estimate / std.error,
    conf.low = quantile(`qtreatment::1`, 0.025),
    conf.high = quantile(`qtreatment::1`, 0.975),
    p_value = 2 * (1 - pnorm(abs(t_value)))  # aus Normalverteilung
  )

# Resultat anzeigen
boot_summary

##PIPE für VCOV
tbl <- table(qdata$qrel_year, qdata$Country)
c(min = min(tbl), zero = sum(tbl == 0))

#-------------------------------------------------------------------------------
##event-study
GDP_ln_idx_2019q4_100


did41 <- did2s(data = qdata,
               yname = "ConfirmedDeaths", 
               treatment = "qtreatment",
               cluster_var = "Country",
               first_stage = ~ 0 | Quarter + Country, 
               second_stage = ~ i(qrel_year, ref = c(0, Inf)),
               bootstrap = FALSE,
               n_bootstraps = 100,
               return_bootstrap = FALSE,
               verbose = FALSE)

summary(did41)
summary(did41, cluster = "Country")
summary(did41, vcov = "HC3")
summary(did41, vcov = ~ Country) 
summary(did41, cluster = ~ Country + Quarter)

vcov(did41, complete=TRUE)


fixest::iplot(
  did41, 
  main = "log(Private Consumption)", 
  xlab = "Relative time to treatment", 
  col = "black", ref.line = 0,
  cluster = "Country"
)


pdf("did41q.pdf", width = 7, height = 5)  # Breite und H?he in Zoll
did41 <- fixest::iplot(
  did41, 
  main = "Effect on Log(Real GDP Growth)", 
  xlab = "Relative time to treatment", 
  col = "black", ref.line = 0,
  cluster = "Country"
)
dev.off()


#EVENT STUDY TEST BOOTSTRAP

did4q1 <- did2s(data = qdata,
                yname = "QReal.GDP.Growth", 
                treatment = "qtreatment",
                cluster_var = "Country",
                first_stage = ~ 0 | Quarter + Country, 
                second_stage = ~ i(qrel_year, ref = c(0, Inf)),
                bootstrap = TRUE,
                n_bootstraps = 10000,
                return_bootstrap = TRUE,
                verbose = FALSE)


summary(did4q1, cluster = "Country")

# 1. Bootstrap Estimates extrahieren
boot_mat <- did4q1  # oder did4q1$bootstrap_estimates, falls korrekt gespeichert

# 2. Namen der Zeitperioden extrahieren
time_labels <- colnames(boot_mat)

# 3. Mittelwerte & Standardabweichungen je Periode berechnen
boot_summary <- data.frame(
  qrel_year = time_labels,
  estimate = apply(boot_mat, 2, mean),
  std.error = apply(boot_mat, 2, sd)
)

# 4. t-Wert und p-Wert berechnen (zweiseitig)
boot_summary$t_value <- boot_summary$estimate / boot_summary$std.error
boot_summary$p_value <- 2 * (1 - pnorm(abs(boot_summary$t_value)))

# 5. Optional: Konfidenzintervalle
boot_summary$conf.low <- boot_summary$estimate - 1.96 * boot_summary$std.error
boot_summary$conf.high <- boot_summary$estimate + 1.96 * boot_summary$std.error

# 6. Ergebnis sortieren nach Zeit
boot_summary$qrel_year <- gsub("qrel_year::", "", boot_summary$qrel_year)
boot_summary$qrel_year <- as.numeric(boot_summary$qrel_year)
boot_summary <- boot_summary[order(boot_summary$qrel_year), ]


boot_summary

fixest::iplot(
  did4q1, 
  main = "Event study: Staggered treatment", 
  xlab = "Relative time to treatment", 
  col = "steelblue", ref.line = 0
)

# Add the (mean) true effects
true_effects = tapply((df_het$te + df_het$te_dynamic), df_het$rel_year, mean)
true_effects = head(true_effects, -1)
points(-20:20, true_effects, pch = 20, col = "grey60")

# Legend
legend(x=-20, y=3, col = c("steelblue", "grey60"), 
       pch = c(20, 20), 
       legend = c("Two-stage estimate", "True effect"))



##vergleich regulär mit bootstrap
# regular_summary z.???B. aus fixest::feols()
regular_df <- broom::tidy(did41, conf.int = TRUE) %>%
  filter(grepl("qrel_year::", term)) %>%
  mutate(qrel_year = as.numeric(gsub("qrel_year::", "", term))) %>%
  select(qrel_year, estimate, std.error, conf.low, conf.high) %>%
  rename_with(~ paste0(.x, "_reg"), -qrel_year)

# Jetzt: mergen mit bootstrap summary
comparison_df <- left_join(boot_summary, regular_df, by = "qrel_year")

View(comparison_df)



# Variante: getrenntes Zusammenf?hren
boot_df <- comparison_df %>%
  select(qrel_year, estimate, conf.low, conf.high) %>%
  mutate(source = "Bootstrap")

reg_df <- comparison_df %>%
  select(qrel_year, estimate = estimate_reg, conf.low = conf.low_reg, conf.high = conf.high_reg) %>%
  mutate(source = "Regul?r")

# Kombinieren
plot_df <- bind_rows(boot_df, reg_df)

ggplot(plot_df, aes(x = qrel_year, y = estimate, color = source, fill = source)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    title = "Vergleich: Bootstrap vs. Regul?re Sch?tzung (Event Study)",
    x = "Relative Periode (qrel_year)",
    y = "Effektsch?tzung (??^)",
    color = "Methode",
    fill = "Methode"
  ) +
  theme_minimal(base_size = 13)





#-------------------------------------------------------------------------------
##plot für alle state of the art estimators

qdata$CountryID <- as.integer(factor(qdata$Country))

qdata <- qdata %>%
  mutate(TimeIndex = as.integer(TimeIndex)) %>%      # sicherstellen, dass numerisch
  group_by(CountryID) %>%
  mutate(
    g = if (any(qstrict == 1, na.rm = TRUE)) 20L else 0L
  ) %>%
  ungroup()



custom_plot_event_study <- function (out, separate = TRUE, horizon = NULL) {
  estimators = unique(out$estimator)
  levels = c("TWFE", "Borusyak, Jaravel, Spiess (2021)", 
             "Callaway and Sant'Anna (2020)", "Gardner (2021)", 
             "Roth and Sant'Anna (2021)", "Sun and Abraham (2020)")
  levels = levels[levels %in% estimators]
  out$estimator = factor(out$estimator, levels = levels)
  color_scale = c(TWFE = "#374E55", `Gardner (2021)` = "#DF8F44", 
                  `Callaway and Sant'Anna (2020)` = "#00A1D5", 
                  `Sun and Abraham (2020)` = "#B24745", `Roth and Sant'Anna (2021)` = "#79AF97", 
                  `Borusyak, Jaravel, Spiess (2021)` = "#6A6599")
  color_scale = color_scale[names(color_scale) %in% estimators]
  out$ci_lower = out$estimate - 1.96 * out$std.error
  out$ci_upper = out$estimate + 1.96 * out$std.error
  if (separate) 
    position = "identity"
  else position = ggplot2::position_dodge(width = 0.5)
  if (!is.null(horizon)) {
    out = out[out$term >= horizon[1] & out$term <= horizon[2], ]
  }
  y_lims = c(min(out$ci_lower), max(out$ci_upper)) * 1.05
  x_lims = c(min(out$term) - 1, max(out$term) + 1)
  ggplot2::ggplot(data = out, mapping = ggplot2::aes(x = .data$term, 
                                                     y = .data$estimate, color = .data$estimator, ymin = .data$ci_lower, 
                                                     ymax = .data$ci_upper)) + {
                                                       if (separate) 
                                                         ggplot2::facet_wrap(~estimator, scales = "free")
                                                     } + ggplot2::geom_point(position = position) + ggplot2::geom_errorbar(position = position) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +  # Changed this line
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") + 
    ggplot2::labs(y = "Point Estimate and 95% Confidence Interval", 
                  x = "Event Time", color = "Estimator") + 
    {
      if (separate) 
        ggplot2::scale_y_continuous(limits = y_lims)
    } + {
      if (separate) 
        ggplot2::scale_x_continuous(limits = x_lims)
    } + ggplot2::theme_minimal(base_size = 16) + ggplot2::scale_color_manual(values = color_scale) + 
    ggplot2::guides(color = ggplot2::guide_legend(title.position = "top", 
                                                  nrow = 2)) + ggplot2::theme(legend.position = "bottom")
}



data(qdata, package = "did2s")
out = event_study(
  data = qdata, yname = "QReal.GDP.Growth", idname = "CountryID",
  tname = "TimeIndex", gname = "g", estimator = "all"
)



head(out)
out
plot_event_study(out, horizon = c(-19, 40))


####################END######################################################
































