# =============================================================================
#  REDUCED-FORM STATE-EVOLUTION SYSTEM  (population-normalised mortality)
#  -----------------------------------------------------------------------------
#  (1) theta-equation:  theta_{k+1} = rho_theta (1 - phi_S S_k) theta_k + eps
#                       (estimated via:  theta_{k+1} = beta_1 theta_k +
#                                                       beta_2 S_k theta_k + alpha_i)
#                       rho_theta = beta_1,   phi_S = -beta_2 / beta_1
#
#  (2) d-equation:      d_{k+1}     = delta_theta * theta_k
#                       DV is excess_pm  (excess deaths per million per week,
#                       population-normalised; replaces p_proj which is the
#                       country-relative p-score).
#
#  Sample: 38 OECD countries, weekly panel_w, date <= 2021-12-31 (pre-Omicron).
#          35 countries enter (CRI, JPN, TUR have no theta_hat).
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(lubridate)
  library(fixest); library(modelsummary)
})

base <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma"
log_path <- file.path(base, "Files/output/reduced_form_state_eq_excpm.log")
sink(log_path, split = TRUE)
cat(strrep("=", 78), "\n  STATE-EVOLUTION REDUCED FORM (DV = excess_pm) — ",
    format(Sys.time()), "\n", strrep("=", 78), "\n", sep = "")

load(file.path(base, "Files/data/processed/dataforanalysis.RData"))

# --- Weekly stringency aggregate (S in [0,1]) -------------------------------
oxd_w <- oxd_d %>%
  mutate(Date = as.Date(Date),
         isoyr = isoyear(Date), isowk = isoweek(Date)) %>%
  group_by(Country, isoyr, isowk) %>%
  summarise(S_daily_mean = mean(StringencyIndex_PopWeighted, na.rm = TRUE) / 100,
            .groups = "drop")

SAMPLE_END <- as.Date("2021-12-31")

wkly <- panel_w %>%
  select(Country, isoyr, isowk, date, theta_hat,
         p_proj, excess, excess_pm, deaths_confirmed_pm, wave_coarse) %>%
  left_join(oxd_w, by = c("Country", "isoyr", "isowk")) %>%
  arrange(Country, date) %>%
  filter(date <= SAMPLE_END) %>%
  group_by(Country) %>%
  mutate(
    theta_lead1 = lead(theta_hat, 1),
    theta_lead2 = lead(theta_hat, 2),
    theta_lead3 = lead(theta_hat, 3),
    theta_lead4 = lead(theta_hat, 4),
    # Population-normalised mortality leads
    d_lead1_excpm   = lead(excess_pm, 1),
    d_lead2_excpm   = lead(excess_pm, 2),
    d_lead3_excpm   = lead(excess_pm, 3),
    d_lead4_excpm   = lead(excess_pm, 4),
    # Same for confirmed-COVID deaths/million (robustness)
    d_lead1_confpm  = lead(deaths_confirmed_pm, 1),
    # p-score retained for comparison
    d_lead1_pproj   = lead(p_proj, 1),
    St_x_th = S_daily_mean * theta_hat
  ) %>%
  ungroup()

cat(sprintf("\nPanel: %d rows, %d countries, %d weeks (<= %s)\n",
            nrow(wkly), n_distinct(wkly$Country),
            n_distinct(wkly$date), SAMPLE_END))

# Range check on the new DV
cat("\nDV summary stats (DV in regression sample, after NA filter on theta_hat):\n")
print(wkly %>% filter(!is.na(theta_hat), !is.na(d_lead1_excpm)) %>%
  summarise(across(c(d_lead1_excpm, d_lead1_pproj, d_lead1_confpm,
                     theta_hat, S_daily_mean),
                   list(N    = ~sum(!is.na(.)),
                        mean = ~mean(., na.rm = TRUE),
                        sd   = ~sd(.,   na.rm = TRUE),
                        min  = ~min(.,  na.rm = TRUE),
                        med  = ~median(., na.rm = TRUE),
                        max  = ~max(.,  na.rm = TRUE)))) %>%
  pivot_longer(everything(), names_to = c("var", ".value"),
               names_sep = "_(?=[^_]+$)"))

# Delta-method helper ---------------------------------------------------------
phi_S_from_fit <- function(fit, n_b1 = "theta_hat", n_b2 = "St_x_th") {
  b <- coef(fit); V <- vcov(fit)
  b1 <- b[n_b1]; b2 <- b[n_b2]
  v1 <- V[n_b1, n_b1]; v2 <- V[n_b2, n_b2]; c12 <- V[n_b1, n_b2]
  g  <- c( b2 / b1^2, -1 / b1 )
  list(rho = unname(b1), se_rho = sqrt(v1),
       phi = unname(-b2 / b1),
       se_phi = sqrt(as.numeric(t(g) %*% matrix(c(v1,c12,c12,v2),2,2) %*% g)))
}

# =============================================================================
# (1) THETA-EQUATION  — pooled / CFE / CFE+wave
#     (re-estimated on the same sample for consistency)
# =============================================================================
cat("\n", strrep("-", 78), "\n  (1) Theta-equation\n", strrep("-", 78), "\n", sep = "")

theta_pool <- feols(theta_lead1 ~ theta_hat + St_x_th,
                    data = wkly, vcov = ~ Country)
theta_cfe  <- feols(theta_lead1 ~ theta_hat + St_x_th | Country,
                    data = wkly, vcov = ~ Country)
theta_cwfe <- feols(theta_lead1 ~ theta_hat + St_x_th | Country + wave_coarse,
                    data = wkly, vcov = ~ Country)

print(summary(theta_pool)); print(summary(theta_cfe)); print(summary(theta_cwfe))

p1 <- phi_S_from_fit(theta_pool)
p2 <- phi_S_from_fit(theta_cfe)
p3 <- phi_S_from_fit(theta_cwfe)

cat("\nStructural parameters (theta equation):\n")
print(tibble(
  spec    = c("Pooled OLS","Country FE","Country + wave FE"),
  rho     = c(p1$rho, p2$rho, p3$rho),
  se_rho  = c(p1$se_rho, p2$se_rho, p3$se_rho),
  phi_S   = c(p1$phi, p2$phi, p3$phi),
  se_phi  = c(p1$se_phi, p2$se_phi, p3$se_phi)
), n = Inf)

# =============================================================================
# (2) D-EQUATION with population-normalised DV: d_lead1_excpm
#     Headline replaces p_proj from the previous run.
# =============================================================================
cat("\n", strrep("-", 78),
    "\n  (2) d-equation  (DV = excess_pm = excess deaths per million per week)\n",
    strrep("-", 78), "\n", sep = "")

d_pool <- feols(d_lead1_excpm ~ theta_hat,
                data = wkly, vcov = ~ Country)
d_cfe  <- feols(d_lead1_excpm ~ theta_hat | Country,
                data = wkly, vcov = ~ Country)
d_cwfe <- feols(d_lead1_excpm ~ theta_hat | Country + wave_coarse,
                data = wkly, vcov = ~ Country)
d_cfeS <- feols(d_lead1_excpm ~ theta_hat + S_daily_mean | Country,
                data = wkly, vcov = ~ Country)
d_cfeQ <- feols(d_lead1_excpm ~ theta_hat + I(theta_hat^2) | Country,
                data = wkly, vcov = ~ Country)

# Robustness: confirmed COVID deaths/million as alternative DV
d_conf <- feols(d_lead1_confpm ~ theta_hat | Country,
                data = wkly, vcov = ~ Country)
# Old DV for context
d_pproj <- feols(d_lead1_pproj ~ theta_hat | Country,
                 data = wkly, vcov = ~ Country)

print(summary(d_pool)); print(summary(d_cfe)); print(summary(d_cwfe))
print(summary(d_cfeS)); print(summary(d_cfeQ))
cat("\n[Robustness] DV = confirmed COVID deaths per million:\n"); print(summary(d_conf))
cat("\n[For comparison] DV = p_proj (the prior run):\n");         print(summary(d_pproj))

# =============================================================================
# (3) Jordà LP on the d-equation with DV = excess_pm
# =============================================================================
cat("\n", strrep("-", 78), "\n  (3) LP for d-equation (DV = excess_pm)\n",
    strrep("-", 78), "\n", sep = "")

lp_d <- bind_rows(lapply(1:4, function(h) {
  dv <- paste0("d_lead", h, "_excpm")
  f  <- as.formula(sprintf("%s ~ theta_hat | Country", dv))
  fit <- feols(f, data = wkly, vcov = ~ Country)
  tibble(h = h, n = nobs(fit),
         delta = unname(coef(fit)["theta_hat"]),
         se    = unname(sqrt(diag(vcov(fit)))["theta_hat"]),
         r2_w  = fitstat(fit, "wr2", verbose = FALSE)$wr2) %>%
    mutate(ci_lo = delta - 1.96 * se, ci_hi = delta + 1.96 * se)
}))
cat("\nLP for d (excess_pm):\n"); print(lp_d, n = Inf)

# =============================================================================
# (4) Updated calibration CSV for MATLAB
# =============================================================================
calib <- bind_rows(
  tibble(equation = "theta", spec = "Pooled OLS",
         rho = p1$rho, se_rho = p1$se_rho,
         phi_S = p1$phi, se_phi = p1$se_phi,
         delta_theta = NA_real_, se_delta = NA_real_,
         d_unit = NA_character_),
  tibble(equation = "theta", spec = "Country FE",
         rho = p2$rho, se_rho = p2$se_rho,
         phi_S = p2$phi, se_phi = p2$se_phi,
         delta_theta = NA_real_, se_delta = NA_real_,
         d_unit = NA_character_),
  tibble(equation = "theta", spec = "Country+wave",
         rho = p3$rho, se_rho = p3$se_rho,
         phi_S = p3$phi, se_phi = p3$se_phi,
         delta_theta = NA_real_, se_delta = NA_real_,
         d_unit = NA_character_),
  tibble(equation = "d", spec = "Pooled OLS",
         rho = NA_real_, se_rho = NA_real_,
         phi_S = NA_real_, se_phi = NA_real_,
         delta_theta = unname(coef(d_pool)["theta_hat"]),
         se_delta    = unname(sqrt(diag(vcov(d_pool)))["theta_hat"]),
         d_unit = "excess_deaths_per_million_per_week"),
  tibble(equation = "d", spec = "Country FE",
         rho = NA_real_, se_rho = NA_real_,
         phi_S = NA_real_, se_phi = NA_real_,
         delta_theta = unname(coef(d_cfe)["theta_hat"]),
         se_delta    = unname(sqrt(diag(vcov(d_cfe)))["theta_hat"]),
         d_unit = "excess_deaths_per_million_per_week"),
  tibble(equation = "d", spec = "Country+wave",
         rho = NA_real_, se_rho = NA_real_,
         phi_S = NA_real_, se_phi = NA_real_,
         delta_theta = unname(coef(d_cwfe)["theta_hat"]),
         se_delta    = unname(sqrt(diag(vcov(d_cwfe)))["theta_hat"]),
         d_unit = "excess_deaths_per_million_per_week")
)

out_csv <- file.path(base, "Files/output/calibration_state_eq_excpm.csv")
write.csv(calib, out_csv, row.names = FALSE)
cat(sprintf("\n  Saved calibration: %s\n", out_csv))

out_tex <- file.path(base, "Files/output/tables/tab_state_eq_excpm.tex")
modelsummary(
  list("(1A) theta — Pooled"  = theta_pool,
       "(1B) theta — CFE"     = theta_cfe,
       "(1C) theta — CFE+wave"= theta_cwfe,
       "(2A) d — Pooled"      = d_pool,
       "(2B) d — CFE"         = d_cfe,
       "(2C) d — CFE+wave"    = d_cwfe),
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared", "r.squared.within"),
  coef_map = c(
    "theta_hat"  = "$\\theta_k$",
    "St_x_th"    = "$S_k\\,\\theta_k$",
    "(Intercept)"= "Constant"),
  output = out_tex,
  title  = paste0("Reduced-form state-evolution (DV = excess deaths / million / week): ",
                  "$\\theta_{k+1} = \\rho_\\theta(1-\\phi_S S_k)\\theta_k$ and ",
                  "$d_{k+1} = \\delta_\\theta\\theta_k$")
)
cat(sprintf("  Saved LaTeX:  %s\n", out_tex))

cat("\n", strrep("=", 78), "\n  END\n", strrep("=", 78), "\n", sep = "")
sink()
cat("Log:", log_path, "\n")
