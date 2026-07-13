# =============================================================================
#  REDUCED-FORM ESTIMATION — full state-evolution system
#  -----------------------------------------------------------------------------
#  (1) theta-equation:   theta_{k+1} = rho_theta * (1 - phi_S * S_k) * theta_k
#                                      + eps^theta_k
#      Linearisation:    theta_{k+1} = beta_1 * theta_k + beta_2 * (S_k * theta_k)
#                                      + eps
#      Derivation:       rho_theta = beta_1
#                        phi_S     = -beta_2 / beta_1
#                        SE(phi_S) via delta method.
#
#  (2) d-equation:       d_{k+1}     = delta_theta * theta_k  (recap)
#
#  Sample: weekly, OECD-38 minus those with no theta_hat reporting (35 left),
#          date <= 2021-12-31 (pre-Omicron).
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(lubridate)
  library(fixest); library(modelsummary); library(ggplot2)
})

base <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma"
log_path <- file.path(base, "Files/output/reduced_form_theta_eq.log")
sink(log_path, split = TRUE)

cat(strrep("=", 78), "\n  THETA-equation reduced form — ", format(Sys.time()), "\n",
    strrep("=", 78), "\n", sep = "")

load(file.path(base, "Files/data/processed/dataforanalysis.RData"))

# --- Weekly stringency aggregate (re-built fresh) ----------------------------
oxd_w <- oxd_d %>%
  mutate(Date = as.Date(Date),
         isoyr = isoyear(Date), isowk = isoweek(Date)) %>%
  group_by(Country, isoyr, isowk) %>%
  summarise(S_daily_mean = mean(StringencyIndex_PopWeighted, na.rm = TRUE) / 100,
            .groups = "drop")

# --- Build estimation panel: theta from panel_w + S from oxd aggregate -------
SAMPLE_END <- as.Date("2021-12-31")

wkly <- panel_w %>%
  select(Country, isoyr, isowk, date, theta_hat, p_proj, excess_pm,
         wave_coarse) %>%
  left_join(oxd_w, by = c("Country", "isoyr", "isowk")) %>%
  arrange(Country, date) %>%
  filter(date <= SAMPLE_END) %>%
  group_by(Country) %>%
  mutate(
    theta_lead1 = lead(theta_hat, 1),
    theta_lead2 = lead(theta_hat, 2),
    theta_lead3 = lead(theta_hat, 3),
    theta_lead4 = lead(theta_hat, 4),
    d_lead1     = lead(p_proj, 1),
    St_x_th     = S_daily_mean * theta_hat       # interaction term
  ) %>%
  ungroup()

cat(sprintf("Panel: %d rows, %d countries, %d weeks (<= %s)\n",
            nrow(wkly), n_distinct(wkly$Country),
            n_distinct(wkly$date), SAMPLE_END))

# Helper for delta-method SE on phi_S = -beta_2 / beta_1
phi_S_from_fit <- function(fit, n_b1 = "theta_hat", n_b2 = "St_x_th") {
  b   <- coef(fit)
  V   <- vcov(fit)
  b1  <- b[n_b1]; b2 <- b[n_b2]
  v1  <- V[n_b1, n_b1]; v2 <- V[n_b2, n_b2]; c12 <- V[n_b1, n_b2]
  rho   <- unname(b1)
  phi   <- unname(-b2 / b1)
  # d(-b2/b1) / d b1 =  b2 / b1^2
  # d(-b2/b1) / d b2 = -1 / b1
  g <- c( b2 / b1^2, -1 / b1 )
  V_phi <- as.numeric(t(g) %*% matrix(c(v1, c12, c12, v2), 2, 2) %*% g)
  se_phi <- sqrt(V_phi)
  list(rho = rho, se_rho = sqrt(v1),
       phi = phi, se_phi = se_phi)
}

# -----------------------------------------------------------------------------
# (1) THETA-EQUATION — pooled OLS, country FE, country+wave FE
# -----------------------------------------------------------------------------
theta_pool <- feols(theta_lead1 ~ theta_hat + St_x_th,
                    data = wkly, vcov = ~ Country)

theta_cfe  <- feols(theta_lead1 ~ theta_hat + St_x_th | Country,
                    data = wkly, vcov = ~ Country)

theta_cwfe <- feols(theta_lead1 ~ theta_hat + St_x_th | Country + wave_coarse,
                    data = wkly, vcov = ~ Country)

# Some specs also benefit from forcing no-intercept on the panel level
# (canonical form has no constant); achieved here by Country FE soaking it up.
cat("\n--- (1A) Pooled OLS: theta_{k+1} = beta_1 theta_k + beta_2 S_k theta_k ---\n")
print(summary(theta_pool))
p1 <- phi_S_from_fit(theta_pool)

cat("\n--- (1B) Country FE: theta_{k+1} = beta_1 theta_k + beta_2 S_k theta_k + alpha_i ---\n")
print(summary(theta_cfe))
p2 <- phi_S_from_fit(theta_cfe)

cat("\n--- (1C) Country + wave_coarse FE ---\n")
print(summary(theta_cwfe))
p3 <- phi_S_from_fit(theta_cwfe)

cat("\n--- Derived structural parameters (rho_theta, phi_S) ---\n")
print(tibble(
  spec    = c("Pooled OLS", "Country FE", "Country + wave FE"),
  rho     = c(p1$rho, p2$rho, p3$rho),
  se_rho  = c(p1$se_rho, p2$se_rho, p3$se_rho),
  phi_S   = c(p1$phi, p2$phi, p3$phi),
  se_phi  = c(p1$se_phi, p2$se_phi, p3$se_phi)
), n = Inf)

# -----------------------------------------------------------------------------
# (2) D-EQUATION recap on the same sample (pre-Omicron only)
# -----------------------------------------------------------------------------
cat("\n", strrep("-", 78), "\n  (2) d-equation recap on same sample\n",
    strrep("-", 78), "\n", sep = "")

d_pool <- feols(d_lead1 ~ theta_hat,             data = wkly, vcov = ~ Country)
d_cfe  <- feols(d_lead1 ~ theta_hat | Country,   data = wkly, vcov = ~ Country)
d_cwfe <- feols(d_lead1 ~ theta_hat | Country + wave_coarse,
                data = wkly, vcov = ~ Country)

print(summary(d_pool))
print(summary(d_cfe))
print(summary(d_cwfe))

# -----------------------------------------------------------------------------
# (3) THETA-equation Local Projection at h = 1..4 (CFE)
#     theta_{k+h} = beta_1^(h) theta_k + beta_2^(h) S_k theta_k + alpha_i + eps
#     rho^(h), phi_S^(h) reported as in (1).
# -----------------------------------------------------------------------------
cat("\n", strrep("-", 78), "\n  (3) Theta-equation LP at h = 1..4\n",
    strrep("-", 78), "\n", sep = "")

run_theta_lp <- function(h) {
  dv  <- paste0("theta_lead", h)
  f   <- as.formula(paste(dv, "~ theta_hat + St_x_th | Country"))
  fit <- feols(f, data = wkly, vcov = ~ Country)
  pp  <- phi_S_from_fit(fit)
  tibble(h = h, n = nobs(fit),
         beta_1 = unname(coef(fit)["theta_hat"]),
         se_b1  = unname(sqrt(diag(vcov(fit)))["theta_hat"]),
         beta_2 = unname(coef(fit)["St_x_th"]),
         se_b2  = unname(sqrt(diag(vcov(fit)))["St_x_th"]),
         rho    = pp$rho, se_rho = pp$se_rho,
         phi    = pp$phi, se_phi = pp$se_phi,
         r2_w   = fitstat(fit, "wr2", verbose = FALSE)$wr2)
}
lp_theta <- bind_rows(lapply(1:4, run_theta_lp))
cat("\nLP for theta:\n")
print(lp_theta, n = Inf)

# -----------------------------------------------------------------------------
# (4) Save calibration-ready CSV with structural-parameter summary
# -----------------------------------------------------------------------------
calib <- bind_rows(
  tibble(equation = "theta",
         spec = "Pooled OLS",     rho = p1$rho, se_rho = p1$se_rho,
         phi_S = p1$phi, se_phi = p1$se_phi,
         delta_theta = NA_real_, se_delta = NA_real_),
  tibble(equation = "theta",
         spec = "Country FE",     rho = p2$rho, se_rho = p2$se_rho,
         phi_S = p2$phi, se_phi = p2$se_phi,
         delta_theta = NA_real_, se_delta = NA_real_),
  tibble(equation = "theta",
         spec = "Country+wave",   rho = p3$rho, se_rho = p3$se_rho,
         phi_S = p3$phi, se_phi = p3$se_phi,
         delta_theta = NA_real_, se_delta = NA_real_),
  tibble(equation = "d",
         spec = "Pooled OLS",     rho = NA_real_, se_rho = NA_real_,
         phi_S = NA_real_, se_phi = NA_real_,
         delta_theta = unname(coef(d_pool)["theta_hat"]),
         se_delta    = unname(sqrt(diag(vcov(d_pool)))["theta_hat"])),
  tibble(equation = "d",
         spec = "Country FE",     rho = NA_real_, se_rho = NA_real_,
         phi_S = NA_real_, se_phi = NA_real_,
         delta_theta = unname(coef(d_cfe)["theta_hat"]),
         se_delta    = unname(sqrt(diag(vcov(d_cfe)))["theta_hat"])),
  tibble(equation = "d",
         spec = "Country+wave",   rho = NA_real_, se_rho = NA_real_,
         phi_S = NA_real_, se_phi = NA_real_,
         delta_theta = unname(coef(d_cwfe)["theta_hat"]),
         se_delta    = unname(sqrt(diag(vcov(d_cwfe)))["theta_hat"]))
)

out_csv <- file.path(base, "Files/output/calibration_state_eq.csv")
write.csv(calib, out_csv, row.names = FALSE)
cat(sprintf("\n  Saved calibration summary: %s\n", out_csv))

# -----------------------------------------------------------------------------
# (5) Modelsummary table for the paper
# -----------------------------------------------------------------------------
ms_list <- list(
  "(1A) theta — Pooled"   = theta_pool,
  "(1B) theta — CFE"      = theta_cfe,
  "(1C) theta — CFE+wave" = theta_cwfe,
  "(2A) d — Pooled"       = d_pool,
  "(2B) d — CFE"          = d_cfe,
  "(2C) d — CFE+wave"     = d_cwfe
)
out_tex <- file.path(base, "Files/output/tables/tab_state_eq_reduced.tex")
modelsummary(
  ms_list,
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared", "r.squared.within"),
  coef_map = c(
    "theta_hat"  = "$\\theta_k$  (= $\\rho_\\theta$ in eq.1)",
    "St_x_th"    = "$S_k\\,\\theta_k$  (= $-\\rho_\\theta\\phi_S$)",
    "(Intercept)"= "Constant"
  ),
  output = out_tex,
  title  = "Reduced-form state-evolution: $\\theta_{k+1} = \\rho_\\theta(1-\\phi_S S_k)\\theta_k$ and $d_{k+1} = \\delta_\\theta\\theta_k$"
)
cat(sprintf("\n  Saved LaTeX table: %s\n", out_tex))

cat("\n", strrep("=", 78), "\n  END\n", strrep("=", 78), "\n", sep = "")
sink()
cat("Log:", log_path, "\n")
