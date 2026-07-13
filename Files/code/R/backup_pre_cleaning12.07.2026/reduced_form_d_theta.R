# =============================================================================
#  REDUCED-FORM ESTIMATION — Excess mortality on infection prevalence
#  Equation:   d_{i, k+1} = delta_theta * theta_{i, k} + (optional FE / controls)
#
#  Frequency: WEEKLY. The dependent variable (excess mortality, p_proj) and
#  the regressor (theta_hat) are both weekly in this dataset; the OxCGRT
#  stringency series is daily but is included here as a weekly mean / max /
#  SD aggregated from oxd_d.
#
#  Inputs:
#    Files/data/processed/dataforanalysis.RData
#      -> panel_w (weekly): excess mortality + theta_hat + S_mean (38 countries)
#      -> oxd_d   (daily):  full OxCGRT stringency index (38 countries)
#  Output:
#    Files/data/processed/weekly_reduced_form_panel.csv
#    Files/output/reduced_form_d_theta.log
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(fixest)
  library(modelsummary)
})

base <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma"

# Capture both the on-screen text and tables into a log file
log_path <- file.path(base, "Files/output/reduced_form_d_theta.log")
sink(log_path, split = TRUE)
cat(strrep("=", 78), "\n  REDUCED-FORM d_{k+1} = delta_theta * theta_k\n",
    "  ", format(Sys.time()), "\n", strrep("=", 78), "\n", sep = "")

# -----------------------------------------------------------------------------
# (1) Load source datasets
# -----------------------------------------------------------------------------
load(file.path(base, "Files/data/processed/dataforanalysis.RData"))

stopifnot(exists("panel_w"), exists("oxd_d"))

# -----------------------------------------------------------------------------
# (2) Aggregate daily OxCGRT stringency to the same isoweek grid used by
#     panel_w so we get S_mean / S_max / S_sd / n_days as proper weekly stats.
#     panel_w$date is the week-start Monday; isoyear / isoweek align with it.
# -----------------------------------------------------------------------------
oxd_w <- oxd_d %>%
  mutate(
    Date    = as.Date(Date),
    isoyr   = isoyear(Date),
    isowk   = isoweek(Date)
  ) %>%
  group_by(Country, isoyr, isowk) %>%
  summarise(
    S_daily_mean = mean(StringencyIndex_PopWeighted, na.rm = TRUE) / 100,
    S_daily_max  = max(StringencyIndex_PopWeighted,  na.rm = TRUE) / 100,
    S_daily_sd   = sd(StringencyIndex_PopWeighted,   na.rm = TRUE) / 100,
    n_obs_daily  = sum(!is.na(StringencyIndex_PopWeighted)),
    .groups      = "drop"
  )

cat(sprintf("\n  oxd_d   -> %d weekly aggregated rows (38 countries x ~149 weeks)\n",
            nrow(oxd_w)))

# -----------------------------------------------------------------------------
# (3) Build the weekly panel for the reduced form.
#     panel_w supplies: theta_hat (weekly), p_proj (weekly p-score),
#                       excess_pm (weekly excess deaths per million),
#                       cases_pm, deaths_w, wave_coarse, S_mean (its own week mean)
# -----------------------------------------------------------------------------
wkly <- panel_w %>%
  select(Country, isoyr, isowk, date, time, n_days,
         S_mean, S_max,
         p_proj, excess, excess_pm,
         theta_hat, theta_hat_l2, theta_hat_l4,
         theta_predicted, deaths_w, cases_w, deaths_confirmed_pm, cases_pm,
         wave, wave_label, wave_coarse) %>%
  rename(S_panelw_mean = S_mean, S_panelw_max = S_max) %>%
  left_join(oxd_w, by = c("Country", "isoyr", "isowk"))

# -----------------------------------------------------------------------------
# (4) Build the lead of d (week k+1 outcome) and lagged theta (for robustness).
#     Sort by Country + date, then use dplyr::lead() within country.
# -----------------------------------------------------------------------------
wkly <- wkly %>%
  arrange(Country, date) %>%
  group_by(Country) %>%
  mutate(
    d_lead1_pproj    = lead(p_proj,    1),  # main dependent: p-score(k+1)
    d_lead1_excess   = lead(excess,    1),
    d_lead1_excpm    = lead(excess_pm, 1),
    theta_lag1       = lag(theta_hat,  1),
    S_lag1           = lag(S_daily_mean, 1),
    week_in_country  = row_number()
  ) %>%
  ungroup()

# Drop rows where the lead doesn't exist (last observation per country)
n_full <- nrow(wkly)
wkly <- wkly %>% filter(!is.na(d_lead1_pproj), !is.na(theta_hat))
cat(sprintf("\n  Panel built: %d rows kept (of %d after lead/NA filter)\n",
            nrow(wkly), n_full))
cat(sprintf("  Countries:   %d   Weeks:       %d   per country (mean): %.1f\n",
            n_distinct(wkly$Country),
            n_distinct(paste(wkly$isoyr, wkly$isowk)),
            mean(table(wkly$Country))))

# -----------------------------------------------------------------------------
# (5) Variable summaries (for the log)
# -----------------------------------------------------------------------------
cat("\n--- Variable summaries (after filter) ---\n")
key_vars <- c("d_lead1_pproj", "d_lead1_excess", "d_lead1_excpm",
              "theta_hat", "theta_lag1",
              "S_daily_mean", "S_daily_max", "S_panelw_mean")
print(wkly %>% summarise(across(all_of(key_vars),
       list(N=~sum(!is.na(.)), NA_=~sum(is.na(.)),
            mean=~mean(.,na.rm=TRUE), sd=~sd(.,na.rm=TRUE),
            min=~min(.,na.rm=TRUE), max=~max(.,na.rm=TRUE)))) %>%
      pivot_longer(everything(), names_to=c("var",".value"), names_sep="_(?=[^_]+$)"))

# Save the panel CSV for downstream / sharing
out_csv <- file.path(base, "Files/data/processed/weekly_reduced_form_panel.csv")
write.csv(wkly, out_csv, row.names = FALSE)
cat(sprintf("\n  Saved: %s   (%d rows x %d cols)\n", out_csv, nrow(wkly), ncol(wkly)))

# -----------------------------------------------------------------------------
# (6) REDUCED-FORM ESTIMATES
#     d_{i,k+1} = mu_i + delta_theta * theta_{i,k} + (optional) gamma' Z_{i,k}
#                + eps_{i,k+1}
#     Dependent variable = excess-mortality p-score (% above expected deaths).
#     theta is a fraction in [0, 1]: delta_theta is in "pp p-score per unit theta".
#     For an economically interpretable scale, also report delta_theta * 0.01
#     i.e. the marginal effect of a +1 pp infection prevalence on next-week
#     excess mortality.
# -----------------------------------------------------------------------------
cat("\n", strrep("=", 78), "\n",
    "  REDUCED-FORM REGRESSIONS (dependent variable: p_proj_{k+1})\n",
    strrep("=", 78), "\n", sep = "")

m1 <- feols(d_lead1_pproj ~ theta_hat,
            data = wkly, vcov = ~ Country)

m2 <- feols(d_lead1_pproj ~ theta_hat | Country,
            data = wkly, vcov = ~ Country)

m3 <- feols(d_lead1_pproj ~ theta_hat | Country + wave_coarse,
            data = wkly, vcov = ~ Country)

m4 <- feols(d_lead1_pproj ~ theta_hat + S_daily_mean | Country,
            data = wkly, vcov = ~ Country)

m5 <- feols(d_lead1_pproj ~ theta_hat + S_daily_mean +
              I(theta_hat^2) | Country,
            data = wkly, vcov = ~ Country)

# Robustness DV: excess_pm (excess deaths per million)
m6 <- feols(d_lead1_excpm ~ theta_hat | Country,
            data = wkly, vcov = ~ Country)

cat("\n--- (1) Pooled OLS, no FE ---\n");                                print(summary(m1))
cat("\n--- (2) Country FE ---\n");                                       print(summary(m2))
cat("\n--- (3) Country + wave_coarse FE ---\n");                         print(summary(m3))
cat("\n--- (4) Country FE + S_daily_mean control ---\n");                print(summary(m4))
cat("\n--- (5) Country FE + S control + theta^2 (nonlinearity check) ---\n"); print(summary(m5))
cat("\n--- (6) Same as (2) but DV = excess deaths per million ---\n");   print(summary(m6))

# Compact comparison table.
# Each feols object already carries its country-clustered VCOV (set via `vcov = ~ Country`
# at fit time), so modelsummary doesn't need a separate vcov argument.
ms_models <- list(
  "(1) OLS"          = m1,
  "(2) CFE"          = m2,
  "(3) CFE+wave"     = m3,
  "(4) CFE+S"        = m4,
  "(5) CFE+S+theta^2"= m5,
  "(6) DV=excpm"     = m6
)
cat("\n--- Compact comparison (delta_theta across specifications) ---\n")
print(modelsummary(
  ms_models,
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared", "r.squared.within"),
  coef_map = c(
    "theta_hat"        = "theta_k",
    "I(theta_hat^2)"   = "theta_k^2",
    "S_daily_mean"     = "S_k (daily-mean, 0-1)",
    "(Intercept)"      = "Intercept"
  ),
  output = "data.frame"
))

# Save the comparison as a LaTeX table for the paper.
out_tex <- file.path(base, "Files/output/tables/tab_reduced_form_d_theta.tex")
modelsummary(
  ms_models,
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared", "r.squared.within"),
  coef_map = c(
    "theta_hat"        = "$\\theta_k$",
    "I(theta_hat^2)"   = "$\\theta_k^2$",
    "S_daily_mean"     = "$S_k$ (daily-mean, 0--1)",
    "(Intercept)"      = "Intercept"
  ),
  output = out_tex,
  title  = "Reduced-form estimation: $d_{k+1} = \\delta_\\theta\\, \\theta_k$"
)
cat(sprintf("\n  Saved LaTeX table: %s\n", out_tex))

# Report which 3 countries got dropped by the sample filter
all_ctries     <- sort(unique(panel_w$Country))
sample_ctries  <- sort(unique(wkly$Country))
dropped        <- setdiff(all_ctries, sample_ctries)
cat("\n--- Coverage check ---\n")
cat(sprintf("  Countries in panel_w: %d  | in regression sample: %d  | dropped: %d\n",
            length(all_ctries), length(sample_ctries), length(dropped)))
if (length(dropped)) {
  cat("  Dropped:", paste(dropped, collapse = ", "), "\n")
  # Why: lack of p_proj (no excess-mortality reporting) or sparse theta_hat
  diag <- panel_w %>%
    filter(Country %in% dropped) %>%
    group_by(Country) %>%
    summarise(n_rows   = n(),
              n_pproj  = sum(!is.na(p_proj)),
              n_theta  = sum(!is.na(theta_hat)),
              .groups  = "drop")
  print(diag)
}

cat("\n", strrep("=", 78), "\n  END\n", strrep("=", 78), "\n", sep = "")
sink()
cat("Log: ", log_path, "\n")






colnames(panel_w)


library(fixest)
library(dplyr)

## ============================================================================
##  ENDOGENEITY TEST: can the transition equation, applied to CONFIRMED CASES
##  (an INPUT measure, not deaths-in-disguise), explain the wave dynamics —
##  and is the implied phi_S plausible?
##
##  If YES (phi_S in literature range 0.6-0.8, good fit) -> containment has
##  genuine bite, endogenous theta is defensible, Path B may work.
##  If NO (phi_S implausible >1, or poor fit) -> the waves are NOT driven by
##  containment (susceptible depletion / variants), confirming Path A.
## ============================================================================

# Build the transition variables on CASES (not deaths).
# S_mean here is the weekly stringency. Check its scale: if 0-100, the
# interaction below matches your MATLAB (1 - phi_S*S/100). If 0-1, drop /100.
dat <- panel_w %>%
  arrange(Country, date) %>%
  group_by(Country) %>%
  mutate(
    cases_lead1 = lead(cases_pm, 1),          # cases one week ahead (LHS)
    S_x_cases   = (S_mean/100) * cases_pm      # containment interaction (S in 0-100)
  ) %>%
  ungroup() %>%
  filter(is.finite(cases_lead1), is.finite(cases_pm), is.finite(S_x_cases),
         cases_pm > 0)                          # drop zeros/NA for the multiplicative form

## --- 1. CONSTANT rho (baseline) ---------------------------------------------
m_const <- feols(cases_lead1 ~ cases_pm + S_x_cases | Country,
                 data = dat, vcov = ~ Country)
cat("\n=== CASES transition, CONSTANT rho ===\n")
print(summary(m_const))

## --- 2. WAVE-SPECIFIC rho ----------------------------------------------------
m_wave <- feols(cases_lead1 ~ i(wave_coarse, cases_pm) + S_x_cases | Country,
                data = dat, vcov = ~ Country)
cat("\n=== CASES transition, WAVE-SPECIFIC rho ===\n")
print(summary(m_wave))

## --- 3. RECOVER implied phi_S -----------------------------------------------
## Transition: cases' = rho*(1 - phi_S*S/100)*cases = rho*cases - rho*phi_S*S_x_cases
##   coef(cases_pm)  = rho
##   coef(S_x_cases) = -rho*phi_S   ->   phi_S = -coef(S_x_cases)/coef(cases_pm)
b <- coef(m_const)
rho_hat   <- b["cases_pm"]
phi_S_hat <- -b["S_x_cases"] / rho_hat
cat(sprintf("\n=== IMPLIED STRUCTURAL PARAMETERS (constant-rho spec) ===\n"))
cat(sprintf("  rho_hat   = %.4f\n", rho_hat))
cat(sprintf("  phi_S_hat = %.4f   (literature plausible range: 0.6 - 0.8)\n", phi_S_hat))
if (phi_S_hat > 1) {
  cat("  -> phi_S > 1 is IMPLAUSIBLE (more than full transmission suppression).\n")
  cat("     The equation needs an impossible containment effect to fit the\n")
  cat("     wave peaks -> peaks are NOT containment-driven -> supports Path A.\n")
} else if (phi_S_hat >= 0.5 && phi_S_hat <= 0.9) {
  cat("  -> phi_S in a plausible range: containment plausibly drives wave shape.\n")
  cat("     Endogenous theta from cases may be defensible (worth pursuing Path B).\n")
} else {
  cat("  -> phi_S outside the expected band; interpret with caution.\n")
}

## --- 4. FIT CHECK: does the equation track the case waves? -------------------
cat(sprintf("\n  Constant-rho within-R2: %.3f\n", fitstat(m_const, "wr2")$wr2))
cat(sprintf("  Wave-rho   within-R2: %.3f\n", fitstat(m_wave,  "wr2")$wr2))
cat("  (High R2 + plausible phi_S = containment explains cases. Low R2 or\n")
cat("   implausible phi_S = it does not — the waves come from elsewhere.)\n")

## --- 5. TESTING-BIAS caveat (diagnostic only) -------------------------------
## cases_pm is test-biased over time. A crude check: does the case-to-excess
## ratio drift? If cases explain a SHRINKING share of excess deaths early
## (undertesting) and more later, the rho is contaminated by ascertainment.
bias_chk <- panel_w %>%
  filter(is.finite(cases_pm), is.finite(excess_pm), excess_pm > 0) %>%
  group_by(wave_coarse) %>%
  summarise(mean_case_to_excess = mean(cases_pm / excess_pm, na.rm = TRUE),
            .groups = "drop")
cat("\n=== Testing-bias diagnostic: cases_pm / excess_pm by wave ===\n")
cat("(If this ratio rises sharply across waves, confirmed cases are\n")
cat(" increasingly capturing infections — i.e. early undertesting biases rho.)\n")
print(bias_chk)








## ============================================================================
##  OECD-AVERAGE STRINGENCY: weekly series + quarterly aggregate
##  Cross-country mean per week (equal country weight), then quarterly mean.
## ============================================================================

library(dplyr)
library(tidyr)

## --- 1. WEEKLY OECD average: mean across countries within each ISO week -----
## Group by week first, average over countries -> equal weight per country.
S_weekly_oecd <- wkly %>%
  group_by(isoyr, isowk) %>%
  summarise(
    S_oecd_w  = mean(S_daily_mean, na.rm = TRUE),  # OECD cross-country mean
    n_country = sum(!is.na(S_daily_mean)),         # how many countries that week
    .groups = "drop"
  ) %>%
  arrange(isoyr, isowk) %>%
  # quarter label: ceiling(week/13) capped at 4  (ADJUST if your MATLAB
  # quarter convention differs — see note below)
  mutate(
    qtr        = pmin(ceiling(isowk / 13), 4),
    quarter_id = paste0(isoyr, "Q", qtr)
  )

cat("=== WEEKLY OECD-average stringency (with quarter label) ===\n")
print(S_weekly_oecd, n = Inf)

## --- 2. QUARTERLY OECD average -----------------------------------------------
## Two ways to aggregate; they differ and you should pick deliberately:
##  (a) mean of the weekly OECD means  (each week equal weight)
##  (b) mean over all country-weeks in the quarter (each country-week equal)
## (a) matches "average the weekly OECD line"; (b) matches the MATLAB
## C.S_exo construction if that averages country-quarter means. Reporting both.

# (a) mean of weekly OECD means
S_quarterly_a <- S_weekly_oecd %>%
  group_by(quarter_id) %>%
  summarise(S_q_from_weekly = mean(S_oecd_w, na.rm = TRUE),
            n_weeks = n(), .groups = "drop")

# (b) country-week pooled mean within quarter
S_quarterly_b <- wkly %>%
  mutate(qtr = pmin(ceiling(isowk / 13), 4),
         quarter_id = paste0(isoyr, "Q", qtr)) %>%
  group_by(quarter_id) %>%
  summarise(S_q_pooled = mean(S_daily_mean, na.rm = TRUE),
            n_cw = sum(!is.na(S_daily_mean)), .groups = "drop")

S_quarterly <- full_join(S_quarterly_a, S_quarterly_b, by = "quarter_id") %>%
  arrange(quarter_id)

cat("\n=== QUARTERLY OECD-average stringency ===\n")
cat("(a) mean of weekly OECD means | (b) pooled country-week mean\n")
print(S_quarterly, n = Inf)

## --- 3. Optional: export to CSV ---------------------------------------------
write.csv(S_weekly_oecd,  "S_weekly_oecd.csv",    row.names = FALSE)
write.csv(S_quarterly,    "S_quarterly_oecd.csv", row.names = FALSE)
cat("\nWritten: S_weekly_oecd.csv, S_quarterly_oecd.csv\n")

## ---------------------------------------------------------------------------
## NOTE on quarter convention:
##  ceiling(isowk/13) gives wk 1-13 -> Q1, 14-26 -> Q2, 27-39 -> Q3, 40-52 -> Q4.
##  ISO weeks do NOT align exactly with calendar quarters (a calendar quarter
##  boundary can fall mid-week). If your MATLAB qord uses CALENDAR quarters
##  (Q1 = Jan-Mar by date), replace the qtr line with a date-based quarter:
##     mutate(qtr = quarter(date), quarter_id = paste0(year(date),"Q",qtr))
##  using your weekly `date` column. Check which matches C.S_exo in MATLAB.
## ---------------------------------------------------------------------------
