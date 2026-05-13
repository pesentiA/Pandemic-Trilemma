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
