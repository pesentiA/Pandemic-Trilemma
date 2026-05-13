# =============================================================================
#  EXPORT WEEKLY MORTALITY / THETA / STRINGENCY PANEL FOR MATLAB
#  -----------------------------------------------------------------------------
#  Output:  Files/data/processed/weekly_mortality_matlab.csv
#
#  Conventions for MATLAB consumption:
#    - ISO-8601 dates (YYYY-MM-DD), week-start Mondays from panel_w.
#    - ISO3 country codes.
#    - All numeric columns are plain decimals; missing values are written as
#      empty string (MATLAB readtable treats them as NaN by default).
#    - One row per (Country, isoyr, isowk).  Full 38 OECD countries x ~149
#      weeks (Mar 2020 - Dec 2022).  No date filter applied at export — the
#      MATLAB side can subset (e.g. date <= 2021-12-31 for the pre-Omicron
#      cut used in the LP analysis).
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(lubridate)
})

base <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma"
load(file.path(base, "Files/data/processed/dataforanalysis.RData"))

# --- Weekly aggregate of daily OxCGRT stringency from oxd_d ------------------
oxd_w <- oxd_d %>%
  mutate(Date = as.Date(Date),
         isoyr = isoyear(Date),
         isowk = isoweek(Date)) %>%
  group_by(Country, isoyr, isowk) %>%
  summarise(
    S_daily_mean = mean(StringencyIndex_PopWeighted, na.rm = TRUE) / 100,
    S_daily_max  = max(StringencyIndex_PopWeighted,  na.rm = TRUE) / 100,
    S_daily_sd   = sd(StringencyIndex_PopWeighted,   na.rm = TRUE) / 100,
    n_obs_daily  = sum(!is.na(StringencyIndex_PopWeighted)),
    .groups = "drop"
  )

# --- Compose the export panel from panel_w + weekly-aggregated stringency ----
wkly <- panel_w %>%
  select(Country, isoyr, isowk, date,
         # Stringency (panel_w's own weekly mean as cross-check)
         S_panelw_mean = S_mean, S_panelw_max = S_max,
         # Infection prevalence
         theta_hat, theta_hat_l2, theta_hat_l4, theta_predicted,
         theta_hat_ifr_lo, theta_hat_ifr_hi,
         # Excess mortality (panel_w / HMD-projected)
         p_proj, excess, excess_pm,
         # Confirmed COVID series
         cases_w, deaths_w, cases_pm, deaths_confirmed_pm,
         # Wave classification
         wave, wave_label, wave_coarse,
         # Bookkeeping
         n_days, pop, pop_th) %>%
  left_join(oxd_w, by = c("Country", "isoyr", "isowk"))

# --- Sanity print ------------------------------------------------------------
cat(sprintf("Rows: %d\nCountries: %d\nDate range: %s -> %s\n",
            nrow(wkly), n_distinct(wkly$Country),
            min(wkly$date), max(wkly$date)))
cat("\nCountries:\n"); print(sort(unique(wkly$Country)))

# --- Write CSV in MATLAB-friendly form --------------------------------------
out_csv <- file.path(base, "Files/data/processed/weekly_mortality_matlab.csv")
write.csv(wkly, out_csv, row.names = FALSE, na = "")
cat(sprintf("\n\nSaved: %s\n  %d rows x %d cols\n",
            out_csv, nrow(wkly), ncol(wkly)))
cat("\nColumn schema for MATLAB readtable:\n")
print(data.frame(col = names(wkly), type = sapply(wkly, class)), row.names = FALSE)
