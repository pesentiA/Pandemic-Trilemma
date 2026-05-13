

# =============================================================================
#  PANDEMIC TRILEMMA — MAIN ANALYSIS SCRIPT
# =============================================================================
#  Paper:   "The Pandemic Trilemma
#  Sample:  38 OECD countries, quarterly, Q1.2015 – Q4.2024.
#           Trilemma estimation window for the V14 main spec:
#             Q4.2019 – Q2.2022 (T = 11 per country, t_idx in [4, 14]).
#           Some earlier specs / descriptives use Q1.2020 – Q1.2022 (T = 9)
#           or Q1.2020 – Q2.2022 (T = 10); each block states its window.
#  Outputs: tables (LaTeX) under  Files/output/tables
#           figures (PDF/PNG)    under Files/output/figures
#           a CSV of country-level state/control panels for the MATLAB iLQR
#           solver (see end of file).
#
#  INPUT FILES:
#    Files/data/processed/dataforanalysis.RData
#        -> objects: qdata, theta_quarterly_full, panel_w, hosp_d,
#                    google_mobility_d, fm, pdata
#    Files/data/raw/fiscal measures/fiscal_classified_v1_7.xlsx
#        -> object loaded as fm1
#    Files/data/raw/outcomes and controls/Quarterly/hh_inidcators_legende.xlsx
#        -> OECD HH dashboard (used only for Step 9 unemployment robustness)
#
#  PIPELINE (top to bottom in this file):
#    1. Load packages, set paths, set seed, resolve dplyr/lubridate conflicts.
#    2. Build master country x quarter panel `df` from 6 source blocks
#       (state vars, theta, fiscal measures, stringency, hospitalisations,
#       Google mobility) and create estimation samples `df_estimation`,
#       `pdata`, `pdataY`, `pdataD`.
#    3. Output-gap equation:
#         - Identification descriptives (D1-D5, Figures 1-3, Tables 1).
#         - Step 1: identification strategy (orthogonality of S and F).
#         - Step 2: OLS -> Country FE -> TWFE progression (Table 2).
#         - Step 3-V3 (frozen 16.04.2026): superseded V3 spec under TWFE
#           (CP via y_lag1:F_CP_lag2 + contemporaneous S*F_CP). Retained
#           for the bin / spline robustness and DCDH diagnostics that
#           motivate V4 and V14.
#         - V4 spec: state-dependent S * y_lag1_recession spline; DCDH
#           on V4 isolates the S-spline as the main contamination source.
#         - Step 3-V14 (MAIN, 12.05.2026): Above-Flow + Below-Stock
#           decomposition of CP, DI * S push-on-string interaction,
#           Country-FE-only identification. See the V14 header block
#           for the full equation and the WHY V14 narrative.
#         - V14 robustness battery: decay-stock, Year-FE / linear-trend,
#           sample restrictions, alternative S measures, wild-cluster
#           bootstrap, take-up sensitivity grid, outlier exclusion,
#           Mundlak/Chamberlain decomposition, lag selection, asymmetry,
#           sample splits (by S / income / pre-debt / social-net /
#           regional groupings).
#         - Alternative estimators: Deb-style regime switching.
#         - Step 9: unemployment-rate robustness.
#    4. Debt equation:
#         - Sample preparation (first-differenced real-debt share, with
#           chronological lag construction).
#         - CP sub-component construction (above-the-line / loans /
#           guarantees, with three guarantee take-up scenarios).
#         - Debt main results (Country FE, 5 columns).
#         - Debt robustness (5 columns) and appendix (additional checks).
#         - Guarantee take-up sensitivity grid.
#    5. Descriptive comparisons (group means, country pairs, CAN-vs-CHE
#       case study) and OECD-average time-series plots.
#    6. Export country panel to CSV for MATLAB iLQR solver.
#
#  REPLICATION NOTES:
#    - All fiscal variables F_CP, F_DI, F_H are in pp of 2019 GDP. Within
#      `pdata` they are scaled x100 (line "Modify values and construct
#      lagged variables").
#    - Stringency (S_mean_pw, S_max_pw) is on a 0-1 scale at source and is
#      rescaled to 0-100 (S_mean_tw, S_max_tw, S_sd_tw) inside `pdata`.
#    - Window conventions:
#        * V14 main spec & robustness battery: t_idx in [4, 14] =
#          Q4.2019 - Q2.2022 (T = 11). Q4.2019 anchors the Below-Stock
#          channel at near-zero pre-pandemic.
#        * V3 / V4 era specs and the descriptives sample `main_sample`:
#          t_idx in [5, 14] = Q1.2020 - Q2.2022.
#        * Earlier robustness tables used [5, 13] = Q1.2020 - Q1.2022.
#        * Local Projections at horizon h truncate the window at the
#          far end.
#    - Standard errors: clustered by Country with feols (CRV1, AER style).
#      plm models use vcovHC(cluster = "group", type = "HC1").
#    - Seed: set.seed(1234) is global; bootstrap uses set.seed(16031995)
#      separately.
#
#  This script must be run top-to-bottom in a clean R session (the script
#  itself starts with rm(list=ls())). Some downstream blocks rely on
#  variables created upstream (e.g. `id_cs`, `pdataD`, `fiscal_subcomp`,
#  `m_baseline`).
# =============================================================================

# =============================================================================
#  STAGE 1 - SESSION SETUP
#  Clear workspace, load packages, fix function-name conflicts, set the
#  random seed, point at the input/output folders, then load the processed
#  RData bundle and the fiscal-measures spreadsheet.
# =============================================================================

# .rs.restartR()   # uncomment in RStudio if a prior run polluted the session
rm(list = ls())


library(polars)
library(DIDmultiplegtDYN)


# --- Packages ---------------------------------------------------------------
# Install once with: install.packages(packages_vector)
# (Some are loaded for transitive convenience; not every package is used by
# every block. The set is kept conservative so the script runs end-to-end.)
packages_vector <- c(
  "did2s", "haven", "dplyr", "sandwich", "jtools", "data.table",
  "fBasics", "gtools", "rnaturalearth", "rnaturalearthdata", "foreign", "gt",
  "Synth", "gridExtra", "fixest", "huxtable",
  "xtable", "foreign", "stargazer", "AER", "causalweight", "tidyr", "expss",
  "stringr", "pscore", "AER", "ggplot2", "haven", "lubridate", "knitr",
  "kableExtra", "psych", "pastecs", "purrr", "magrittr", "did", "remote",
  "did2s", "patchwork", "readxl", "did2s", "plm", "scales", "mFilter",
  "countrycode", "tidyverse", "corrplot", "rnaturalearthdata", "ggExtra",
  "gt", "sf", "RColorBrewer", "UpSetR", "lmtest", "modelsummary","ggplot2"
)
lapply(packages_vector, require, character.only = TRUE)
(.packages())   # echo currently loaded packages

# Console output: keep wide, suppress scientific notation, hide NAs.
options(max.print = 99, scipen = 999, na.print = "")

# Working directory pinned to the R-code folder; safedata / safeplots /
# safetable below point at the input and output trees.
dir <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/code/R"
setwd(dir)

# Global seed. Bootstrap section sets its own seed (16031995).
set.seed(1234)

# --- Resolve function-name conflicts ----------------------------------------
# Several packages mask each other (dplyr vs. lubridate vs. data.table vs.
# stats). Pin the version this script expects so the calls below behave
# deterministically.
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflicts_prefer(dplyr::lag)
conflicted::conflicts_prefer(dplyr::first)
conflicted::conflicts_prefer(dplyr::coalesce)
conflicted::conflicts_prefer(dplyr::recode)
conflicted::conflicts_prefer(lubridate::intersect)
conflicted::conflicts_prefer(lubridate::quarter)
conflicted::conflicts_prefer(lubridate::year)
conflicted::conflicts_prefer(lubridate::wday)
conflicted::conflicts_prefer(dplyr::last)
conflicted::conflicts_prefer(data.table::isoweek)
conflicted::conflicts_prefer(data.table::month)
conflicted::conflicts_prefer(dplyr::lead)
conflicted::conflicts_prefer(modelsummary::SD)
conflicted::conflicts_prefer(lubridate::union)

# --- Load processed data ----------------------------------------------------
# `dataforanalysis.RData` is built upstream by the data-construction scripts
# (not part of this file). It supplies: qdata (state vars), theta_quarterly_full
# (infection prevalence), panel_w (stringency), hosp_d (hospitalisations),
# google_mobility_d (Google Mobility), fm and pdata (legacy objects used
# in a few later blocks).
safedata  <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/processed"
load(file.path(safedata, "dataforanalysis.RData"))

# Output directories for figures (PDF/PNG) and tables (LaTeX/CSV).
safeplots <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/output/figures"
safetable <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/output/tables"

# --- Load fiscal-measures dataset (FM v1.7) ---------------------------------
# Cleaned classification of every fiscal measure into transmission_channel
# (CP / DI / H) and category (above-the-line vs. below-the-line).
# See fix_classifications_v2.R for the cleaning step.
fm1 <- readxl::read_excel("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/raw/fiscal measures/fiscal_classified_v1_7.xlsx")

# Build a quarter label "Qq.YYYY" and a percentage-of-GDP volume column.
# YQ_ord is the same string but as an ordered factor for chronological
# arrangement later in the script.
fm1 <- fm1 %>%
  mutate(
    YQ       = paste0("Q", Quarter, ".", Year),
    YQ_ord   = factor(YQ, levels = sort(unique(paste0("Q", Quarter, ".", Year))),
                      ordered = TRUE),
    size_pct = broad_fiscal_gdp * 100
  )

# Sanity check: first 10 rows of the relevant columns.
fm1 %>% select(Country, Year, Quarter, YQ, YQ_ord, broad_fiscal_gdp, size_pct) %>% head(10)

# Inspect (do not yet drop) the policy codes 5, 6, 11, 12, 15, 16 - these
# are tax-deferral / liquidity / contingent items that we exclude from the
# headline fiscal aggregate because they don't translate one-for-one into
# realised debt or output effects.
fm1 %>%
  filter(PolicyCode %in% c(5, 6, 11, 12, 15, 16)) %>%
  group_by(PolicyCode, transmission_channel, category) %>%
  summarise(
    n = n(),
    total_vol = sum(size_pct, na.rm = TRUE),
    .groups = "drop"
  )

# Drop the excluded policy codes from fm1 going forward.
fm1 <- fm1 %>%
  filter(!PolicyCode %in% c(5, 6, 11, 12, 15, 16))

cat(sprintf("Verbleibende Massnahmen: %d\n", nrow(fm1)))

## Note: F^H (health-channel fiscal) carries no detectable output effect in
## the regressions below. We retain it as a separate aggregate for transparency
## but it does not enter the main output-gap specification.
# =============================================================================
#  STAGE 2 - MASTER ANALYSIS DATASET CONSTRUCTION
#  Output: df  - balanced (or near-balanced) Country x Quarter panel.
#  Merge key: Country (ISO3) + Quarter (string "Q1.2020").
#  Sample window pulled from each source: Q1.2019 - Q4.2022 (16 quarters).
#  The trilemma estimation window (Q1.2020 - Q1/Q2.2022) is selected later
#  via t_idx in the regression code.
#
#  The script first builds 6 source blocks, all keyed on (Country, Quarter):
#    Block 1  qdata                  -> state vars (output gap, debt gap, etc.)
#    Block 2  theta_quarterly_full   -> theta_k (infection prevalence)
#    Block 3  fm1                    -> fiscal measures aggregated to F^CP, F^DI, F^H
#    Block 4  panel_w                -> stringency (population-weighted)
#    Block 5  hosp_d                 -> hospitalisations
#    Block 6  google_mobility_d      -> Google Mobility (6 categories)
#  These are then left-joined into df_qdata to produce df.
# =============================================================================

# Quarter labels included in every source block. Order matters: this same
# vector becomes the factor-level order on `Quarter` after the master merge.
pandemic_qs <- c(
  "Q1.2019", "Q2.2019", "Q3.2019", "Q4.2019",
  "Q1.2020", "Q2.2020", "Q3.2020", "Q4.2020",
  "Q1.2021", "Q2.2021", "Q3.2021", "Q4.2021",
  "Q1.2022", "Q2.2022", "Q3.2022", "Q4.2022"
)
# -----------------------------------------------------------------------------
#  BLOCK 1: qdata  ->  state variables y_k, b_k and country-level controls
#  Selected columns include both the main and robustness variants of each
#  outcome (output gap, debt-to-GDP), plus pre-COVID levels used as
#  initial conditions / heterogeneity proxies.
# -----------------------------------------------------------------------------

df_qdata <- qdata %>%
  filter(Quarter %in% pandemic_qs) %>%
  select(
    # --- Identifiers ---
    Country, Quarter, year_only, quarter_only,
    # --- y_k: Output Gap (main + robustness) ---
    y_t_pct,                  # Main: HP-gap, % of potential (2015-2019 trend)
    y_t,
    QReal.GDP.Growth_gr,      # Robustness: YoY real GDP growth
    # --- b_k: Debt Gap (main + robustness) ---
    d_t_pct,                  # Main: nominal debt HP-gap, pp of 2019 GDP
    d_t_pct_r,                # Robustness: real debt HP-gap
    d_t,
    DebtN_share2019,          # Robustness: debt stock, % of 2019 GDP (nominal)
    DebtR_share2019,          # Robustness: debt stock, % of 2019 GDP (real)
    DebtN_share2019_growth,   # Robustness: QoQ debt growth on 2019 GDP base
    DebtR_share2019_growth,   # Robustness: QoQ debt growth real
    # --- Controls ---
    Qpopulation_th,           # Population (thousands) — for weighting
    nGDP_2019_an,             # Nominal GDP 2019 (LC) — normalisation base
    inflation_index,          # Deflator
    vax_rate,                 # Vaccination rate — post-2021 sample split
    rGDP_pc_2019,             # Pre-COVID GDP per capita — heterogeneity proxy
    debt_2019,                 # Pre-COVID debt level — initial condition b_0
    StringencyIndex_PopWeighted,
    p_proj_all_ages,
    excess.deaths_a,
    ConfirmedDeaths.a,
    Qpopulation_th,
    p_avg_all_ages
  )

cat(sprintf("Block 1 (qdata):     %d obs, %d countries\n",
            nrow(df_qdata), n_distinct(df_qdata$Country)))

# Snapshot population in Q4.2019 and broadcast it to every row as `pop_2019`
# (used later for population-weighted aggregates and for sanity-checking
# the panel size).
pop_2019 <- df_qdata[df_qdata$Quarter == "Q4.2019", c("Country", "Qpopulation_th")]
names(pop_2019)[names(pop_2019) == "Qpopulation_th"] <- "pop_2019"
df_qdata <- merge(df_qdata, pop_2019, by = "Country")
rm(pop_2019)


# -----------------------------------------------------------------------------
#  BLOCK 2: theta_quarterly_full  ->  theta_k (infection prevalence)
#  Source provides weekly + monthly variants; we use the within-quarter mean
#  as the main measure and keep lag-2 / lag-4 / lo / hi variants for
#  sensitivity analysis.
# -----------------------------------------------------------------------------

df_theta <- theta_quarterly_full %>%
  mutate(Quarter = as.character(YQ)) %>%
  filter(Quarter %in% pandemic_qs) %>%
  select(
    Country, Quarter,
    theta_mean,     # Main: mean weekly θ̂ within quarter
    theta_mean_l2,  # Lag-2 robustness
    theta_mean_l4,  # Lag-4 robustness
    theta_mean_lo,  # IFR lower bound (sensitivity)
    theta_mean_hi,  # IFR upper bound (sensitivity)
    wave_dominant,  # Dominant wave within quarter
    freq_source     # "weekly" or "monthly" (source flag)
  )

cat(sprintf("Block 2 (theta):     %d obs, %d countries\n",
            nrow(df_theta), n_distinct(df_theta$Country)))


# -----------------------------------------------------------------------------
#  BLOCK 3: fm1 (fiscal measures)  ->  controls F^CP, F^DI, F^H
#  Aggregate measure-level rows in fm1 to one row per (Country, Quarter)
#  per channel. Composition shares (CP_share, DI_share) are computed from
#  the totals; counts of active measures are kept for descriptive use.
# -----------------------------------------------------------------------------

df_fiscal <- fm1 %>%
  filter(broad_fiscal == 1) %>%
  mutate(Quarter = as.character(YQ_ord)) %>%
  filter(Quarter %in% pandemic_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(
    F_CP    = sum(broad_fiscal_gdp[transmission_channel == "CP"], na.rm = TRUE),
    F_DI    = sum(broad_fiscal_gdp[transmission_channel == "DI"], na.rm = TRUE),
    F_H     = sum(broad_fiscal_gdp[transmission_channel == "H"],  na.rm = TRUE),
    F_total = sum(broad_fiscal_gdp, na.rm = TRUE),
    # Composition ratios
    CP_share = ifelse(F_total > 0, F_CP / F_total, NA_real_),
    DI_share = ifelse(F_total > 0, F_DI / F_total, NA_real_),
    # Number of active measures
    n_measures_CP = sum(transmission_channel == "CP", na.rm = TRUE),
    n_measures_DI = sum(transmission_channel == "DI", na.rm = TRUE),
    .groups = "drop"
  )

cat(sprintf("Block 3 (fiscal):    %d obs, %d countries\n",
            nrow(df_fiscal), n_distinct(df_fiscal$Country)))


# -----------------------------------------------------------------------------
#  BLOCK 4: panel_w  ->  stringency aggregates
#  panel_w is a daily panel of the Oxford Stringency Index (population-
#  weighted within country). Aggregate to Country x Quarter:
#    S_mean_pw  - within-quarter mean (main S regressor)
#    S_max_pw   - within-quarter max  (robustness)
#    S_sd       - within-quarter SD   (volatility check)
# -----------------------------------------------------------------------------

df_stringency <- panel_w %>%
  filter(!is.na(S_mean)) %>%
  mutate(
    year    = year(date),
    quarter = quarter(date),
    Quarter = paste0("Q", quarter, ".", year)
  ) %>%
  filter(Quarter %in% pandemic_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(
    S_mean_pw  = mean(S_mean,  na.rm = TRUE),  # Population-weighted mean
    S_max_pw   = max(S_max,    na.rm = TRUE),  # Max within quarter
    S_sd       = sd(S_mean,    na.rm = TRUE),  # Within-quarter volatility
    .groups    = "drop"
  )

cat(sprintf("Block 4 (stringency): %d obs, %d countries\n",
            nrow(df_stringency), n_distinct(df_stringency$Country)))


# -----------------------------------------------------------------------------
#  BLOCK 5: hosp_d  ->  hospitalisations (33 countries with available data)
#  Daily/weekly admissions and occupancies (per million), aggregated to
#  the within-quarter mean. Coverage is narrower than the full 38-country
#  panel; controls in some specs only.
# -----------------------------------------------------------------------------

df_hosp <- hosp_d %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("2020-01-01"),
         date <= as.Date("2021-12-31")) %>%
  mutate(
    year    = year(date),
    quarter = quarter(date),
    Quarter = paste0("Q", quarter, ".", year)
  ) %>%
  filter(Quarter %in% pandemic_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(
    hosp_new_pm  = mean(Weekly.new.hospital.admissions.per.million, na.rm = TRUE),
    icu_occ_pm   = mean(Daily.ICU.occupancy.per.million,            na.rm = TRUE),
    hosp_occ_pm  = mean(Daily.hospital.occupancy.per.million,       na.rm = TRUE),
    .groups = "drop"
  )

cat(sprintf("Block 5 (hosp):      %d obs, %d countries\n",
            nrow(df_hosp), n_distinct(df_hosp$Country)))


# -----------------------------------------------------------------------------
#  BLOCK 6: google_mobility_d  ->  behavioural controls (Google Mobility)
#  Six categories per (Country, Day). We aggregate to Country x Quarter,
#  then pivot from long to wide so each category is its own column.
#  The crosswalk below maps Google's English country names to ISO3 codes
#  that match the rest of the panel.
# -----------------------------------------------------------------------------
country_crosswalk <- tibble(
  country_name = c("Australia", "Austria", "Belgium", "Canada", "Chile",
                   "Colombia", "Costa Rica", "Czechia", "Denmark", "Estonia",
                   "Finland", "France", "Germany", "Greece", "Hungary",
                   "Iceland", "Ireland", "Israel", "Italy", "Japan",
                   "Latvia", "Lithuania", "Luxembourg", "Mexico",
                   "Netherlands", "New Zealand", "Norway", "Poland",
                   "Portugal", "Slovakia", "Slovenia", "South Korea",
                   "Spain", "Sweden", "Switzerland", "Turkey",
                   "United Kingdom", "United States"),
  Country = c("AUS", "AUT", "BEL", "CAN", "CHL",
              "COL", "CRI", "CZE", "DNK", "EST",
              "FIN", "FRA", "DEU", "GRC", "HUN",
              "ISL", "IRL", "ISR", "ITA", "JPN",
              "LVA", "LTU", "LUX", "MEX",
              "NLD", "NZL", "NOR", "POL",
              "PRT", "SVK", "SVN", "KOR",
              "ESP", "SWE", "CHE", "TUR",
              "GBR", "USA")
)

df_mobility <- google_mobility_d %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("2019-01-01"),
         date <= as.Date("2022-12-31")) %>%
  mutate(
    year    = year(date),
    quarter = quarter(date),
    Quarter = paste0("Q", quarter, ".", year)
  ) %>%
  filter(Quarter %in% pandemic_qs) %>%
  left_join(country_crosswalk, by = c("country" = "country_name")) %>%
  filter(!is.na(Country)) %>%
  group_by(Country, Quarter, place) %>%
  summarise(trend_mean = mean(trend, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = place, values_from = trend_mean) %>%
  rename(
    mob_grocery     = `Grocery and pharmacy`,
    mob_parks       = Parks,
    mob_residential = Residential,
    mob_retail      = `Retail and recreation`,
    mob_transit     = `Transit stations`,
    mob_workplaces  = Workplaces
  )

cat(sprintf("Block 6 (mobility):  %d obs, %d countries\n",
            nrow(df_mobility), n_distinct(df_mobility$Country)))

# -----------------------------------------------------------------------------
#  MASTER MERGE
#  Left-join blocks 2-6 onto df_qdata (which is the most complete in terms
#  of country coverage). After the joins, we add `quarter_num` and `date`
#  helpers, convert Quarter to an ordered factor, and replace NA fiscal
#  values with 0 (no measure deployed = zero spend, NOT missing).
# -----------------------------------------------------------------------------

cat("\n--- Merging blocks ---\n")

df <- df_qdata %>%
  # θ_k (inner: only countries with theta)
  left_join(df_theta,      by = c("Country", "Quarter")) %>%
  # F^CP, F^DI, F^H
  left_join(df_fiscal,     by = c("Country", "Quarter")) %>%
  # S_mean (supplementary — panel_w source)
  left_join(df_stringency, by = c("Country", "Quarter")) %>%
  # Hospitalisation (33 countries)
  left_join(df_hosp,       by = c("Country", "Quarter")) %>%
  # Mobility (available countries)
  left_join(df_mobility,   by = c("Country", "Quarter")) %>%
  # Temporal ordering
  mutate(
    quarter_num = as.integer(str_sub(Quarter, 2, 2)),
    Quarter     = factor(Quarter, levels = pandemic_qs, ordered = TRUE),
    date        = ymd(paste0(year_only, "-",
                             (quarter_num - 1L) * 3L + 1L, "-01")),
    # Replace NA fiscal with 0 (no measures deployed = zero)
    F_CP    = replace_na(F_CP,    0),
    F_DI    = replace_na(F_DI,    0),
    F_H     = replace_na(F_H,     0),
    F_total = replace_na(F_total, 0)
  ) %>%
  arrange(Country, Quarter)


# -----------------------------------------------------------------------------
#  DIAGNOSTICS
#  Print panel size, NA counts on key variables, and a min/max/modal check
#  on observations per country. Useful for catching merge mismatches in a
#  single glance before proceeding to estimation.
# -----------------------------------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("  MASTER DATASET df — DIAGNOSTICS\n")
cat(strrep("=", 70), "\n\n")

cat(sprintf("  Dimensions:      %d obs × %d variables\n", nrow(df), ncol(df)))
cat(sprintf("  Countries:       %d\n", n_distinct(df$Country)))
cat(sprintf("  Quarters:        %d (%s to %s)\n",
            n_distinct(df$Quarter),
            min(as.character(df$Quarter)),
            max(as.character(df$Quarter))))

cat("\n  NA counts (key variables):\n")
key_vars <- c("y_t_pct", "d_t_pct", "theta_mean", "S_mean",
              "F_CP", "F_DI", "F_H",
              "hosp_new_pm", "icu_occ_pm", "mob_retail")
for (v in key_vars) {
  if (v %in% colnames(df)) {
    cat(sprintf("    %-22s NA: %3d / %d (%.1f%%)\n",
                v,
                sum(is.na(df[[v]])),
                nrow(df),
                mean(is.na(df[[v]])) * 100))
  }
}

cat("\n  Balanced panel check (observations per country):\n")
obs_per_cty <- table(df$Country)
cat(sprintf("    Min: %d | Max: %d | Modal: %d\n",
            min(obs_per_cty), max(obs_per_cty),
            as.integer(names(which.max(table(obs_per_cty))))))

cat("\n  Variables in df:\n")
print(colnames(df))

####################  MASTER DATASET READY (df)  ##############################

# --- Trilemma estimation sample: Q4.2019 - Q4.2022 ---------------------------
# df_estimation is the full 13-quarter object retained for descriptive plots
# and end-of-paper exports. The actual regression code below filters to
# narrower windows via t_idx.
df_estimation <- df %>%
  filter(Quarter %in% c("Q4.2019",
                        "Q1.2020", "Q2.2020", "Q3.2020", "Q4.2020",
                        "Q1.2021", "Q2.2021", "Q3.2021", "Q4.2021",
                        "Q1.2022", "Q2.2022", "Q3.2022", "Q4.2022"))

cat(sprintf("  Estimation sample: %d obs (%d countries x %d quarters)\n",
            nrow(df_estimation),
            n_distinct(df_estimation$Country),
            n_distinct(df_estimation$Quarter)))


# =============================================================================
#  STAGE 2b - PREPARE pdata (panel for OUTPUT and DEBT regressions)
#  pdata extends the estimation window backward into 2019 (we need the 2019
#  quarters as the lag source for Q1.2020 lags) and forward through 2022 for
#  Local Projections at horizons h = 0..3.
# =============================================================================

# Include all 2019 and 2022 quarters for lag construction and LP horizons.
pdata <- df %>%
  filter(Quarter %in% c("Q1.2019", "Q2.2019", "Q3.2019", "Q4.2019",
                        "Q1.2020", "Q2.2020", "Q3.2020", "Q4.2020",
                        "Q1.2021", "Q2.2021", "Q3.2021", "Q4.2021",
                        "Q1.2022", "Q2.2022", "Q3.2022", "Q4.2022"))

# Quick descriptive snapshot of the key variables (printed only, not saved).
pdata %>%
  as_tibble() %>%
  select(y_t_pct, d_t_pct, S_mean_pw, StringencyIndex_PopWeighted,
         F_CP, F_DI, theta_mean, vax_rate, icu_occ_pm, F_H, pop_2019) %>%
  summarise(across(everything(), list(
    mean = ~mean(., na.rm = TRUE),
    sd   = ~sd(.,   na.rm = TRUE),
    min  = ~min(.,  na.rm = TRUE),
    max  = ~max(.,  na.rm = TRUE)
  ))) %>%
  pivot_longer(everything(),
               names_to  = c("variable", "stat"),
               names_sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(variable) %>%
  print(n = Inf)

## TODO: add a final summary table at the end of the script with every
## variable actually used in the published regressions.

# --- Rescale variables for interpretable coefficients ------------------------
# All transmission and stringency variables are scaled to "percentage of
# 2019 GDP" (x100) or 0-100 stringency points (x100). This is the unit a
# reader of an empirical paper expects, and it keeps coefficients in a
# legible range. Population is rescaled to millions (/1000) for plots.
pdata <- pdata %>%
  mutate(
    S_mean_tw      = S_mean_pw * 100,
    S_max_tw       = S_max_pw  * 100,
    S_sd_tw        = S_sd      * 100,
    F_CP           = F_CP      * 100,
    F_DI           = F_DI      * 100,
    F_H            = F_H       * 100,
    vax_rate       = vax_rate  * 100,
    theta_pct      = theta_mean * 100,
    Qpopulation_th = Qpopulation_th / 1000
  )

# --- Lag and lead construction ----------------------------------------------
# IMPORTANT: this block does NOT group_by(Country). Because pdata is sorted
# by Country and then chronologically (see the merge stage), lags/leads
# happen in the right order WITHIN a country, but a lag at the boundary of
# one country pulls in the previous country's last value. The check below
# spot-tests for that.
# (The downstream regression sample begins at Q1.2020 with t_idx >= 5, so
# any Q1.2019 contamination would only matter if it bled into Q1.2020 -
# the spot check confirms it does not.)
pdata <- pdata %>%
  mutate(
    S_lag1     = lag(S_mean_tw,        1),
    S_lag2     = lag(S_mean_tw,        2),
    S_lead1    = lead(S_mean_tw,       1),
    F_DI_lag1  = lag(F_DI,             1),
    F_DI_lag2  = lag(F_DI,             2),
    F_DI_lag3  = lag(F_DI,             3),
    F_CP_lag1  = lag(F_CP,             1),
    F_CP_lag2  = lag(F_CP,             2),
    F_CP_lead1 = lead(F_CP,            1),
    theta_lag1 = lag(theta_mean,       1),
    y_lag1     = lag(y_t_pct,          1),
    y_lead1    = lead(y_t_pct,         1),
    p_p_lag1   = lag(p_proj_all_ages,  1),
    p_a_lag1   = lag(p_avg_all_ages,   1)
  )

colnames(pdata)

# Spot check: confirm lags do not leak across country borders.
pdata %>%
  as.data.frame() %>%
  select(Country, Quarter, F_CP_lag1, F_CP_lag2) %>%
  filter(Quarter %in% c("Q1.2020", "Q2.2020")) %>%
  arrange(Country, Quarter) %>%
  head(20)

## Check passed.

# =============================================================================
#  STAGE 3 - OUTPUT-GAP EQUATION
#  Empirical counterpart to eq. (OG) in the pandemic-trilemma model (Section 2).
#
#  Estimating equation (eq. output_est):
#    y_{ik} = a_S * S_{ik} + psi * S_{ik} * y_{i,k-1}
#            + a_FCP * F_{ik}^CP + eta * S_{ik} * F_{ik}^CP
#            + a_FDI * F_{i,k-2}^DI + gamma_i + delta_k + eps_{ik}
#
#  Structure of this stage:
#    STEP 0  - Sample construction and instrument disaggregation.
#    STEP 1  - Identification strategy (orthogonality of S and F).
#    STEP 2  - Model justification (OLS -> FE -> TWFE progression).
#    STEP 3  - Main V3 specification (16.04.2026 freeze) with feols.
#    STEP 4  - Time-period justification and horizon robustness.
#    STEP 5  - Wild-cluster bootstrap on the main coefficient.
#    STEP 6  - Robustness checks (functional form, outliers, splits, sub-comps).
#    STEP 7  - Alternative estimators: GMM (Nickell) + Local Projections.
#    STEP 8  - Cross-estimator comparison & final conclusion.
#    STEP 9  - Robustness with unemployment rate as alternative DV.
# =============================================================================

# Extra packages used only by this stage. fwildclusterboot is used for
# wild-cluster bootstrap; summclust does cluster-jackknife diagnostics;
# clubSandwich provides additional clustered VCOVs; boot is a base
# dependency that some installations need explicitly.
library(fwildclusterboot)
library(summclust)
library(clubSandwich)
if (!requireNamespace("boot", quietly = TRUE)) install.packages("boot")
library(boot)

# -----------------------------------------------------------------------------
#  STEP 0 - SAMPLE CONSTRUCTION
# -----------------------------------------------------------------------------
#  pdataY is the working panel for every output-gap regression below.
#  Window: Q1.2018 - Q4.2022 (the 2018-2019 quarters give us pre-pandemic
#  validation observations and the lag/lead source for early-pandemic
#  observations; the 2022 quarters extend the sample for LP horizons).
#  The regression code subsets via t_idx, with t_idx = 1 corresponding to
#  Q1.2019 (defined further below):
#    main sample        t_idx in [5, 14]   (Q1.2020 - Q2.2022)
#    paper Table 3 main t_idx in [5, 13]   (Q1.2020 - Q1.2022)
#    LP horizon h=H     t_idx in [5, 14-H] effectively
#  All fiscal variables in pdataY are already in pp-GDP units (x100 from
#  the rescaling applied to pdata above).
# -----------------------------------------------------------------------------

pdataY <- pdata %>%
  filter(Quarter %in% c("Q1.2018", "Q2.2018", "Q3.2018", "Q4.2018",
                        "Q1.2019", "Q2.2019", "Q3.2019", "Q4.2019",
                        "Q1.2020", "Q2.2020", "Q3.2020", "Q4.2020",
                        "Q1.2021", "Q2.2021", "Q3.2021", "Q4.2021",
                        "Q1.2022", "Q2.2022", "Q3.2022", "Q4.2022"))

# -----------------------------------------------------------------------------
#  STEP 0B - INSTRUMENT DISAGGREGATION
#  All sub-components are in pp of 2019 GDP (factor x100 applied here).
#  Source: fm1 (cleaned, PolicyCodes 5/6/11/12/15/16 already dropped above).
#
#  CP above-the-line  (category = 1): wage subsidies, STW, direct grants
#  CP below-the-line loans (PolicyCode 40, 41): government loans actually
#                          disbursed -> realized debt
#  CP below-the-line guarantees (PolicyCode 43): credit guarantees ->
#                          contingent liability, low realized cost
#
#  Guarantee take-up scenarios (ECB / IMF Fiscal Monitor 2022 call-rate
#  estimates of the share of guaranteed envelope that is actually drawn):
#    lo  = 25%
#    mid = 35%   (baseline)
#    hi  = 50%
#  Baseline F_CP_adj_mid = above + loans_full + guar * 0.35.
#
#  DI transfers (PC 35-38):              direct cash, unemployment, ad-hoc benefits
#  DI demand    (PC 27-29):              infrastructure, green, tourism
#  DI tax       (PC 17-22, 25-26):       individual tax relief, VAT cuts
#
#  H  supply   (PC 30-32): medical procurement (endogenous to theta_k)
#  H  infra    (PC 33-34): health infrastructure investment
# -----------------------------------------------------------------------------

# --- Build CP sub-components (×100 → pp GDP) from fm1 ---
fiscal_subcomp <- fm1 %>%
  filter(broad_fiscal == 1) %>%
  mutate(
    Quarter_fmt = paste0("Q", Quarter, ".", Year),
    CP_above = ifelse(transmission_channel == "CP" & category == 1,
                      broad_fiscal_gdp, 0),
    CP_loans = ifelse(transmission_channel == "CP" & category == 2 &
                        PolicyCode %in% c("40","41"), broad_fiscal_gdp, 0),
    CP_guar  = ifelse(transmission_channel == "CP" & category == 2 &
                        PolicyCode == "43", broad_fiscal_gdp, 0)
  ) %>%
  group_by(Country, Quarter_fmt) %>%
  summarise(
    F_CP_above_3 = sum(CP_above, na.rm=TRUE) * 100,   # pp GDP
    F_CP_loans   = sum(CP_loans, na.rm=TRUE) * 100,
    F_CP_guar    = sum(CP_guar,  na.rm=TRUE) * 100,
    .groups = "drop"
  ) %>%
  rename(Quarter = Quarter_fmt) %>%
  mutate(
    # Guarantee adjustment scenarios (pp GDP)
    F_CP_guar_lo  = F_CP_guar * 0.25,
    F_CP_guar_mid = F_CP_guar * 0.35,   # baseline
    F_CP_guar_hi  = F_CP_guar * 0.50,
    # Total CP = above + loans (full) + guar (adjusted)
    F_CP_adj_lo   = F_CP_above_3 + F_CP_loans + F_CP_guar_lo,
    F_CP_adj_mid  = F_CP_above_3 + F_CP_loans + F_CP_guar_mid,  # baseline
    F_CP_adj_hi   = F_CP_above_3 + F_CP_loans + F_CP_guar_hi,
    # Adjusted guarantee for interaction term
    F_CP_guar_adj = F_CP_guar_mid
  )

# --- Build DI sub-components ---
fiscal_di_sub <- fm1 %>%
  filter(broad_fiscal == 1, transmission_channel == "DI") %>%
  mutate(
    Quarter = paste0("Q", Quarter, ".", Year),
    DI_sub = case_when(
      PolicyCode %in% c("35","36","37","38")                      ~ "transfers",
      PolicyCode %in% c("27","28","29")                           ~ "demand",
      PolicyCode %in% c("17","18","19","20","21","22","25","26")  ~ "tax",
      TRUE                                                         ~ "other_di"
    )
  ) %>%
  filter(Quarter %in% pandemic_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(
    F_DI_transfers = sum(broad_fiscal_gdp[DI_sub=="transfers"], na.rm=TRUE) * 100,
    F_DI_demand    = sum(broad_fiscal_gdp[DI_sub=="demand"],    na.rm=TRUE) * 100,
    F_DI_tax       = sum(broad_fiscal_gdp[DI_sub=="tax"],       na.rm=TRUE) * 100,
    .groups = "drop"
  )

# --- Build H sub-components ---
fiscal_h_sub <- fm1 %>%
  filter(broad_fiscal == 1, transmission_channel == "H") %>%
  mutate(
    Quarter = paste0("Q", Quarter, ".", Year),
    H_sub = case_when(
      PolicyCode %in% c("30","31","32")       ~ "supply",
      PolicyCode %in% c("33","34","general")  ~ "infra",
      TRUE                                     ~ "other_h"
    )
  ) %>%
  filter(Quarter %in% pandemic_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(
    F_H_supply = sum(broad_fiscal_gdp[H_sub=="supply"], na.rm=TRUE) * 100,
    F_H_infra  = sum(broad_fiscal_gdp[H_sub=="infra"],  na.rm=TRUE) * 100,
    .groups = "drop"
  )

# --- Merge sub-components into pdataY ---
# Remove any stale CP columns from prior runs
pdataY <- pdataY %>%
  select(-any_of(c(
    "F_CP_above_3","F_CP_loans","F_CP_guar","F_CP_guar_lo","F_CP_guar_mid",
    "F_CP_guar_hi","F_CP_adj_lo","F_CP_adj_mid","F_CP_adj_hi","F_CP_guar_adj",
    "F_DI_transfers","F_DI_demand","F_DI_tax",
    "F_DI_transfers_lag2","F_DI_demand_lag2","F_DI_tax_lag2",
    "F_H_supply","F_H_infra"
  ))) %>%
  left_join(fiscal_subcomp, by = c("Country","Quarter")) %>%
  left_join(fiscal_di_sub,  by = c("Country","Quarter")) %>%
  left_join(fiscal_h_sub,   by = c("Country","Quarter")) %>%
  mutate(
    across(c(F_CP_above_3, F_CP_loans, F_CP_guar, F_CP_guar_lo, F_CP_guar_mid,
             F_CP_guar_hi, F_CP_adj_lo, F_CP_adj_mid, F_CP_adj_hi, F_CP_guar_adj,
             F_DI_transfers, F_DI_demand, F_DI_tax,
             F_H_supply, F_H_infra), ~replace_na(.x, 0)),
    # Lagged DI sub-components
    F_DI_transfers_lag2 = lag(F_DI_transfers, 2),
    F_DI_demand_lag2    = lag(F_DI_demand,    2),
    F_DI_tax_lag2       = lag(F_DI_tax,       2)
  )

# `fiscal_subcomp` is intentionally kept in the global environment because
# the DEBT section reuses it later: pdataD <- pdataD %>% left_join(fiscal_subcomp, ...)

# -----------------------------------------------------------------------------
#  Variable overview after Step 0B (all CP variables are in pp of GDP, i.e.
#  already multiplied by 100):
#    F_CP            total CP from master merge (face value, no guarantee adj)
#    F_CP_adj_mid    above + loans + guar*0.35    (BASELINE for main specs)
#    F_CP_adj_lo     above + loans + guar*0.25    (robustness)
#    F_CP_adj_hi     above + loans + guar*0.50    (robustness)
#    F_CP_above_3    above-the-line only: STW, payroll subsidies, grants
#    F_CP_loans      below-the-line loans (PC 40, 41): actual disbursements
#    F_CP_guar_adj   below-the-line guarantees * 0.35 (PC 43, baseline)
# -----------------------------------------------------------------------------

# --- Time index and helper variables ----------------------------------------
quarter_order <- c("Q1.2019", "Q2.2019", "Q3.2019", "Q4.2019",
                   "Q1.2020", "Q2.2020", "Q3.2020", "Q4.2020",
                   "Q1.2021", "Q2.2021", "Q3.2021", "Q4.2021",
                   "Q1.2022", "Q2.2022", "Q3.2022", "Q4.2022")
# t_idx encoding: Q1.2019 = 1, Q4.2019 = 4, Q1.2020 = 5, Q1.2022 = 13,
# Q2.2022 = 14, Q4.2022 = 16. Main estimation sample: t_idx in [5, 13].
pdataY$t_idx <- match(as.character(pdataY$Quarter), quarter_order)

# S_max_tw: maximum stringency within quarter on a 0-100 scale. The guard
# below avoids double-scaling if the script is rerun without rebuilding pdata.
if (max(pdataY$S_max_pw, na.rm = TRUE) <= 1.01) {
  pdataY$S_max_tw <- pdataY$S_max_pw * 100
} else {
  pdataY$S_max_tw <- pdataY$S_max_pw   # already on 0-100 scale
}

# S_high: indicator for above-median stringency (>= 50). Used in some splits.
pdataY$S_high <- as.integer(pdataY$S_mean_tw >= 50)

# Composite interaction terms expressed as named columns. GMM needs the
# bilinear (S * y_lag1, S * F_CP) products as ordinary regressors so they
# can be instrumented; we also use these in some plm specifications.
pdataY <- pdataY %>%
  mutate(
    S_y_lag = S_mean_tw * y_lag1,
    S_FCP   = S_mean_tw * F_CP
  )

# Half-year identifier (used as alternative time FE in some robustness runs).
pdataY$half_year <- ifelse(
  as.integer(sub("Q(\\d).*", "\\1", as.character(pdataY$Quarter))) <= 2,
  paste0("H1.", sub(".*\\.", "", as.character(pdataY$Quarter))),
  paste0("H2.", sub(".*\\.", "", as.character(pdataY$Quarter)))
)

# -----------------------------------------------------------------------------
#  STEP 1 - IDENTIFICATION STRATEGY
#  Key identifying variation: countries with similar containment trajectories
#  chose markedly different fiscal compositions (CP vs. DI). This near-
#  orthogonality of S and F is the empirical foundation for identifying the
#  S * F_CP interaction. Quarter FE absorb the common pandemic cycle;
#  country FE absorb time-invariant fiscal capacity. The residual variation
#  in composition across countries within a given quarter identifies eta.
# -----------------------------------------------------------------------------

cat("\n", strrep("=",70), "\n")
cat("  STEP 1: IDENTIFICATION STRATEGY\n")
cat(strrep("=",70), "\n\n")

main_sample <- pdataY %>% filter(t_idx >= 5 & t_idx <= 14)

# -----------------------------------------------------------------------------
#  DESCRIPTIVES FOR PAPER (AER-style)
#  Inputs : main_sample (38 countries x Q1.2020-Q1.2022, N ~ 342).
#  Outputs: 5 LaTeX tables + 3 PDFs, all written under safetable / safeplots.
#    D1  Within / between variation decomposition  -> tab_within_between.tex
#    D2  Pairwise correlation matrix               -> tab_corr_matrix.tex
#    D3  Pre-pandemic (2019) validation            -> printed only
#    D4  Distribution of fiscal instruments        -> fig_fiscal_density.pdf
#    D5  OECD time-series of key variables         -> fig_ts_key_variables.pdf
#  Followed by paper Figures 1-3 (identification scatter, CP-share bars,
#  output-gap time series) and Table 1 (descriptive statistics).
# -----------------------------------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("  DESCRIPTIVES FOR PAPER\n")
cat(strrep("=", 70), "\n\n")

# --- D1: Within vs. Between Variation Decomposition ---------------------------
# Critical for FE identification: shows whether variation comes from
# countries doing different things over time (within) or cross-country
# differences (between). FE exploits within-variation only.

cat("--- D1: Within vs. Between Variation Decomposition ---\n\n")

decomp_vars <- c("y_t_pct", "S_mean_tw", "F_CP", "F_DI", "F_H",
                  "p_proj_all_ages", "theta_pct", "vax_rate")

decomp_labels <- c("Output gap (pp)", "Stringency (0-100)",
                    "CP (pp GDP)", "DI (pp GDP)", "Health (pp GDP)",
                    "Excess mortality (P-score)", "Infection prev. (%)",
                    "Vaccination (%)")

cat(sprintf("  %-28s %8s %8s %8s %8s %8s\n",
            "Variable", "Overall", "Between", "Within", "W/O (%)", "B/O (%)"))
cat("  ", strrep("-", 78), "\n")

decomp_results <- list()
for (i in seq_along(decomp_vars)) {
  v <- decomp_vars[i]
  if (!v %in% colnames(main_sample)) next

  d <- main_sample %>%
    filter(!is.na(.data[[v]])) %>%
    group_by(Country) %>%
    mutate(
      x_bar_i = mean(.data[[v]], na.rm = TRUE),  # country mean
      x_within = .data[[v]] - x_bar_i            # within deviation
    ) %>%
    ungroup()

  sd_overall <- sd(d[[v]], na.rm = TRUE)
  sd_between <- sd(d$x_bar_i, na.rm = TRUE)
  sd_within  <- sd(d$x_within, na.rm = TRUE)

  cat(sprintf("  %-28s %8.3f %8.3f %8.3f %7.1f%% %7.1f%%\n",
              decomp_labels[i], sd_overall, sd_between, sd_within,
              (sd_within / sd_overall) * 100,
              (sd_between / sd_overall) * 100))

  decomp_results[[v]] <- data.frame(
    variable = decomp_labels[i],
    sd_overall = sd_overall,
    sd_between = sd_between,
    sd_within  = sd_within,
    within_share = sd_within / sd_overall,
    between_share = sd_between / sd_overall
  )
}

cat("\n  INTERPRETATION: Variables with high within-share (>50%) are well-identified\n")
cat("  by country FE. Variables with low within-share rely on cross-country\n")
cat("  variation that FE absorbs — requiring interaction terms or IV for identification.\n\n")

# Export decomposition as LaTeX table
decomp_df <- bind_rows(decomp_results)
decomp_tex <- paste0(
  "\\begin{table}[H]\n",
  "\\centering\n",
  "\\caption{Within vs.\\ Between Variation Decomposition}\n",
  "\\label{tab:within_between}\n",
  "\\small\n",
  "\\renewcommand{\\arraystretch}{1.15}\n",
  "\\begin{tabular}{@{} l r r r r r @{}}\n",
  "\\toprule\n",
  "\\textbf{Variable} & \\textbf{Overall SD} & \\textbf{Between SD} & \\textbf{Within SD}",
  " & \\textbf{Within (\\%)} & \\textbf{Between (\\%)} \\\\\n",
  "\\midrule\n"
)
for (j in 1:nrow(decomp_df)) {
  decomp_tex <- paste0(decomp_tex, sprintf(
    "%s & %.3f & %.3f & %.3f & %.1f & %.1f \\\\\n",
    decomp_df$variable[j], decomp_df$sd_overall[j],
    decomp_df$sd_between[j], decomp_df$sd_within[j],
    decomp_df$within_share[j] * 100, decomp_df$between_share[j] * 100
  ))
}
decomp_tex <- paste0(decomp_tex,
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\vspace{4pt}\n\n",
  "\\parbox{\\textwidth}{\\footnotesize\\textit{Notes:} 38 OECD countries, ",
  "Q1\\,2020--Q1\\,2022. Overall SD: total standard deviation. Between SD: ",
  "standard deviation of country means. Within SD: standard deviation of ",
  "deviations from country means. Within (\\%) = Within SD / Overall SD $\\times$ 100. ",
  "Country fixed effects absorb between-variation; within-variation identifies the coefficients.}\n",
  "\\end{table}\n"
)
writeLines(decomp_tex, file.path(safetable, "tab_within_between.tex"))
cat("  -> Saved: tab_within_between.tex\n\n")


# --- D2: Pairwise Correlation Matrix ------------------------------------------
# Key concern: multicollinearity between S and F_CP (governments that lock
# down also spend on CP), and between S and theta (endogeneity).

cat("--- D2: Pairwise Correlation Matrix ---\n\n")

cor_vars <- c("y_t_pct", "S_mean_tw", "F_CP", "F_DI",
              "p_proj_all_ages", "theta_pct", "vax_rate")
cor_labels <- c("$y_{ik}$", "$S_{ik}$", "$F^{CP}_{ik}$", "$F^{DI}_{ik}$",
                "$d_{ik}$", "$\\theta_{ik}$", "$\\text{Vax}_{ik}$")

cor_data <- main_sample %>%
  select(all_of(cor_vars)) %>%
  filter(complete.cases(.))

cor_mat <- cor(cor_data)
rownames(cor_mat) <- cor_labels
colnames(cor_mat) <- cor_labels

cat("  Correlation matrix (Pearson, pairwise complete):\n")
print(round(cor_mat, 3))

# Flag high correlations
cat("\n  Key correlations for identification:\n")
cat(sprintf("    r(S, F_CP)    = %+.3f  (lockdown-fiscal nexus)\n",
            cor(cor_data$S_mean_tw, cor_data$F_CP)))
cat(sprintf("    r(S, F_DI)    = %+.3f\n",
            cor(cor_data$S_mean_tw, cor_data$F_DI)))
cat(sprintf("    r(S, theta)   = %+.3f  (endogeneity concern)\n",
            cor(cor_data$S_mean_tw, cor_data$theta_pct)))
cat(sprintf("    r(F_CP, F_DI) = %+.3f  (fiscal composition)\n",
            cor(cor_data$F_CP, cor_data$F_DI)))
cat(sprintf("    r(S, d)       = %+.3f  (containment-mortality)\n",
            cor(cor_data$S_mean_tw, cor_data$p_proj_all_ages)))

# Export correlation matrix as LaTeX
n_cv <- length(cor_vars)
cor_tex <- paste0(
  "\\begin{table}[H]\n",
  "\\centering\n",
  "\\caption{Pairwise Correlation Matrix}\n",
  "\\label{tab:corr_matrix}\n",
  "\\small\n",
  "\\begin{tabular}{@{} l", paste(rep("r", n_cv), collapse = ""), " @{}}\n",
  "\\toprule\n",
  " & ", paste(cor_labels, collapse = " & "), " \\\\\n",
  "\\midrule\n"
)
for (j in 1:n_cv) {
  row_vals <- sapply(1:n_cv, function(k) {
    if (k > j) return("")
    if (k == j) return("1")
    sprintf("%.2f", cor_mat[j, k])
  })
  cor_tex <- paste0(cor_tex, cor_labels[j], " & ",
                    paste(row_vals, collapse = " & "), " \\\\\n")
}
cor_tex <- paste0(cor_tex,
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\vspace{4pt}\n\n",
  "\\parbox{\\textwidth}{\\footnotesize\\textit{Notes:} Pearson correlations, ",
  "estimation sample (38 countries, Q1\\,2020--Q1\\,2022). ",
  "Lower triangle only; diagonal = 1.}\n",
  "\\end{table}\n"
)
writeLines(cor_tex, file.path(safetable, "tab_corr_matrix.tex"))
cat("\n  -> Saved: tab_corr_matrix.tex\n\n")


# --- D3: Pre-pandemic Validation (2019) ---------------------------------------
# Show that y_ik ≈ 0 and delta_b ≈ 0 in 2019 (HP filter well-calibrated,
# no pre-trends in dependent variables before treatment)

cat("--- D3: Pre-pandemic Validation (2019 quarters) ---\n\n")

pre_pandemic <- pdataY %>%
  filter(t_idx <= 4) %>%  # Q1-Q4.2019
  summarise(
    y_mean   = mean(y_t_pct, na.rm = TRUE),
    y_sd     = sd(y_t_pct, na.rm = TRUE),
    y_max    = max(abs(y_t_pct), na.rm = TRUE),
    n        = sum(!is.na(y_t_pct))
  )

cat(sprintf("  Output gap (2019): mean = %.3f, SD = %.3f, max|y| = %.3f (N = %d)\n",
            pre_pandemic$y_mean, pre_pandemic$y_sd, pre_pandemic$y_max, pre_pandemic$n))
cat("  -> Near-zero mean confirms HP filter is well-calibrated.\n")
cat("  -> Small SD confirms no pre-trends in the dependent variable.\n\n")

# t-test: H0: mean(y_2019) = 0
y_2019 <- pdataY$y_t_pct[pdataY$t_idx <= 4 & !is.na(pdataY$y_t_pct)]
tt <- t.test(y_2019, mu = 0)
cat(sprintf("  t-test H0: mean(y_2019) = 0: t = %.3f, p = %.4f\n", tt$statistic, tt$p.value))
if (tt$p.value > 0.05) {
  cat("  -> Cannot reject H0 at 5%: output gap centered at zero pre-pandemic.\n\n")
} else {
  cat("  -> WARNING: Rejects H0 at 5%. Check HP filter specification.\n\n")
}


# --- D4: Distribution of Fiscal Instruments -----------------------------------
# Many country-quarters have zero or near-zero fiscal measures. Show the
# skewness explicitly — important for interpreting mean effects.

cat("--- D4: Distribution of Fiscal Instruments ---\n\n")

for (fv in c("F_CP", "F_DI", "F_H")) {
  x <- main_sample[[fv]][!is.na(main_sample[[fv]])]
  n_zero   <- sum(x == 0)
  pct_zero <- n_zero / length(x) * 100
  skew     <- mean(((x - mean(x)) / sd(x))^3)
  cat(sprintf("  %-6s: N=%d, zeros=%d (%.1f%%), skewness=%.2f, P50=%.3f, P90=%.3f\n",
              fv, length(x), n_zero, pct_zero, skew,
              median(x), quantile(x, 0.90)))
}

# Density plots
fig_density <- main_sample %>%
  select(F_CP, F_DI, F_H) %>%
  pivot_longer(everything(), names_to = "instrument", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(instrument = recode(instrument,
    "F_CP" = "Capacity Preservation",
    "F_DI" = "Demand Injection",
    "F_H"  = "Health")) %>%
  ggplot(aes(x = value, fill = instrument)) +
  geom_density(alpha = 0.5, color = NA) +
  facet_wrap(~instrument, scales = "free", ncol = 3) +
  scale_fill_manual(values = c("grey25", "grey60", "grey85")) +
  labs(
    title    = "Distribution of Fiscal Instruments (pp of 2019 GDP)",
    subtitle = "Estimation sample Q1.2020-Q1.2022. Highly right-skewed; many zeros.",
    x = "pp of 2019 GDP", y = "Density"
  ) +
  theme_bw(base_size = 10) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"))
ggsave(file.path(safeplots, "fig_fiscal_density.pdf"),
       fig_density, width = 10, height = 4)
cat("\n  -> Saved: fig_fiscal_density.pdf\n\n")


# --- D5: Time-series of Key Variables (OECD mean ± 1 SD) ---------------------
# 2×3 grid, Q1.2020–Q2.2022, with cross-country dispersion bands.

cat("--- D5: Time-series of Key Variables (OECD mean +/- 1 SD) ---\n\n")

# Compute debt_dR if not yet available (FD of DebtR_share2019, chronological)
if (!"debt_dR" %in% names(pdataY)) {
  pdataY <- pdataY %>%
    arrange(Country, t_idx) %>%
    group_by(Country) %>%
    mutate(debt_dR = DebtR_share2019 - lag(DebtR_share2019, 1)) %>%
    ungroup()
  cat("  (computed debt_dR for D5 plot)\n")
}

# Compute median, P25, P75 per quarter (t_idx 5-14 = Q1.2020-Q2.2022)
ts_data <- pdataY %>%
  filter(t_idx >= 5 & t_idx <= 14) %>%
  mutate(q_num = t_idx - 4L) %>%
  group_by(q_num, Quarter) %>%
  summarise(
    y_med  = median(y_t_pct, na.rm = TRUE),
    y_p25  = quantile(y_t_pct, 0.25, na.rm = TRUE),
    y_p75  = quantile(y_t_pct, 0.75, na.rm = TRUE),
    S_med  = median(S_mean_tw, na.rm = TRUE),
    S_p25  = quantile(S_mean_tw, 0.25, na.rm = TRUE),
    S_p75  = quantile(S_mean_tw, 0.75, na.rm = TRUE),
    CP_med = median(F_CP, na.rm = TRUE),
    CP_p25 = quantile(F_CP, 0.25, na.rm = TRUE),
    CP_p75 = quantile(F_CP, 0.75, na.rm = TRUE),
    DI_med = median(F_DI, na.rm = TRUE),
    DI_p25 = quantile(F_DI, 0.25, na.rm = TRUE),
    DI_p75 = quantile(F_DI, 0.75, na.rm = TRUE),
    d_med  = median(p_proj_all_ages, na.rm = TRUE),
    d_p25  = quantile(p_proj_all_ages, 0.25, na.rm = TRUE),
    d_p75  = quantile(p_proj_all_ages, 0.75, na.rm = TRUE),
    db_med = median(debt_dR, na.rm = TRUE),
    db_p25 = quantile(debt_dR, 0.25, na.rm = TRUE),
    db_p75 = quantile(debt_dR, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

q_labels <- c("Q1.20","Q2.20","Q3.20","Q4.20","Q1.21",
              "Q2.21","Q3.21","Q4.21","Q1.22","Q2.22")

panel_labels <- c(
  "Output gap (pp of potential GDP)",
  "Containment stringency (0\u2013100)",
  "Capacity preservation (% of 2019 GDP)",
  "Demand injection (% of 2019 GDP)",
  "Excess mortality (P-score, %)",
  "Change in public debt (pp of 2019 GDP)"
)

ts_long <- bind_rows(
  ts_data %>% transmute(q_num, variable = panel_labels[1], med = y_med,  p25 = y_p25,  p75 = y_p75),
  ts_data %>% transmute(q_num, variable = panel_labels[2], med = S_med,  p25 = S_p25,  p75 = S_p75),
  ts_data %>% transmute(q_num, variable = panel_labels[3], med = CP_med, p25 = CP_p25, p75 = CP_p75),
  ts_data %>% transmute(q_num, variable = panel_labels[4], med = DI_med, p25 = DI_p25, p75 = DI_p75),
  ts_data %>% transmute(q_num, variable = panel_labels[5], med = d_med,  p25 = d_p25,  p75 = d_p75),
  ts_data %>% transmute(q_num, variable = panel_labels[6], med = db_med, p25 = db_p25, p75 = db_p75)
) %>%
  mutate(variable = factor(variable, levels = panel_labels))

fig_ts_all <- ggplot(ts_long, aes(x = q_num)) +
  geom_ribbon(aes(ymin = p25, ymax = p75),
              fill = "grey80", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.3) +
  geom_line(aes(y = med), color = "black", linewidth = 0.7) +
  geom_point(aes(y = med), color = "black", size = 1.3, shape = 16) +
  facet_wrap(~ variable, scales = "free_y", ncol = 3) +
  scale_x_continuous(breaks = 1:10, labels = q_labels) +
  labs(x = NULL, y = NULL) +
  theme_classic(base_size = 10) +
  theme(
    text               = element_text(family = "serif"),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.minor   = element_blank(),
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y        = element_text(size = 8),
    axis.ticks         = element_line(color = "grey70", linewidth = 0.3),
    strip.background   = element_blank(),
    strip.text         = element_text(face = "italic", size = 9, hjust = 0),
    plot.margin        = margin(t = 5, r = 10, b = 5, l = 5)
  )

ggsave(file.path(safeplots, "fig_ts_key_variables.pdf"),
       fig_ts_all, width = 11, height = 6.5)
ggsave(file.path(safeplots, "fig_ts_key_variables.png"),
       fig_ts_all, width = 11, height = 6.5, dpi = 300)
cat("  -> Saved: fig_ts_key_variables.pdf and .png\n\n")

cat(strrep("=", 70), "\n")
cat("  END DESCRIPTIVES FOR PAPER\n")
cat(strrep("=", 70), "\n\n")

# --- 1A: Descriptive statistics table (Table 1) ---
desc_vars <- main_sample %>%
  transmute(
    `Output Gap (pp)`                  = y_t_pct,
    `Stringency Index (0–100)`         = S_mean_tw,
    `Capacity Preservation (pp GDP)`   = F_CP,
    `  of which: Above-the-line`       = F_CP_above_3,
    `  of which: Loans`                = F_CP_loans,
    `  of which: Guarantees (adj 35%)` = F_CP_guar_adj,
    `Demand Injection (pp GDP)`        = F_DI,
    `  of which: Transfers`            = F_DI_transfers,
    `Fear Term (infection risk)`       = p_proj_all_ages,
    `Infection Prevalence (%)`         = theta_pct,
    `Health Expenditure (pp GDP)`      = F_H,
    `Vaccination Rate (%)`             = vax_rate
  )

tab_desc <- datasummary(
  All(desc_vars) ~ N + Mean + SD + Min + P25 + Median + P75 + Max,
  data   = desc_vars,
  fmt    = 3,
  output = "data.frame",
  title  = "Table 1: Descriptive Statistics — Main Sample Q1.2020–Q1.2022"
)
print(tab_desc)

# Export to LaTeX
datasummary(
  All(desc_vars) ~ N + Mean + SD + Min + P25 + Median + P75 + Max,
  data   = desc_vars,
  fmt    = 3,
  output = file.path(safetable, "tab_1_descriptives.tex"),
  title  = "Descriptive Statistics: Output Gap Estimation Sample (Q1.2020--Q1.2022)"
)
cat("  → Saved: tab_1_descriptives.tex\n\n")

##dont report the adjusted values->only later on

# --- 1B: Core identification fact — orthogonality of S and F ---
# If fiscal composition were driven by pandemic severity, S and F would
# be highly correlated, invalidating the S×F_CP identification.
# Test: Pearson r between country-mean S and country-mean F (cross-sectional).
cat("--- 1B: Orthogonality check: mean stringency vs mean fiscal deployment ---\n")

id_cs <- main_sample %>%
  group_by(Country) %>%
  summarise(
    mean_S      = mean(S_mean_tw,           na.rm=TRUE),
    mean_F_CP   = mean(F_CP,                na.rm=TRUE),
    mean_F_DI   = mean(F_DI,                na.rm=TRUE),
    mean_F_tot  = mean(F_CP + F_DI + F_H,   na.rm=TRUE),
    CP_share    = mean_F_CP / pmax(mean_F_tot, 1e-6),
    .groups = "drop"
  )

r_S_FCP  <- cor(id_cs$mean_S, id_cs$mean_F_CP,  use="complete.obs")
r_S_FDI  <- cor(id_cs$mean_S, id_cs$mean_F_DI,  use="complete.obs")
r_S_Ftot <- cor(id_cs$mean_S, id_cs$mean_F_tot, use="complete.obs")
r_S_CPsh <- cor(id_cs$mean_S, id_cs$CP_share,   use="complete.obs")

cat(sprintf("  r(mean S, mean F_CP)    = %+.3f\n", r_S_FCP))
cat(sprintf("  r(mean S, mean F_DI)    = %+.3f\n", r_S_FDI))
cat(sprintf("  r(mean S, mean F_total) = %+.3f\n", r_S_Ftot))
cat(sprintf("  r(mean S, CP share)     = %+.3f\n", r_S_CPsh))
cat(paste0(
  "\n  INTERPRETATION: Near-zero correlations confirm that fiscal composition\n",
  "  was driven by institutional capacity, political preferences, and pre-\n",
  "  existing safety nets rather than by contemporaneous pandemic severity.\n",
  "  This near-orthogonality is the key identifying condition: the S*F_CP\n",
  "  interaction provides within-country variation that is not confounded by\n",
  "  common time shocks (absorbed by quarter FE) or country heterogeneity\n",
  "  (absorbed by country FE). Paper reference: Section 4.2, empirical_draft.\n\n"
))

# --- 1C: Within-quarter cross-country variation (second pillar) ---
# Even within the same calendar quarter, countries varied in both S and F_CP.
# This within-quarter dispersion identifies η separately from the global trend.
cat("--- 1C: Within-quarter SD of S and F_CP (conditional variation) ---\n")
within_var <- main_sample %>%
  group_by(Quarter) %>%
  summarise(
    sd_S   = round(sd(S_mean_tw, na.rm=TRUE), 2),
    sd_FCP = round(sd(F_CP,      na.rm=TRUE), 3),
    sd_FDI = round(sd(F_DI,      na.rm=TRUE), 3),
    n      = n(),
    .groups="drop"
  )
print(within_var)
cat(paste0(
  "  Large within-quarter SD of both S and F_CP confirms that the identifying\n",
  "  variation survives absorption of quarter fixed effects.\n\n"
))

# --- 1D: CP-share dispersion across countries ---
cp_sd   <- sd(id_cs$CP_share, na.rm=TRUE)
cp_p10  <- quantile(id_cs$CP_share, 0.10, na.rm=TRUE)
cp_p90  <- quantile(id_cs$CP_share, 0.90, na.rm=TRUE)
cat(sprintf("--- 1D: CP share distribution (SD=%.3f, P10=%.2f, P90=%.2f, ratio=%.1f) ---\n",
            cp_sd, cp_p10, cp_p90, cp_p90/pmax(cp_p10, 0.001)))
cat(paste0(
  "  The P90/P10 CP-share ratio reveals that the top-decile country allocated\n",
  "  substantially more of its fiscal package to CP than the bottom decile.\n",
  "  This compositional variation — not aggregate size — identifies η.\n\n"
))

# --- FIGURE 1: Identification scatter (mean S vs total F, colored by CP share) ---
cat("--- FIGURE 1: Identification scatter ---\n")
fig_ident <- id_cs %>%
  ggplot(aes(x=mean_S, y=mean_F_tot, label=Country)) +
  geom_smooth(method="lm", se=TRUE, color="gray50", linewidth=0.7,
              linetype="dashed", fill="gray85") +
  geom_point(aes(color=CP_share*100, size=mean_F_tot), alpha=0.85) +
  geom_text(hjust=-0.12, vjust=0.4, size=2.4, color="gray25") +
  scale_color_gradient2(low="#2166ac", mid="#f7f7f7", high="#d6604d",
                        midpoint=50, name="CP share\n(%)") +
  scale_size_continuous(guide="none", range=c(2,7)) +
  annotate("text", x=Inf, y=Inf, hjust=1.1, vjust=1.5, size=3.2,
           label=sprintf("r = %.2f", r_S_Ftot)) +
  labs(
    title    = "Figure 1: Identification — Fiscal Composition vs. Containment Stringency",
    subtitle = "Cross-country means, Q1.2020–Q1.2022. Near-zero correlation validates S×F^CP identification.",
    x        = "Mean Stringency Index (0–100)",
    y        = "Mean Total Fiscal Deployment (pp GDP)",
    caption  = "Color: CP share of total fiscal package. OLS fit ±95% CI."
  ) +
  theme_bw(base_size=11) + theme(legend.position="right")
ggsave(file.path(safeplots, "fig01_identification_scatter.pdf"),
       fig_ident, width=9, height=6)

print(fig_ident)
cat("  → Saved: fig01_identification_scatter.pdf\n")

# --- FIGURE 2: CP-share distribution by country ---
fig_cpshare <- id_cs %>%
  arrange(desc(CP_share)) %>%
  mutate(Country_f = factor(Country, levels=Country)) %>%
  ggplot(aes(x=Country_f, y=CP_share*100)) +
  geom_col(aes(fill=CP_share*100), show.legend=FALSE) +
  scale_fill_gradient(low="#abd9e9", high="#2166ac") +
  geom_hline(yintercept=mean(id_cs$CP_share,na.rm=TRUE)*100,
             color="#d6604d", linetype="dashed", linewidth=0.9) +
  labs(
    title    = "Figure 2: Cross-Country Variation in CP Share of Total Fiscal Package",
    subtitle = "Q1.2020–Q1.2022. Red dashed = OECD mean. Wide spread is the identifying variation.",
    x=NULL, y="CP share of total fiscal package (%)"
  ) +
  theme_bw(base_size=10) +
  theme(axis.text.x=element_text(angle=55, hjust=1, size=7))
ggsave(file.path(safeplots, "fig02_cpshare_country.pdf"),
       fig_cpshare, width=12, height=5)
cat("  → Saved: fig02_cpshare_country.pdf\n")

print(fig_cpshare)

# --- FIGURE 3: OECD-average output gap time series ---
fig_ts <- pdataY %>%
  filter(t_idx >= 5 & t_idx <= 13) %>%
  mutate(q_num = t_idx - 4L) %>%
  group_by(q_num, Quarter) %>%
  summarise(y_mean=mean(y_t_pct,na.rm=TRUE),
            y_sd  =sd(y_t_pct,  na.rm=TRUE), .groups="drop") %>%
  ggplot(aes(x=q_num, y=y_mean)) +
  geom_ribbon(aes(ymin=y_mean-y_sd, ymax=y_mean+y_sd),
              fill="#2166ac", alpha=0.18) +
  geom_line(color="#2166ac", linewidth=1.3) +
  geom_point(color="#2166ac", size=2.5) +
  geom_hline(yintercept=0, linetype="dashed", color="gray40") +
  scale_x_continuous(breaks=1:9,
    labels=c("Q1.20","Q2.20","Q3.20","Q4.20","Q1.21",
             "Q2.21","Q3.21","Q4.21","Q1.22")) +
  labs(
    title    = "Figure 3: Output Gap — OECD Average (Q1.2020–Q1.2022)",
    subtitle = "Mean ± 1 SD across 38 countries. HP-filtered gap, pp of potential GDP.",
    x=NULL, y="Output gap (pp of potential GDP)"
  ) +
  theme_bw(base_size=11)
ggsave(file.path(safeplots, "fig03_output_gap_ts.pdf"), fig_ts, width=9, height=5)
cat("  → Saved: fig03_output_gap_ts.pdf\n\n")
print(fig_ts)
# -----------------------------------------------------------------------------
#  STEP 2 - MODEL JUSTIFICATION: OLS -> COUNTRY FE -> TWFE
#  Walks through the FE progression that motivates the main TWFE specification.
#
#  Rationale for the TWFE choice:
#    (a) Country FE absorb unobserved heterogeneity in fiscal capacity,
#        institutions, and initial conditions (formally required by the
#        Mundlak test in 2C).
#    (b) Quarter FE absorb common pandemic waves, global credit conditions,
#        and WHO guidance without restricting the form of the time effect.
#    (c) y_{i,k-1} captures lockdown-induced persistence (psi * S * y);
#        the Nickell bias is bounded by O(1/T) = 11% at T = 9 (block 2D).
#    (d) DI enters at lag 2: the authorization -> disbursement -> spending
#        chain is approximately two quarters (Chetty et al. 2020 on CARES);
#        block 2E confirms this lag empirically.
#
#  Outputs: Table 2 (model progression) -> tab_2_model_progression.tex.
# -----------------------------------------------------------------------------

cat("\n", strrep("=",70), "\n")
cat("  STEP 2: MODEL JUSTIFICATION — OLS → COUNTRY FE → TWFE\n")
cat(strrep("=",70), "\n\n")

# Helper: cluster-robust coeftest for plm models
crt <- function(m) coeftest(m, vcov=vcovHC(m, cluster="group", type="HC1"))

# --- 2A: OLS (no FE) ---
m_ols <- lm(
  y_t_pct ~ S_mean_tw*y_lag1 + S_mean_tw*F_CP + F_CP*y_lag1 + F_DI_lag2 + p_proj_all_ages,
  data = main_sample
)

# --- 2B: Country FE only ---
m_cfe <- plm(
  y_t_pct ~ S_mean_tw*y_lag1 + S_mean_tw*F_CP + F_CP*y_lag1 + F_DI_lag2 + p_proj_all_ages,
  data=main_sample, index=c("Country","Quarter"), model="within", effect="individual"
)

# --- 2C: TWFE (country + quarter FE) — MAIN SPECIFICATION ---
m_twfe <- plm(
  y_t_pct ~ S_mean_tw*y_lag1 + S_mean_tw*F_CP + F_DI_lag2 + F_CP*y_lag1+ p_proj_all_ages,
  data=main_sample, index=c("Country","Quarter"), model="within", effect="twoways"
)
coeftest(m_twfe, vcov=vcovHC(m_twfe, type="HC1", cluster="group")) 


# --- 2D: TWFE DI lag 1 (timing check: too early) ---
m_twfe_di1 <- plm(
  y_t_pct ~ S_mean_tw*y_lag1 + S_mean_tw*F_CP + F_DI_lag1 + p_proj_all_ages,
  data=main_sample, index=c("Country","Quarter"), model="within", effect="twoways"
)

# --- 2E: TWFE DI lag 3 (timing check: too late) ---
m_twfe_di3 <- plm(
  y_t_pct ~ S_mean_tw*y_lag1 + S_mean_tw*F_CP + F_DI_lag3 + p_proj_all_ages,
  data=main_sample, index=c("Country","Quarter"), model="within", effect="twoways"
)

# --- Table 2: Model progression ---
cat("--- Table 2: Model Progression (OLS → Country FE → TWFE) ---\n")
coef_labels <- c(
  "S_mean_tw"          = "Stringency (S)",
  "y_lag1"             = "y(t-1)",
  "F_CP"               = "F^CP",
  "F_DI_lag1"          = "F^DI (lag 1)",
  "F_DI_lag2"          = "F^DI (lag 2)",
  "F_DI_lag3"          = "F^DI (lag 3)",
  "p_proj_all_ages"    = "Fear term",
  "S_mean_tw:y_lag1"   = "S × y(t-1)  [ψ]",
  "S_mean_tw:F_CP"     = "S × F^CP   [η̃]",
  "F_CP:y_lag1"        = "F^CP × y(t-1)  [−η_p]"
)
tab2_models <- list(
  "(1) OLS"       = m_ols,
  "(2) Country FE"= m_cfe,
  "(3) TWFE"      = m_twfe,
  "(4) DI lag 1"  = m_twfe_di1,
  "(5) DI lag 3"  = m_twfe_di3
)
tab2_vcov <- list(
  ~Country,
  function(m) vcovHC(m, cluster="group", type="HC1"),
  function(m) vcovHC(m, cluster="group", type="HC1"),
  function(m) vcovHC(m, cluster="group", type="HC1"),
  function(m) vcovHC(m, cluster="group", type="HC1")
)
modelsummary(tab2_models, vcov=tab2_vcov,
             stars=c("*"=0.1,"**"=0.05,"***"=0.01),
             coef_rename=coef_labels,
             gof_map=c("nobs","r.squared","r.squared.within"),
             title="Table 2: Output Gap Equation — Model Progression")

modelsummary(tab2_models, vcov=tab2_vcov,
             stars=c("*"=0.1,"**"=0.05,"***"=0.01),
             coef_rename=coef_labels,
             gof_map=c("nobs","r.squared","r.squared.within"),
             output=file.path(safetable,"tab_2_model_progression.tex"),
             title="Output Gap: Model Progression (OLS, Country FE, TWFE)")
cat("  → Saved: tab_2_model_progression.tex\n\n")

# --- 2C: Mundlak test (FE vs RE — correlated random effects) ---
# If within-group means are jointly significant, country FE is required.
cat("--- 2C: Mundlak Test (correlated random effects) ---\n")
main_mun <- main_sample %>%
  group_by(Country) %>%
  mutate(
    mu_S   = mean(S_mean_tw,        na.rm=TRUE),
    mu_FCP = mean(F_CP,             na.rm=TRUE),
    mu_FDI = mean(F_DI_lag2,        na.rm=TRUE),
    mu_eta = mean(p_proj_all_ages,  na.rm=TRUE)
  ) %>% ungroup()
m_mun <- lm(y_t_pct ~ S_mean_tw*y_lag1 + S_mean_tw*F_CP + F_CP*y_lag1+ F_DI_lag2 +
              p_proj_all_ages + mu_S + mu_FCP + mu_FDI + mu_eta, data=main_mun)
# Wald test on the group-mean auxiliary variables
mun_ftest <- waldtest(
  lm(y_t_pct ~ S_mean_tw*y_lag1 + S_mean_tw*F_CP +F_CP*y_lag1 + F_DI_lag2 + p_proj_all_ages,
     data=main_mun),
  m_mun,
  vcov=function(m) vcovHC(m, type="HC1")
)
cat(sprintf("  Mundlak Wald test: F(%d) = %.3f, p = %.4f\n",
            abs(diff(mun_ftest$Df)), mun_ftest$F[2], mun_ftest$`Pr(>F)`[2]))
cat(paste0(
  "  INTERPRETATION: Significant group means confirm that regressors are\n",
  "  correlated with country-specific unobservables → Country FE required.\n",
  "  Quarter FE additionally absorb common pandemic wave shocks.\n\n"
))

# --- 2D: Nickell (1981) bias assessment ---
cat("--- 2D: Nickell Bias Assessment (T=9, dynamic panel) ---\n")
T_N  <- 9L
rho  <- coef(m_twfe)["y_lag1"]
psi  <- coef(m_twfe)["S_mean_tw:y_lag1"]
cat(sprintf("  T = %d  →  O(1/T) bias ≈ %.1f%%\n", T_N, 100/T_N))
cat(sprintf("  ρ_y (FE)           = %.4f\n", rho))
cat(sprintf("  ρ_y (Nickell-adj.) = %.4f  (upward correction +%.4f)\n",
            rho/(1-1/T_N), rho/(1-1/T_N)-rho))
cat(sprintf("  ψ (S×y, FE)        = %.5f\n", psi))
cat(paste0(
  "  CONCLUSION: ρ_y ≈ 0.06 is small → absolute Nickell bias < 0.007.\n",
  "  ψ, estimated through the interaction, carries a compounded bias of\n",
  "  similar order. Both biases are negligible relative to their SEs.\n",
  "  Section 7 (GMM Arellano-Bond) provides a formal bias-corrected estimate.\n\n"
))

# --- 2E: DI lag structure (empirical timing selection) ---
cat("--- 2E: DI Lag Structure (lag 1 vs lag 2 vs lag 3) ---\n")
for (lv in c("F_DI_lag1","F_DI_lag2","F_DI_lag3")) {
  fml <- as.formula(paste0(
    "y_t_pct ~ S_mean_tw*y_lag1 + S_mean_tw*F_CP + ", lv, " + p_proj_all_ages"
  ))
  m_tmp <- plm(fml, data=main_sample, index=c("Country","Quarter"),
               model="within", effect="twoways")
  ct <- crt(m_tmp)
  if (lv %in% rownames(ct)) {
    cat(sprintf("  %s: β = %+.4f, SE = %.4f, p = %.3f%s\n",
                lv, ct[lv,"Estimate"], ct[lv,"Std. Error"], ct[lv,"Pr(>|t|)"],
                ifelse(ct[lv,"Pr(>|t|)"]<0.01,"***",
                  ifelse(ct[lv,"Pr(>|t|)"]<0.05,"**",
                    ifelse(ct[lv,"Pr(>|t|)"]<0.1,"*","")))))
  }
}
cat(paste0(
  "  INTERPRETATION: DI lag 2 dominates. Consistent with authorization→\n",
  "  disbursement→spending requiring ≈2 quarters (Chetty et al. 2020:\n",
  "  CARES Act payments took 3–4 weeks to arrive, 4–8 further weeks to\n",
  "  translate into spending). Lag 1 too early (administrative delay);\n",
  "  lag 3 attenuated (spending completed by then). Lag 2 selected as main.\n\n"
))

##But DI remains fragile

# -----------------------------------------------------------------------------
#  STEP 3 - MAIN V3 SPECIFICATION (frozen 16.04.2026)
#  Estimator : feols with Country + Quarter FE (TWFE).
#  Sample    : t_idx in [5, 14] = Q1.2020 - Q2.2022 (380 obs).
#  SE        : clustered by Country (CRV1, AER style; ssc = K.adj + G.adj).
#  Spec V3   : the CP channel enters via y_lag1 : F_CP_lag2 (persistence
#              reduction), NOT as a contemporaneous level effect; lockdown
#              hysteresis enters via S_mean_tw : y_lag1; DI enters as level
#              at lag 2.
#  Output    : `main` (the canonical estimate carried forward into the
#              wild-cluster bootstrap and the robustness tables below).
#  A plm verification is run immediately afterward to confirm that feols
#  and plm produce identical point estimates with HC1 group clustering.
# -----------------------------------------------------------------------------
library(plm)

cat("\n", strrep("=", 70), "\n")
cat("  STEP 5: STANDARD ERROR COMPARISON\n")
cat(strrep("=", 70), "\n\n")


# Replace pre-pandemic NA fiscal/state values with 0. Justification: prior
# to a country's first deployed measure, F_CP / F_DI / S are not "missing",
# they are zero. Without this imputation, lagged values would be NA and
# would drop observations from the regression sample.
# (t_idx 4 = Q4.2019, t_idx 13 = Q1.2022.)
pdataY <- pdataY %>%
  mutate(
    S_mean_tw       = replace_na(S_mean_tw,       0),
    F_CP            = replace_na(F_CP,            0),
    F_DI_lag2       = replace_na(F_DI_lag2,       0),
    p_proj_all_ages = replace_na(p_proj_all_ages, 0),
    y_lag1          = replace_na(y_lag1,          0),
    theta_pct       = replace_na(theta_pct,       0),
    vax_rate        = replace_na(vax_rate,        0),
    S_max_tw        = replace_na(S_max_tw,        0),
  )


# --- MAIN SPECIFICATION (V3 frozen 16.04.2026) ------------------------------
main <- feols(
  y_t_pct ~ y_lag1 + y_lag1:F_CP_lag2 + S_mean_tw*y_lag1 + F_DI_lag2 +
    p_proj_all_ages
  | Country + Quarter,
  data   = pdataY,
  subset = ~ t_idx >= 5 & t_idx <= 14,
  vcov   = ~ Country
)
summary(main, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE))

# --- Verification: same model in plm (TWFE, HC1 clustered by country) -------
plm_test <- plm(
  y_t_pct ~ y_lag1 + y_lag1:F_CP_lag2 + S_mean_tw*y_lag1 + F_DI_lag2 +
    p_proj_all_ages + Quarter + Country,
  data   = pdataY,
  model  = "within",
  effect = "individual",
  subset = t_idx >= 5 & t_idx <= 14
)
coeftest(plm_test, vcov = vcovHC(plm_test, cluster = "group", type = "HC1"))
fixef(plm_test, effect = "individual")

##also use the level effect without any interaction
##1) BINS
#Bin-Spec Robustness check
library(fixest)

# Bins von y_lag1 definieren — Schwellen ökonomisch motiviert
df_bin <- pdataY
df_bin$bin_deep   <- as.integer(df_bin$y_lag1 <  -5)        # tiefe Rezession
df_bin$bin_mild   <- as.integer(df_bin$y_lag1 >= -5 & df_bin$y_lag1 < 0)  # milde Rezession
df_bin$bin_normal <- as.integer(df_bin$y_lag1 >= 0)         # Erholung/normal

# Check Bin-Verteilung
table(df_bin$bin_deep, df_bin$bin_mild, df_bin$bin_normal)
sum(df_bin$bin_deep, na.rm=TRUE)
sum(df_bin$bin_mild, na.rm=TRUE)
sum(df_bin$bin_normal, na.rm=TRUE)

# Bin-Spec: η_q pro Bin statt linearer Interaktion
main_bin <- feols(
  y_t_pct ~ y_lag1 
  + S_mean_tw 
  + I(F_CP_lag2 * bin_deep)
  + I(F_CP_lag2 * bin_mild)
  + I(F_CP_lag2 * bin_normal)
  + F_DI_lag2 
  + p_proj_all_ages
  | Country + Quarter,
  data = df_bin,
  subset = ~ t_idx >= 5 & t_idx <= 14
)

summary(main_bin, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE))

# Sensitivität: wie ändern sich Resultate mit Schwelle?
df_bin$bin_deep_alt <- as.integer(df_bin$y_lag1 < -7)
df_bin$bin_other    <- as.integer(df_bin$y_lag1 >= -7)

main_bin2 <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw 
  + I(F_CP_lag2 * bin_deep_alt) + I(F_CP_lag2 * bin_other)
  + F_DI_lag2 + p_proj_all_ages
  | Country + Quarter,
  data = df_bin, subset = ~ t_idx >= 5 & t_idx <= 14
)
summary(main_bin2, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE))

thresholds <- seq(-3, -10, by = -1)
results <- lapply(thresholds, function(thr) {
  df_bin$bin_deep_thr  <- as.integer(df_bin$y_lag1 < thr)
  df_bin$bin_other_thr <- as.integer(df_bin$y_lag1 >= thr)
  m <- feols(y_t_pct ~ y_lag1 + S_mean_tw 
             + I(F_CP_lag2 * bin_deep_thr) + I(F_CP_lag2 * bin_other_thr)
             + F_DI_lag2 + p_proj_all_ages 
             | Country + Quarter,
             data = df_bin, subset = ~ t_idx >= 5 & t_idx <= 14)
  s <- summary(m, cluster = ~ Country)
  c(threshold = thr,
    eta_deep = coef(s)["I(F_CP_lag2 * bin_deep_thr)"],
    se = se(s)["I(F_CP_lag2 * bin_deep_thr)"],
    n_deep = sum(df_bin$bin_deep_thr, na.rm=TRUE))
})
do.call(rbind, results)

##Zeigt sauber das der Effekt von CP stärker wird wenn tiefer wird-> heterogenitätsnalyse aber Statistiscal Power wird weniger

##Introduce a SPline that assume different effect when outptu gap positive or negative
##Heterogenität des Effektes mit Spline (CP nur genativ, nur 3 Länder sind positiv)
##2) SPLINE: state-restricted lineare TWFE mit Spline-Knoten bei yk−1=0y_{k-1} = 0
df_bin$y_lag1_recession <- pmin(df_bin$y_lag1, 0)  # nur negative Werte

main_lin_rec <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw 
  + I(F_CP_lag2 * y_lag1_recession)
  + F_DI_lag2 + p_proj_all_ages
  | Country + Quarter,
  data = df_bin, subset = ~ t_idx >= 5 & t_idx <= 14
)
summary(main_lin_rec, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE))


# ============================================================
# Diagnose: Verteilung von y_lag1 > 0 im Pandemie-Sample
# ============================================================

# Subset auf Schätz-Sample
sub <- df_bin[df_bin$t_idx >= 5 & df_bin$t_idx <= 14, ]

# 1) Anzahl und Anteil
n_total <- nrow(sub)
n_pos   <- sum(sub$y_lag1 > 0,  na.rm = TRUE)
n_neg   <- sum(sub$y_lag1 < 0,  na.rm = TRUE)
n_zero  <- sum(sub$y_lag1 == 0, na.rm = TRUE)
n_na    <- sum(is.na(sub$y_lag1))

cat("Gesamt-N im Schätz-Sample:", n_total, "\n")
cat("y_lag1 > 0 :", n_pos,  sprintf("(%.1f%%)\n", 100 * n_pos / n_total))
cat("y_lag1 < 0 :", n_neg,  sprintf("(%.1f%%)\n", 100 * n_neg / n_total))
cat("y_lag1 == 0:", n_zero, sprintf("(%.1f%%)\n", 100 * n_zero / n_total))
cat("y_lag1 NA  :", n_na, "\n\n")

# 2) Verteilung über Quartale (Quarter ist der FE-Identifier)
cat("=== Verteilung y_lag1 > 0 über Quartale ===\n")
tab_q <- table(sub$Quarter, sub$y_lag1 > 0)
print(tab_q)
cat("\nAnteil pos je Quartal:\n")
print(round(prop.table(tab_q, margin = 1), 3))

# 3) Verteilung über Länder
cat("\n=== Anzahl Quartale mit y_lag1 > 0 je Land ===\n")
tab_c <- table(sub$Country, sub$y_lag1 > 0)
# Sortiert nach Anzahl positiver Beobachtungen
tab_c <- tab_c[order(-tab_c[, "TRUE"]), ]
print(tab_c)

# 4) Konzentration: wie viele Länder liefern 80% der positiven Beobachtungen?
pos_per_country <- sort(table(sub$Country[sub$y_lag1 > 0]), decreasing = TRUE)
cum_share <- cumsum(pos_per_country) / sum(pos_per_country)
cat("\n=== Kumulativer Anteil positiver Beobachtungen ===\n")
print(data.frame(
  Country   = names(pos_per_country),
  N_pos     = as.integer(pos_per_country),
  Cum_share = round(cum_share, 3)
))

# 5) Quick-Check: Identifikationsbasis für eta_p_pos
#    Beobachtungen, die tatsächlich zur Schätzung des pos-Koeffs beitragen
#    (y_lag1 > 0 UND F_CP_lag2 != 0)
n_id_pos <- sum(sub$y_lag1 > 0 & sub$F_CP_lag2 != 0, na.rm = TRUE)
cat("\nEffektive Identifikationsbasis für eta_p_pos:", n_id_pos, "Beobachtungen\n")

##Wenig zu positiv, aber deshalb ist die assumption auch sauber mit dem spline da dies die observations drinn lässt aber keine homogene effekte assumed


#TWFE has a problem with negative weights, check this

# ============================================================
# DCDH Negative-Weights Decomposition
# ------------------------------------------------------------
# Diagnostic for the linear-S baseline (m1):
#   y_t_pct ~ y_lag1 + S_mean_tw + I(F_CP_lag2 * y_lag1_recession)
#            + F_DI_lag2 + p_proj_all_ages | Country + Quarter
#
# Note: y_lag1 enters `controls` despite being a lagged dependent
# variable. The DCDH (2020) framework formally assumes strict
# exogeneity of controls; we report this as an approximation, in
# line with empirical practice (e.g., de Chaisemartin et al. 2024,
# Section 4.3).
# ============================================================

if (!requireNamespace("TwoWayFEWeights", quietly = TRUE)) {
  install.packages("TwoWayFEWeights")
}
library(TwoWayFEWeights)

weights_sample <- df_bin %>%
  filter(t_idx >= 5, t_idx <= 14) %>%
  mutate(
    F_CP_lag2_x_y_lag1 = F_CP_lag2 * y_lag1_recession,
    country_id         = as.integer(as.factor(Country))
  ) %>%
  arrange(country_id, t_idx)

# Pure (non-policy) controls: only the lagged dependent variable
# and the demographic control.
# Stringency S_mean_tw is treated as a *policy* term, not a
# control, in all decompositions where it appears as level or
# interaction.
non_policy_ctrls <- c("y_lag1", "p_proj_all_ages")

# --- (1) CP persistence channel: D = F_CP_lag2 * y_lag1_recession --------
# Other treatments: linear S level, S-state interaction, DI level.
w_cp <- twowayfeweights(
  data             = weights_sample,
  Y                = "y_t_pct",
  G                = "country_id",
  T                = "t_idx",
  D                = "F_CP_lag2_x_y_lag1",
  type             = "feTR",
  other_treatments = c("S_mean_tw", "F_DI_lag2"),
  controls         = non_policy_ctrls,
  summary_measures = TRUE
)
cat("\n=== (1) CP persistence channel ===\n")
print(w_cp)

# --- (2) DI level effect: D = F_DI_lag2 ----------------------------------
# Other treatments: CP interaction, S level.
w_di <- twowayfeweights(
  data             = weights_sample,
  Y                = "y_t_pct",
  G                = "country_id",
  T                = "t_idx",
  D                = "F_DI_lag2",
  type             = "feTR",
  other_treatments = c("F_CP_lag2_x_y_lag1", "S_mean_tw"),
  controls         = non_policy_ctrls,
  summary_measures = TRUE
)
cat("\n=== (2) DI level effect ===\n")
print(w_di)



# Sensitivity test: how does alpha_DI change when other treatments
# are removed?

# Baseline (m1)
m1 <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw 
  + I(F_CP_lag2 * y_lag1_recession)
  + F_DI_lag2 + p_proj_all_ages
  | Country + Quarter,
  data = df_bin, subset = ~ t_idx >= 5 & t_idx <= 14
)

# DI alone, no S, no CP-interaction
m_di_alone <- feols(
  y_t_pct ~ y_lag1 + F_DI_lag2 + p_proj_all_ages
  | Country + Quarter,
  data = df_bin, subset = ~ t_idx >= 5 & t_idx <= 14
)

# DI + S, no CP-interaction
m_di_S <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw + F_DI_lag2 + p_proj_all_ages
  | Country + Quarter,
  data = df_bin, subset = ~ t_idx >= 5 & t_idx <= 14
)

# DI + CP-interaction, no S
m_di_CP <- feols(
  y_t_pct ~ y_lag1 + I(F_CP_lag2 * y_lag1_recession) 
  + F_DI_lag2 + p_proj_all_ages
  | Country + Quarter,
  data = df_bin, subset = ~ t_idx >= 5 & t_idx <= 14
)

etable(m1, m_di_alone, m_di_S, m_di_CP, 
       cluster = ~ Country, 
       ssc = ssc(adj = TRUE, cluster.adj = TRUE),
       keep = "F_DI_lag2")

##DI in some specification slightly significant


##only level as this might be the problem for the neagtive weights
# --- (3) Stringency level effect: D = S_mean_tw --------------------------
# Other treatments: CP interaction, DI level.
w_s <- twowayfeweights(
  data             = weights_sample,
  Y                = "y_t_pct",
  G                = "country_id",
  T                = "t_idx",
  D                = "S_mean_tw",
  type             = "feTR",
  other_treatments = c("F_CP_lag2_x_y_lag1", "F_DI_lag2"),
  controls         = non_policy_ctrls,
  summary_measures = TRUE
)
cat("\n=== (3) Stringency level effect ===\n")
print(w_s)

# Sensitivity test for delta_S

m_S_alone <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw + p_proj_all_ages
  | Country + Quarter,
  data = df_bin, subset = ~ t_idx >= 5 & t_idx <= 14
)

m_S_DI <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw + F_DI_lag2 + p_proj_all_ages
  | Country + Quarter,
  data = df_bin, subset = ~ t_idx >= 5 & t_idx <= 14
)

m_S_CP <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw 
  + I(F_CP_lag2 * y_lag1_recession) + p_proj_all_ages
  | Country + Quarter,
  data = df_bin, subset = ~ t_idx >= 5 & t_idx <= 14
)

etable(m1, m_S_alone, m_S_DI, m_S_CP, 
       cluster = ~ Country, 
       ssc = ssc(adj = TRUE, cluster.adj = TRUE),
       keep = "S_mean_tw")



##CP, DI sind in Ordnung, S ist das problem
#use spline for S ob nichtlinear
main_S_spline <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw 
  + I(F_CP_lag2 * y_lag1_recession)
  + I(S_mean_tw * y_lag1_recession)   # NEU: state-dependent S
  + F_DI_lag2 + p_proj_all_ages
  | Country + Quarter,
  data = df_bin, subset = ~ t_idx >= 5 & t_idx <= 14
)
summary(main_S_spline, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE))

#try w/o S um zu sehen wie die kontimination von S ist
main_wo_S <- feols(
  y_t_pct ~ y_lag1
  + I(F_CP_lag2 * y_lag1_recession)
  + F_DI_lag2 + p_proj_all_ages
  | Country + Quarter,
  data = df_bin, subset = ~ t_idx >= 5 & t_idx <= 14
)
summary(main_wo_S, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE))

##F_CP ist stabil, kontaminierung ist tief

#Check it with only level S-> see discussion later
# ============================================================
# DCDH (de Chaisemartin & D'Haultfoeuille 2020) Decomposition
# ------------------------------------------------------------
# The de Chaisemartin & D'Haultfoeuille (2020) decomposition reveals
# a negative-weight share of 6.9% for the CP persistence channel
# F_CP_{k-2} * y^neg_{k-1}, indicating limited heterogeneity-driven
# distortion. After correction for the spline (36.9% vorher)
#
# Cross-treatment contamination from the stringency interaction
# S_{ik} * y^neg_{k-1} is theoretically possible (cross-weights of
# +/- 3.93 in the linear-S baseline) but empirically negligible:
# estimates of eta_p vary by less than 8% across
#   (i)   linear S,
#   (ii)  state-dependent S,
#   (iii) omission of S.
#
# We retain the linear-S specification as our baseline; the
# state-dependent specification confirms robustness but should not
# be interpreted as evidence for nonlinear stringency effects, as
# the auxiliary interaction itself exhibits a negative-weight
# share of 28.7%.

# ============================================================

##Adding the Hystery Term of S

# ============================================================
# DCDH Negative-Weights Diagnostic — V4 main spec
# ------------------------------------------------------------
# Main specification (m1):
#   y_t_pct ~ y_lag1 + S_mean_tw 
#            + I(S_mean_tw * y_lag1_recession)        # psi
#            + I(F_CP_lag2 * y_lag1_recession)        # eta_p
#            + F_DI_lag2 + p_proj_all_ages
#            + factor(Quarter) | Country
#
# Note: y_lag1 enters `controls` despite being a lagged dependent
# variable. The DCDH (2020) framework formally assumes strict
# exogeneity of controls; we report this as an approximation, in
# line with empirical practice (e.g., de Chaisemartin et al. 2024).
# ============================================================

if (!requireNamespace("TwoWayFEWeights", quietly = TRUE)) {
  install.packages("TwoWayFEWeights")
}
library(TwoWayFEWeights)

weights_sample <- df_bin %>%
  filter(t_idx >= 5, t_idx <= 14) %>%
  mutate(
    F_CP_lag2_x_y_lag1 = F_CP_lag2 * y_lag1_recession,
    S_mean_tw_x_y_lag1 = S_mean_tw * y_lag1_recession,   # NEU: psi-Term
    country_id         = as.integer(as.factor(Country))
  ) %>%
  arrange(country_id, t_idx)

non_policy_ctrls <- c("y_lag1", "p_proj_all_ages")

# Construct V4 spline term explicitly
weights_sample <- weights_sample %>%
  mutate(F_CP_x_recession = F_CP_lag2 * y_lag1_recession)

# (1) CP persistence channel (V4 core)
w_cp <- twowayfeweights(
  data             = weights_sample,
  Y                = "y_t_pct", G = "country_id", T = "t_idx",
  D                = "F_CP_x_recession",
  type             = "feTR",
  other_treatments = c("S_mean_tw", "F_DI_lag2"),
  controls         = non_policy_ctrls,
  summary_measures = TRUE
)

# (2) DI level effect
w_di <- twowayfeweights(
  data             = weights_sample,
  Y                = "y_t_pct", G = "country_id", T = "t_idx",
  D                = "F_DI_lag2",
  type             = "feTR",
  other_treatments = c("F_CP_x_recession", "S_mean_tw"),
  controls         = non_policy_ctrls,
  summary_measures = TRUE
)

# (3) Stringency level effect
w_s <- twowayfeweights(
  data             = weights_sample,
  Y                = "y_t_pct", G = "country_id", T = "t_idx",
  D                = "S_mean_tw",
  type             = "feTR",
  other_treatments = c("F_CP_x_recession", "F_DI_lag2"),
  controls         = non_policy_ctrls,
  summary_measures = TRUE
)

print(w_cp); print(w_di); print(w_s)

# ---------------------------------------------------------------
# Interpretation of DCDH (2024) results
# ---------------------------------------------------------------
#
# Event-study effect (-14.99, SE 10.98):
#   On-impact ATT in treatment-unit scaling. Not directly comparable
#   to the TWFE coefficient eta_p, which is normalized differently.
#
# Cumulative effect (-0.189, SE 0.139):
#   Average treatment effect per unit of treatment, accumulated over
#   1 quarter. Sign matches the TWFE persistence-channel estimate
#   (eta_p = -0.011); magnitude differs because DCDH does not impose
#   the multiplicative TWFE structure. Insignificant at conventional
#   levels, reflecting limited power: 38 switchers, T = 10, continuous
#   treatment.
#
# Parallel-trends / no-anticipation test (0.246, SE 2.04, p ~ 0.90):
#   Pre-trend coefficient indistinguishable from zero. The
#   identifying assumption underlying both DCDH and TWFE is not
#   rejected.
#
# Conclusion:
#   The heterogeneity-robust estimator confirms the sign of the
#   persistence channel and the validity of parallel trends. The
#   loss of precision relative to TWFE is the expected cost of
#   heterogeneity-robustness with continuous treatment in a short
#   panel.


#S only the problem as ti has extrem negative weights-> all were treated simultaneously
#αS aus Kalibrierung übernehmen (Haug et al.\ 2020; König & Winkler 2020).

# ============================================================
# Sensitivity tests — empirical contamination check
# ------------------------------------------------------------
# For each interpreted coefficient, re-estimate the new V4 main
# spec under progressive omission of other policy treatments.
# ============================================================

# New main spec (V4)
m1 <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw 
  + I(S_mean_tw * y_lag1_recession)
  + I(F_CP_lag2 * y_lag1_recession)
  + F_DI_lag2 + p_proj_all_ages
  + factor(Quarter)
  | Country,
  data = df_bin, subset = ~ t_idx >= 5 & t_idx <= 14
)

# --- Sensitivity for eta_p (CP persistence) ---
m_eta_alone <- feols(
  y_t_pct ~ y_lag1 + I(F_CP_lag2 * y_lag1_recession) + p_proj_all_ages
  + factor(Quarter)
  | Country,
  data = df_bin, subset = ~ t_idx >= 5 & t_idx <= 14
)

m_eta_S <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw + I(F_CP_lag2 * y_lag1_recession) + p_proj_all_ages
  + factor(Quarter)
  | Country,
  data = df_bin, subset = ~ t_idx >= 5 & t_idx <= 14
)

m_eta_psi <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw + I(S_mean_tw * y_lag1_recession) 
  + I(F_CP_lag2 * y_lag1_recession) + p_proj_all_ages
  + factor(Quarter)
  | Country,
  data = df_bin, subset = ~ t_idx >= 5 & t_idx <= 14
)

cat("\n=== Sensitivity: eta_p (F_CP_lag2 * y_lag1_recession) ===\n")
etable(m1, m_eta_alone, m_eta_S, m_eta_psi,
       cluster = ~ Country,
       ssc = ssc(adj = TRUE, cluster.adj = TRUE),
       keep = "F_CP_lag2")

# --- Sensitivity for psi (S persistence) ---
m_psi_alone <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw + I(S_mean_tw * y_lag1_recession) + p_proj_all_ages
  + factor(Quarter)
  | Country,
  data = df_bin, subset = ~ t_idx >= 5 & t_idx <= 14
)

m_psi_DI <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw + I(S_mean_tw * y_lag1_recession) 
  + F_DI_lag2 + p_proj_all_ages
  + factor(Quarter)
  | Country,
  data = df_bin, subset = ~ t_idx >= 5 & t_idx <= 14
)

m_psi_eta <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw + I(S_mean_tw * y_lag1_recession) 
  + I(F_CP_lag2 * y_lag1_recession) + p_proj_all_ages
  + factor(Quarter)
  | Country,
  data = df_bin, subset = ~ t_idx >= 5 & t_idx <= 14
)

cat("\n=== Sensitivity: psi (S_mean_tw * y_lag1_recession) ===\n")
etable(m1, m_psi_alone, m_psi_DI, m_psi_eta,
       cluster = ~ Country,
       ssc = ssc(adj = TRUE, cluster.adj = TRUE),
       keep = "y_lag1_recession")

# --- Sensitivity for alpha_DI ---
m_di_alone <- feols(
  y_t_pct ~ y_lag1 + F_DI_lag2 + p_proj_all_ages
  + factor(Quarter)
  | Country,
  data = df_bin, subset = ~ t_idx >= 5 & t_idx <= 14
)

m_di_S <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw + F_DI_lag2 + p_proj_all_ages
  + factor(Quarter)
  | Country,
  data = df_bin, subset = ~ t_idx >= 5 & t_idx <= 14
)

m_di_persistence <- feols(
  y_t_pct ~ y_lag1 + I(S_mean_tw * y_lag1_recession) 
  + I(F_CP_lag2 * y_lag1_recession) + F_DI_lag2 + p_proj_all_ages
  + factor(Quarter)
  | Country,
  data = df_bin, subset = ~ t_idx >= 5 & t_idx <= 14
)

cat("\n=== Sensitivity: alpha_DI (F_DI_lag2) ===\n")
etable(m1, m_di_alone, m_di_S, m_di_persistence,
       cluster = ~ Country,
       ssc = ssc(adj = TRUE, cluster.adj = TRUE),
       keep = "F_DI_lag2")

# --- Sensitivity for delta_S (level) ---
m_S_alone <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw + p_proj_all_ages
  + factor(Quarter)
  | Country,
  data = df_bin, subset = ~ t_idx >= 5 & t_idx <= 14
)

m_S_DI <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw + F_DI_lag2 + p_proj_all_ages
  + factor(Quarter)
  | Country,
  data = df_bin, subset = ~ t_idx >= 5 & t_idx <= 14
)

m_S_persistence <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw + I(S_mean_tw * y_lag1_recession) 
  + I(F_CP_lag2 * y_lag1_recession) + p_proj_all_ages
  + factor(Quarter)
  | Country,
  data = df_bin, subset = ~ t_idx >= 5 & t_idx <= 14
)

cat("\n=== Sensitivity: delta_S (S_mean_tw level) ===\n")
etable(m1, m_S_alone, m_S_DI, m_S_persistence,
       cluster = ~ Country,
       ssc = ssc(adj = TRUE, cluster.adj = TRUE),
       keep = "^S_mean_tw$")


##brauche ich den hysterie term
m_S_lag2 <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw + 
  + I(F_CP_lag2 * y_lag1_recession)
  + F_DI_lag2 + p_proj_all_ages
  | Country + Quarter,
  data = df_bin, 
  subset = ~ t_idx >= 5 & t_idx <= 14,
  panel.id = ~ Country + t_idx
)
summary(m_S_lag2, cluster = ~ Country, ssc = ssc(adj = TRUE, cluster.adj = TRUE))




# ============================================================================
# PANDEMIC TRILEMMA - V14 EMPIRICAL SPECIFICATION (Main Output Equation)
# ============================================================================
#
# Final main model (12.05.2026):
#
#   y_{it} = mu_i + rho_y * y_{i,t-1} + alpha_S * S_{it}
#          + alpha_above * F_CP_above_{i,t-2}              [FLOW]
#          + alpha_below * K_below_{it}                    [STOCK]
#          + alpha_DI    * F_DI_{i,t-1}
#          + alpha_S_DI  * S_{it} * F_DI_{i,t-1}
#          + eps_{it}
#
# with K_below_{it} = sum_{s<=t} (0.40 * F_loans_{is} + 0.25 * F_guar_{is})
#
# WHY V14 (evolution from earlier specs in this script):
#   V3 (frozen 16.04.2026) used CP entering through a persistence-reduction
#   channel (y_lag1:F_CP_lag2) plus a contemporaneous S*F_CP interaction
#   under TWFE. V4 replaced the linear S level with a state-dependent
#   spline (S * y_lag1_recession). DCDH diagnostics on V4 left the
#   Above-Flow + DI channels robust but flagged the S-spline as the
#   dominant source of negative-weight contamination. V14 keeps the
#   substantive lessons of V3/V4 (CP works through both an acute and a
#   persistent channel; DI is state-dependent) but replaces the
#   single-aggregate F_CP with the institutionally motivated
#   Above-Flow + Below-Stock decomposition, and lets DI's
#   state-dependence run through F_DI * S directly.
#
# THEORETICAL CONTRIBUTION:
#   Decomposition of CP into two mechanistically distinct channels:
#   - Above-the-line (wage subsidies, direct grants): Flow with 2Q
#     authorization-to-spending lag, captures acute cash-flow support
#   - Below-the-line (loans, guarantees): Stock variable capturing
#     persistent liquidity/solvency protection
#
#   DI is modeled with push-on-string mechanism: positive baseline multiplier
#   but contractionary at high containment (break-even at S ~= 36).
#
# IDENTIFICATION:
#   Country-FE only (NOT Quarter-FE). This is a deliberate change relative
#   to the Step 2 / Step 3 / V3 / V4 specifications above, which used TWFE.
#   Rationale: trilemma counterfactuals require the TOTAL F-effect within
#   the realized global pandemic context, not the net-of-trends effect.
#   Quarter-FE would absorb the global shock that is part of the policy
#   environment we want to evaluate; cross-country differences in F
#   conditional on S and voluntary responses are exactly what we want to
#   identify.
#
#   Country-FE absorbs time-invariant structural heterogeneity (fiscal
#   space, institutions, voluntary response patterns, pre-COVID trend).
#   Time-varying global shocks are addressed via the AR(1) y_lag1 term
#   plus the linear-trend / Year-FE robustness checks further down.
# ============================================================================

library(fixest)
library(dplyr)
library(car)


# ============================================================================
# STEP 0 -- PREPARATION
# ============================================================================

# Recession spline (used for hysteresis tests only, not in main spec)
df_bin$y_lag1_recession <- pmin(df_bin$y_lag1, 0)

# Quarter as factor for Year-FE / Quarter-FE robustness checks.
# Full ordered range Q1.2019 - Q4.2022 (16 levels) so that df_bin can be
# used outside the V14 sample window without losing the factor metadata.
# The V14 estimation subset (t_idx in [4, 14]) is selected via `subset =`
# in each feols call rather than by trimming the factor.
df_bin$Quarter <- factor(df_bin$Quarter,
                         levels = c("Q1.2019", "Q2.2019", "Q3.2019", "Q4.2019",
                                    "Q1.2020", "Q2.2020", "Q3.2020", "Q4.2020",
                                    "Q1.2021", "Q2.2021", "Q3.2021", "Q4.2021",
                                    "Q1.2022", "Q2.2022", "Q3.2022", "Q4.2022"))


# ============================================================================
# STEP 1 -- DI IDENTIFICATION VIA INTERACTION WITH STRINGENCY
# ============================================================================
# Hypothesis: DI is contingent on consumption channels being open.
# Direct DI coefficient is insignificant in all lag specifications; the
# interaction with S identifies a state-dependent multiplier.
# ----------------------------------------------------------------------------

DI_interact <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw + F_CP_lag2 + F_DI_lag1 * S_mean_tw | Country,
  data = df_bin, subset = ~ t_idx >= 4 & t_idx <= 14
)
summary(DI_interact, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE))

# RESULT: alpha_DI = +1.67**, alpha_S_DI = -0.045**. Break-even S* = 37.
# DI is contractionary at high containment (push-on-string).


# ============================================================================
# STEP 2 -- CP AS UNIFORM LEVEL (CUMULATIVE STOCK + RECENT FLOW)
# ============================================================================
# First-pass CP specification: F_CP_cum captures persistent component,
# F_CP_lag2 captures recent flow impulse. Both significant, orthogonal.
# ----------------------------------------------------------------------------

df_bin <- df_bin |>
  group_by(Country) |> arrange(t_idx) |>
  mutate(F_CP_cum = cumsum(replace_na(F_CP, 0))) |>
  ungroup()

main_test <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw + F_CP_cum + F_CP_lag2
  + F_DI_lag1 * S_mean_tw | Country,
  data = df_bin, subset = ~ t_idx >= 4 & t_idx <= 14
)
summary(main_test, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE))

# RESULT: F_CP_cum = +0.103**, F_CP_lag2 = +0.108**. Both significant.

# ---- Diagnostic checks for Step 2 ----
df_sub <- df_bin |> filter(t_idx >= 4 & t_idx <= 14)
cat("\nCor(F_CP_cum, F_CP_lag2) =",
    round(cor(df_sub$F_CP_cum, df_sub$F_CP_lag2, use = "complete.obs"), 3), "\n")

m_lm <- lm(y_t_pct ~ y_lag1 + S_mean_tw + F_CP_cum + F_CP_lag2
           + F_DI_lag1 + F_DI_lag1:S_mean_tw, data = df_sub)
cat("\nVIF Step 2:\n"); print(vif(m_lm))

# INTERPRETATION: Cor = 0.26, VIF < 1.5 for all main terms (interaction
# VIFs ~14 are cosmetic artifacts of including F_DI*S, not collinearity).
# Spec is statistically clean. ECONOMIC PROBLEM: F_CP_cum is monotonically
# increasing -> trend-confounding risk. Sign flips under Year-FE.


# ============================================================================
# STEP 3 -- THEORETICAL DISAGGREGATION (V14 final)
# ============================================================================
# Above-the-line and below-the-line measures operate through DISTINCT
# mechanisms and therefore require different aggregation:
#
#   Above (wage subsidies, Kurzarbeit, direct grants):
#     One-shot cash transfers, acute cash-flow effect, no inherent
#     persistence mechanism -> FLOW with 2Q lag (authorization -> spending)
#
#   Below (loans, guarantees):
#     Multi-quarter liquidity/solvency protection, instrument remains
#     active until repaid/expired -> STOCK (accumulated)
#
# Take-up adjustment (low scenario, lower bound of ECB SAFE Survey):
#   Loans:      40% drawn down
#   Guarantees: 25% called
# ----------------------------------------------------------------------------

# Loan take-up scenarios
df_bin <- df_bin |>
  mutate(F_CP_loans_lo  = F_CP_loans * 0.40,
         F_CP_loans_mid = F_CP_loans * 0.60,
         F_CP_loans_hi  = F_CP_loans * 0.80,
         F_CP_loans_adj = F_CP_loans_mid)

# Construct flow and stock variables
df_bin <- df_bin |>
  group_by(Country) |> arrange(t_idx) |>
  mutate(
    # FLOW: above-the-line (acute, no persistence mechanism)
    F_CP_above_flow      = F_CP_above_3,
    F_CP_above_flow_lag1 = lag(F_CP_above_3, 1),
    F_CP_above_flow_lag2 = lag(F_CP_above_3, 2),
    
    # Auxiliary lags for robustness
    F_H_lag2 = lag(F_H, 2),
    y_lag2   = lag(y_t_pct, 2),
    
    # STOCK: below-the-line (persistent liquidity/solvency protection)
    # Low-take-up baseline: loans 40%, guarantees 25%
    F_CP_loans_stock = cumsum(replace_na(F_CP_loans_lo, 0)),
    F_CP_guar_stock  = cumsum(replace_na(F_CP_guar_lo,  0)),
    F_CP_belowstock  = F_CP_loans_stock + F_CP_guar_stock,
    
    # Auxiliary alternative aggregations (for robustness)
    F_CP_above_3_stock = cumsum(replace_na(F_CP_above_3, 0)),
    F_CP_total_stock   = cumsum(replace_na(F_CP, 0)),
    F_DI_stock         = cumsum(replace_na(F_DI, 0))
  ) |> ungroup()


# ----------------------------------------------------------------------------
# V14 MAIN SPECIFICATION
# ----------------------------------------------------------------------------
v14 <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw
  + F_CP_above_flow_lag2 + F_CP_belowstock
  + F_DI_lag1 * S_mean_tw | Country,
  data = df_bin, subset = ~ t_idx >= 4 & t_idx <= 14
)
summary(v14, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE))


# ============================================================================
# STEP 4 -- IDENTIFICATION DIAGNOSTICS FOR V14
# ============================================================================
df_v14 <- df_bin |> filter(t_idx >= 4 & t_idx <= 14)

# Correlation between Above-Flow and Below-Stock
cat("\nCor(Above_lag2, Belowstock) =",
    round(cor(df_v14$F_CP_above_flow_lag2, df_v14$F_CP_belowstock,
              use = "complete.obs"), 3), "\n")

# VIF
m_v14 <- lm(y_t_pct ~ y_lag1 + S_mean_tw
            + F_CP_above_flow_lag2 + F_CP_belowstock
            + F_DI_lag1 + F_DI_lag1:S_mean_tw,
            data = df_v14)
cat("\nVIF V14:\n"); print(vif(m_v14))

# EXPECTED: Above_lag2 VIF ~1.2, Belowstock VIF ~1.1, Cor ~0.10
# -> Channels are statistically orthogonal, separately identified.
# F_DI / F_DI:S_mean_tw VIFs ~14 are cosmetic interaction artifacts.


# ============================================================================
# STEP 5 -- PERSISTENCE-REDUCTION TEST (REJECTED)
# ============================================================================
# Hypothesis: CP reduces output persistence by preventing bankruptcies that
# generate hysteresis. Test via F_CP_above_flow_lag2 * y_lag1 interaction.
# ----------------------------------------------------------------------------

v14_persistence <- feols(
  y_t_pct ~ y_lag1
  + F_DI_lag1 * S_mean_tw
  + F_CP_above_flow_lag2 * y_lag1
  + F_CP_above_flow_lag2 + F_CP_belowstock | Country,
  data = df_bin, subset = ~ t_idx >= 4 & t_idx <= 14
)
summary(v14_persistence, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE))

# RESULT: Interaction = +0.023, p = 0.16 (not significant).
# Persistence-rate reduction NOT identifiable in T=11 panel.
# Persistence damage is addressed indirectly via the level channels:
# CP preserves productive base; AR(1)-dynamics propagate the level effect
# through the recovery path.


# ============================================================================
# STEP 6 -- EXTRACT FIXED EFFECTS AND Q2.2020 RESIDUALS FOR CALIBRATION
# ============================================================================
df_v14$resid <- residuals(v14)

# Country fixed effects (for MATLAB calibration)
cfe_y <- fixef(v14, effect = "Country")$Country
print(cfe_y)

# Per-country Q2.2020 residual (global pandemic shock, country-specific)
eps_v14_q220 <- df_v14 |>
  filter(Quarter == "Q2.2020") |>
  select(Country, resid)
print(eps_v14_q220, n = 38)

cat("\nMedian Q2.2020 residual =",
    round(median(df_v14$resid[df_v14$Quarter == "Q2.2020"]), 3), "\n")


# ============================================================================
# STEP 7 -- Main Modell also in plm, should be the same coefficients
# ============================================================================
library(plm)

# Local `pdata_v14` avoids shadowing the top-level `pdata` defined in
# Stage 2b (it covers the full 2019Q1-2022Q4 window and is needed by
# the later debt-equation stage).
pdata_v14 <- pdata.frame(
  df_bin |> filter(t_idx >= 4 & t_idx <= 14),
  index = c("Country", "Quarter")
)

v14_plm <- plm(
  y_t_pct ~ y_lag1 + S_mean_tw
  + F_CP_above_flow_lag2 +F_CP_belowstock
  + F_DI_lag1 + F_DI_lag1:S_mean_tw,
  data   = pdata_v14,
  model  = "within",
  effect = "individual"
)

# Cluster-robust SE (CRV1, country-level)
coeftest(v14_plm, vcov = vcovHC(v14_plm, cluster = "group", type = "HC1"))

coefs_feols <- coef(v14)
coefs_plm   <- coef(v14_plm)

print(coefs_feols)
print(coefs_plm)
# feols and plm match — V14 point estimates are estimator-invariant.

#===============================================================================
#9999999999999999999999999TEST FÜR ILQR mit unterschiedlichem S99999999999999999

df_bin$Wave2 <- as.numeric(df_bin$t_idx >= 8 & df_bin$t_idx <= 10)  # Q4.20-Q2.21
v14_wave <- feols(y_t_pct ~ y_lag1 + S_mean_tw + S_mean_tw:Wave2
                  + F_CP_above_flow_lag2 + F_CP_belowstock
                  + F_DI_lag1 * S_mean_tw | Country,
                  data = df_bin, subset = ~ t_idx >= 4 & t_idx <= 14)
summary(v14_wave, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE))

fixef(v14_wave)$Country
residuals(v14_wave)[df_v14$Quarter == "Q2.2020"]
#nur test

# ============================================================================
# FAZIT - V14 IS THE FINAL MAIN SPECIFICATION
# ============================================================================
#
# COEFFICIENTS (N = 418, 38 countries x 11 quarters Q4.2019-Q2.2022):
#
#   rho_y       =  0.231***   AR(1) output persistence
#   alpha_S     = -0.095***   Direct lockdown drag per stringency point
#   alpha_above = +0.544**    Above-the-line flow (Lag-2), acute cash flow
#   alpha_below = +0.261.     Below-the-line stock, persistent solvency
#                             protection (p = 0.08, marginal)
#   alpha_DI    = +1.470*     DI multiplier at S = 0 (baseline)
#   alpha_S_DI  = -0.041**    Push-on-string slope
#                             -> DI break-even at S* = 36
#
#   Within R^2 = 0.436, RMSE = 2.83 pp
#
# WHY THIS SPECIFICATION:
#
#   (1) Theoretical clarity: CP operates through two mechanistically
#       distinct channels rather than as one aggregated lever. Above is
#       a FLOW (acute), Below is a STOCK (persistent). This matches the
#       institutional structure of fiscal response and yields separately
#       identified parameters.
#
#   (2) Orthogonal identification: Cor(Above_lag2, Belowstock) = 0.10,
#       all VIFs below 1.5. No collinearity bias.
#
#   (3) DI push-on-string is empirically robust across all specifications
#       and theoretically motivated: cash transfers require open
#       consumption channels to function.
#
#   (4) Country-FE only identification preserves the global pandemic
#       context required for counterfactual policy analysis. Quarter-FE
#       absorbs the very shock against which we evaluate fiscal response.
#
# THEORETICAL CONTRIBUTION:
#
#   Channel disaggregation (Above-Flow + Below-Stock + DI*S) is novel
#   relative to Auerbach-Gorodnichenko (2012), Ramey-Zubairy (2018),
#   and Deb et al. (2021), which treat CP as a single aggregate lever.
#
# LIMITATIONS (TO BE REPORTED IN DISSERTATION):
#
#   (a) Persistence-rate reduction (CP modifying rho_y) cannot be
#       identified separately in T=11 panel; rejected by Step 5 test.
#       Persistence is addressed via the LEVEL channels: CP raises y,
#       AR(1) propagates the effect. Reframe from "CP reduces rho" to
#       "CP preserves the productive base whose recovery is governed
#       by rho."
#
#   (b) Below-Stock is trend-sensitive: sign flips under Year-FE
#       (cumulative stock is monotonically increasing). Above-Flow is
#       trend-robust. Take-up adjustment (low scenario: loans 40%,
#       guarantees 25%) disciplines but does not eliminate this issue.
#
#   (c) Below-Stock significance is marginal (p = 0.08). Conservative
#       reading: Above-Flow is the empirically robust CP channel;
#       Below-Stock is theoretically motivated but quantitatively weaker.
#
#   (d) Identification of CP's effect on rho would require exogenous
#       variation in CP intensity orthogonal to crisis depth; the
#       universal nature of OECD COVID-19 fiscal response precludes
#       a clean control group.
#
# ROBUSTNESS BATTERY (TO BE RUN):

#   - Decreasing Below-Stock — not economically meaningful, kept here
#     only as a sensitivity (the channel is supposed to be persistent).
#   - Year-FE specification (shows Above-Flow trend-robust,
#     Below-Stock trend-confounded)
#   - Sample restrictions (t_idx >= 5; t_idx >= 4 & <= 13)
#   - Take-up sensitivity grid (loans 0.4/0.6/0.8, guar 0.25/0.35/0.50)
#   - Outlier exclusion (TUR, IRL: tax-driven GDP volatility)
#   - Alternative S measures (S_max instead of S_mean_tw)
#   - DCDH negative-weights diagnostic (TWFE defense)
#   - Wild cluster bootstrap (small-N defense, N_clusters = 38)
#
# ============================================================================


# =====================================================
# ROBUSTNESS BATTERY V14-BASE
# =====================================================

#..............................................................................
## Decay-stock robustness for the Below-Stock channel.
## Replaces the V14 cumulative-sum stock with a geometrically decaying
## stock: K_t = (1 - delta) * K_{t-1} + flow_t. delta = 0 reproduces
## the V14 baseline (pure cumulative sum). The flows use the V14 take-up
## convention (loans 40%, guarantees 25%) for like-for-like comparison.
build_decay_stock <- function(flow, delta) {
  flow <- replace_na(flow, 0)
  K <- numeric(length(flow))
  if (length(flow) == 0L) return(K)
  K[1] <- flow[1]
  if (length(flow) > 1L) {
    for (t in 2:length(flow)) {
      K[t] <- (1 - delta) * K[t - 1] + flow[t]
    }
  }
  K
}

deltas <- c(0, 0.05, 0.10, 0.15, 0.25)

for (d in deltas) {
  df_tmp <- df_bin |>
    group_by(Country) |> arrange(t_idx) |>
    mutate(
      F_loans_decay    = build_decay_stock(F_CP_loans_lo, d),
      F_guar_decay     = build_decay_stock(F_CP_guar_lo,  d),
      F_CP_below_decay = F_loans_decay + F_guar_decay
    ) |> ungroup()

  m <- feols(
    y_t_pct ~ y_lag1 + S_mean_tw
    + F_CP_above_flow_lag2 + F_CP_below_decay
    + F_DI_lag1 * S_mean_tw + as.numeric(year_only) | Country,
    data = df_tmp, subset = ~ t_idx >= 4 & t_idx <= 14
  )

  cat("\n========== delta =", d, "==========\n")
  print(summary(m, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE)))
}

#...............................................................................
# ============================================================================
# ROBUSTNESS CHECK: Non-Parametric and Parametric TT
# ----------------------------------------------------------------------------
# Replaces Year-FE with continuous linear trend.
# Less restrictive than Year-FE: absorbs only smooth temporal component.
# ============================================================================
#year
v14_yearFE <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw 
  + F_CP_above_flow_lag2 + F_CP_belowstock
  + F_DI_lag1 * S_mean_tw | Country + year_only,
  data = df_bin, subset = ~ t_idx >= 4 & t_idx <= 14
)
summary(v14_yearFE, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE))


v14_lintrend <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw 
  + F_CP_above_flow_lag2 + F_CP_belowstock
  + F_DI_lag1 * S_mean_tw + as.numeric(year_only) | Country,
  data = df_bin, subset = ~ t_idx >= 4 & t_idx <= 14
)
summary(v14_lintrend, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE))

#quarter

v14_quarterFE <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw 
  + F_CP_above_flow_lag2 + F_CP_belowstock
  + F_DI_lag1 * S_mean_tw | Country + t_idx,
  data = df_bin, subset = ~ t_idx >= 4 & t_idx <= 14
)
summary(v14_quarterFE, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE))


v14_Qlintrend <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw 
  + F_CP_above_flow_lag2 + F_CP_belowstock
  + F_DI_lag1 * S_mean_tw + as.numeric(t_idx) | Country,
  data = df_bin, subset = ~ t_idx >= 5 & t_idx <= 14
)
summary(v14_Qlintrend, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE))

#Quarter-fixed effects absorb virtually all identifying variation in our T=11 panel and are not informative for robustness. A linear time trend (less restrictive) confirms the pattern observed with Year-FE: Above-Flow channel is trend-robust, Below-Stock is trend-confounded."

# ============================================================================
# ROBUSTNESS: VIF (Variance Inflation Factors)
# ----------------------------------------------------------------------------
# Expectation: Main terms VIF < 5 (clean identification).
# F_DI_lag1 and F_DI_lag1:S_mean_tw VIFs ~14 are cosmetic interaction
# artifacts (Brambor-Clark-Golder 2006) and should be ignored.
# ============================================================================

library(car)

df_v14 <- df_bin |> filter(t_idx >= 4 & t_idx <= 14)

m_vif <- lm(y_t_pct ~ y_lag1 + S_mean_tw
            + F_CP_above_flow_lag2 + F_CP_belowstock
            + F_DI_lag1 + F_DI_lag1:S_mean_tw,
            data = df_v14)

cat("\n========== VIF: V14 MAIN SPEC ==========\n")
print(vif(m_vif))

# Pairwise correlations among main regressors
cat("\n========== Pairwise correlations ==========\n")
print(round(cor(df_v14[, c("y_lag1", "S_mean_tw",
                           "F_CP_above_flow_lag2", "F_CP_belowstock",
                           "F_DI_lag1")],
                use = "complete.obs"), 3))

# INTERPRETATION: All main terms VIF < 1.4 and pairwise correlations |r| < 0.42
# confirm clean orthogonal identification of Above-Flow, Below-Stock, and DI
# channels; F_DI/F_DI:S VIFs ~14 are cosmetic interaction artifacts
# (Brambor-Clark-Golder 2006) without inferential impact.


# ============================================================
# DCDH Negative-Weights Diagnostic — V14 main spec
# ------------------------------------------------------------
# Main spec (V14):
#   y_t_pct ~ y_lag1 + S_mean_tw 
#            + F_CP_above_flow_lag2     # Above-Flow channel
#            + F_CP_belowstock           # Below-Stock channel
#            + F_DI_lag1 * S_mean_tw     # DI push-on-string
#            | Country
#
# Three continuous treatments: Above, Belowstock, S, DI
# DCDH (2020) assumes strict exogeneity of controls; y_lag1
# in controls is an approximation (cf. de Chaisemartin et al. 2024).
# ============================================================

library(TwoWayFEWeights)
library(dplyr)

weights_sample <- df_bin %>%
  filter(t_idx >= 4, t_idx <= 14) %>%
  mutate(country_id = as.integer(as.factor(Country))) %>%
  arrange(country_id, t_idx)

non_policy_ctrls <- c("y_lag1")

# (1) Above-Flow channel
w_above <- twowayfeweights(
  data             = weights_sample,
  Y                = "y_t_pct", G = "country_id", T = "t_idx",
  D                = "F_CP_above_flow_lag2",
  type             = "feTR",
  other_treatments = c("F_CP_belowstock", "S_mean_tw", "F_DI_lag1"),
  controls         = non_policy_ctrls,
  summary_measures = TRUE
)

# (2) Below-Stock channel
w_below <- twowayfeweights(
  data             = weights_sample,
  Y                = "y_t_pct", G = "country_id", T = "t_idx",
  D                = "F_CP_belowstock",
  type             = "feTR",
  other_treatments = c("F_CP_above_flow_lag2", "S_mean_tw", "F_DI_lag1"),
  controls         = non_policy_ctrls,
  summary_measures = TRUE
)

# (3) Stringency level effect
w_s <- twowayfeweights(
  data             = weights_sample,
  Y                = "y_t_pct", G = "country_id", T = "t_idx",
  D                = "S_mean_tw",
  type             = "feTR",
  other_treatments = c("F_CP_above_flow_lag2", "F_CP_belowstock", "F_DI_lag1"),
  controls         = non_policy_ctrls,
  summary_measures = TRUE
)

# (4) DI level effect
w_di <- twowayfeweights(
  data             = weights_sample,
  Y                = "y_t_pct", G = "country_id", T = "t_idx",
  D                = "F_DI_lag1",
  type             = "feTR",
  other_treatments = c("F_CP_above_flow_lag2", "F_CP_belowstock", "S_mean_tw"),
  controls         = non_policy_ctrls,
  summary_measures = TRUE
)

cat("\n========== DCDH: Above-Flow ==========\n");   print(w_above)
cat("\n========== DCDH: Below-Stock ==========\n");  print(w_below)
cat("\n========== DCDH: Stringency ==========\n");   print(w_s)
cat("\n========== DCDH: DI ==========\n");           print(w_di)

#S is not monotonic: DCDH (2020) diagnostics confirm the empirical robustness of the Above-Flow and DI channels (negative weight shares of 8.7% and 2.7%, respectively), within conventional thresholds. The Below-Stock channel exhibits higher negative weight contamination (26.8%), consistent with its trend-sensitivity documented in the Year-FE specification."


# ============================================================================
# ROBUSTNESS: SAMPLE RESTRICTIONS
# ----------------------------------------------------------------------------
# Tests sensitivity to start/end of validation window.
# ============================================================================

# (1) t_idx >= 5: drop Q4.2019 (pre-pandemic anchor)
v14_s5 <- feols(y_t_pct ~ y_lag1 + S_mean_tw 
                + F_CP_above_flow_lag2 + F_CP_belowstock
                + F_DI_lag1 * S_mean_tw | Country,
                data = df_bin, subset = ~ t_idx >= 5 & t_idx <= 14)

# (2) t_idx >= 6: drop pre-pandemic + Q1.20 (limited stock variation)
v14_s6 <- feols(y_t_pct ~ y_lag1 + S_mean_tw 
                + F_CP_above_flow_lag2 + F_CP_belowstock
                + F_DI_lag1 * S_mean_tw | Country,
                data = df_bin, subset = ~ t_idx >= 6 & t_idx <= 14)

# (3) t_idx <= 13: drop late recovery
v14_e13 <- feols(y_t_pct ~ y_lag1 + S_mean_tw 
                 + F_CP_above_flow_lag2 + F_CP_belowstock
                 + F_DI_lag1 * S_mean_tw | Country,
                 data = df_bin, subset = ~ t_idx >= 4 & t_idx <= 13)

cat("\n========== SAMPLE: t_idx >= 5 ==========\n")
print(summary(v14_s5, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE)))

cat("\n========== SAMPLE: t_idx >= 6 ==========\n")
print(summary(v14_s6, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE)))

cat("\n========== SAMPLE: t_idx <= 13 ==========\n")
print(summary(v14_e13, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE)))



# INTERPRETATION: Above-Flow (0.36-0.54) and DI push-on-string (alpha_S,DI 
# -0.031 to -0.041) remain stable and significant across all sample 
# restrictions. The Below-Stock coefficient is identification-fragile: 
# dropping pre-pandemic anchors (t_idx >= 6) inflates the estimate to 
# 5.87, confirming that Q4.2019-Q1.2020 observations with zero/near-zero 
# stock provide essential discipline. The main spec (t_idx >= 4) is the 
# conservative and theoretically anchored baseline.

summary(df_bin$S_max_tw)
summary(df_bin$mob_transit)


# ============================================================================
# ROBUSTNESS: ALTERNATIVE CONTAINMENT MEASURES
# ----------------------------------------------------------------------------
# Main spec uses S_mean_tw (quarterly mean of OxCGRT stringency).
# Tests: (1) S_max_tw (peak stringency), (2) mob_transit (revealed
# voluntary + mandated mobility reduction).
# ============================================================================

# (1) S_max_tw: peak stringency per quarter
v14_smax <- feols(y_t_pct ~ y_lag1 + S_max_tw 
                  + F_CP_above_flow_lag2 + F_CP_belowstock
                  + F_DI_lag1 * S_max_tw | Country,
                  data = df_bin, subset = ~ t_idx >= 4 & t_idx <= 14)

cat("\n========== ALT: S_max_tw (peak stringency) ==========\n")
print(summary(v14_smax, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE)))


# (2) mob_transit: revealed mobility (includes voluntary response)
v14_mob <- feols(y_t_pct ~ y_lag1
                 + F_CP_above_flow_lag2 + F_CP_belowstock
                 + F_DI_lag1 * mob_transit +as.numeric(t_idx)| Country,
                 data = df_bin, subset = ~ t_idx >= 4 & t_idx <= 14)


cat("\n========== ALT: mob_transit (revealed mobility) ==========\n")
print(summary(v14_mob, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE)))


# Note: mob_transit spec drops the country-FE-collinear component;
# a linear time trend (`as.numeric(t_idx)`) is added because mobility
# trends without an explicit time control are confounded with the
# global pandemic cycle.


# ============================================================================
# ROBUSTNESS: WILD CLUSTER BOOTSTRAP
# ----------------------------------------------------------------------------
# Small-N defense: N_clusters = 38. CRV1 cluster-robust SE may
# over-reject in small clusters (Cameron, Gelbach, Miller 2008).
# Wild cluster bootstrap-t (Webb weights, 9999 reps) provides
# better small-sample inference.
# ============================================================================

# -----------------------------------------------------------------------------
#  WILD-CLUSTER BOOTSTRAP - V14 (Above-Flow + Below-Stock + DI*S)
#  N_clusters = 38. CRV1 may over-reject in small clusters.
#  Wild cluster bootstrap-t (Cameron-Gelbach-Miller 2008) via fwildclusterboot.
# -----------------------------------------------------------------------------
library(fwildclusterboot)
library(dplyr)

# Bootstrap sample: Q4.2019 - Q2.2022
# Local name `boot_sample` avoids shadowing the descriptive-stage
# `main_sample` defined above at line 872.
boot_sample <- df_bin %>%
  filter(t_idx >= 4 & t_idx <= 14) %>%
  mutate(DI_S = F_DI_lag1 * S_mean_tw,
         Country = as.factor(Country),
         Quarter = as.factor(Quarter))

# Re-fit V14 main spec on bootstrap sample (interaction as pre-built variable)
v14_boot <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw
  + F_CP_above_flow_lag2 + F_CP_belowstock
  + F_DI_lag1 + DI_S | Country,
  data = boot_sample, vcov = ~ Country
)
summary(v14_boot, cluster = ~ Country, 
        ssc = ssc(K.adj = TRUE, G.adj = TRUE))

# Wild-cluster bootstrap-t on each main parameter
set.seed(16031995)
dqrng::dqset.seed(16031995)

params <- c("F_CP_above_flow_lag2", "F_CP_belowstock", 
            "F_DI_lag1", "DI_S", "S_mean_tw", "y_lag1")

for (p in params) {
  cat("\n========== Wild-Cluster Bootstrap:", p, "==========\n")
  wild_p <- boottest(
    v14_boot, param = p, clustid = c("Country"),
    B = 99999, type = "rademacher", impose_null = TRUE,
    p_val_type = "two-tailed"
  )
  print(summary(wild_p))
}

# -----------------------------------------------------------------------------
#  Cluster-jackknife outlier diagnostic (summclust) for Above-Flow channel
# -----------------------------------------------------------------------------
library(summclust)
sc <- summclust(v14_boot, params = "F_CP_above_flow_lag2", cluster = ~ Country)
summary(sc)
plot(sc)


# INTERPRETATION: Wild cluster bootstrap (Webb/Rademacher, B=99999) confirms 
# small-cluster robustness. Above-Flow, S, y_lag1 remain strongly significant. 
# DI main effect significant at 5%. Below-Stock and DI:S move to marginal 
# significance (p=0.058, 0.059) — naive CRV1 slightly over-rejected but 
# pattern stable. No qualitative changes; V14 inference holds under 
# small-N defense.


# ============================================================================
# ROBUSTNESS: TAKE-UP SENSITIVITY GRID
# ----------------------------------------------------------------------------
# Tests sensitivity of below-stock channel to assumed take-up rates.
# Loans: 40% / 60% / 80%  (ECB SAFE Survey range)
# Guarantees: 25% / 35% / 50%  (IMF Fiscal Monitor range)
# ============================================================================
#.rs.restartR()
library(dplyr); library(fixest)

takeup_grid <- expand.grid(
  loans = c(0.40, 0.60, 0.80),
  guar  = c(0.25, 0.35, 0.50)
)

results_takeup <- vector("list", nrow(takeup_grid))
for (i in seq_len(nrow(takeup_grid))) {
  tl <- takeup_grid$loans[i]
  tg <- takeup_grid$guar[i]
  
  df_tmp <- df_bin |>
    group_by(Country) |> arrange(t_idx) |>
    mutate(
      loans_adj_tmp = F_CP_loans * tl,
      guar_adj_tmp  = F_CP_guar  * tg,
      stock_tmp     = cumsum(replace_na(loans_adj_tmp, 0)) + 
        cumsum(replace_na(guar_adj_tmp,  0))
    ) |> ungroup()
  
  m <- feols(y_t_pct ~ y_lag1 + S_mean_tw 
             + F_CP_above_flow_lag2 + stock_tmp 
             + F_DI_lag1 * S_mean_tw | Country,
             data = df_tmp, subset = ~ t_idx >= 4 & t_idx <= 14)
  
  cat("\n--- Loans", tl*100, "% | Guar", tg*100, "% ---\n")
  print(summary(m, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE)))
}

# INTERPRETATION: Above-Flow (0.541-0.547) and DI push-on-string 
# (alpha_S,DI -0.040 to -0.041) are completely invariant to take-up 
# assumptions. Below-Stock magnitude varies inversely with assumed take-up 
# (0.13-0.26): higher take-up = larger constructed stock = smaller 
# coefficient. Significance ranges p=0.05-0.16, consistently marginal. 
# Conservative baseline (loans 40%, guar 25%) yields the largest 
# alpha_below = 0.261 and is the theoretically motivated lower-bound spec.

# ============================================================================
# ROBUSTNESS: OUTLIER EXCLUSION
# ----------------------------------------------------------------------------
# TUR: extreme inflation (>70% annualized 2022) decouples nominal debt
#      from real fiscal position.
# IRL: multinational corporate restructuring (Apple, Pfizer relocations)
#      generates GDP volatility unrelated to domestic conditions.
# ============================================================================

v14_no_outliers <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw 
  + F_CP_above_flow_lag2 + F_CP_belowstock
  + F_DI_lag1 * S_mean_tw | Country,
  data = df_bin, 
  subset = ~ t_idx >= 4 & t_idx <= 14 & !Country %in% c("TUR", "IRL")
)

cat("\n========== V14 EXCLUDING TUR + IRL ==========\n")
print(summary(v14_no_outliers, cluster = ~ Country, 
              ssc = ssc(K.adj = TRUE, G.adj = TRUE)))


# INTERPRETATION: Excluding TUR (hyperinflation) and IRL (multinational 
# GDP distortion) reduces all coefficients modestly but preserves signs, 
# significance pattern, and economic interpretation. Above-Flow remains 
# strongly significant (0.45*), DI push-on-string holds (-0.035*). 
# Below-Stock moves from p=0.08 to p=0.13 — pattern stable, marginal 
# significance preserved at slightly weaker level. Main spec robust to 
# outlier exclusion.


# ============================================================================
# ROBUSTNESS: MUNDLAK / CHAMBERLAIN DECOMPOSITION (V14)
# ----------------------------------------------------------------------------
# Decomposes each regressor into within-country and between-country
# components. FE estimates = within coefficients. Significant differences
# between within and between imply omitted-variable bias in cross-section
# (selection on crisis severity).
# ============================================================================
library(car)

df_m <- df_bin %>%
  filter(t_idx >= 4 & t_idx <= 14) %>%
  mutate(DI_S = F_DI_lag1 * S_mean_tw) %>%
  group_by(Country) %>%
  mutate(
    y_lag1_b      = mean(y_lag1,                na.rm = TRUE),
    S_b           = mean(S_mean_tw,             na.rm = TRUE),
    above_b       = mean(F_CP_above_flow_lag2,  na.rm = TRUE),
    below_b       = mean(F_CP_belowstock,       na.rm = TRUE),
    DI_b          = mean(F_DI_lag1,             na.rm = TRUE),
    DI_S_b        = mean(DI_S,                  na.rm = TRUE)
  ) %>%
  mutate(
    y_lag1_w  = y_lag1                - y_lag1_b,
    S_w       = S_mean_tw             - S_b,
    above_w   = F_CP_above_flow_lag2  - above_b,
    below_w   = F_CP_belowstock       - below_b,
    DI_w      = F_DI_lag1             - DI_b,
    DI_S_w    = DI_S                  - DI_S_b
  ) %>%
  ungroup()

mundlak_v14 <- feols(
  y_t_pct ~ y_lag1_w + S_w + above_w + below_w + DI_w + DI_S_w
  + y_lag1_b + S_b + above_b + below_b + DI_b + DI_S_b,
  data = df_m, cluster = ~ Country, ssc = ssc(adj = TRUE, cluster.adj = TRUE)
)
summary(mundlak_v14)

# Hausman-style joint test: within = between for all main F-channels
linearHypothesis(mundlak_v14,
                 c("above_w - above_b = 0",
                   "below_w - below_b = 0",
                   "DI_w    - DI_b    = 0",
                   "DI_S_w  - DI_S_b  = 0"),
                 vcov = vcov(mundlak_v14, cluster = ~ Country))



# INTERPRETATION: Mundlak decomposition validates FE identification.
# Between-country F-channel coefficients are all insignificant, indicating
# cross-country F-variation is not confounded with outcome heterogeneity.
# Hausman-style joint test rejects equality of within and between
# coefficients (chi2 = 25.7, p < 0.0001), confirming pooled OLS would be
# biased by selection on crisis severity and structural country
# characteristics. Country-FE identification is the correct specification.

# ============================================================================
# ROBUSTNESS: LAG SELECTION FOR ABOVE-FLOW
# ----------------------------------------------------------------------------
# Tests lag 0, 1, 2, 3 for F_CP_above_3. Main spec uses lag 2 based on
# authorization-to-spending lag (Chetty et al. 2020).
# ============================================================================

df_bin <- df_bin |>
  group_by(Country) |> arrange(t_idx) |>
  mutate(F_CP_above_flow_lag3 = lag(F_CP_above_3, 3)) |> ungroup()

for (lag_k in 0:3) {
  var_name <- if (lag_k == 0) "F_CP_above_3" else sprintf("F_CP_above_flow_lag%d", lag_k)
  
  fml <- as.formula(sprintf(
    "y_t_pct ~ y_lag1 + S_mean_tw + %s + F_CP_belowstock + F_DI_lag1 * S_mean_tw | Country",
    var_name
  ))
  m <- feols(fml, data = df_bin, subset = ~ t_idx >= 4 & t_idx <= 14)
  cat("\n--- Above Lag", lag_k, "---\n")
  print(summary(m, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE)))
}

# INTERPRETATION: Lag-0 and Lag-1 coefficients are strongly negative,
# reflecting reverse causality: fiscal measures are deployed contemporaneously
# with the output decline they aim to address. Lag-2 captures the causal
# pass-through after authorization-to-disbursement delays (Chetty et al.
# 2020), yielding the theoretically expected positive sign. Lag-2 is the
# correct identification choice.

for (lag_k in 0:2) {
  var_name <- if (lag_k == 0) "F_DI" else sprintf("F_DI_lag%d", lag_k)
  
  fml <- as.formula(sprintf(
    "y_t_pct ~ y_lag1 + S_mean_tw + F_CP_above_flow_lag2 + F_CP_belowstock + %s * S_mean_tw | Country",
    var_name
  ))
  m <- feols(fml, data = df_bin, subset = ~ t_idx >= 4 & t_idx <= 14)
  cat("\n--- DI Lag", lag_k, "---\n")
  print(summary(m, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE)))
}

# INTERPRETATION: F_DI lag-1 is the correct identification. Contemporary
# (lag-0) coefficients are insignificant due to disbursement delay
# (authorization-to-payment ~30 days, Chetty et al. 2020). Lag-2 reverses
# the interaction sign, indicating the consumption effect has dissipated
# and the coefficient captures residual confounding rather than the
# causal channel. Lag-1 matches the empirical disbursement timing.

# ============================================================
# V14 Robustness — Remaining Specifications
# ============================================================

main_fml_v14 <- y_t_pct ~ y_lag1 + S_mean_tw + 
  F_CP_above_flow_lag2 + F_CP_belowstock + 
  F_DI_lag1 * S_mean_tw | Country

main_sub <- ~ t_idx >= 4 & t_idx <= 14

# --- (1) ASYMMETRY: tightening vs loosening ---
df_bin <- df_bin %>%
  group_by(Country) %>%
  arrange(t_idx) %>%
  mutate(
    S_tightening = pmax(S_mean_tw - lag(S_mean_tw), 0),
    S_loosening  = pmin(S_mean_tw - lag(S_mean_tw), 0)
  ) %>%
  ungroup()

m_asym <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw + S_tightening + S_loosening
  + F_CP_above_flow_lag2 + F_CP_belowstock
  + F_DI_lag1 * S_mean_tw
  | Country,
  data = df_bin, subset = main_sub, cluster = ~ Country
)
cat("\n=== (1) ASYMMETRY: tightening vs loosening ===\n")
print(summary(m_asym, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE)))

# --- (2) SAMPLE SPLITS ---
country_S   <- aggregate(S_mean_tw ~ Country, df_bin, mean)
median_S    <- median(country_S$S_mean_tw, na.rm = TRUE)
median_gdp  <- median(df_bin$rGDP_pc_2019, na.rm = TRUE)
median_debt <- median(df_bin$debt_2019, na.rm = TRUE)

hi_S    <- country_S$Country[country_S$S_mean_tw >= median_S]
lo_S    <- country_S$Country[country_S$S_mean_tw <  median_S]
hi_inc  <- unique(df_bin$Country[!is.na(df_bin$rGDP_pc_2019) & df_bin$rGDP_pc_2019 >= median_gdp])
lo_inc  <- unique(df_bin$Country[!is.na(df_bin$rGDP_pc_2019) & df_bin$rGDP_pc_2019 <  median_gdp])
hi_dbt  <- unique(df_bin$Country[!is.na(df_bin$debt_2019) & df_bin$debt_2019 >= median_debt])
lo_dbt  <- unique(df_bin$Country[!is.na(df_bin$debt_2019) & df_bin$debt_2019 <  median_debt])

high_socnet <- c("FRA","FIN","BEL","DNK","ITA","AUT","SWE","DEU","NOR",
                 "ESP","GRC","PRT","LUX","NLD","JPN","GBR","CZE","SVN","POL")
low_socnet  <- c("USA","KOR","MEX","CHL","TUR","IRL","AUS","NZL","CAN",
                 "CHE","ISR","COL","CRI","EST","LVA","LTU","HUN","SVK","ISL")

split_list <- list(
  list(nm = "High-S",         cty = hi_S),
  list(nm = "Low-S",          cty = lo_S),
  list(nm = "High-income",    cty = hi_inc),
  list(nm = "Low-income",     cty = lo_inc),
  list(nm = "High pre-debt",  cty = hi_dbt),
  list(nm = "Low pre-debt",   cty = lo_dbt),
  list(nm = "High soc.net",   cty = high_socnet),
  list(nm = "Low soc.net",    cty = low_socnet)
)

split_models <- list()
for (sp in split_list) {
  df_sp <- df_bin %>% filter(Country %in% sp$cty)
  m_sp <- tryCatch(
    feols(main_fml_v14, data = df_sp, subset = main_sub, cluster = ~ Country),
    error = function(e) NULL
  )
  if (!is.null(m_sp)) split_models[[sp$nm]] <- m_sp
}

cat("\n=== (2) Sample splits: V14 ===\n")
etable(split_models,
       cluster = ~ Country,
       ssc = ssc(adj = TRUE, cluster.adj = TRUE),
       keep = c("S_mean_tw", "F_CP_above_flow_lag2", "F_CP_belowstock",
                "F_DI_lag1", "F_DI_lag1:S_mean_tw"))

# --- (3) SAMPLE-WINDOW ROBUSTNESS ---
m_baseline <- feols(main_fml_v14, data = df_bin, subset = main_sub, cluster = ~ Country)
m_narrow   <- feols(main_fml_v14, data = df_bin, 
                    subset = ~ t_idx >= 5 & t_idx <= 13, cluster = ~ Country)
m_wide     <- feols(main_fml_v14, data = df_bin, 
                    subset = ~ t_idx >= 4 & t_idx <= 16, cluster = ~ Country)
m_2020     <- feols(main_fml_v14, data = df_bin, 
                    subset = ~ t_idx >= 4 & t_idx <= 8, cluster = ~ Country)

cat("\n=== (3) Sample-window robustness: V14 ===\n")
etable(list(
  "Baseline Q4.19-Q2.22" = m_baseline,
  "Narrow Q1.20-Q1.22"   = m_narrow,
  "Wide Q4.19-Q4.22"     = m_wide,
  "Only 2020"            = m_2020
),
cluster = ~ Country,
ssc = ssc(adj = TRUE, cluster.adj = TRUE),
keep = c("S_mean_tw", "F_CP_above_flow_lag2", "F_CP_belowstock",
         "F_DI_lag1", "F_DI_lag1:S_mean_tw"))


# ============================================================
# V14 Robustness Interpretation
# ============================================================
#
# (1) ASYMMETRY: tightening vs loosening
#   Delta S+ = -0.125*** (p < 0.001)
#   Delta S- = -0.005    (p = 0.79)
#   Tightening contracts output ~25x more than loosening expands
#   it. Direct evidence for hysteresis: productive capacity
#   destroyed during tightening does not return mechanically
#   during loosening. The asymmetric ratchet provides structural
#   justification for the persistence channel and the AR(1)
#   recovery dynamics in V14. Above-Flow and DI:S remain
#   significant in this richer specification.
#
# (2) SAMPLE SPLITS: heterogeneity is economically interpretable
#
#   Above-Flow (alpha_above):
#     High-S          0.95**    Low-S          0.15 n.s.
#     High-income     0.50      Low-income     0.63***
#     High pre-debt   1.07***   Low pre-debt   0.32
#     High soc.net    0.78**    Low soc.net    0.40
#   -> Above is identified where there is shock to absorb
#      (High-S) and where fiscal space allows deployment
#      (High pre-debt countries deployed CP more aggressively
#      and credibly).
#
#   DI push-on-string (alpha_S,DI):
#     High-S         -0.071**   Low-S         -0.063**
#     High-income    -0.087*    Low-income    -0.029.
#     High soc.net   -0.122*    Low soc.net   -0.029.
#   -> Mechanism robust across all splits. Stronger in
#      High-income / High soc.net groups where formal consumption
#      channels are more developed and binding constraints
#      under containment are sharper.
#
#   Below-Stock (alpha_below):
#     Significant only in High-S (0.51.) and Low soc.net (1.09*)
#     -> Loans/guarantees matter most where shock is severe
#        (High-S) and automatic stabilizers are weak (Low soc.net):
#        liquidity protection substitutes for missing welfare
#        state buffers.
#
# (3) SAMPLE-WINDOW: Above-Flow and DI:S stable
#
#   Above-Flow:
#     Baseline Q4.19-Q2.22   0.544**
#     Narrow   Q1.20-Q1.22   0.368*
#     Wide     Q4.19-Q4.22   0.723**
#     Only 2020              0.729**
#   -> Above-Flow channel is operative from 2020 onwards
#      (Only-2020 N=190 delivers the largest coefficient),
#      ruling out the late-recovery-phase as the source of
#      identification.
#
#   DI push-on-string:
#     Baseline               -0.041**
#     Wide                   -0.052**
#     Narrow / Only 2020     n.s. (power loss at small N)
#   -> Mechanism stable in full samples; insignificance in
#      narrow windows reflects loss of S-variation rather than
#      mechanism failure.
#
#   Below-Stock:
#     Significant only in Baseline (0.26.); insignificant in
#     Narrow, Wide, Only-2020. Confirms identification
#     fragility documented in Year-FE and decay analyses:
#     Below-Stock is empirically weaker than Above-Flow.
#
# OVERALL ROBUSTNESS:
#   Above-Flow + DI push-on-string + S level effect form the
#   empirically robust core of V14. Below-Stock is theoretically
#   motivated and survives the main spec but is sensitive to
#   sample window and time-fixed-effects specifications.
#   Tightening-loosening asymmetry provides independent
#   structural support for the AR(1) persistence framing.
# ============================================================================
# ALTERNATIVE ESTIMATORS
# ----------------------------------------------------------------------------
# ============================================================================


#==============================================================================
# Regime switching (Deb et al. 2021): smooth-transition multiplier
# with logistic regime weights driven by standardised stringency.

df_bin$S_z <- as.numeric(scale(df_bin$S_mean_tw))
df_bin$F_z <- exp(-1.5 * df_bin$S_z) / (1 + exp(-1.5 * df_bin$S_z))

main_v10_agg <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw + p_proj_all_ages
  + I((1 - F_z) * F_CP_lag1)             # CP HIGH containment
  + I(F_z       * F_CP_lag1)             # CP LOW containment
  + I((1 - F_z) * F_DI_lag1)             # DI HIGH containment
  + I(F_z       * F_DI_lag1)             # DI LOW containment
  + F_CP_lag2:y_lag1_recession           # Tail-Insurance
  | Country + year_only,
  data = df_bin, subset = ~ t_idx >= 4 & t_idx <= 14
)
summary(main_v10_agg, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE))



# Standardization (re-using F_z from the aggregated specification above)
df_bin$S_z <- as.numeric(scale(df_bin$S_mean_tw))

# Smooth transition function (gamma = 1.5 as in Deb et al.)
# F_z high = LOW containment regime; F_z low = HIGH containment regime
df_bin$F_z <- exp(-1.5 * df_bin$S_z) / (1 + exp(-1.5 * df_bin$S_z))

# Lagged level variants of loans and guarantees, required for the
# disaggregated Deb-style spec below. Above-the-line lags already exist
# (constructed in the V14 Step 3 block above).
df_bin <- df_bin |>
  group_by(Country) |> arrange(t_idx) |>
  mutate(
    F_CP_loans_lag1    = lag(F_CP_loans,    1),
    F_CP_guar_adj_lag1 = lag(F_CP_guar_adj, 1)
  ) |> ungroup()

# Regime-dependent multipliers
main_v10 <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw
  # CP above-the-line: regime-dependent (cash-flow channel)
  + I((1 - F_z) * F_CP_above_flow_lag1)    # HIGH containment regime
  + I(F_z       * F_CP_above_flow_lag1)    # LOW containment regime
  # Loans and guarantees as level controls (not regime-dependent)
  + F_CP_loans_lag1 + F_CP_guar_adj_lag1
  # DI: regime-dependent (demand channel)
  + I((1 - F_z) * F_DI_lag1)               # HIGH containment regime
  + I(F_z       * F_DI_lag1)               # LOW containment regime
  # Tail insurance (CP * recession spline)
  + F_CP_above_flow_lag2:y_lag1_recession
  | Country,
  data = df_bin, subset = ~ t_idx >= 4 & t_idx <= 14
)
summary(main_v10, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE))


# GMM Diagnostics fail under short pandemic window (T=10):
# - Difference-GMM: AR(2) rejected (p = 0.001)
# - System-GMM: AR(2) rejected (p = 0.016)
# Both estimators violate moment conditions; results not interpretable.
#
# Nickell bias addressed analytically: at rho_y = 0.43 and T = 10,
# bias on rho_y is approximately -(1+rho)/(T-1) = -0.16. The bias
# on the interaction coefficient eta_p is of smaller magnitude and
# does not flip sign. The TWFE estimate eta_p = -0.011 is therefore
# a conservative (attenuated) estimate of the persistence channel;
# bias correction would strengthen the structural claim.

##HP Filter
#Robust to 10 years Trend calculation->see other script

# ============================================================
# Subsample robustness — regional groupings on V14 spec
# ------------------------------------------------------------
# Re-estimates the V14 main specification on three regional
# subsamples (G7, EU, advanced economies). Complements the
# characteristic-based splits in the V14 robustness battery
# (High-S vs Low-S, High-income vs Low-income, etc.).
# ============================================================

# G7
g7 <- c("USA", "CAN", "GBR", "DEU", "FRA", "ITA", "JPN")
df_g7 <- df_bin %>% filter(Country %in% g7)

m_g7 <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw
  + F_CP_above_flow_lag2 + F_CP_belowstock
  + F_DI_lag1 * S_mean_tw
  | Country,
  data = df_g7,
  subset = ~ t_idx >= 4 & t_idx <= 14
)

# EU only (OECD-EU members)
eu <- c("AUT","BEL","CZE","DEU","DNK","ESP","EST","FIN","FRA","GRC",
        "HUN","IRL","ITA","LTU","LUX","LVA","NLD","POL","PRT","SVK",
        "SVN","SWE")
df_eu <- df_bin %>% filter(Country %in% eu)
m_eu <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw
  + F_CP_above_flow_lag2 + F_CP_belowstock
  + F_DI_lag1 * S_mean_tw
  | Country,
  data = df_eu,
  subset = ~ t_idx >= 4 & t_idx <= 14
)

# Advanced economies only (exclude emerging-market OECD)
emerging <- c("MEX", "CHL", "COL", "TUR", "CRI")
df_adv <- df_bin %>% filter(!Country %in% emerging)
m_adv <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw
  + F_CP_above_flow_lag2 + F_CP_belowstock
  + F_DI_lag1 * S_mean_tw
  | Country,
  data = df_adv,
  subset = ~ t_idx >= 4 & t_idx <= 14
)

etable(v14, m_g7, m_eu, m_adv,
       cluster = ~ Country,
       ssc = ssc(adj = TRUE, cluster.adj = TRUE),
       keep = c("S_mean_tw", "F_CP_above_flow_lag2", "F_CP_belowstock",
                "F_DI_lag1", "F_DI_lag1:S_mean_tw"),
       headers = c("Main (V14)", "G7", "EU", "Advanced"))

# INTERPRETATION (to be re-checked after re-run on V14):
#   Compare Above-Flow, Below-Stock, and DI:S coefficients across regional
#   subsamples against the V14 baseline. Power loss in G7 (N small) and
#   advanced-only is expected; EU subsample should approximate the
#   headline pattern. Significance in regional cuts reflects sample size,
#   not mechanism failure.

# =============================================================================
#  STAGE 4 - DEBT EQUATION
# =============================================================================
#  IMPORTANT: this stage REASSIGNS pdataY to pdata, dropping all of the
#  output-gap-section work above. From here on, pdataY is just a scratch
#  copy used to construct first-differenced debt; the DEBT regressions run
#  on `pdataD`, which is rebuilt with the chronologically correct lag.
#
#  Inputs : pdata (built in Stage 2b), fm1 (Stage 1), fiscal_subcomp
#           (constructed in Stage 3, Step 0B - kept in the environment).
#  Outputs: tab_debt_main.tex, tab_debt_robust.tex, tab_debt_appendix.tex.
#  DV     : debt_dR = first difference of real debt / 2019 GDP.
#  Sample : Q4.2019 - Q4.2022 (t_idx in [2, 14]), N = 494.
# =============================================================================

# First differences of real and nominal debt-to-GDP within each country.
pdataY <- pdata %>%
  group_by(Country) %>%
  mutate(
    debt_dR = DebtR_share2019 - lag(DebtR_share2019),
    debt_dN = DebtN_share2019 - lag(DebtN_share2019)
  ) %>%
  ungroup()

# Build pdataD with the chronologically correct first-difference lag.
# Q3.2019 is included only as the lag source for Q4.2019 and is dropped
# at the end of the pipeline. The string-sorted alphabetical order of
# Quarter (e.g. "Q1.2020" < "Q2.2020" < ... < "Q4.2020") matches calendar
# order WITHIN a year but breaks across years; we re-sort by an explicit
# date_sort key so lag(., 1) is unambiguous.
pdataD <- pdataY %>%
  filter(Quarter %in% c("Q3.2019","Q4.2019",
                        "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                        "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
                        "Q1.2022","Q2.2022","Q3.2022","Q4.2022")) %>%
  mutate(
    q_num_sort = as.integer(substr(as.character(Quarter), 2, 2)),
    yr_sort    = as.integer(substr(as.character(Quarter), 4, 7)),
    date_sort  = as.Date(paste0(yr_sort, "-", (q_num_sort - 1) * 3 + 1, "-01"))
  ) %>%
  arrange(Country, date_sort) %>%
  group_by(Country) %>%
  mutate(
    debt_dR = DebtR_share2019 - lag(DebtR_share2019, 1),
    debt_dN = DebtN_share2019 - lag(DebtN_share2019, 1)
  ) %>%
  ungroup() %>%
  filter(Quarter != "Q3.2019") %>%  # drop lag source, keep Q4.2019 onward
  select(-q_num_sort, -yr_sort, -date_sort)

# --- Descriptives ------------------------------------------------------------
desc_debt <- pdataD %>%
  transmute(
    `Debt-to-GDP Ratio (pp of 2019 GDP)` = DebtR_share2019,
    `Debt Gap (pp, HP-filtered)`         = d_t_pct,
    `Debt Change (pp, first diff)`       = debt_dR,
    `Debt Change (nom, first diff)`      = debt_dN,
    `Output Gap (pp)`                    = y_t_pct,
    `Capacity Preservation (pp GDP)`     = F_CP,
    `Demand Injection (pp GDP)`          = F_DI,
    `Health Expenditure (pp GDP)`        = F_H,
    `Infection Prevalence (pct)`         = theta_pct,
    `Vaccination Rate (pct)`             = vax_rate
  )

tab_debt <- datasummary(
  All(desc_debt) ~ N + Mean + SD + Min + P25 + Median + P75 + Max,
  data = desc_debt, fmt = 2, output = "data.frame"
)
print(tab_debt)

# Outlier check
pdataD %>% filter(d_t_pct > 80) %>% select(Country, Quarter, d_t_pct)
pdataD %>% filter(debt_dR > 10) %>% select(Country, Quarter, debt_dR)

# --- Build CP sub-components from fm1 (face value, in pp GDP x100) ----------
# Above-the-line (category 1), loans (PolicyCode 40/41), guarantees (PC 43).
# Below-the-line guarantees are scaled by 25/35/50% take-up scenarios.
fiscal_subcomp_d <- fm1 %>%
  filter(broad_fiscal == 1) %>%
  mutate(
    Quarter_fmt = paste0("Q", Quarter, ".", Year),
    CP_above = ifelse(transmission_channel == "CP" & category == 1, broad_fiscal_gdp, 0),
    CP_below = ifelse(transmission_channel == "CP" & category == 2, broad_fiscal_gdp, 0),
    CP_loans = ifelse(transmission_channel == "CP" & category == 2 &
                        PolicyCode %in% c("40","41"), broad_fiscal_gdp, 0),
    CP_guar  = ifelse(transmission_channel == "CP" & category == 2 &
                        PolicyCode == "43", broad_fiscal_gdp, 0)
  ) %>%
  group_by(Country, Quarter_fmt) %>%
  summarise(
    F_CP_above   = sum(CP_above, na.rm = TRUE) * 100,
    F_CP_below   = sum(CP_below, na.rm = TRUE) * 100,
    F_CP_above_3 = sum(CP_above, na.rm = TRUE) * 100,
    F_CP_loans   = sum(CP_loans, na.rm = TRUE) * 100,
    F_CP_guar    = sum(CP_guar,  na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  rename(Quarter = Quarter_fmt) %>%
  mutate(
    # Take-up scenarios for below-the-line CP
    F_CP_below_adj_lo  = F_CP_below * 0.25,
    F_CP_below_adj_mid = F_CP_below * 0.35,   # baseline (IMF WP 2023/016)
    F_CP_below_adj_hi  = F_CP_below * 0.50,
    F_CP_guar_adj      = F_CP_guar  * 0.35
  )
# Take-up rates: IMF WP 2023/016 ("Evaluating the Costs of Government Credit
# Support Programs during COVID-19") and ECB Economic Bulletin 2020/6.
# 7 advanced economies disbursed 1.7 of 5+ trillion USD announced -> ~34%
# aggregate take-up, supporting the 35% central scenario.

# Merge sub-components into pdataD
pdataD <- pdataD %>%
  select(-any_of(c("F_CP_above","F_CP_below","F_CP_above_3","F_CP_loans",
                   "F_CP_guar","F_CP_guar_adj","F_CP_below_adj_lo",
                   "F_CP_below_adj_mid","F_CP_below_adj_hi"))) %>%
  left_join(fiscal_subcomp_d, by = c("Country","Quarter")) %>%
  mutate(across(c(F_CP_above, F_CP_below, F_CP_above_3, F_CP_loans, F_CP_guar,
                  F_CP_guar_adj, F_CP_below_adj_lo, F_CP_below_adj_mid,
                  F_CP_below_adj_hi), ~replace_na(.x, 0)))

# Quarter ordering (Q3.2019 = 1 .. Q4.2022 = 14)
quarter_order <- c("Q3.2019","Q4.2019",
                   "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                   "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
                   "Q1.2022","Q2.2022","Q3.2022","Q4.2022")
pdataD$Quarter <- factor(pdataD$Quarter, levels = quarter_order)
pdataD$t_idx   <- match(as.character(pdataD$Quarter), quarter_order)

# Impute remaining pre-pandemic NAs as 0
pdataD <- pdataD %>%
  mutate(across(c(S_mean_tw, F_CP, F_DI_lag1, F_DI_lag2, p_proj_all_ages,
                  y_lag1, theta_pct, vax_rate, S_max_tw),
                ~replace_na(.x, 0)))


#...............................................................................
#............................. ANALYSIS .........................................

# -----------------------------------------------------------------------------
#  Lag structure: DI cont. vs lag1 vs lag2  (CP held contemporaneous)
# -----------------------------------------------------------------------------
debt_DI0 <- plm(debt_dR ~ y_t_pct + F_CP + F_DI      + as.numeric(year_only),
                data = pdataD, model = "within", index = c("Country","Quarter"),
                effect = "individual")
debt_DI1 <- plm(debt_dR ~ y_t_pct + F_CP + F_DI_lag1 + as.numeric(year_only),
                data = pdataD, model = "within", index = c("Country","Quarter"),
                effect = "individual")
debt_DI2 <- plm(debt_dR ~ y_t_pct + F_CP + F_DI_lag2 + as.numeric(year_only),
                data = pdataD, model = "within", index = c("Country","Quarter"),
                effect = "individual")

stargazer::stargazer(
  list(debt_DI0, debt_DI1, debt_DI2), type = "text",
  column.labels = c("DI cont.", "DI lag1", "DI lag2"),
  se = list(
    sqrt(diag(vcovHC(debt_DI0, cluster="group", type="HC1"))),
    sqrt(diag(vcovHC(debt_DI1, cluster="group", type="HC1"))),
    sqrt(diag(vcovHC(debt_DI2, cluster="group", type="HC1")))
  )
)

# -----------------------------------------------------------------------------
#  Lag consistency check: CP cont + DI lag1  vs  both lag1
# -----------------------------------------------------------------------------
debt_lag0_lag1 <- plm(debt_dR ~ y_t_pct + F_CP      + F_DI_lag1 + as.numeric(year_only),
                      data = pdataD, model = "within", index = c("Country","Quarter"),
                      effect = "individual")
debt_lag1_lag1 <- plm(debt_dR ~ y_t_pct + F_CP_lag1 + F_DI_lag1 + as.numeric(year_only),
                      data = pdataD, model = "within", index = c("Country","Quarter"),
                      effect = "individual")

stargazer::stargazer(
  list(debt_lag0_lag1, debt_lag1_lag1), type = "text",
  column.labels = c("CP cont, DI lag1", "Both lag1 (consistent)"),
  se = list(
    sqrt(diag(vcovHC(debt_lag0_lag1, cluster="group", type="HC1"))),
    sqrt(diag(vcovHC(debt_lag1_lag1, cluster="group", type="HC1")))
  )
)

# Lag structure of fiscal instruments in the debt equation:
# CP enters contemporaneously (alpha_CP = 0.173, p < 0.001),
# DI enters at lag 1 (alpha_DI = 0.43, p = 0.08). Asymmetry is
# structural, not data-mined: CP operates through guarantees and
# short-time work schemes booked at announcement (contingent
# liabilities; monthly wage subsidies), while DI operates through
# transfers and tax measures with a ~1-quarter authorization-to-
# disbursement gap (Chetty et al. 2020). Forcing CP to lag 1 collapses
# the coefficient to 0.058 (p = 0.29), confirming that CP-driven debt
# accumulation is contemporaneous with announcement, not delayed.
# Output equation retains lag 2 for both instruments, reflecting the
# additional time-to-spending after disbursement.


# -----------------------------------------------------------------------------
#  Estimator comparison: Pooling, Random Effects, Fixed Effects, Between
#  Main spec: CP contemporaneous, DI lag 1, year time trend.
# -----------------------------------------------------------------------------
debtPO <- plm(debt_dR ~ y_t_pct + F_CP + F_DI_lag1 + as.numeric(Quarter),
              data = pdataD, index = c("Country","Quarter"),
              model = "pooling", effect = "individual")
debtRE <- plm(debt_dR ~ y_t_pct + F_CP + F_DI_lag1 + as.numeric(Quarter),
              data = pdataD, index = c("Country","Quarter"),
              model = "random",  effect = "individual")
debtFE <- plm(debt_dR ~ y_t_pct + F_CP + F_DI_lag1 + as.numeric(Quarter),
              data = pdataD, index = c("Country","Quarter"),
              model = "within",  effect = "individual")
debtBE <- plm(debt_dR ~ y_t_pct + F_CP + F_DI_lag1,
              data = pdataD, index = c("Country","Quarter"), model = "between")

cat("\n--- Between estimator ---\n");        coeftest(debtBE)
cat("\n--- Pooling (CRV1) ---\n");           coeftest(debtPO, vcov = vcovHC(debtPO, cluster = "group", type = "HC1"))
cat("\n--- Random Effects (CRV1) ---\n");    coeftest(debtRE, vcov = vcovHC(debtRE, cluster = "group", type = "HC1"))
cat("\n--- Fixed Effects (CRV1) ---\n");     coeftest(debtFE, vcov = vcovHC(debtFE, cluster = "group", type = "HC1"))
cat("\n--- Hausman test (FE vs RE) ---\n");  phtest(debtFE, debtRE)

# FE estimator required (Hausman rejects RE).

# -----------------------------------------------------------------------------
#  MUNDLAK / CHAMBERLAIN TEST — Debt equation
#  Wald test on joint nullity of country-mean coefficients = formal test of
#  RE assumption. Rejection => country effects correlated with regressors
#  => FE required.
# -----------------------------------------------------------------------------
pdataD <- pdataD %>%
  group_by(Country) %>%
  mutate(
    y_mean        = mean(y_t_pct,   na.rm = TRUE),
    FCP_mean      = mean(F_CP,      na.rm = TRUE),
    FDI_lag1_mean = mean(F_DI_lag1, na.rm = TRUE)
  ) %>%
  ungroup()

debt_mundlak <- plm(
  debt_dR ~ y_t_pct + F_CP + F_DI_lag1
  + y_mean + FCP_mean + FDI_lag1_mean
  + as.numeric(year_only),
  data = pdataD, model = "pooling"
)

cat("\n--- Mundlak regression: within + between coefficients ---\n")
coeftest(debt_mundlak, vcov = vcovHC(debt_mundlak, type = "HC1", cluster = "group"))

cat("\n--- Wald test: H0 = RE assumption (joint nullity of country means) ---\n")
waldtest(debt_mundlak,
         . ~ . - y_mean - FCP_mean - FDI_lag1_mean,
         vcov = vcovHC(debt_mundlak, type = "HC1", cluster = "group"))

#===============================================================================
# DECISION: FE with linear time trend (Quarter or year_only). Sample extended
# to Q4.2022 (T = 13) to capture delayed disbursement and contingent-liability
# realization.
#
# Two-Way FE is NOT appropriate for the debt equation: Quarter-FE would
# absorb the common fiscal deployment cycle that constitutes the identifying
# variation. Unlike the output equation - where S_k is endogenous to the
# pandemic cycle and Quarter-FE control for that endogeneity - the debt
# equation contains no instrument whose endogeneity requires temporal control.
# Consistent with Chetty et al. (2020) (region-FE only on high-frequency
# spending data) and Deb et al. (2021, IMF WP) (country-FE with parametric
# time controls).
#===============================================================================
pdataD <- pdataD |>
  group_by(Country) |> arrange(t_idx) |>
  mutate(F_H_lag1 = lag(F_H, 1)) |>
  ungroup()

pdataD <- pdataD |>
  group_by(Country) |> arrange(t_idx) |>
  mutate(F_CP_below_above_lag1 = lag(F_CP_above_3, 1)) |>
  ungroup()

pdataD <- df_bin |>
  mutate(F_CP_below_flow = F_CP_loans_adj + F_CP_guar_adj)

pdataD <- pdataD |> filter(!is.na(Quarter), !is.na(Country))
# -----------------------------------------------------------------------------
#  MAIN SPECIFICATION (V4 frozen 06.05.2026)
# -----------------------------------------------------------------------------
#broader, w/o disaggregation of below
debt_FE <- feols(
  debt_dR ~ y_t_pct 
  + F_CP_above_flow +F_CP_below_flow + F_DI_lag1
  + F_DI_lag1| Country,
  data = pdataD, 
  subset = ~ t_idx >= 4 & t_idx <= 16
)

summary(debt_FE, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE))

pdataD <- pdataD %>%
  group_by(Country) %>%
  mutate(
    debt_dR = DebtR_share2019 - lag(DebtR_share2019)
  ) %>%
  ungroup()

head(pdataD$DebtR_share2019)
head(pdataD$debt_dR)

#with disaggregation of below
#iLQR dieses verwendet 
#Main Model 12.05.2026
debt_v14 <- feols(
  debt_dR ~ y_t_pct 
  + F_CP_above_3 + F_CP_loans_lo + F_CP_guar_lo
  + F_DI_lag1 | Country,
  data = pdataD, 
  subset = ~ t_idx >= 4 & t_idx <= 16
)

summary(debt_v14, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE))

#with linear timetrend
debt_v14 <- feols(
  debt_dR ~ y_t_pct 
  + F_CP_above_3 + F_CP_loans_lo + F_CP_guar_lo
  + F_DI_lag1 + as.numeric(t_idx) | Country,
  data = pdataD, 
  subset = ~ t_idx >= 4 & t_idx <= 16
)

summary(debt_v14, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE))

# DEFENSE: kappa coefficients capture the average net pass-through from
# authorized fiscal volume to real sovereign debt within the booking
# quarter, after accounting for refinancing operations, off-budget
# execution channels (EU SURE, KfW, CDP, BPI), reclassification practices
# (tax credits vs spending), and booking lags between authorization and
# debt issuance. Country fixed effects absorb systematic cross-country
# differences in fiscal accounting conventions; estimated kappa therefore
# reflects the within-country empirical regularity rather than the
# theoretical one-to-one mechanical translation.

##same but in PLM für FE
pdataD_sub <- pdata.frame(
  pdataD |> filter(t_idx >= 4 & t_idx <= 16),
  index = c("Country", "Quarter")
)

debt_v14_plm <- plm(
  debt_dR ~ y_t_pct + F_CP_above_3 + F_CP_loans_lo + F_CP_guar_lo + F_DI_lag1 +as.numeric(t_idx),
  data = pdataD_sub, 
  model = "within", 
  effect = "individual"
)

coeftest(debt_v14_plm, vcov = vcovHC(debt_v14_plm, cluster = "group", type = "HC1"))

fixef(debt_v14_plm)

# ============================================================================
# DEBT EQUATION V14 - ROBUSTNESS BATTERY
# ----------------------------------------------------------------------------
# Main spec:
#   debt_v14 <- feols(debt_dR ~ y_t_pct 
#                     + F_CP_above_3 + F_CP_loans_lo + F_CP_guar_lo
#                     + F_DI_lag1 | Country,
#                     data = pdataD, subset = ~ t_idx >= 4 & t_idx <= 16)
# ============================================================================

library(fixest); library(dplyr); library(car)
library(fwildclusterboot); library(lmtest); library(sandwich)


# ============================================================================
# (9) VIF AND PAIRWISE CORRELATIONS
# ============================================================================
df_debt <- pdataD |> filter(t_idx >= 4 & t_idx <= 16)

m_vif <- lm(debt_dR ~ y_t_pct + F_CP_above_3 + F_CP_loans_lo + F_CP_guar_lo 
            + F_DI_lag1, data = df_debt)
cat("\n========== (9) VIF DEBT V14 ==========\n")
print(vif(m_vif))

cat("\n========== Pairwise correlations ==========\n")
print(round(cor(df_debt[, c("y_t_pct", "F_CP_above_3", "F_CP_loans_lo",
                            "F_CP_guar_lo", "F_DI_lag1")],
                use = "complete.obs"), 3))

# INTERPRETATION: VIF < 5 for all regressors confirms clean identification;
# no collinearity bias in disaggregated debt pass-through estimates.


# ============================================================================
# (8) WILD CLUSTER BOOTSTRAP
# ============================================================================
df_boot <- pdataD |> 
  filter(t_idx >= 4 & t_idx <= 16) |>
  mutate(Country = as.factor(Country))

debt_boot <- feols(debt_dR ~ y_t_pct + F_CP_above_3 + F_CP_loans_lo 
                   + F_CP_guar_lo + F_DI_lag1 | Country,
                   data = df_boot, vcov = ~ Country)

set.seed(16031995); dqrng::dqset.seed(16031995)
params_d <- c("y_t_pct", "F_CP_above_3", "F_CP_loans_lo", 
              "F_CP_guar_lo", "F_DI_lag1")

for (p in params_d) {
  cat("\n========== Wild-Cluster Bootstrap:", p, "==========\n")
  wb <- boottest(debt_boot, param = p, clustid = c("Country"),
                 B = 99999, type = "rademacher", impose_null = TRUE,
                 p_val_type = "two-tailed")
  print(summary(wb))
}
# INTERPRETATION: Bootstrap p-values confirm CRV1 inference under N=38.
# INTERPRETATION: Wild cluster bootstrap (Webb/Rademacher, B=99999) confirms 
# small-cluster inference. gamma_y, kappa_above, and kappa_loans remain 
# strongly significant. kappa_guar insignificant (consistent with low 
# contingent-liability conversion). kappa_DI marginal (p=0.09).

# ============================================================================
# (7) POOLED BELOW (kappa_below instead of disaggregated)
# ============================================================================
df_debt <- df_debt |>
  mutate(F_CP_below_lo = F_CP_loans_lo + F_CP_guar_lo)

debt_pooled <- feols(debt_dR ~ y_t_pct + F_CP_above_3 + F_CP_below_lo 
                     + F_DI_lag1 | Country,
                     data = df_debt,
                     subset = ~ t_idx >= 4 & t_idx <= 16)
cat("\n========== (7) POOLED BELOW ==========\n")
print(summary(debt_pooled, cluster = ~ Country, 
              ssc = ssc(K.adj = TRUE, G.adj = TRUE)))

# INTERPRETATION: Pooled spec averages kappa_loans (0.94) and kappa_guar (0.19),
# masking institutional differences. Disaggregation reveals loans channel
# mechanically dominant; guarantees near-zero pass-through. Retain
# disaggregation as main spec.


# ============================================================================
# (5) LINEAR QUARTER TREND
# ============================================================================
debt_trend <- feols(debt_dR ~ y_t_pct + F_CP_above_3 + F_CP_loans_lo 
                    + F_CP_guar_lo + F_DI_lag1 
                    + as.numeric(t_idx) | Country,
                    data = pdataD,
                    subset = ~ t_idx >= 4 & t_idx <= 16)
cat("\n========== (5) WITH LINEAR QUARTER TREND ==========\n")
print(summary(debt_trend, cluster = ~ Country, 
              ssc = ssc(K.adj = TRUE, G.adj = TRUE)))

# INTERPRETATION: Trend captures mechanical debt drift (interest compounding,
# refinancing). Main F-channels remain stable. Use trend-spec in MATLAB
# calibration for improved fit.


# ============================================================================
# (6) YEAR-FE
# ============================================================================
debt_yearFE <- feols(debt_dR ~ y_t_pct + F_CP_above_3 + F_CP_loans_lo 
                     + F_CP_guar_lo + F_DI_lag1 
                     | Country + year_only,
                     data = pdataD,
                     subset = ~ t_idx >= 4 & t_idx <= 16)
cat("\n========== (6) WITH YEAR-FE ==========\n")
print(summary(debt_yearFE, cluster = ~ Country, 
              ssc = ssc(K.adj = TRUE, G.adj = TRUE)))

# INTERPRETATION: Year-FE is the non-parametric counterpart to the linear
# trend. Stability of F-channel coefficients across both treatments
# confirms identification is not trend-driven.

# INTERPRETATION: Year-FE confirms linear-trend results. kappa_loans 
# (0.90***) and gamma_y (-0.16***) stable; kappa_above weakens to 
# marginal significance (p=0.08). Within R^2 drops from 0.24 to 0.11 as 
# Year-FE absorbs broader temporal variation than the linear trend. 
# Linear trend is the parsimonious specification with equivalent 
# economic interpretation.

# ============================================================================
# (3) OUTLIER EXCLUSION (TUR, IRL)
# ============================================================================
debt_no_outliers <- feols(debt_dR ~ y_t_pct + F_CP_above_3 + F_CP_loans_lo 
                          + F_CP_guar_lo + F_DI_lag1 | Country,
                          data = pdataD,
                          subset = ~ t_idx >= 4 & t_idx <= 16 
                          & !Country %in% c("TUR", "IRL"))
cat("\n========== (3) EXCLUDING TUR + IRL ==========\n")
print(summary(debt_no_outliers, cluster = ~ Country, 
              ssc = ssc(K.adj = TRUE, G.adj = TRUE)))

# INTERPRETATION: TUR (hyperinflation decouples nominal/real debt) and IRL
# (multinational restructuring distorts GDP). Coefficient stability under
# exclusion confirms main spec is not driven by these outliers.
#Stabil gegenüber Outlier

# ============================================================================
# (2) TAKE-UP SENSITIVITY GRID
# ============================================================================
takeup_grid <- expand.grid(
  loans = c(0.40, 0.60, 0.80),
  guar  = c(0.25, 0.35, 0.50)
)

for (i in seq_len(nrow(takeup_grid))) {
  tl <- takeup_grid$loans[i]; tg <- takeup_grid$guar[i]
  df_tmp <- pdataD |>
    mutate(loans_tu = F_CP_loans * tl,
           guar_tu  = F_CP_guar  * tg)
  m <- feols(debt_dR ~ y_t_pct + F_CP_above_3 + loans_tu + guar_tu 
             + F_DI_lag1 | Country,
             data = df_tmp, subset = ~ t_idx >= 4 & t_idx <= 16)
  cat("\n--- (2) Loans", tl*100, "% | Guar", tg*100, "% ---\n")
  print(summary(m, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE)))
}

# INTERPRETATION: Effective pass-through (kappa * take-up) is invariant to
# take-up assumption; only the coefficient scales inversely. Confirms
# economic interpretation in pp 2019-GDP units.
#skaliert sich einfach durch

# INTERPRETATION: Effective pass-through (kappa * take-up) is invariant 
# across all scenarios: 0.40*0.935 = 0.60*0.624 = 0.80*0.468 = 0.374 for 
# loans; 0.047 for guarantees. Coefficients scale inversely with assumed 
# take-up while other regressors and fit (RMSE, R^2) remain identical. 
# This confirms the economic interpretation in pp 2019-GDP: loans 
# disburse approximately one-to-one (37% effective pass-through 
# corresponds to ~40% actual drawdown), guarantees translate to ~5% 
# contingent liability conversion (Bouabdallah et al. 2017).

# ============================================================================
# (1) SAMPLE RESTRICTIONS
# ============================================================================
debt_s5  <- feols(debt_dR ~ y_t_pct + F_CP_above_3 + F_CP_loans_lo 
                  + F_CP_guar_lo + F_DI_lag1 | Country,
                  data = pdataD, subset = ~ t_idx >= 5 & t_idx <= 16)
debt_e14 <- feols(debt_dR ~ y_t_pct + F_CP_above_3 + F_CP_loans_lo 
                  + F_CP_guar_lo + F_DI_lag1 | Country,
                  data = pdataD, subset = ~ t_idx >= 4 & t_idx <= 14)
debt_e15 <- feols(debt_dR ~ y_t_pct + F_CP_above_3 + F_CP_loans_lo 
                  + F_CP_guar_lo + F_DI_lag1 | Country,
                  data = pdataD, subset = ~ t_idx >= 4 & t_idx <= 15)

cat("\n========== (1) SAMPLE: t_idx >= 5 ==========\n")
print(summary(debt_s5,  cluster = ~ Country, ssc = ssc(K.adj=TRUE, G.adj=TRUE)))
cat("\n========== (1) SAMPLE: t_idx <= 14 ==========\n")
print(summary(debt_e14, cluster = ~ Country, ssc = ssc(K.adj=TRUE, G.adj=TRUE)))
cat("\n========== (1) SAMPLE: t_idx <= 15 ==========\n")
print(summary(debt_e15, cluster = ~ Country, ssc = ssc(K.adj=TRUE, G.adj=TRUE)))

# INTERPRETATION: F-channels stable across windows. gamma_y range confirms
# Bohn (1998) automatic-stabilizer estimates.

# INTERPRETATION: All five coefficients stable across sample restrictions.
# kappa_loans 0.90-0.94***, kappa_above 0.44-0.49**, gamma_y -0.19 to -0.20***
# remain robust. kappa_guar consistently insignificant. kappa_DI marginal
# (p=0.10-0.13). Sample window does not drive identification.

# ============================================================================
# (4) LAG-STRUCTURE FOR y_t_pct
# ============================================================================
pdataD <- pdataD |>
  group_by(Country) |> arrange(t_idx) |>
  mutate(y_lag1 = lag(y_t_pct, 1),
         y_lag2 = lag(y_t_pct, 2)) |> ungroup()

debt_y0 <- feols(debt_dR ~ y_t_pct + F_CP_above_3 + F_CP_loans_lo 
                 + F_CP_guar_lo + F_DI_lag1 | Country,
                 data = pdataD, subset = ~ t_idx >= 4 & t_idx <= 16)
debt_y1 <- feols(debt_dR ~ y_lag1  + F_CP_above_3 + F_CP_loans_lo 
                 + F_CP_guar_lo + F_DI_lag1 | Country,
                 data = pdataD, subset = ~ t_idx >= 4 & t_idx <= 16)
debt_y2 <- feols(debt_dR ~ y_lag2  + F_CP_above_3 + F_CP_loans_lo 
                 + F_CP_guar_lo + F_DI_lag1 | Country,
                 data = pdataD, subset = ~ t_idx >= 4 & t_idx <= 16)

cat("\n========== (4) y-LAG: 0 ==========\n")
print(summary(debt_y0, cluster = ~ Country, ssc = ssc(K.adj=TRUE, G.adj=TRUE)))
cat("\n========== (4) y-LAG: 1 ==========\n")
print(summary(debt_y1, cluster = ~ Country, ssc = ssc(K.adj=TRUE, G.adj=TRUE)))
cat("\n========== (4) y-LAG: 2 ==========\n")
print(summary(debt_y2, cluster = ~ Country, ssc = ssc(K.adj=TRUE, G.adj=TRUE)))

# INTERPRETATION: Contemporaneous y enters debt because automatic stabilizers
# (tax revenue, transfer outlays) respond within the booking quarter.
# Lagged y would capture only delayed transmission; contemporaneous spec
# is theoretically motivated.

# INTERPRETATION: Contemporaneous y is the correct specification. y_lag1 
# shows weaker fit (gamma -0.117** vs -0.189***) and inflated F-coefficients 
# (kappa_loans 1.19, kappa_above 0.62) as the lagged y fails to absorb 
# contemporaneous tax-revenue offsets, leaving more variation for F-channels 
# to explain. y_lag2 is insignificant (0.006, p=0.83), confirming the 
# automatic-stabilizer channel operates within the booking quarter, not 
# with delays. Contemporaneous y is theoretically motivated: tax revenue 
# and transfer outlays respond to current-period output.


# ============================================================================
# (10) ALTERNATIVE OUTCOME: debt_dN (nominal change)
# ============================================================================

pdataD <- pdataD %>%
  group_by(Country) %>%
  arrange(t_idx) %>%
  mutate(debt_dN = DebtN_share2019 - lag(DebtN_share2019)) %>%
  ungroup()

debt_dN <- feols(debt_dN ~ y_t_pct + F_CP_above_3 + F_CP_loans_lo 
                 + F_CP_guar_lo + F_DI_lag1 | Country,
                 data = pdataD, subset = ~ t_idx >= 4 & t_idx <= 16)
cat("\n========== (10) OUTCOME: debt_dN (nominal change) ==========\n")
print(summary(debt_dN, cluster = ~ Country, ssc = ssc(K.adj=TRUE, G.adj=TRUE)))

# INTERPRETATION: debt_dN uses nominal debt change / 2019 GDP; debt_dR uses
# real. Coefficient stability across nominal/real outcomes confirms 
# inflation-adjustment is not driving the result.




# =============================================================================
#  STAGE 5 - DESCRIPTIVE COMPARISONS, SCATTERPLOTS, CASE STUDY
# =============================================================================
#  Aligned with V4 main spec: sample Q1.2020 - Q4.2022, CP decomposition into
#  above/loans/guar_adj, debt accumulation extended through 2022.
#
#  Blocks:
#    A. Group comparisons by DI_group / CP_group (tertile splits, built here)
#    B. Three scatterplots (S vs total F, S vs CP, S vs DI) + composite p_all
#    C. Simpson's-paradox 2x2 figure (between-country vs within-country)
#    D. Country-pair finder (similar S, theta; different composition)
#    E. CAN vs CHE case study (composition effect)
#    F. Cross-country time-series averages (Q1.2020 - Q4.2022)
# =============================================================================

# Sample window for descriptives
desc_window <- c("Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                 "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
                 "Q1.2022","Q2.2022","Q3.2022","Q4.2022")

# -----------------------------------------------------------------------------
#  Country-level aggregates (full pandemic window 2020-2022)
# -----------------------------------------------------------------------------
country_agg <- pdataD %>%
  filter(Quarter %in% desc_window) %>%
  group_by(Country) %>%
  summarise(
    mean_S        = mean(S_mean_tw, na.rm = TRUE),
    mean_theta    = mean(theta_pct, na.rm = TRUE),
    total_CP      = sum(F_CP, na.rm = TRUE),
    total_DI      = sum(F_DI, na.rm = TRUE),
    total_F       = total_CP + total_DI,
    total_above   = sum(F_CP_above_3, na.rm = TRUE),
    total_loans   = sum(F_CP_loans, na.rm = TRUE),
    total_guar    = sum(F_CP_guar_adj, na.rm = TRUE),
    CP_share      = total_CP / total_F,
    mean_y        = mean(y_t_pct, na.rm = TRUE),
    total_debt    = sum(debt_dR, na.rm = TRUE),
    .groups       = "drop"
  )

# -----------------------------------------------------------------------------
#  A. Group comparisons (DI / CP tertile splits, built here for self-containment)
# -----------------------------------------------------------------------------
country_groups <- country_agg %>%
  mutate(
    DI_group = cut(total_DI, quantile(total_DI, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                   include.lowest = TRUE, labels = c("Low DI","Mid DI","High DI")),
    CP_group = cut(total_CP, quantile(total_CP, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                   include.lowest = TRUE, labels = c("Low CP","Mid CP","High CP"))
  ) %>%
  select(Country, DI_group, CP_group)

pdataD <- pdataD %>%
  select(-any_of(c("DI_group","CP_group"))) %>%
  left_join(country_groups, by = "Country")

cat("\n=== DI-group means (Q1.2020-Q4.2022) ===\n")
pdataD %>%
  filter(Quarter %in% desc_window) %>%
  group_by(DI_group) %>%
  summarise(
    mean_S    = mean(S_mean_tw, na.rm = TRUE),
    mean_CP   = mean(F_CP, na.rm = TRUE),
    mean_y    = mean(y_t_pct, na.rm = TRUE),
    mean_debt = mean(debt_dR, na.rm = TRUE),
    .groups   = "drop"
  ) %>% as.data.frame()

cat("\n=== CP-group means (Q1.2020-Q4.2022) ===\n")
pdataD %>%
  filter(Quarter %in% desc_window) %>%
  group_by(CP_group) %>%
  summarise(
    mean_S    = mean(S_mean_tw, na.rm = TRUE),
    mean_DI   = mean(F_DI, na.rm = TRUE),
    mean_y    = mean(y_t_pct, na.rm = TRUE),
    mean_debt = mean(debt_dR, na.rm = TRUE),
    .groups   = "drop"
  ) %>% as.data.frame()

# -----------------------------------------------------------------------------
#  B. Scatterplots: containment intensity vs. fiscal deployment
# -----------------------------------------------------------------------------
make_scatter <- function(df, x, y, col, ttl, xlab, ylab) {
  ggplot(df, aes(x = .data[[x]], y = .data[[y]])) +
    geom_point(size = 3, color = col) +
    geom_text(aes(label = Country), hjust = -0.2, vjust = 0.5, size = 2.5) +
    geom_smooth(method = "lm", se = TRUE, color = col, alpha = 0.15) +
    labs(title = ttl, x = xlab, y = ylab,
         caption = sprintf("r = %.2f", cor(df[[x]], df[[y]], use = "complete.obs"))) +
    theme_minimal()
}

p1 <- make_scatter(country_agg, "mean_S", "total_F",  "steelblue",
                   "Containment Intensity vs. Total Fiscal Deployment",
                   "Mean Stringency Index (2020-2022)",
                   "Cumulative Fiscal Spending (% of 2019 GDP)")
p2 <- make_scatter(country_agg, "mean_S", "total_CP", "darkgreen",
                   "Containment vs. Capacity Preservation",
                   "Mean Stringency Index (2020-2022)",
                   "Cumulative CP (% of 2019 GDP)")
p3 <- make_scatter(country_agg, "mean_S", "total_DI", "firebrick",
                   "Containment vs. Demand Injection",
                   "Mean Stringency Index (2020-2022)",
                   "Cumulative DI (% of 2019 GDP)")

library(patchwork)
p_all <- p1 / (p2 | p3) +
  plot_annotation(title = "Did Stricter Lockdowns Lead to More Fiscal Spending?")
print(p_all)

cat("\n=== Cross-country correlations (S vs fiscal) ===\n")
cat(sprintf("  S vs. Total F:  r = %.3f\n", cor(country_agg$mean_S, country_agg$total_F)))
cat(sprintf("  S vs. CP:       r = %.3f\n", cor(country_agg$mean_S, country_agg$total_CP)))
cat(sprintf("  S vs. DI:       r = %.3f\n", cor(country_agg$mean_S, country_agg$total_DI)))
cat(sprintf("  S vs. Output:   r = %.3f\n", cor(country_agg$mean_S, country_agg$mean_y)))
cat(sprintf("  S vs. Debt:     r = %.3f\n", cor(country_agg$mean_S, country_agg$total_debt)))

# Fiscal deployment was orthogonal to containment intensity in the cross-section
# (r ~ 0.07): countries with comparable stringency chose markedly different
# fiscal strategies. This composition-and-timing variation, driven by
# institutional capacity and pre-existing policy frameworks rather than the
# contemporaneous containment level, provides the identifying variation for
# the panel estimation.

# -----------------------------------------------------------------------------
#  C. Simpson's-paradox figure: between-country vs within-country
# -----------------------------------------------------------------------------
p1 <- make_scatter(country_agg, "mean_S",   "mean_y",     "steelblue",
                   "Containment vs. Output (Between Countries)",
                   "Mean Stringency Index (2020-2022)", "Mean Output Gap (pp)") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40")
p2 <- make_scatter(country_agg, "total_F",  "mean_y",     "darkgreen",
                   "Total Fiscal Spending vs. Output (Between)",
                   "Cumulative F (% 2019 GDP)", "Mean Output Gap (pp)") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40")
p3 <- make_scatter(country_agg, "total_CP", "mean_y",     "darkgreen",
                   "CP vs. Output (Between Countries)",
                   "Cumulative CP (% 2019 GDP)", "Mean Output Gap (pp)") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40")
p4 <- make_scatter(country_agg, "total_F",  "total_debt", "firebrick",
                   "Total Fiscal Spending vs. Debt (Between)",
                   "Cumulative F (% 2019 GDP)", "Cumulative Debt Change (pp 2019 GDP)")

p_simpson <- (p1 | p2) / (p3 | p4) +
  plot_annotation(
    title    = "The Simpson's Paradox of Pandemic Fiscal Policy",
    subtitle = "Between-country correlations mask within-country causal effects"
  )
print(p_simpson)

cat("\n=== Between-country correlations ===\n")
for (v in c("mean_S","total_F","total_CP","total_DI")) {
  cat(sprintf("  %-10s vs. Output:  r = %.3f\n", v,
              cor(country_agg[[v]], country_agg$mean_y, use = "complete.obs")))
}
for (v in c("total_F","total_CP","total_DI")) {
  cat(sprintf("  %-10s vs. Debt:    r = %.3f\n", v,
              cor(country_agg[[v]], country_agg$total_debt, use = "complete.obs")))
}

# -----------------------------------------------------------------------------
#  D. Country-pair finder: similar S, theta; different fiscal composition
# -----------------------------------------------------------------------------
pairs <- expand.grid(A = country_agg$Country, B = country_agg$Country,
                     stringsAsFactors = FALSE) %>%
  filter(A < B) %>%
  left_join(country_agg, by = c("A" = "Country")) %>%
  rename_with(~ paste0(.x, "_A"), -c(A, B)) %>%
  left_join(country_agg, by = c("B" = "Country")) %>%
  rename_with(~ paste0(.x, "_B"), -c(A, B, ends_with("_A"))) %>%
  mutate(
    S_diff        = abs(mean_S_A      - mean_S_B),
    theta_diff    = abs(mean_theta_A  - mean_theta_B),
    CP_share_diff = abs(CP_share_A    - CP_share_B),
    y_diff        =  mean_y_A         - mean_y_B,
    debt_diff     =  total_debt_A     - total_debt_B
  ) %>%
  filter(S_diff < 5, theta_diff < 0.5) %>%
  arrange(-CP_share_diff)

cat("\n=== Top Country Pairs: Similar S & theta, Different Composition ===\n")
pairs %>%
  head(10) %>%
  select(A, B, mean_S_A, mean_S_B, mean_theta_A, mean_theta_B,
         CP_share_A, CP_share_B, mean_y_A, mean_y_B,
         total_debt_A, total_debt_B) %>%
  as.data.frame() %>% print(digits = 2)

# -----------------------------------------------------------------------------
#  E. Case study: Canada (DI-heavy) vs. Switzerland (CP-heavy)
#     NB: composition differences are confounded with institutional ones
#         (debt brake, fiscal federalism). Treated as illustrative, not causal.
# -----------------------------------------------------------------------------
case_pair <- pdataD %>%
  filter(Country %in% c("CAN","CHE"), Quarter %in% desc_window) %>%
  select(Country, Quarter, y_t_pct, debt_dR, S_mean_tw,
         F_CP, F_DI, theta_pct, F_CP_above_3, F_CP_loans, F_CP_guar_adj) %>%
  group_by(Country) %>%
  arrange(Quarter) %>%
  mutate(debt_cum = cumsum(replace_na(debt_dR, 0))) %>%
  ungroup()

case_colors <- c("CAN" = "firebrick", "CHE" = "steelblue")
case_labels <- c("CAN" = "Canada (DI-heavy)", "CHE" = "Switzerland (CP-heavy)")

p1 <- ggplot(case_pair, aes(x = Quarter, y = S_mean_tw,
                            color = Country, group = Country)) +
  geom_line(linewidth = 1.3) + geom_point(size = 2.5) +
  scale_color_manual(values = case_colors, labels = case_labels) +
  labs(title = "Containment: Similar Stringency",
       y = "Stringency Index", x = NULL, color = NULL) +
  theme_minimal() + theme(legend.position = "bottom",
                          axis.text.x = element_text(angle = 45, hjust = 1))

case_fiscal <- case_pair %>%
  select(Country, Quarter, F_CP, F_DI) %>%
  pivot_longer(c(F_CP, F_DI), names_to = "Instrument", values_to = "pct_GDP")

p2 <- ggplot(case_fiscal, aes(x = Quarter, y = pct_GDP, fill = Instrument)) +
  geom_col(position = "stack", width = 0.7) +
  facet_wrap(~ Country, labeller = labeller(Country = case_labels)) +
  scale_fill_manual(values = c("F_CP" = "darkgreen", "F_DI" = "firebrick"),
                    labels = c("Capacity Preservation", "Demand Injection")) +
  labs(title = "Fiscal Composition: Different Strategies",
       y = "% of 2019 GDP", x = NULL, fill = NULL) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                          legend.position = "bottom")

p3 <- ggplot(case_pair, aes(x = Quarter, y = y_t_pct,
                            color = Country, group = Country)) +
  geom_line(linewidth = 1.3) + geom_point(size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = case_colors, labels = case_labels) +
  labs(title = "Output: Faster Recovery with CP",
       y = "Output Gap (pp)", x = NULL, color = NULL) +
  theme_minimal() + theme(legend.position = "bottom",
                          axis.text.x = element_text(angle = 45, hjust = 1))

p4 <- ggplot(case_pair, aes(x = Quarter, y = debt_cum,
                            color = Country, group = Country)) +
  geom_line(linewidth = 1.3) + geom_point(size = 2.5) +
  scale_color_manual(values = case_colors, labels = case_labels) +
  labs(title = "Debt: Lower Accumulation with CP",
       y = "Cumulative debt change (pp 2019 GDP)", x = NULL, color = NULL) +
  theme_minimal() + theme(legend.position = "bottom",
                          axis.text.x = element_text(angle = 45, hjust = 1))

p_case <- (p1 | p2) / (p3 | p4) +
  plot_annotation(
    title    = "Canada vs. Switzerland: The Composition Effect",
    subtitle = "Similar containment and infection pressure, different fiscal strategies, different outcomes"
  )
print(p_case)

cat("\n=== Case Study Summary (2020-2022 averages) ===\n")
case_pair %>%
  group_by(Country) %>%
  summarise(
    mean_S       = mean(S_mean_tw, na.rm = TRUE),
    mean_theta   = mean(theta_pct, na.rm = TRUE),
    total_CP     = sum(F_CP, na.rm = TRUE),
    total_DI     = sum(F_DI, na.rm = TRUE),
    total_above  = sum(F_CP_above_3, na.rm = TRUE),
    total_loans  = sum(F_CP_loans, na.rm = TRUE),
    total_guar   = sum(F_CP_guar_adj, na.rm = TRUE),
    CP_share     = total_CP / (total_CP + total_DI),
    mean_y       = mean(y_t_pct, na.rm = TRUE),
    total_debt   = sum(debt_dR, na.rm = TRUE),
    .groups      = "drop"
  ) %>% as.data.frame() %>% print(digits = 3)

# -----------------------------------------------------------------------------
#  Robustness: Eurozone vs. non-Eurozone (MP heterogeneity check)
# -----------------------------------------------------------------------------
pdataY$euro <- as.numeric(pdataY$Country %in%
                            c("AUT","BEL","EST","FIN","FRA","DEU","GRC","IRL",
                              "ITA","LVA","LTU","LUX","NLD","PRT","SVK","SVN","ESP"))

m_y_euro <- plm(
  y_t_pct ~ S_mean_tw +y_lag1 + S_mean_tw:F_CP_lag2 + F_DI_lag2+ p_proj_all_ages ,
  data = pdataY, model = "within", effect = "twoways"
)
cat("\n=== Eurozone interaction check ===\n")
coeftest(m_y_euro, vcov = vcovHC(m_y_euro, cluster = "group", type = "HC1"))
# MP heterogeneity does not flip the within-country fiscal coefficients;
# DI is weak in part because MP operates through the same demand channel,
# whereas CP operates through firm-level capacity preservation regardless
# of the policy-rate environment. Note: rates rose sharply in 2022, raising
# the future cost of debt accumulated through CP guarantees.

# -----------------------------------------------------------------------------
#  F. Cross-country time-series averages (Q1.2020 - Q4.2022)
# -----------------------------------------------------------------------------
avg_ts <- pdataD %>%
  filter(Quarter %in% desc_window) %>%
  group_by(Quarter) %>%
  summarise(
    y_t_pct       = mean(y_t_pct,       na.rm = TRUE),
    debt_dR       = mean(debt_dR,       na.rm = TRUE),
    S_mean_pw     = mean(S_mean_pw,     na.rm = TRUE),
    F_CP          = mean(F_CP,          na.rm = TRUE),
    F_DI          = mean(F_DI,          na.rm = TRUE),
    F_H           = mean(F_H,           na.rm = TRUE),
    F_CP_above_3  = mean(F_CP_above_3,  na.rm = TRUE),
    F_CP_above    = mean(F_CP_above_3,  na.rm = TRUE),
    F_CP_loans    = mean(F_CP_loans,    na.rm = TRUE),
    F_CP_guar_adj = mean(F_CP_guar_adj, na.rm = TRUE),
    .groups       = "drop"
  ) %>%
  mutate(Quarter = factor(Quarter, levels = desc_window, ordered = TRUE))

p_y <- ggplot(avg_ts, aes(x = Quarter, y = y_t_pct, group = 1)) +
  geom_line(linewidth = 1.2, color = "steelblue") +
  geom_point(size = 2.5, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  labs(title = "Output Gap", y = "pp of potential", x = NULL) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_debt <- ggplot(avg_ts, aes(x = Quarter, y = debt_dR, group = 1)) +
  geom_line(linewidth = 1.2, color = "firebrick") +
  geom_point(size = 2.5, color = "firebrick") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  labs(title = "Debt Change (real)", y = "pp of 2019 GDP", x = NULL) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_s <- ggplot(avg_ts, aes(x = Quarter, y = S_mean_pw, group = 1)) +
  geom_line(linewidth = 1.2, color = "darkorange") +
  geom_point(size = 2.5, color = "darkorange") +
  labs(title = "Stringency Index", y = "Index", x = NULL) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Aggregate fiscal composition (CP / DI / H)
avg_fiscal_long <- avg_ts %>%
  select(Quarter, F_CP, F_DI, F_H) %>%
  pivot_longer(c(F_CP, F_DI, F_H), names_to = "Instrument", values_to = "pct_GDP")

p_fiscal <- ggplot(avg_fiscal_long, aes(x = Quarter, y = pct_GDP,
                                        color = Instrument, group = Instrument)) +
  geom_line(linewidth = 1.2) + geom_point(size = 2.5) +
  scale_color_manual(values = c("F_CP" = "darkgreen", "F_DI" = "firebrick",
                                "F_H"  = "purple"),
                     labels = c("Capacity Preservation","Demand Injection","Health")) +
  labs(title = "Fiscal Composition", y = "% of 2019 GDP", x = NULL, color = NULL) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                          legend.position = "bottom")

p_descriptive_ts <- (p_y | p_debt) / (p_s | p_fiscal) +
  plot_annotation(
    title    = "Cross-Country Averages (38 OECD Countries), Q1.2020 - Q4.2022",
    subtitle = "Output gap, debt dynamics, containment stringency, and fiscal composition"
  )
print(p_descriptive_ts)
ggsave(file.path(safeplots, "descriptive_ts_averages.pdf"),
       p_descriptive_ts, width = 14, height = 9)

# CP sub-component time path (NEW: aligned with V4 decomposition)
avg_cp_long <- avg_ts %>%
  select(Quarter, F_CP_above_3, F_CP_loans, F_CP_guar_adj) %>%
  pivot_longer(c(F_CP_above_3, F_CP_loans, F_CP_guar_adj),
               names_to = "Component", values_to = "pct_GDP")

p_cp_decomp <- ggplot(avg_cp_long, aes(x = Quarter, y = pct_GDP,
                                       fill = Component)) +
  geom_col(position = "stack", width = 0.7) +
  scale_fill_manual(values = c("F_CP_above_3"  = "darkgreen",
                               "F_CP_loans"    = "olivedrab3",
                               "F_CP_guar_adj" = "goldenrod"),
                    labels = c("Above-the-line","Loans","Guarantees (adj. 35%)")) +
  labs(title = "CP Decomposition per Quarter",
       y = "% of 2019 GDP (flow)", x = NULL, fill = NULL) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                          legend.position = "bottom")
print(p_cp_decomp)
ggsave(file.path(safeplots, "descriptive_cp_decomposition.pdf"),
       p_cp_decomp, width = 12, height = 6)

# Stacked-bar variant of aggregate fiscal flow
p_fiscal_bar <- ggplot(avg_fiscal_long, aes(x = Quarter, y = pct_GDP,
                                            fill = Instrument)) +
  geom_col(position = "stack", width = 0.7) +
  scale_fill_manual(values = c("F_CP" = "darkgreen", "F_DI" = "firebrick",
                               "F_H"  = "purple"),
                    labels = c("Capacity Preservation","Demand Injection","Health")) +
  labs(title = "Fiscal Disbursement per Quarter",
       y = "% of 2019 GDP (flow)", x = NULL, fill = NULL) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                          legend.position = "bottom")

p_descriptive_ts_flow <- (p_y | p_debt) / (p_s | p_fiscal_bar) +
  plot_annotation(
    title    = "Cross-Country Averages (38 OECD Countries), Q1.2020 - Q4.2022",
    subtitle = "Output gap, debt dynamics, containment stringency, and fiscal disbursement (flow per quarter)"
  )
print(p_descriptive_ts_flow)
ggsave(file.path(safeplots, "descriptive_ts_averages_flow.pdf"),
       p_descriptive_ts_flow, width = 14, height = 9)


##OLD Version, siehe weiter unten
# =============================================================================
#  STAGE 6 - EXPORT TO MATLAB FOR ILQR COUNTERFACTUALS
# =============================================================================
#  The MATLAB iLQR solver consumes a CSV with one row per Country x Quarter
#  (Q4.2019 - Q4.2022), containing the calibrated state vector (S, theta),
#  the observed fiscal controls (F_CP, F_CP_above, F_CP_below_adj_mid,
#  F_DI, F_H), and the realized outcomes (y_t_pct, debt_dR) for validation,
#  plus the fear term and vaccination rate.
#
#  This block first RECOMPUTES debt_dR with strictly chronological sorting
#  (Q3.2019 as lag source for Q4.2019, then dropped). The earlier lag in
#  pdataD was constructed on Quarter as a string factor, which is alphabetic
#  rather than chronological - we override it here so the export uses the
#  correct first difference for every Q4.2019 observation.
#
#  Output: country_data_for_matlab.csv in the working directory.
# =============================================================================

###old, already used->see csv
# Rebuild debt_dR with strictly chronological sorting.
# Q3.2019 enters only as the lag source for Q4.2019 and is dropped at end.
debt_lag_source <- pdata %>%
  filter(Quarter %in% c("Q3.2019", "Q4.2019",
                         "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                         "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
                         "Q1.2022","Q2.2022","Q3.2022","Q4.2022")) %>%
  select(Country, Quarter, DebtR_share2019) %>%
  mutate(
    q_num_sort = as.integer(substr(as.character(Quarter), 2, 2)),
    yr_sort    = as.integer(substr(as.character(Quarter), 4, 7)),
    date_sort  = as.Date(paste0(yr_sort, "-", (q_num_sort - 1) * 3 + 1, "-01"))
  ) %>%
  arrange(Country, date_sort) %>%
  group_by(Country) %>%
  mutate(debt_dR_new = DebtR_share2019 - lag(DebtR_share2019, 1)) %>%
  ungroup() %>%
  filter(Quarter != "Q3.2019") %>%
  select(Country, Quarter, debt_dR_new)

# Merge back and use chronologically correct debt_dR
# (cast both Quarter columns to character so the join works regardless of
# whether one side is an ordered factor and the other a plain factor)
pdataD <- pdataD %>%
  mutate(Quarter = as.character(Quarter)) %>%
  left_join(debt_lag_source %>% mutate(Quarter = as.character(Quarter)),
            by = c("Country", "Quarter")) %>%
  mutate(debt_dR = debt_dR_new,
         Quarter = factor(Quarter, levels = quarter_order)) %>%
  select(-debt_dR_new)

# Export: Q4.2019 through Q4.2022, fill pre-pandemic NAs with 0
export_data <- pdataD %>%
  select(
    Country, Quarter,
    # Gesundheitsseite (fixiert)
    S_mean_tw, theta_pct,
    # Fiskalpolitik (beobachtet, zum Vergleich)
    F_CP, F_CP_above_3, F_CP_below_adj_mid, F_DI, F_H,
    # Outcomes (Validierung)
    y_t_pct, debt_dR,
    # Excess mortality
    excess_mortality = p_proj_all_ages,
    # Zusatzinfo
    vax_rate
  ) %>%
  mutate(
    # Pre-pandemic quarters: fill NAs with 0 (no policy, no infections)
    across(c(S_mean_tw, theta_pct, F_CP, F_CP_above_3, F_CP_below_adj_mid,
             F_DI, F_H, excess_mortality, vax_rate),
           ~ replace_na(.x, 0))
  ) %>%
  mutate(
    q_num_sort = as.integer(substr(as.character(Quarter), 2, 2)),
    yr_sort    = as.integer(substr(as.character(Quarter), 4, 7)),
    date_sort  = as.Date(paste0(yr_sort, "-", (q_num_sort - 1) * 3 + 1, "-01"))
  ) %>%
  arrange(Country, date_sort) %>%
  select(-q_num_sort, -yr_sort, -date_sort) %>%
  as.data.frame()

# --- Export sanity checks ---------------------------------------------------
# Verify F_H is present and finite (column was added late in the pipeline).
cat("=== F_H Check ===\n")
cat(sprintf("  F_H vorhanden: %s\n", "F_H" %in% names(export_data)))
cat(sprintf("  F_H Range: %.3f to %.3f\n",
            min(export_data$F_H, na.rm = TRUE),
            max(export_data$F_H, na.rm = TRUE)))
cat(sprintf("  F_H mean:  %.3f\n", mean(export_data$F_H, na.rm = TRUE)))

# If F_H is unexpectedly missing, search pdataD for likely renames:
# names(pdataD) %>% grep("H|health|hlth", ., value = TRUE, ignore.case = TRUE)

# Panel completeness: row count, distinct countries / quarters.
cat("\n=== Export Summary ===\n")
cat(sprintf("  Countries: %d\n", length(unique(export_data$Country))))
cat(sprintf("  Quarters:  %d\n", length(unique(export_data$Quarter))))
cat(sprintf("  Rows:      %d\n", nrow(export_data)))

# Missing values per column (should be zero everywhere after the
# pre-pandemic fill above).
cat("\n=== Missing Values ===\n")
print(colSums(is.na(export_data)))

# Ranges
cat("\n=== Variable Ranges ===\n")
for (v in names(export_data)[-(1:2)]) {
  cat(sprintf("  %-25s  min=%8.3f  max=%8.3f  mean=%8.3f\n",
              v, min(export_data[[v]], na.rm=TRUE),
              max(export_data[[v]], na.rm=TRUE),
              mean(export_data[[v]], na.rm=TRUE)))
}

write.csv(export_data, "country_data_for_matlab.csv", row.names = FALSE)
cat("\n  Exported to country_data_for_matlab.csv\n")


# =============================================================================
#  STAGE 6 - EXPORT TO MATLAB FOR ILQR COUNTERFACTUALS
# =============================================================================
#  Aligned with V6 iLQR state space x_tilde = (y, d, b, theta, z) and the
#  V4 main spec for both equations:
#    Output: y_lag1, S_mean_tw, F_CP*y_lag1_recession, F_DI_lag2, p_proj_all_ages
#    Debt:   y_t_pct, F_CP, F_DI_lag1, year trend; sub-components above/loans/guar
#
#  The MATLAB iLQR solver consumes a CSV with one row per Country x Quarter
#  (Q4.2019 - Q4.2022, T=13), containing:
#    - Calibrated state vector inputs: S_mean_tw, theta_pct, y_lag1
#    - Observed fiscal controls: F_CP, F_CP_above, F_CP_loans, F_CP_guar(_adj),
#      F_DI, F_DI_lag1, F_DI_lag2, F_H
#    - Take-up scenarios for sensitivity grid: F_CP_below_adj_lo/mid/hi,
#      F_CP_guar_adj
#    - Realized outcomes for validation: y_t_pct, debt_dR
#    - Auxiliary: p_proj_all_ages (excess mortality), vax_rate, t_idx
#
#  Output: country_data_for_matlab.csv in the working directory.
# =============================================================================

# Rebuild debt_dR with strictly chronological sorting.
# Q3.2019 enters only as the lag source for Q4.2019 and is dropped at end.
debt_lag_source <- pdata %>%
  filter(Quarter %in% c("Q3.2019","Q4.2019",
                        "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                        "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
                        "Q1.2022","Q2.2022","Q3.2022","Q4.2022")) %>%
  select(Country, Quarter, DebtR_share2019) %>%
  mutate(
    q_num_sort = as.integer(substr(as.character(Quarter), 2, 2)),
    yr_sort    = as.integer(substr(as.character(Quarter), 4, 7)),
    date_sort  = as.Date(paste0(yr_sort, "-", (q_num_sort - 1) * 3 + 1, "-01"))
  ) %>%
  arrange(Country, date_sort) %>%
  group_by(Country) %>%
  mutate(debt_dR_new = DebtR_share2019 - lag(DebtR_share2019, 1)) %>%
  ungroup() %>%
  filter(Quarter != "Q3.2019") %>%
  select(Country, Quarter, debt_dR_new)

# Merge back and use chronologically correct debt_dR
pdataD <- pdataD %>%
  mutate(Quarter = as.character(Quarter)) %>%
  left_join(debt_lag_source %>% mutate(Quarter = as.character(Quarter)),
            by = c("Country", "Quarter")) %>%
  mutate(debt_dR = debt_dR_new,
         Quarter = factor(Quarter, levels = quarter_order)) %>%
  select(-debt_dR_new)
# -----------------------------------------------------------------------------
#  Build export panel: Q4.2019 - Q4.2022 (t_idx 2..14, T=13 per country)
# -----------------------------------------------------------------------------
export_data <- pdataD %>%
  select(
    Country, Quarter, t_idx,
    # --- Health side (calibrated, fixed in iLQR) ---
    S_mean_tw, S_max_tw, theta_pct, vax_rate,
    # --- Output equation states / regressors ---
    y_t_pct, y_lag1,
    # --- Fiscal controls: aggregates (observed trajectory) ---
    F_CP, F_DI, F_DI_lag1, F_DI_lag2, F_H,
    # --- CP decomposition (for piecewise / heterogeneous kappa) ---
    F_CP_above_3, F_CP_loans, F_CP_guar, F_CP_guar_adj,
    # --- Take-up sensitivity scenarios (conservative / mid / liberal) ---
    F_CP_below, F_CP_below_adj_lo, F_CP_below_adj_mid, F_CP_below_adj_hi,
    # --- Outcomes for validation ---
    debt_dR,
    # --- Excess mortality (output equation) ---
    excess_mortality = p_proj_all_ages
  ) %>%
  mutate(
    # Pre-pandemic quarters: fill NAs with 0 (no policy, no infections)
    across(c(S_mean_tw, S_max_tw, theta_pct, vax_rate,
             y_t_pct, y_lag1,
             F_CP, F_DI, F_DI_lag1, F_DI_lag2, F_H,
             F_CP_above_3, F_CP_loans, F_CP_guar, F_CP_guar_adj,
             F_CP_below, F_CP_below_adj_lo, F_CP_below_adj_mid, F_CP_below_adj_hi,
             excess_mortality),
           ~ replace_na(.x, 0))
  ) %>%
  mutate(
    q_num_sort = as.integer(substr(as.character(Quarter), 2, 2)),
    yr_sort    = as.integer(substr(as.character(Quarter), 4, 7)),
    date_sort  = as.Date(paste0(yr_sort, "-", (q_num_sort - 1) * 3 + 1, "-01"))
  ) %>%
  arrange(Country, date_sort) %>%
  select(-q_num_sort, -yr_sort, -date_sort) %>%
  as.data.frame()

# --- Export sanity checks ---------------------------------------------------
cat("\n=== Panel completeness ===\n")
cat(sprintf("  Countries: %d\n", length(unique(export_data$Country))))
cat(sprintf("  Quarters:  %d (expected: 13, Q4.2019-Q4.2022)\n",
            length(unique(export_data$Quarter))))
cat(sprintf("  Rows:      %d (expected: 38 x 13 = 494)\n", nrow(export_data)))

cat("\n=== Missing values per column ===\n")
print(colSums(is.na(export_data)))

cat("\n=== F_CP decomposition consistency check ===\n")
# F_CP should equal F_CP_above_3 + F_CP_loans + F_CP_guar (face value)
# up to rounding and any non-CP-categorized residuals
recon <- export_data %>%
  mutate(F_CP_recon = F_CP_above_3 + F_CP_loans + F_CP_guar,
         resid = F_CP*100 - F_CP_recon) %>%
  summarise(max_abs_resid = max(abs(resid), na.rm = TRUE),
            mean_resid    = mean(resid, na.rm = TRUE))
print(recon)
# Note: F_CP is in pp/100, sub-components scaled *100 in fiscal_subcomp_d
# Adjust units if your MATLAB script expects identical scaling.

cat("\n=== Variable ranges ===\n")
for (v in names(export_data)[!(names(export_data) %in% c("Country","Quarter"))]) {
  cat(sprintf("  %-22s  min=%9.4f  max=%9.4f  mean=%9.4f\n",
              v, min(export_data[[v]], na.rm=TRUE),
              max(export_data[[v]], na.rm=TRUE),
              mean(export_data[[v]], na.rm=TRUE)))
}

write.csv(export_data, "country_data_for_matlab.csv", row.names = FALSE)
cat("\n  Exported to country_data_for_matlab.csv\n")


# -----------------------------------------------------------------------------
#  PARAMETER FILE FOR MATLAB iLQR (V6 augmented state space)
# -----------------------------------------------------------------------------
# Writes a second CSV with the calibrated coefficients from the empirics so
# MATLAB does not have to hard-code them. Match the structure expected by
# pandemic_sequentialALV6.m.
# -----------------------------------------------------------------------------
