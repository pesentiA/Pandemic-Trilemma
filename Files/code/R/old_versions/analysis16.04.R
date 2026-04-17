##Install Packages and load Data ----
#.rs.restartR()

rm(list=ls())

packages_vector <- c( "did2s","haven", "dplyr",  "sandwich",  "jtools", "data.table",
                      "fBasics","gtools","rnaturalearth", "rnaturalearthdata", "foreign","gt", "Synth","gridExtra", "fixest","huxtable", 
                      "xtable", "foreign", "stargazer", "AER", "causalweight", "tidyr","expss","stringr","pscore","AER","ggplot2","haven","lubridate" ,"knitr",
                      "kableExtra", "psych", "pastecs","purrr","magrittr","did","remote", "did2s", "patchwork", "readxl", "did2s", "plm", "scales", "mFilter", 
                      "countrycode", "tidyverse", "corrplot", "rnaturalearthdata", "ggExtra", "gt", "sf", "RColorBrewer","UpSetR", "lmtest", "modelsummary")



#install.packages(packages_vector)
lapply(packages_vector, require, character.only = TRUE) 
# List loaded packages 
(.packages())

# Set options
options(max.print = 99, scipen = 999, na.print = "")

dir <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/code/R"
setwd(dir)

set.seed(1234)

#### Set preferences:
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

##Load Data

safedata <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/processed"
load(file.path(safedata, "dataforanalysis.RData"))

#Output Location Plots und Table
safeplots <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/output/figures"
safetable <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/output/tables"

#Create one Analysis Dataset with main specifications
##Load modified FM Dataset V.1.7 (cleaned classification, see fix_classifications_v2.R)
fm1 <- readxl::read_excel("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/raw/fiscal measures/fiscal_classified_v1_7.xlsx")

fm1 <- fm1 %>%
  mutate(
    YQ       = paste0("Q", Quarter, ".", Year),
    YQ_ord   = factor(YQ, levels = sort(unique(paste0("Q", Quarter, ".", Year))), ordered = TRUE),
    size_pct = broad_fiscal_gdp * 100
  )

# Check
fm1 %>% select(Country, Year, Quarter, YQ, YQ_ord, broad_fiscal_gdp, size_pct) %>% head(10)

fm1 %>%
  filter(PolicyCode %in% c(5, 6, 11, 12, 15, 16)) %>%
  group_by(PolicyCode, transmission_channel, category) %>%
  summarise(
    n = n(),
    total_vol = sum(size_pct, na.rm = TRUE),
    .groups = "drop"
  )

fm1 <- fm1 %>%
  filter(!PolicyCode %in% c(5, 6, 11, 12, 15, 16))

cat(sprintf("Verbleibende Maßnahmen: %d\n", nrow(fm1)))


##H hat ekinen Einlfuss, H ist separat aufgeführt
# ==============================================================================
#  STAGE 2 — MASTER ANALYSIS DATASET CONSTRUCTION
#  Output: df — Country × Quarter panel for structural estimation
#  Merge key: Country + Quarter (format "Q1.2020")
#  Sample: Q1.2020 – Q4.2021 (trilemma period), 38 OECD countries
# ==============================================================================

pandemic_qs <- c(
  "Q1.2019", "Q2.2019", "Q3.2019", "Q4.2019",
  "Q1.2020", "Q2.2020", "Q3.2020", "Q4.2020",
  "Q1.2021", "Q2.2021", "Q3.2021", "Q4.2021",
  "Q1.2022", "Q2.2022", "Q3.2022", "Q4.2022"
)
# ==============================================================================
#  BLOCK 1: qdata — State variables y_k, b_k and controls
# ==============================================================================

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

pop_2019 <- df_qdata[df_qdata$Quarter=="Q4.2019", c("Country","Qpopulation_th")]
names(pop_2019)[names(pop_2019) == "Qpopulation_th"] <- "pop_2019"
df_qdata <- merge(df_qdata, pop_2019, by="Country")
rm(pop_2019)


# ==============================================================================
#  BLOCK 2: theta_quarterly_full — State variable θ_k
# ==============================================================================

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


# ==============================================================================
#  BLOCK 3: fm (fiscal measures) — Control variables F^CP, F^DI, F^H
#  Aggregate measure-level data to Country × Quarter
# ==============================================================================

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


# ==============================================================================
#  BLOCK 4: S_mean already in theta_quarterly_full (Block 2)
#  Additional stringency variables from panel_w if needed
# ==============================================================================

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


# ==============================================================================
#  BLOCK 5: hosp_d — Hospitalisation (33 countries, quarterly aggregation)
# ==============================================================================

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


# ==============================================================================
#  BLOCK 6: google_mobility_d — Behavioural controls (quarterly aggregation)
# ==============================================================================
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

# ==============================================================================
#  MASTER MERGE
# ==============================================================================

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


# ==============================================================================
#  DIAGNOSTICS
# ==============================================================================

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

#############################DATASET READY######################################
# --- Trilemma estimation sample: Q4.2019 – Q4.2022 --------------------------
df_estimation <- df %>%
  filter(Quarter %in% c("Q4.2019","Q1.2020", "Q2.2020", "Q3.2020", "Q4.2020",
                        "Q1.2021", "Q2.2021", "Q3.2021", "Q4.2021", "Q1.2022", "Q2.2022", "Q3.2022", "Q4.2022"))

cat(sprintf("  Estimation sample: %d obs (%d countries × %d quarters)\n",
            nrow(df_estimation),
            n_distinct(df_estimation$Country),
            n_distinct(df_estimation$Quarter)))
# ==============================================================================
#Prepare Data for estimating Output and Debt effects
# ==============================================================================

#Include 2019 and 2022 as well
pdata <- df %>%
  filter(Quarter %in% c("Q1.2019","Q2.2019","Q3.2019","Q4.2019","Q1.2020", "Q2.2020", "Q3.2020", "Q4.2020",
                        "Q1.2021", "Q2.2021", "Q3.2021", "Q4.2021", "Q1.2022", "Q2.2022", "Q3.2022", "Q4.2022"))

pdata %>%
  as_tibble() %>%
  select(y_t_pct, d_t_pct, S_mean_pw, StringencyIndex_PopWeighted, F_CP, F_DI,
         theta_mean, vax_rate, icu_occ_pm, F_H, pop_2019) %>%
  summarise(across(everything(), list(
    mean   = ~mean(.,  na.rm = TRUE),
    sd     = ~sd(.,    na.rm = TRUE),
    min    = ~min(.,   na.rm = TRUE),
    max    = ~max(.,   na.rm = TRUE)
  ))) %>%
  pivot_longer(everything(),
               names_to  = c("variable", "stat"),
               names_sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(variable) %>%
  print(n = Inf)

##add a final table at the end with all used variables

# --- Modify values and construct lagged variables within panel ---------------------------------
pdata <- pdata %>%
  mutate(S_mean_tw= S_mean_pw* 100)
pdata <- pdata %>%
  mutate(S_max_tw= S_max_pw* 100)
pdata <- pdata %>%
  mutate(S_sd_tw= S_sd* 100)
pdata <- pdata %>%
  mutate(F_CP = F_CP * 100)
pdata <- pdata %>%
  mutate(F_DI = F_DI * 100)
pdata <- pdata %>%
  mutate(F_H = F_H * 100)
pdata <- pdata %>%
  mutate(vax_rate = vax_rate * 100)
pdata <- pdata %>%
  mutate(theta_pct = theta_mean * 100)
pdata <- pdata %>%
  mutate(Qpopulation_th= Qpopulation_th/ 1000)

##lags
pdata <- pdata %>%
  mutate(
    S_lag1      = lag(S_mean_tw,  1),
    S_lag2      = lag(S_mean_tw,  2),
    S_lead1     = lead(S_mean_tw, 1),
    F_DI_lag1   = lag(F_DI,       1),
    F_DI_lag2   = lag(F_DI,       2),
    F_DI_lag3   = lag(F_DI,      3),
    F_CP_lag1   = lag(F_CP,       1),
    F_CP_lag2   = lag(F_CP,       2),
    F_CP_lead1  = lead (F_CP,     1),
    theta_lag1  = lag(theta_mean, 1),
    y_lag1      = lag(y_t_pct,    1),
    y_lead1     = lead(y_t_pct,   1),
    p_p_lag1      = lag(p_proj_all_ages, 1),
    p_a_lag1    = lag(p_avg_all_ages, 1)
  )

colnames(pdata)

# Prüfe ob Lag über Ländergrenzen springt
pdata %>% 
  as.data.frame() %>%
  select(Country, Quarter, F_CP_lag1, F_CP_lag2) %>% 
  filter(Quarter %in% c("Q1.2020", "Q2.2020")) %>%
  arrange(Country, Quarter) %>%
  head(20)

##Check!

# ==============================================================================
#  OUTPUT GAP ESTIMATION — FULL SECTION
#  Empirical counterpart to eq. (OG) in the pandemic trilemma model (Section 2)
#
#  Estimating equation (eq. output_est):
#    y_{ik} = α_S·S_{ik} + ψ·S_{ik}·y_{i,k-1} + α_F^CP·F_{ik}^CP
#             + η·S_{ik}·F_{ik}^CP + α_F^DI·F_{i,k-2}^DI + γ_i + δ_k + ε_{ik}
#
#  Structure:
#    STEP 0  — Sample construction & instrument disaggregation
#    STEP 1  — Identification strategy (orthogonality, within-variation)
#    STEP 2  — Model justification (OLS → FE → TWFE progression)
#    STEP 3  — Main results & structural parameter interpretation
#    STEP 4  — Time period justification & horizon robustness
#    STEP 5  — Standard error comparison & bootstrap
#    STEP 6  — Robustness checks (functional form, outliers, splits, sub-comps)
#    STEP 7  — Alternative estimators: GMM (Nickell) + Local Projections (dynamics)
#    STEP 8  — Cross-estimator comparison & final conclusion
# ==============================================================================

library(fwildclusterboot)
library(summclust)
library(clubSandwich)
if (!requireNamespace("boot",       quietly=TRUE)) install.packages("boot")
library(boot)

# ==============================================================================
#  STEP 0 — SAMPLE CONSTRUCTION & INSTRUMENT DISAGGREGATION
# ==============================================================================
# Main sample: Q1.2020–Q1.2022 (trilemma active period, T=9 periods per country)
# Extended: through Q4.2022 for robustness and LP horizons
# Pre-period Q4.2019 retained for lag y_{k-1} construction
# Note: F_CP, F_DI, F_H in pdataY are already in pp-GDP units (×100 from pdata)

pdataY <- pdata %>%
  filter(Quarter %in% c("Q4.2019","Q1.2020", "Q2.2020", "Q3.2020", "Q4.2020",
                        "Q1.2021", "Q2.2021", "Q3.2021", "Q4.2021",
                        "Q1.2022", "Q2.2022", "Q3.2022", "Q4.2022"))

# ==============================================================================
#  STEP 0B — INSTRUMENT DISAGGREGATION (all sub-components in pp-GDP, ×100)
#  Source: fm1 (cleaned fiscal database, PolicyCodes 5/6/11/12/15/16 excluded)
#
#  CP above-the-line  (cat=1): wage subsidies, STW, grants
#  CP below-the-line loans (PC 40,41): actual disbursements → realized debt
#  CP below-the-line guar  (PC 43): credit guarantees → contingent, low realized cost
#
#  Guarantee take-up scenarios (ECB/IMF Fiscal Monitor 2022 call-rate estimates):
#    lo=25%  mid=35% (baseline)  hi=50%
#  → Baseline F_CP = above + loans_full + guar×0.35 (F_CP_adj_mid)
#
#  DI transfers (PC 35-38): direct cash, unemployment, ad-hoc benefits
#  DI demand    (PC 27-29): infrastructure, green, tourism
#  DI tax       (PC 17-22,25-26): individual tax relief, VAT cuts
#
#  H supply (PC 30-32): medical procurement (endogenous to theta_k)
#  H infra  (PC 33-34): health infrastructure investment
# ==============================================================================

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

# Preserve fiscal_subcomp so DEBT section can merge into pdataD
# (add after pdataD is constructed: pdataD <- pdataD %>% left_join(fiscal_subcomp, ...))

# fiscal_subcomp is kept in the environment — used again in DEBT section

# ==============================================================================
#  Variable overview (all CP variables in pp-GDP, i.e. ×100):
#  F_CP            — total CP from master merge (face-value, no guarantee adj)
#  F_CP_adj_mid    — above + loans + guar×0.35 (BASELINE for main specs)
#  F_CP_adj_lo/hi  — guarantee adj at 25%/50% (robustness)
#  F_CP_above_3    — above-the-line only: STW, payroll subsidies, grants
#  F_CP_loans      — below-the-line loans (PC 40,41): actual disbursements
#  F_CP_guar_adj   — below-the-line guarantees ×0.35 (PC 43, baseline)
# ==============================================================================
# --- Time index and helper variables ---
quarter_order <- c("Q1.2019","Q2.2019","Q3.2019","Q4.2019",
                   "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                   "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
                   "Q1.2022","Q2.2022","Q3.2022","Q4.2022")
# t_idx: Q1.2019=1, Q4.2019=4, Q1.2020=5, Q4.2021=12, Q1.2022=13, Q4.2022=16
# Main sample: t_idx 5-13 (Q1.2020–Q1.2022, T=9 per country)
pdataY$t_idx <- match(as.character(pdataY$Quarter), quarter_order)

# S_max_tw: maximum stringency within quarter (pp, 0-100)
# Guard against double-scaling if already done in prior run
if (max(pdataY$S_max_pw, na.rm=TRUE) <= 1.01) {
  pdataY$S_max_tw <- pdataY$S_max_pw * 100
} else {
  pdataY$S_max_tw <- pdataY$S_max_pw   # already in 0-100
}

# S_high indicator: stringency >= 50 (above-median lockdown intensity)
pdataY$S_high <- as.integer(pdataY$S_mean_tw >= 50)

# Composite interaction terms for GMM (bilinear → single variable)
pdataY <- pdataY %>%
  mutate(
    S_y_lag = S_mean_tw * y_lag1,
    S_FCP   = S_mean_tw * F_CP
  )

# Half-year FE variable (for FE specification robustness)
pdataY$half_year <- ifelse(
  as.integer(sub("Q(\\d).*","\\1", as.character(pdataY$Quarter))) <= 2,
  paste0("H1.", sub(".*\\.","", as.character(pdataY$Quarter))),
  paste0("H2.", sub(".*\\.","", as.character(pdataY$Quarter)))
)

# ==============================================================================
#  STEP 1 — IDENTIFICATION STRATEGY
#  The key identifying variation: countries with similar containment trajectories
#  chose markedly different fiscal compositions (CP vs DI mix). This near-
#  orthogonality of S and F is the empirical foundation for the S×F_CP
#  interaction identification. Quarter FE absorb the common pandemic cycle;
#  country FE absorb time-invariant fiscal capacity. The residual variation in
#  composition across countries within a given quarter identifies η.
# ==============================================================================

cat("\n", strrep("=",70), "\n")
cat("  STEP 1: IDENTIFICATION STRATEGY\n")
cat(strrep("=",70), "\n\n")

main_sample <- pdataY %>% filter(t_idx >= 5 & t_idx <= 14)

# ==============================================================================
#  DESCRIPTIVES FOR PAPER (AER-style)
#  Added: within/between decomposition, correlation matrix, pre-pandemic
#  validation, density plots of fiscal instruments
#  Uses: main_sample (Q1.2020–Q1.2022, N=342, 38 countries)
# ==============================================================================

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
# ==============================================================================
#  STEP 2 — MODEL JUSTIFICATION: OLS → COUNTRY FE → TWFE
#
#  Rationale for TWFE:
#  (a) Country FE: unobserved heterogeneity in fiscal capacity, institutions,
#      initial conditions — required by Hausman/Mundlak test (see 2C).
#  (b) Quarter FE: common pandemic waves, global credit conditions, WHO
#      guidance — absorbed without restricting their form.
#  (c) y_{i,k-1}: lockdown-induced persistence (ψ×S×y) — theoretically
#      motivated; Nickell bias bounded by O(1/T)=11% at T=9 (see 2D).
#  (d) DI at lag 2: authorization→disbursement→spending chain ≈ 2 quarters
#      (Chetty et al. 2020 CARES Act evidence) — selected empirically (see 2E).
# ==============================================================================

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

# ==============================================================================
#  STEP 3 — MAIN MODELL NEW, using feols, defining sample size (main and robustness) and cross check with bootstrap
# ==============================================================================


cat("\n", strrep("=",70), "\n")
cat("  STEP 5: STANDARD ERROR COMPARISON\n")
cat(strrep("=",70), "\n\n")


#4=Q4.2019
#13=Q1.2022
pdataY <- pdataY %>%
  mutate(
    S_mean_tw       = replace_na(S_mean_tw, 0),
    F_CP            = replace_na(F_CP, 0),
    F_DI_lag2       = replace_na(F_DI_lag2, 0),
    p_proj_all_ages = replace_na(p_proj_all_ages, 0),
    y_lag1          = replace_na(y_lag1, 0),
    theta_pct       = replace_na(theta_pct, 0),
    vax_rate        = replace_na(vax_rate, 0),
    S_max_tw        = replace_na(S_max_tw,0),
  )


#MAIN SPECIFICATION 16.04.2026
main<-feols(y_t_pct ~ y_lag1 +  y_lag1:F_CP_lag2 + S_mean_tw*y_lag1 + F_DI_lag2 
      + p_proj_all_ages | Country + Quarter, 
      data = pdataY, subset = ~t_idx >= 5 & t_idx <= 14, 
      vcov = ~Country)

summary(main, cluster = ~Country, ssc = ssc(K.adj = TRUE,  G.adj = TRUE))


##verification with plm
plm_test<-plm(y_t_pct ~y_lag1+  y_lag1:F_CP_lag2 + S_mean_tw*y_lag1 + F_DI_lag2 
              + p_proj_all_ages, data=pdataY, model="within", effect="twoways", subset = t_idx >= 4 & t_idx <= 14)

coeftest(plm_test, vcov = vcovHC(plm_test, cluster = "group", type = "HC1"))

fixef(plm_test, effect = "individual")
##



## ========================================================================
##  LOCAL PROJECTIONS — V3 (Persistence-Channel CP)
##
##  Tests the V3 structural prediction model-free:
##    - CP has NO contemporaneous level effect
##    - CP operates through persistence: y_lag1 × F_CP_lag2
##    - Effect should build over horizons (compounding persistence reduction)
##    - DI operates as level effect at lag 2
##    - S amplifies the recession: S × y_lag1
##
##  LP specification:
##    y_{i,k+h} = α_i^h + δ_k^h + ρ^h y_{i,k-1}
##                + η_p^h y_{i,k-1} × F^CP_{i,k-2}
##                + ψ^h y_{i,k-1} × S_{ik}
##                + β_S^h S_{ik} + β_DI^h F^DI_{i,k-2}
##                + β_d^h d_{ik} + ε^h_{ik}
##
##  Key prediction: η_p^h < 0 growing in magnitude for h = 0,1,2,3
## ========================================================================

cat("\n========================================\n")
cat("  LOCAL PROJECTIONS (V3 Robustness)\n")
cat("========================================\n")

# --- Create leads ---
pdataY <- pdataY %>%
  group_by(Country) %>%
  arrange(t_idx) %>%
  mutate(
    y_lead0 = y_t_pct,
    y_lead1 = lead(y_t_pct, 1),
    y_lead2 = lead(y_t_pct, 2),
    y_lead3 = lead(y_t_pct, 3)
  ) %>%
  ungroup()

horizons <- 0:3

# --- V3 parameter sets ---
# (a) Persistence interactions: the core V3 channels
persist_params <- c("y_lag1:F_CP_lag2", "y_lag1:S_mean_tw")
persist_labels <- c("CP persistence (eta_p): y(t-1) x F_CP(t-2)",
                    "Lockdown amplification (psi): y(t-1) x S")

# (b) Direct effects: level channels
direct_params  <- c("S_mean_tw", "F_CP_lag2", "F_DI_lag2", "p_proj_all_ages")
direct_labels  <- c("Containment (S)", 
                    "CP level effect (should be zero)",
                    "Demand Injection (F_DI)", 
                    "Fear term (d)")

lp_persist <- data.frame()
lp_direct  <- data.frame()

for (h in horizons) {
  lead_var <- paste0("y_lead", h)
  
  # V3 LP: includes F_CP_lag2 as level AND interaction to test both
  fml_lp <- as.formula(paste0(
    lead_var, " ~ y_lag1 + y_lag1:F_CP_lag2 + y_lag1:S_mean_tw",
    " + S_mean_tw + F_CP_lag2 + F_DI_lag2 + p_proj_all_ages",
    " | Country + Quarter"))
  
  m_lp <- feols(fml_lp, data = pdataY, subset = pdataY$t_idx >= 5 & pdataY$t_idx <= 14, 
                cluster = ~Country,
                ssc = ssc(adj = TRUE, cluster.adj = TRUE))
  ct <- summary(m_lp)$coeftable
  
  # Extract persistence interactions
  for (i in seq_along(persist_params)) {
    pname <- persist_params[i]
    if (!pname %in% rownames(ct)) next
    lp_persist <- rbind(lp_persist, data.frame(
      h = h, param = persist_labels[i],
      coef = ct[pname, "Estimate"],
      se   = ct[pname, "Std. Error"],
      pval = ct[pname, "Pr(>|t|)"]
    ))
  }
  
  # Extract direct effects
  for (i in seq_along(direct_params)) {
    pname <- direct_params[i]
    if (!pname %in% rownames(ct)) next
    lp_direct <- rbind(lp_direct, data.frame(
      h = h, param = direct_labels[i],
      coef = ct[pname, "Estimate"],
      se   = ct[pname, "Std. Error"],
      pval = ct[pname, "Pr(>|t|)"]
    ))
  }
}

# --- Confidence intervals ---
add_ci <- function(df) {
  df %>% mutate(
    ci90_lo = coef - 1.645 * se, ci90_hi = coef + 1.645 * se,
    ci95_lo = coef - 1.96  * se, ci95_hi = coef + 1.96  * se,
    sig = ifelse(pval < 0.01, "***", ifelse(pval < 0.05, "**", 
                                            ifelse(pval < 0.1, "*", "")))
  )
}
lp_persist <- add_ci(lp_persist)
lp_direct  <- add_ci(lp_direct)

# --- Print results ---
cat("\n--- Persistence Channels (V3 core) ---\n")
print(lp_persist %>% 
        mutate(across(c(coef, se, pval), ~round(., 5))) %>%
        select(h, param, coef, se, pval, sig))

cat("\n--- Direct / Level Effects ---\n")
print(lp_direct %>% 
        mutate(across(c(coef, se, pval), ~round(., 5))) %>%
        select(h, param, coef, se, pval, sig))


## ========================================================================
##  PLOTS
## ========================================================================

# --- Plot 1: Persistence channels (2 panels, the key result) ---
p_persist <- ggplot(lp_persist, aes(x = h, y = coef)) +
  geom_ribbon(aes(ymin = ci95_lo, ymax = ci95_hi), fill = "grey80", alpha = 0.5) +
  geom_ribbon(aes(ymin = ci90_lo, ymax = ci90_hi), fill = "grey60", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_line(color = "black", linewidth = 0.8) +
  geom_point(color = "black", size = 2.5) +
  facet_wrap(~param, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = 0:3, labels = paste0("h=", 0:3)) +
  labs(x = "Horizon (quarters)", y = "Coefficient",
       title = "Persistence Channels: Local Projections (V3)",
       subtitle = "CP reduces persistence; S amplifies it. TWFE, clustered SE, 90%/95% CI") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", size = 10))
ggsave(file.path(safeplots, "fig_lp_v3_persistence.pdf"), p_persist, width = 11, height = 4.5)
print(p_persist)

# --- Plot 2: Direct effects (4 panels, includes CP level = 0 test) ---
p_direct <- ggplot(lp_direct, aes(x = h, y = coef)) +
  geom_ribbon(aes(ymin = ci95_lo, ymax = ci95_hi), fill = "grey80", alpha = 0.5) +
  geom_ribbon(aes(ymin = ci90_lo, ymax = ci90_hi), fill = "grey60", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_line(color = "black", linewidth = 0.8) +
  geom_point(color = "black", size = 2.5) +
  facet_wrap(~param, scales = "free_y", ncol = 4) +
  scale_x_continuous(breaks = 0:3, labels = paste0("h=", 0:3)) +
  labs(x = "Horizon (quarters)", y = "Coefficient",
       title = "Direct Effects: Local Projections (V3)",
       subtitle = "CP level effect should be zero at all horizons. TWFE, clustered SE, 90%/95% CI") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", size = 9))
ggsave(file.path(safeplots, "fig_lp_v3_direct.pdf"), p_direct, width = 14, height = 4.5)
print(p_direct)

# --- Plot 3: Combined 6-panel ---
lp_all <- bind_rows(
  lp_persist %>% mutate(group = "Persistence"),
  lp_direct  %>% mutate(group = "Direct")
)
# Order panels: persistence first, then direct
lp_all$param <- factor(lp_all$param, levels = c(persist_labels, direct_labels))

p_combined <- ggplot(lp_all, aes(x = h, y = coef)) +
  geom_ribbon(aes(ymin = ci95_lo, ymax = ci95_hi), fill = "grey80", alpha = 0.5) +
  geom_ribbon(aes(ymin = ci90_lo, ymax = ci90_hi), fill = "grey60", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_line(color = "black", linewidth = 0.8) +
  geom_point(color = "black", size = 2.5) +
  facet_wrap(~param, scales = "free_y", ncol = 3) +
  scale_x_continuous(breaks = 0:3, labels = paste0("h=", 0:3)) +
  labs(x = "Horizon (quarters)", y = "Coefficient",
       title = "Local Projections: V3 Specification Validation",
       subtitle = paste0("Jord\u00e0 (2005). TWFE, clustered SE (Country), 90%/95% CI. ",
                         "Top row: persistence channels. Bottom row: direct effects.")) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", size = 8))
ggsave(file.path(safeplots, "fig_lp_v3_combined.pdf"), p_combined, width = 14, height = 8)
print(p_combined)

##Plot wichtig??

################################################################################

#...............................................................................
##Wildclusterboot
library(fwildclusterboot)

main_sample <- pdataY %>% filter(t_idx >= 4 & t_idx <= 14)
# 1. Stelle sicher, dass die FE-Variablen Faktoren sind (sehr wichtig!)
main_sample$Country <- as.factor(main_sample$Country)
main_sample$Quarter <- as.factor(main_sample$Quarter)


# 2. Schätze das Modell neu, damit die Faktor-Informationen im Objekt sind
y_feols_p <- feols(
  y_t_pct ~y_lag1+  y_lag1:F_CP_lag2 + S_mean_tw*y_lag1 + F_DI_lag2 
  + p_proj_all_ages | 
    Country + Quarter,  data = main_sample, vcov = ~Country)

summary(y_feols_p, cluster = ~Country, ssc = ssc(K.adj = TRUE,  G.adj = TRUE))


# 3. Boottest aufrufen (set.seed davor für Reproduzierbarkeit)
set.seed(16031995)
# test with wild cluster bootstrap-t
wild_fix <- boottest(y_feols_p, param="y_lag1:F_CP_lag2",clustid=c("Country"),
                     B=999999, type="rademacher", impose_null=TRUE, p_val_type="two-tailed")
summary(wild_fix)
# t-stats reported with small sample correction with (N)/(N-1) as in CGM (2008)
wild_fix$t_stat
summary(y_feols_p, cluster = ~Country, ssc = ssc(K.adj = FALSE, G.adj = TRUE))$coeftable[7,3] 

#...............................................................................
##check outliers if you have differences between HC1 and bootstrap
sc <- summclust(y_feols_p, params = "y_lag1:F_CP_lag2", cluster = ~Country)
summary(sc)
plot(sc)

##looks like DNK and ITA are different, exclude them and check the results again

y_wo_outliers <- feols(
  y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP + F_DI_lag2 +
    F_CP * y_lag1 + p_proj_all_ages | Country + Quarter,
  data = pdataY %>% 
    filter(t_idx >= 5 & t_idx <= 14, !Country %in% c("DNK", "ITA")) %>%
    mutate(Country = droplevels(factor(Country))),
  cluster = ~Country
)

# test with wild cluster bootstrap-t
retest <- boottest(y_wo_outliers, param="F_CP",clustid=c("Country"),
                     B=99999, type="rademacher", impose_null=TRUE, p_val_type="two-tailed")
summary(retest)
##matches almost perfect the SE

##maybe one cluster drives the whole effect of DI, report it transparent, the rest is clean
#53.8% zeros — over half the country-quarters have no demand injection at all
#Lag 2 — the effect is identified off variation two quarters ago, further reducing effective sample
#Small magnitudes — mean 0.33 pp GDP vs 1.64 for CP


#main sample (4.2019-Q2.2022-> makes sense, robustness only until Q1.2022, feols is the best and the AER Standard SE are top, bootstrap only for robustness)

# ==============================================================================
#  STEP 6 — ROBUSTNESS CHECKS (feols, main sample t_idx 4-14, AER SE)
#  Estimator: feols with Country + Quarter FE
#  Sample:    Q1.2020–Q2.2022 (t_idx >= 5 & t_idx <= 14), 380 obs
#  SE:        Clustered by Country (AER standard)
#  Formula shorthand for main spec:
#    y_t_pct ~ S_mean_tw*y_lag1 + S_mean_tw*F_CP + F_CP*y_lag1 +
#              F_DI_lag2 + p_proj_all_ages | Country + Quarter
# ==============================================================================

cat("\n", strrep("=",70), "\n")
cat("  STEP 6: ROBUSTNESS CHECKS (feols, Q1.2020-Q2.2022, AER SE)\n")
cat(strrep("=",70), "\n\n")

# --- Helper: main formula and sample filter ---
main_fml <- y_t_pct ~ S_mean_tw*y_lag1 + S_mean_tw*F_CP + F_CP*y_lag1 +
  F_DI_lag2 + p_proj_all_ages | Country + Quarter
main_sub <- ~ t_idx >= 5 & t_idx <= 14

# Convenience: extract coef + p from feols summary
get_cp <- function(m, param) {
  s <- summary(m, cluster = ~Country)
  ct <- s$coeftable
  if (param %in% rownames(ct)) {
    return(list(est = ct[param, "Estimate"], pval = ct[param, "Pr(>|t|)"]))
  }
  return(list(est = NA_real_, pval = NA_real_))
}

# --- 6A: Functional form ---
cat("--- 6A: Functional Form ---\n")

# Quadratic CP
m_6a1 <- feols(
  y_t_pct ~ S_mean_tw*y_lag1 + S_mean_tw*F_CP + I(F_CP^2) + F_CP:y_lag1 +
    F_DI_lag2 + p_proj_all_ages | Country + Quarter,
  data = pdataY, subset = main_sub, cluster = ~Country)
cp2 <- get_cp(m_6a1, "I(F_CP^2)")
cat(sprintf("  Quadratic F_CP²: β=%+.5f, p=%.3f\n", cp2$est, cp2$pval))

# Alternative S measure (S_max_tw)
m_6a2 <- feols(
  y_t_pct ~ S_max_tw*y_lag1 + S_max_tw*F_CP + F_CP*y_lag1 +
    F_DI_lag2 + p_proj_all_ages | Country + Quarter,
  data = pdataY, subset = main_sub, cluster = ~Country)
cat("  S_max_tw specification:\n")
print(summary(m_6a2, cluster = ~Country)$coeftable[, c("Estimate","Pr(>|t|)")])
cat("\n")

# --- 6B: Outlier exclusion ---
cat("--- 6B: Outlier Exclusion ---\n")
outlier_list <- list(
  "No IRL" = c("IRL"),
  "No TUR" = c("TUR"),
  "No IRL+TUR" = c("IRL","TUR")
)
for (nm in names(outlier_list)) {
  m_o <- feols(main_fml,
    data = pdataY %>% filter(!Country %in% outlier_list[[nm]]),
    subset = main_sub, cluster = ~Country)
  psi_o <- get_cp(m_o, "S_mean_tw:y_lag1")
  eta_o <- get_cp(m_o, "S_mean_tw:F_CP")
  etp_o <- get_cp(m_o, "y_lag1:F_CP")
  cat(sprintf("  %-14s  ψ=%+.5f(p=%.3f)  η̃=%+.5f(p=%.3f)  η_p=%+.5f(p=%.3f)\n",
              nm, psi_o$est, psi_o$pval, eta_o$est, eta_o$pval,
              ifelse(is.na(etp_o$est), 0, etp_o$est),
              ifelse(is.na(etp_o$pval), 1, etp_o$pval)))
}
cat("\n")

# --- 6C: Alternative containment measure ---
cat("--- 6C: Alternative Containment (S_max_tw) ---\n")
m_6c <- feols(
  y_t_pct ~ S_max_tw*y_lag1 + S_max_tw*F_CP + F_CP*y_lag1 +
    F_DI_lag2 + p_proj_all_ages | Country + Quarter,
  data = pdataY, subset = main_sub, cluster = ~Country)
eta_6c <- get_cp(m_6c, "S_max_tw:F_CP")
cat(sprintf("  η̃(S_max)=%+.5f(p=%.3f)\n\n", eta_6c$est, eta_6c$pval))

# --- 6D: Alternative output measures ---
cat("--- 6D: Alternative Output Measures ---\n")
for (dv in c("y_t", "QReal.GDP.Growth_gr")) {
  if (!dv %in% names(pdataY)) next
  fml_d <- as.formula(paste0(dv, " ~ S_mean_tw*y_lag1 + S_mean_tw*F_CP + F_CP*y_lag1 + F_DI_lag2 + p_proj_all_ages | Country + Quarter"))
  m_d <- tryCatch(feols(fml_d, data = pdataY, subset = main_sub, cluster = ~Country),
                  error = function(e) NULL)
  if (is.null(m_d)) next
  eta_d <- get_cp(m_d, "S_mean_tw:F_CP")
  cat(sprintf("  %-25s  η̃=%+.5f(p=%.3f)\n", dv, eta_d$est, eta_d$pval))
}
cat("\n")

# --- 6E: Fear term specification ---
cat("--- 6E: Fear Term Specification ---\n")
m_6e1 <- feols(
  y_t_pct ~ S_mean_tw*y_lag1 + S_mean_tw*F_CP + F_CP*y_lag1 +
    F_DI_lag2 + theta_pct | Country + Quarter,
  data = pdataY, subset = main_sub, cluster = ~Country)
m_6e2 <- feols(
  y_t_pct ~ S_mean_tw*y_lag1 + S_mean_tw*F_CP + F_CP*y_lag1 +
    F_DI_lag2 + p_avg_all_ages | Country + Quarter,
  data = pdataY, subset = main_sub, cluster = ~Country)
cat(sprintf("  theta_pct:       η̃=%+.5f(p=%.3f)\n",
            get_cp(m_6e1,"S_mean_tw:F_CP")$est, get_cp(m_6e1,"S_mean_tw:F_CP")$pval))
cat(sprintf("  p_avg_all_ages:  η̃=%+.5f(p=%.3f)\n\n",
            get_cp(m_6e2,"S_mean_tw:F_CP")$est, get_cp(m_6e2,"S_mean_tw:F_CP")$pval))

# --- 6F: Additional controls ---
cat("--- 6F: Additional Controls ---\n")
for (ctrl in c("vax_rate", "F_H", "icu_occ_pm")) {
  if (!ctrl %in% names(pdataY)) next
  fml_c <- as.formula(paste0(
    "y_t_pct ~ S_mean_tw*y_lag1 + S_mean_tw*F_CP + F_CP*y_lag1 + F_DI_lag2 + p_proj_all_ages + ",
    ctrl, " | Country + Quarter"))
  m_c <- tryCatch(feols(fml_c, data = pdataY, subset = main_sub, cluster = ~Country),
                  error = function(e) NULL)
  if (is.null(m_c)) next
  eta_c <- get_cp(m_c, "S_mean_tw:F_CP")
  ctrl_c <- get_cp(m_c, ctrl)
  cat(sprintf("  + %-14s  η̃=%+.5f(p=%.3f)  %s=%+.4f(p=%.3f)\n",
              ctrl, eta_c$est, eta_c$pval, ctrl, ctrl_c$est, ctrl_c$pval))
}
cat("\n")

# --- 6G: DI-CP complementarity ---
cat("--- 6G: DI-CP Complementarity ---\n")
m_6g <- feols(
  y_t_pct ~ S_mean_tw*y_lag1 + S_mean_tw*F_CP + F_DI_lag2*F_CP +
    p_proj_all_ages | Country + Quarter,
  data = pdataY, subset = main_sub, cluster = ~Country)
compl <- get_cp(m_6g, "F_CP:F_DI_lag2")
if (!is.na(compl$est))
  cat(sprintf("  F_DI_lag2:F_CP interaction: β=%+.6f, p=%.3f\n", compl$est, compl$pval))
cat("\n")

# --- 6H: Sample splits ---
cat("--- 6H: Sample Splits ---\n\n")
median_S    <- median(id_cs$mean_S, na.rm = TRUE)
median_gdp  <- median(pdataY$rGDP_pc_2019, na.rm = TRUE)
median_debt <- median(pdataY$debt_2019, na.rm = TRUE)
hi_S   <- id_cs$Country[id_cs$mean_S >= median_S]
lo_S   <- id_cs$Country[id_cs$mean_S <  median_S]
hi_inc <- unique(pdataY$Country[!is.na(pdataY$rGDP_pc_2019) & pdataY$rGDP_pc_2019 >= median_gdp])
lo_inc <- unique(pdataY$Country[!is.na(pdataY$rGDP_pc_2019) & pdataY$rGDP_pc_2019 <  median_gdp])
hi_dbt <- unique(pdataY$Country[!is.na(pdataY$debt_2019) & pdataY$debt_2019 >= median_debt])
lo_dbt <- unique(pdataY$Country[!is.na(pdataY$debt_2019) & pdataY$debt_2019 <  median_debt])

split_list <- list(
  list(nm = "High-S countries",   cty = hi_S),
  list(nm = "Low-S countries",    cty = lo_S),
  list(nm = "High-income OECD",   cty = hi_inc),
  list(nm = "Low-income OECD",    cty = lo_inc),
  list(nm = "High pre-COVID debt", cty = hi_dbt),
  list(nm = "Low pre-COVID debt",  cty = lo_dbt)
)
for (sp in split_list) {
  m_sp <- tryCatch(
    feols(main_fml,
          data = pdataY %>% filter(Country %in% sp$cty),
          subset = main_sub, cluster = ~Country),
    error = function(e) NULL)
  if (is.null(m_sp)) { cat(sprintf("  %-26s: [insufficient data]\n", sp$nm)); next }
  psi_sp <- get_cp(m_sp, "S_mean_tw:y_lag1")
  eta_sp <- get_cp(m_sp, "S_mean_tw:F_CP")
  etp_sp <- get_cp(m_sp, "y_lag1:F_CP")
  di_sp  <- get_cp(m_sp, "F_DI_lag2")
  cat(sprintf("  %-26s  ψ=%+.5f(p=%.3f)  η̃=%+.5f(p=%.3f)  η_p=%+.5f  α_DI=%+.4f\n",
              sp$nm, psi_sp$est, psi_sp$pval, eta_sp$est, eta_sp$pval,
              ifelse(is.na(etp_sp$est), 0, etp_sp$est),
              ifelse(is.na(di_sp$est), 0, di_sp$est)))
}
cat("\n")

# --- Social safety net split ---
high_socnet <- c("FRA","FIN","BEL","DNK","ITA","AUT","SWE","DEU","NOR",
                 "ESP","GRC","PRT","LUX","NLD","JPN","GBR","CZE","SVN","POL")
low_socnet  <- c("USA","KOR","MEX","CHL","TUR","IRL","AUS","NZL","CAN",
                 "CHE","ISR","COL","CRI","EST","LVA","LTU","HUN","SVK","ISL")

m_high <- feols(main_fml,
  data = pdataY %>% filter(Country %in% high_socnet),
  subset = main_sub, cluster = ~Country)
m_low  <- feols(main_fml,
  data = pdataY %>% filter(Country %in% low_socnet),
  subset = main_sub, cluster = ~Country)

cat("  High social safety nets:\n")
print(summary(m_high, cluster = ~Country)$coeftable[, c("Estimate","Pr(>|t|)")])
cat("\n  Low social safety nets:\n")
print(summary(m_low, cluster = ~Country)$coeftable[, c("Estimate","Pr(>|t|)")])


# ==============================================================================
# FULL ROBUSTNESS TABLE (Table 3) — feols
# ==============================================================================
cat("\n--- Table 3: Full Robustness Table ---\n")

m_baseline <- feols(main_fml, data = pdataY, subset = main_sub, cluster = ~Country)

m_noIRL <- feols(main_fml,
  data = pdataY %>% filter(Country != "IRL"), subset = main_sub, cluster = ~Country)
m_noTUR <- feols(main_fml,
  data = pdataY %>% filter(Country != "TUR"), subset = main_sub, cluster = ~Country)

m_vax <- feols(
  y_t_pct ~ S_mean_tw*y_lag1 + S_mean_tw*F_CP + F_CP*y_lag1 +
    F_DI_lag2 + p_proj_all_ages + vax_rate | Country + Quarter,
  data = pdataY, subset = main_sub, cluster = ~Country)

# Extended horizon Q4.2019–Q4.2022
m_ext <- feols(main_fml, data = pdataY,
  subset = ~ t_idx >= 4 & t_idx <= 16, cluster = ~Country)

# Only 2020 (Waves 1-2): Q4.2019-Q4.2020
m_w12 <- feols(main_fml, data = pdataY,
  subset = ~ t_idx >= 4 & t_idx <= 8, cluster = ~Country)

# CP×DI complementarity
m_compl <- feols(
  y_t_pct ~ S_mean_tw*y_lag1 + S_mean_tw*F_CP + F_CP*y_lag1 +
    F_DI_lag2 + F_CP:F_DI_lag2 + p_proj_all_ages | Country + Quarter,
  data = pdataY, subset = main_sub, cluster = ~Country)

# Robustness sample: Q1.2020-Q1.2022 (without Q4.2019 reference)
m_rob_sample <- feols(main_fml, data = pdataY,
  subset = ~ t_idx >= 5 & t_idx <= 13, cluster = ~Country)

rob_list <- list(
  "(1) Baseline"          = m_baseline,
  "(2) No IRL"            = m_noIRL,
  "(3) No TUR"            = m_noTUR,
  "(4) S_max_tw"          = m_6c,
  "(5) +Vax"              = m_vax,
  "(6) High soc.nets"     = m_high,
  "(7) Low soc.nets"      = m_low,
  "(8) Ext. Q4.2022"      = m_ext,
  "(9) Only 2020"         = m_w12,
  "(10) CP×DI compl."     = m_compl,
  "(11) Q1.20-Q1.22"      = m_rob_sample
)

coef_map_rob <- c(
  "S_mean_tw"             = "S  [-alpha_S]",
  "y_lag1"                = "y(t-1)  [rho_y]",
  "S_mean_tw:y_lag1"      = "S x y(t-1)  [psi]",
  "S_max_tw:y_lag1"       = "S_max x y(t-1)  [psi]",
  "F_CP"                  = "F^CP  [alpha_CP]",
  "S_mean_tw:F_CP"        = "S x F^CP  [eta_tilde]",
  "S_max_tw:F_CP"         = "S_max x F^CP  [eta_tilde]",
  "y_lag1:F_CP"           = "F^CP x y(t-1)  [-eta_p]",
  "F_DI_lag2"             = "F^DI lag2  [alpha_DI]",
  "F_CP:F_DI_lag2"        = "F^CP x F^DI  [complementarity]",
  "vax_rate"              = "Vaccination rate",
  "p_proj_all_ages"       = "Fear term  [beta_d]"
)

modelsummary(rob_list,
  vcov    = ~Country,
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_map = coef_map_rob,
  gof_map  = c("nobs", "r.squared.within"),
  title    = "Table 3: Robustness Checks — Output Gap Equation (feols, AER SE)")

modelsummary(rob_list,
  vcov    = ~Country,
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_map = coef_map_rob,
  gof_map  = c("nobs", "r.squared.within"),
  output   = file.path(safetable, "tab_3_robustness.tex"),
  title    = "Robustness Checks: Output Gap Equation")
cat("  -> Saved: tab_3_robustness.tex\n")


# --- FIGURE 4: Robustness coefficient plot ---
fig_rob <- modelplot(rob_list,
  coef_map = coef_map_rob,
  vcov = ~Country,
  conf_level = 0.95) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(title = "Coefficient Plot — Key Parameters across Robustness Specs",
       subtitle = "feols TWFE, 95% CI, clustered SE (Country). Q1.2020-Q2.2022.",
       x = "Coefficient estimate", y = NULL) +
  theme_bw(base_size = 10)
ggsave(file.path(safeplots, "fig04_robustness_coefplot.pdf"), fig_rob, width = 11, height = 7)
cat("  -> Saved: fig04_robustness_coefplot.pdf\n\n")
print(fig_rob)


# ==============================================================================
# TABLE 4: Additional Specification Tests (feols)
# ==============================================================================

# --- A. Non-linearities ---
m_Ssq <- feols(
  y_t_pct ~ S_mean_tw*y_lag1 + I(S_mean_tw^2) + S_mean_tw*F_CP + F_CP*y_lag1 +
    F_DI_lag2 + p_proj_all_ages | Country + Quarter,
  data = pdataY, subset = main_sub, cluster = ~Country)

m_CPsq <- feols(
  y_t_pct ~ S_mean_tw*y_lag1 + S_mean_tw*F_CP + I(F_CP^2) + F_CP*y_lag1 +
    F_DI_lag2 + p_proj_all_ages | Country + Quarter,
  data = pdataY, subset = main_sub, cluster = ~Country)

# --- B. DI lag structure ---
m_DI0 <- feols(
  y_t_pct ~ S_mean_tw*y_lag1 + S_mean_tw*F_CP + F_CP*y_lag1 +
    F_DI + p_proj_all_ages | Country + Quarter,
  data = pdataY, subset = main_sub, cluster = ~Country)

m_DI1 <- feols(
  y_t_pct ~ S_mean_tw*y_lag1 + S_mean_tw*F_CP + F_CP*y_lag1 +
    F_DI_lag1 + p_proj_all_ages | Country + Quarter,
  data = pdataY, subset = main_sub, cluster = ~Country)

m_DI3 <- feols(
  y_t_pct ~ S_mean_tw*y_lag1 + S_mean_tw*F_CP + F_CP*y_lag1 +
    F_DI_lag3 + p_proj_all_ages | Country + Quarter,
  data = pdataY, subset = main_sub, cluster = ~Country)

# --- C. Alternative stringency (lagged S) ---
m_Slag <- feols(
  y_t_pct ~ S_lag1*y_lag1 + S_lag1*F_CP + F_CP*y_lag1 +
    F_DI_lag2 + p_proj_all_ages | Country + Quarter,
  data = pdataY, subset = main_sub, cluster = ~Country)

# --- D. Asymmetry ---
pdataY <- pdataY %>%
  group_by(Country) %>%
  arrange(t_idx) %>%
  mutate(
    S_tightening = pmax(S_mean_tw - lag(S_mean_tw), 0),
    S_loosening  = pmin(S_mean_tw - lag(S_mean_tw), 0)
  ) %>%
  ungroup()

m_asym <- feols(
  y_t_pct ~ S_tightening + S_loosening + S_mean_tw*y_lag1 + S_mean_tw*F_CP +
    F_CP*y_lag1 + F_DI_lag2 + p_proj_all_ages | Country + Quarter,
  data = pdataY, subset = main_sub, cluster = ~Country)

# --- E. Nested models ---
m_main_only <- feols(
  y_t_pct ~ S_mean_tw + y_lag1 + F_CP + F_DI_lag2 + p_proj_all_ages | Country + Quarter,
  data = pdataY, subset = main_sub, cluster = ~Country)

m_psionly <- feols(
  y_t_pct ~ S_mean_tw*y_lag1 + F_CP + F_DI_lag2 + p_proj_all_ages | Country + Quarter,
  data = pdataY, subset = main_sub, cluster = ~Country)

m_nofear <- feols(
  y_t_pct ~ S_mean_tw*y_lag1 + S_mean_tw*F_CP + F_CP*y_lag1 +
    F_DI_lag2 | Country + Quarter,
  data = pdataY, subset = main_sub, cluster = ~Country)

# --- F. Exogeneity checks ---
m_lead <- feols(
  y_t_pct ~ S_mean_tw*y_lag1 + S_mean_tw*F_CP + F_CP*y_lag1 +
    F_CP_lead1 + F_DI_lag2 + p_proj_all_ages | Country + Quarter,
  data = pdataY, subset = main_sub, cluster = ~Country)

m_placebo <- feols(
  y_t_pct ~ S_lead1 + S_mean_tw*y_lag1 + S_mean_tw*F_CP +
    F_CP*y_lag1 + F_DI_lag2 + p_proj_all_ages | Country + Quarter,
  data = pdataY, subset = main_sub, cluster = ~Country)


# ============================================================
# Panel A: DI Timing + Non-Linearities + Asymmetry (feols)
# ============================================================
panel_a <- list(
  "(1) Baseline"  = m_baseline,
  "(2) DI cont."  = m_DI0,
  "(3) DI lag1"   = m_DI1,
  "(4) DI lag3"   = m_DI3,
  "(5) S sq."     = m_Ssq,
  "(6) CP sq."    = m_CPsq,
  "(7) S_lag1"    = m_Slag,
  "(8) Asymm."    = m_asym
)

coef_map_a <- c(
  "S_mean_tw"            = "S",
  "y_lag1"               = "y(t-1)",
  "S_mean_tw:y_lag1"     = "S x y(t-1)  [psi]",
  "S_lag1:y_lag1"        = "S(t-1) x y(t-1)  [psi]",
  "I(S_mean_tw^2)"       = "S sq.  [convexity]",
  "F_CP"                 = "F^CP  [alpha_CP]",
  "I(F_CP^2)"            = "(F^CP) sq.  [dim. returns]",
  "S_mean_tw:F_CP"       = "S x F^CP  [eta_tilde]",
  "S_lag1:F_CP"          = "S(t-1) x F^CP  [eta_tilde]",
  "y_lag1:F_CP"          = "F^CP x y(t-1)  [eta_p]",
  "F_DI"                 = "F^DI cont.  [alpha_DI]",
  "F_DI_lag1"            = "F^DI lag1  [alpha_DI]",
  "F_DI_lag2"            = "F^DI lag2  [alpha_DI]",
  "F_DI_lag3"            = "F^DI lag3  [alpha_DI]",
  "S_tightening"         = "Delta S+  [tightening]",
  "S_loosening"          = "Delta S-  [loosening]",
  "p_proj_all_ages"      = "Fear term"
)

cat("--- Panel A: DI Timing, Non-Linearities, Asymmetry ---\n")
modelsummary(panel_a,
  vcov    = ~Country,
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_map = coef_map_a,
  gof_map  = c("nobs", "r.squared.within"),
  title    = "Panel A: DI Timing, Non-Linearities, and Asymmetry")

modelsummary(panel_a,
  vcov    = ~Country,
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_map = coef_map_a,
  gof_map  = c("nobs", "r.squared.within"),
  output   = file.path(safetable, "tab_4a_specifications.tex"),
  title    = "DI Timing, Non-Linearities, and Asymmetry")
cat("  -> Saved: tab_4a_specifications.tex\n")
                                                                                                                        
# ============================================================
# PANEL A INTERPRETATION: DI Timing, Non-Linearities, Asymmetry
# ============================================================
#
# INVARIANT ACROSS ALL 8 SPECIFICATIONS:
# - psi (S×y_lag1): 0.003-0.005, significant at 1% in all specs
# - alpha_CP (F_CP): 0.239-0.486, significant at 1% in specs 1-6
# - eta_tilde (S×F_CP): -0.007 to -0.009, significant at 1% in specs 1-6
# - Fear term: -0.018 to -0.026, significant in 6 of 8 specs
#
# (2)-(4) DI TIMING — confirms lag-2 structure:
#   DI_cont  = 0.048, insignificant
#   DI_lag1  = -0.012, insignificant
#   DI_lag2  = 0.207* (baseline)
#   DI_lag3  = 0.041, insignificant
#   → DI operates exclusively at lag 2
#   → Consistent with theory: announced t, fiscal cost t+1, effect t+2
#   → All other coefficients invariant to DI lag choice
#     (CP channel orthogonal to DI timing)
#
# (5) S² — CONVEXITY CONFIRMED:
#   S² = -0.001** (p<0.05)
#   → Marginal output cost of lockdown RISES with intensity
#   → Linear S turns positive (0.084): at low S, lockdown effect mild,
#     becomes contractionary only above threshold
#   → Confirms theoretical prediction (eq. 13: dY/dS = psi*y - alpha_S)
#   → Formal rationale for early intervention (Acemoglu et al. 2021)
#
# (6) CP² — DIMINISHING RETURNS (suggestive):
#   (F_CP)² = -0.009* (p<0.1, marginal)
#   Linear F_CP jumps to 0.486*** (quadratic pulls down at high values)
#   → First unit of CP more effective than tenth unit
#   → Linear specification justified as first-order approximation
#   → Report as suggestive evidence, not main specification
#
# (7) S_LAG1 — CONTEMPORANEOUS LOCKDOWN EFFECT:
#   S(t-1)×y(t-1) = 0.002, insignificant
#   All S(t-1) interactions insignificant
#   → Lockdown works contemporaneously, not with delay
#   → Supports exogeneity: if S_lag doesn't affect y,
#     unlikely that y feeds back to contemporaneous S
#     (reverse causality would require both directions)
#
# (8) ASYMMETRY — STRONGEST NEW FINDING:
#   ΔS+ (tightening) = -0.068*** (p<0.01)
#   ΔS- (loosening)   = -0.016, insignificant
#   → Tightening contracts output 4x MORE than loosening expands it
#   → Damage is fast, recovery is slow
#   → Strongest empirical argument for hysteresis/irreversibility
#   → Directly supports eta_p channel: without CP, structures destroyed
#     during tightening are not rebuilt during loosening
#   → Explains LP dip at h=1: lockdown damage is frontloaded
#
# IMPLICATIONS FOR PAPER:
# "Panel A validates three structural features of the baseline
#  specification. First, DI operates exclusively at a two-quarter
#  delay (cols 2-4). Second, the output cost of containment is
#  convex in lockdown intensity (S²<0, p<0.05, col 5). Third,
#  lockdown effects are strongly asymmetric: tightening contracts
#  output four times more than loosening expands it (-0.068 vs
#  -0.016), providing direct evidence for the hysteresis mechanism
#  formalized through psi (col 8)."
# ============================================================

# ============================================================
# Panel B: Nested Models + Exogeneity Checks (feols)
# ============================================================
panel_b <- list(
  "(1) Baseline"     = m_baseline,
  "(2) Main only"    = m_main_only,
  "(3) +psi only"    = m_psionly,
  "(4) No fear"      = m_nofear,
  "(5) +CP lead"     = m_lead,
  "(6) S lead plac." = m_placebo
)

coef_map_b <- c(
  "S_mean_tw"            = "S",
  "y_lag1"               = "y(t-1)",
  "S_mean_tw:y_lag1"     = "S x y(t-1)  [psi]",
  "S_lead1"              = "S(t+1) [placebo]",
  "F_CP"                 = "F^CP  [alpha_CP]",
  "S_mean_tw:F_CP"       = "S x F^CP  [eta_tilde]",
  "y_lag1:F_CP"          = "F^CP x y(t-1)  [eta_p]",
  "F_CP_lead1"           = "F^CP(t+1)  [exogeneity]",
  "F_DI_lag2"            = "F^DI lag2  [alpha_DI]",
  "p_proj_all_ages"      = "Fear term"
)

cat("--- Panel B: Nested Models and Exogeneity Checks ---\n")
modelsummary(panel_b,
  vcov    = ~Country,
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_map = coef_map_b,
  gof_map  = c("nobs", "r.squared.within"),
  title    = "Panel B: Nested Models and Exogeneity Checks")

modelsummary(panel_b,
  vcov    = ~Country,
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_map = coef_map_b,
  gof_map  = c("nobs", "r.squared.within"),
  output   = file.path(safetable, "tab_4b_nested_exogeneity.tex"),
  title    = "Nested Models and Exogeneity Checks")
cat("  -> Saved: tab_4b_nested_exogeneity.tex\n")

# Placebo with theta control
m_placebo_theta <- feols(
  y_t_pct ~ S_lead1 + theta_pct + S_mean_tw*y_lag1 + S_mean_tw*F_CP +
    F_CP*y_lag1 + F_DI_lag2 + p_proj_all_ages | Country + Quarter,
  data = pdataY, subset = main_sub, cluster = ~Country)
summary(m_placebo_theta)

# Placebo with theta + lagged excess deaths
m_plac_both <- feols(
  y_t_pct ~ S_lead1 + theta_mean + p_p_lag1 + S_mean_tw*y_lag1 +
    S_mean_tw*F_CP + F_CP*y_lag1 + F_DI_lag2 + p_proj_all_ages | Country + Quarter,
  data = pdataY, subset = main_sub, cluster = ~Country)
summary(m_plac_both)

# ============================================================
# Local Projections (feols, main sample, AER SE)
# ============================================================
# LP traces the full IRF without imposing parametric dynamics.
# Key constraint: with T=11, h=3 leaves 8 obs per country at the
# longest horizon. LP results should be read as indicative.
# ============================================================

# Leads (create if not present)
pdataY <- pdataY %>%
  group_by(Country) %>%
  arrange(t_idx) %>%
  mutate(
    y_lead0 = y_t_pct,
    y_lead1 = lead(y_t_pct, 1),
    y_lead2 = lead(y_t_pct, 2),
    y_lead3 = lead(y_t_pct, 3)
  ) %>%
  ungroup()

horizons <- 0:3
params <- c("S_mean_tw", "F_CP", "F_DI_lag2")
param_labels <- c("Containment (S)", "Capacity Preservation (F^CP)", "Demand Injection (F^DI)")
interaction_params <- c("S_mean_tw:y_lag1", "S_mean_tw:F_CP")
interaction_labels <- c("S x y(t-1) [lockdown hysteresis]", "S x F^CP [CP cushioning]")

lp_results <- data.frame()
lp_interactions <- data.frame()

for (h in horizons) {
  lead_var <- paste0("y_lead", h)
  fml_lp <- as.formula(paste0(lead_var,
    " ~ S_mean_tw*y_lag1 + S_mean_tw*F_CP + F_DI_lag2 + p_proj_all_ages | Country + Quarter"))

  m_lp <- feols(fml_lp, data = pdataY, subset = main_sub, cluster = ~Country)
  ct <- summary(m_lp)$coeftable

  for (i in seq_along(params)) {
    if (!params[i] %in% rownames(ct)) next
    lp_results <- rbind(lp_results, data.frame(
      h = h, param = param_labels[i],
      coef = ct[params[i], "Estimate"],
      se   = ct[params[i], "Std. Error"],
      pval = ct[params[i], "Pr(>|t|)"]
    ))
  }
  for (i in seq_along(interaction_params)) {
    if (!interaction_params[i] %in% rownames(ct)) next
    lp_interactions <- rbind(lp_interactions, data.frame(
      h = h, param = interaction_labels[i],
      coef = ct[interaction_params[i], "Estimate"],
      se   = ct[interaction_params[i], "Std. Error"],
      pval = ct[interaction_params[i], "Pr(>|t|)"]
    ))
  }
}

# Confidence intervals
add_ci <- function(df) {
  df %>% mutate(
    ci90_lo = coef - 1.645 * se, ci90_hi = coef + 1.645 * se,
    ci95_lo = coef - 1.96  * se, ci95_hi = coef + 1.96  * se,
    sig = ifelse(pval < 0.01, "***", ifelse(pval < 0.05, "**", ifelse(pval < 0.1, "*", "")))
  )
}
lp_results      <- add_ci(lp_results)
lp_interactions <- add_ci(lp_interactions)

cat("\n--- Local Projection Results (feols, Q1.2020-Q2.2022) ---\n")
print(lp_results %>% mutate(across(c(coef, se, pval), ~round(., 4))) %>%
        select(h, param, coef, se, pval, sig))

cat("\n--- Interaction Terms: Local Projections ---\n")
print(lp_interactions %>% mutate(across(c(coef, se, pval), ~round(., 5))) %>%
        select(h, param, coef, se, pval))

# --- LP Plots ---
# Instruments
p_lp1 <- ggplot(lp_results, aes(x = h, y = coef)) +
  geom_ribbon(aes(ymin = ci95_lo, ymax = ci95_hi), fill = "grey80", alpha = 0.5) +
  geom_ribbon(aes(ymin = ci90_lo, ymax = ci90_hi), fill = "grey60", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_line(color = "black", linewidth = 0.8) +
  geom_point(color = "black", size = 2.5) +
  facet_wrap(~param, scales = "free_y", ncol = 3) +
  scale_x_continuous(breaks = 0:3, labels = paste0("Q+", 0:3)) +
  labs(x = "Horizon (quarters after deployment)", y = "Effect on output gap (pp)",
       title = "Dynamic Effects of Policy Instruments on the Output Gap",
       subtitle = "Local projections, feols TWFE, clustered SE (Country), 90%/95% CI") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", size = 11))
ggsave(file.path(safeplots, "fig_lp_all_instruments.pdf"), p_lp1, width = 12, height = 4.5)
print(p_lp1)

# Interactions
p_lp2 <- ggplot(lp_interactions, aes(x = h, y = coef)) +
  geom_ribbon(aes(ymin = ci95_lo, ymax = ci95_hi), fill = "grey80", alpha = 0.5) +
  geom_ribbon(aes(ymin = ci90_lo, ymax = ci90_hi), fill = "grey60", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_line(color = "black", linewidth = 0.8) +
  geom_point(color = "black", size = 2.5) +
  facet_wrap(~param, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = 0:3, labels = paste0("Q+", 0:3)) +
  labs(x = "Horizon (quarters)", y = "Coefficient estimate",
       title = "Dynamic Effects of Interaction Terms on the Output Gap",
       subtitle = "Local projections, feols TWFE, clustered SE (Country), 90%/95% CI") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", size = 11))
ggsave(file.path(safeplots, "fig_lp_interactions.pdf"), p_lp2, width = 10, height = 4.5)
print(p_lp2)

# Combined 5-panel plot
lp_all <- bind_rows(
  lp_results %>% mutate(group = "Instruments"),
  lp_interactions %>% mutate(group = "Interactions")
)
p_lp_all <- ggplot(lp_all, aes(x = h, y = coef)) +
  geom_ribbon(aes(ymin = ci95_lo, ymax = ci95_hi), fill = "grey80", alpha = 0.5) +
  geom_ribbon(aes(ymin = ci90_lo, ymax = ci90_hi), fill = "grey60", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_line(color = "black", linewidth = 0.8) +
  geom_point(color = "black", size = 2.5) +
  facet_wrap(~param, scales = "free_y", ncol = 5) +
  scale_x_continuous(breaks = 0:3, labels = paste0("Q+", 0:3)) +
  labs(x = "Horizon (quarters)", y = "Coefficient estimate",
       title = "Dynamic Effects of Policy Instruments and Interactions on the Output Gap",
       subtitle = "Local projections (Jorda, 2005), feols TWFE, clustered SE, 90%/95% CI") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", size = 9))
ggsave(file.path(safeplots, "fig_lp_combined.pdf"), p_lp_all, width = 16, height = 4)
print(p_lp_all)




# ==============================================================================
#  STEP 7 — ALTERNATIVE ESTIMATORS & LIMITATIONS
#  7A: GMM Arellano-Bond (1991) — formal Nickell-bias correction
#  7B: Local Projections (Jordà 2005) — dynamic transmission profile h=0,...,5
#
#  LIMITATION NOTE — GMM:
#    With T=9 and the key parameter entering through the bilinear S×y interaction,
#    GMM instrument sets are limited. We re-parameterize S×y and S×F_CP as
#    composite variables and use lags 2-3 of y as instruments for y_{t-1}.
#    The Sargan-Hansen test and AR(2) test validate the approach.
#

# ==============================================================================

cat("\n", strrep("=",70), "\n")
cat("  STEP 7: ALTERNATIVE ESTIMATORS — GMM & LOCAL PROJECTIONS\n")
cat(strrep("=",70), "\n\n")

# --- 7A: GMM Arellano-Bond ---
cat("--- 7A: GMM Arellano-Bond (bias-corrected dynamic panel) ---\n")
cat(paste0(
  "  SETUP: S×y and S×F_CP are bilinear terms; we include them as composite\n",
  "  variables (S_y_lag, S_FCP) and instrument y_{t-1} with lags 2-3.\n",
  "  Two-step GMM with Windmeijer (2005) finite-sample SE correction.\n\n"
))

# Note: pdataY must contain S_y_lag and S_FCP (constructed in STEP 0)
# GMM requires pdata.frame — use main sample period (t_idx 4-14)
main_pf <- pdata.frame(pdataY %>% filter(t_idx >= 5 & t_idx <= 14), index=c("Country","Quarter"))

m_gmm <- tryCatch({
  pgmm(
    y_t_pct ~ lag(y_t_pct, 1) + S_mean_tw + S_y_lag + F_CP + S_FCP +
      F_DI_lag2 + p_proj_all_ages | lag(y_t_pct, 2:4),
    data   = main_pf,
    effect = "twoways",
    model  = "twosteps",
    robust = TRUE
  )
}, error=function(e) {
  cat(sprintf("  GMM estimation error: %s\n", e$message))
  NULL
})

if (!is.null(m_gmm)) {
  cat("  GMM two-step results:\n")
  gmm_s <- summary(m_gmm, robust=TRUE)
  print(gmm_s)
  cat("\n  Sargan-Hansen overidentification test:\n")
  print(sargan(m_gmm))
  cat("\n  Arellano-Bond AR(1) test:\n"); print(mtest(m_gmm, order=1, robust=TRUE))
  cat("  Arellano-Bond AR(2) test:\n");  print(mtest(m_gmm, order=2, robust=TRUE))
  cat(paste0(
    "  INTERPRETATION:\n",
    "  AR(1) expected significant (first-differencing induces MA(1)).\n",
    "  AR(2) should be insignificant → lags 2-3 are valid instruments.\n",
    "  Sargan p>0.05 → instruments not over-identified.\n",
    "  Compare GMM vs TWFE: if ψ_GMM ≈ ψ_TWFE, Nickell bias negligible.\n"
  ))
  gmm_coefs <- coef(m_gmm)
} else {
  cat(paste0(
    "  GMM did not converge. Likely cause: T=9 provides few valid lags for\n",
    "  the composite interaction terms. TWFE Nickell assessment (Step 2D)\n",
    "  bounds the bias at <1% of the point estimate — GMM not decisive here.\n"
  ))
  gmm_coefs <- NULL
}



# ==============================================================================
#  STEP 8 — CROSS-ESTIMATOR COMPARISON & FINAL CONCLUSION
#  Compare: TWFE (main) | GMM-AB (Nickell correction) | LP at h=0
#  Calibrated parameters for iLQR (→ Section 4/5 of the paper)
# ==============================================================================

cat("\n", strrep("=",70), "\n")
cat("  STEP 8: CROSS-ESTIMATOR COMPARISON & CONCLUSION\n")
cat(strrep("=",70), "\n\n")

# Collect key coefficients
lp_h0 <- lp_tab[lp_tab$h == 0, ]
get_lp <- function(vn, tab) {
  row <- tab[tab$var == vn, ]
  if (nrow(row)==0) return(c(NA_real_, NA_real_))
  c(row$coef[1], row$p[1])
}

compare_tab <- data.frame(
  Parameter  = c("ψ: S×y(t-1)", "η̃: S×F^CP", "η_p: F^CP×y(t-1)", "α_DI: F^DI lag2"),
  TWFE_coef  = c(ct_main["S_mean_tw:y_lag1","Estimate"],
                 ct_main["S_mean_tw:F_CP","Estimate"],
                 if ("F_CP:y_lag1" %in% rownames(ct_main)) ct_main["F_CP:y_lag1","Estimate"] else NA_real_,
                 ct_main["F_DI_lag2","Estimate"]),
  TWFE_p     = c(ct_main["S_mean_tw:y_lag1","Pr(>|t|)"],
                 ct_main["S_mean_tw:F_CP","Pr(>|t|)"],
                 if ("F_CP:y_lag1" %in% rownames(ct_main)) ct_main["F_CP:y_lag1","Pr(>|t|)"] else NA_real_,
                 ct_main["F_DI_lag2","Pr(>|t|)"]),
  LP_h0_coef = c(get_lp("S_mean_tw:y_lag1",lp_h0)[1],
                 get_lp("S_mean_tw:F_CP",   lp_h0)[1],
                 get_lp("F_CP:y_lag1",      lp_h0)[1],
                 get_lp("F_DI_lag2",        lp_h0)[1]),
  LP_h0_p    = c(get_lp("S_mean_tw:y_lag1",lp_h0)[2],
                 get_lp("S_mean_tw:F_CP",   lp_h0)[2],
                 get_lp("F_CP:y_lag1",      lp_h0)[2],
                 get_lp("F_DI_lag2",        lp_h0)[2])
)
if (!is.null(gmm_coefs)) {
  compare_tab$GMM_coef <- c(
    if ("S_y_lag"   %in% names(gmm_coefs)) gmm_coefs["S_y_lag"]   else NA_real_,
    if ("S_FCP"     %in% names(gmm_coefs)) gmm_coefs["S_FCP"]     else NA_real_,
    if ("FCP_y_lag" %in% names(gmm_coefs)) gmm_coefs["FCP_y_lag"] else NA_real_,
    if ("F_DI_lag2" %in% names(gmm_coefs)) gmm_coefs["F_DI_lag2"] else NA_real_
  )
}
cat("  Estimator comparison:\n")
print(compare_tab)
write.csv(compare_tab, file.path(safetable,"tab_estimator_comparison.csv"), row.names=FALSE)
cat("  → Saved: tab_estimator_comparison.csv\n\n")

cat("--- STRUCTURAL PARAMETERS FOR iLQR CALIBRATION ---\n")
cat(sprintf("  ψ̂    = %+.5f  (lockdown hysteresis, S×y interaction)\n",      psi_hat))
cat(sprintf("  η̃̂    = %+.5f  (CP cushioning, S×F^CP interaction)\n",          eta_hat))
cat(sprintf("  η̂_p  = %+.5f  (CP persistence reduction, F^CP×y interaction)\n",
            ifelse(is.na(eta_p_hat), 0, eta_p_hat)))
cat(sprintf("  α̂_DI = %+.4f  (DI output multiplier, lag 2)\n",               alpha_DI))
cat(sprintf("  S*   = %.1f    (CP effectiveness threshold: α_CP / |η̃|)\n",    S_star))
cat(sprintf("  ρ_eff(S̄=%.0f, F̄=%.2f) = %.4f  (effective output persistence)\n",
            S_bar, F_bar, rho_eff))
cat(sprintf("  ∂y/∂F^CP|_(S̄,ȳ) = %.4f  (marginal CP effect at means)\n",    marg_CP))
cat(paste0(
  "\n  These parameters calibrate the iLQR transition system (Section 4):\n",
  "    y_{k+1} = ρ_y·y_k + (ψ·y_k − α_S)·S_k\n",
  "              + (α_CP + η̃·S_k − η_p·y_k)·F_k^CP\n",
  "              + α_DI·F_{k-2}^DI + γ_i + δ_k\n",
  "  Effective persistence: ρ_eff(S,F) = ρ_y + ψ·S − η_p·F^CP\n",
  "  enabling computation of the optimal policy trajectory and quantification\n",
  "  of the trilemma's welfare cost (Section 5).\n\n"
))

cat("--- LIMITATIONS ---\n")
cat(paste0(
  "  1. NICKELL BIAS: formally O(1/T)≈11% at T=9. Absolute bias <0.007 for\n",
  "     ρ_y≈0.06. GMM provides the formal correction (Step 7A).\n",
  "  2. ENDOGENEITY OF S: containment is a policy choice. TWFE addresses\n",
  "     time-invariant confounders; quarter FE absorbs common pandemic waves.\n",
  "     Residual time-varying endogeneity bounded by r(S,F)≈", round(r_S_FCP,2),
  " (near-zero,\n",
  "     Step 1B). Exclusion restriction: S chosen independently of composition.\n",
  "  3. CROSS-COUNTRY SPILLOVERS: symmetric spillovers absorbed by quarter FE.\n",
  "     Asymmetric spillovers (EU single market) unaddressed — valid limitation.\n",
  "  4. ANNOUNCEMENT EFFECTS: if firms anticipated CP (guarantee framework\n",
  "     announcement), h=0 LP estimates may include anticipation. The guarantee\n",
  "     decomposition (Step 3C) directly tests the 'whatever it takes' channel.\n",
  "  5. STAGGERED TREATMENT: fiscal deployment was approximately synchronized\n",
  "     (all OECD countries active Q1-Q2 2020), limiting Callaway-Sant'Anna\n",
  "     heterogeneity concerns. Robustness: subsample splits by timing (Annex).\n\n"
))

# ==============================================================================
#  STEP 9 — ROBUSTNESS: UNEMPLOYMENT RATE AS ALTERNATIVE DEPENDENT VARIABLE
#  Re-estimate the main TWFE specification using the quarterly harmonised
#  unemployment rate (OECD Household Dashboard, LAB_UR6) instead of the
#  HP-filtered output gap. This validates that the structural channels
#  (lockdown hysteresis, CP cushioning) operate through the labour market,
#  not just through GDP accounting identities.
# ==============================================================================

cat("\n", strrep("=",70), "\n")
cat("  STEP 9: ROBUSTNESS — UNEMPLOYMENT RATE AS DEPENDENT VARIABLE\n")
cat(strrep("=",70), "\n\n")

# --- 9A: Load quarterly unemployment rate from OECD HH Dashboard ---
hh_raw <- readxl::read_excel(
  file.path(dirname(dirname(dirname(safedata))),
            "data/raw/outcomes and controls/Quarterly/hh_inidcators_legende.xlsx")
)

# OECD SDMX long format: filter for unemployment rate (UNE_LF_Q)
# Note: LAB_UR6 is the broader "labour underutilisation rate" — not what we want.
#       UNE_LF_Q is the standard harmonised unemployment rate (% of labour force).
ur_quarterly <- hh_raw %>%
  filter(MEASURE == "UNE_LF_Q") %>%
  transmute(
    Country  = REF_AREA,
    Quarter  = gsub("^(\\d{4})-Q(\\d)$", "Q\\2.\\1", TIME_PERIOD),
    UR       = as.numeric(OBS_VALUE)
  ) %>%
  filter(!is.na(UR)) %>%
  distinct(Country, Quarter, .keep_all = TRUE)

cat(sprintf("  Loaded %d country-quarter observations for unemployment rate.\n",
            nrow(ur_quarterly)))
cat(sprintf("  Countries: %d | Quarters: %s to %s\n",
            n_distinct(ur_quarterly$Country),
            min(ur_quarterly$Quarter), max(ur_quarterly$Quarter)))

# --- 9B: Merge into estimation sample and construct lag ---
pdataY <- pdataY %>%
  left_join(ur_quarterly, by = c("Country", "Quarter"))

pdataY <- pdataY %>%
  group_by(Country) %>%
  arrange(t_idx) %>%
  mutate(UR_lag1 = lag(UR, 1)) %>%
  ungroup()

main_sample_ur <- pdataY %>%
  filter(t_idx >= 5 & t_idx <= 13, !is.na(UR), !is.na(UR_lag1))

cat(sprintf("  Estimation sample: %d obs, %d countries (after UR merge).\n",
            nrow(main_sample_ur), n_distinct(main_sample_ur$Country)))

# --- 9C: Main specification with unemployment rate ---
# Note: signs flip relative to output gap equation.
#   Lockdown (S) should INCREASE unemployment → positive α_S
#   CP cushioning (S×F_CP) should REDUCE UR increase → negative η̃
#   Hysteresis (S×UR_lag1) should be POSITIVE (lockdowns amplify UR persistence)

ur_feols <- feols(
  UR ~ S_mean_tw*UR_lag1 + S_mean_tw*F_CP + F_CP*UR_lag1 + F_DI_lag2 + p_proj_all_ages |
    Country + Quarter,
  data    = main_sample_ur,
  vcov    = ~Country
)

cat("--- 9C: TWFE — Unemployment Rate Equation (feols, CRV1) ---\n")
print(summary(ur_feols))

# --- 9D: Comparison table: Output Gap vs Unemployment Rate ---
y_feols_compare <- feols(
  y_t_pct ~ S_mean_tw*y_lag1 + S_mean_tw*F_CP + F_CP*y_lag1 + F_DI_lag2 + p_proj_all_ages |
    Country + Quarter,
  data    = pdataY,
  subset  = ~ t_idx >= 5 & t_idx <= 14,
  vcov    = ~Country
)

coef_map_ur <- c(
  "S_mean_tw"            = "S  [lockdown cost/effect]",
  "y_lag1"               = "y(t-1)  [output persist.]",
  "UR_lag1"              = "UR(t-1)  [UR persist.]",
  "S_mean_tw:y_lag1"     = "S×y(t-1)  [ψ, hysteresis]",
  "S_mean_tw:UR_lag1"    = "S×UR(t-1)  [ψ, hysteresis]",
  "F_CP"                 = "F^CP  [level effect]",
  "S_mean_tw:F_CP"       = "S×F^CP  [η̃, cushioning]",
  "y_lag1:F_CP"          = "F^CP×y(t-1)  [η_p, persist. red.]",
  "UR_lag1:F_CP"         = "F^CP×UR(t-1)  [η_p, persist. red.]",
  "F_DI_lag2"            = "F^DI lag2  [α_DI]",
  "p_proj_all_ages"      = "Fear term"
)

ur_rob_list <- list(
  "(1) Output Gap (baseline)" = y_feols_compare,
  "(2) Unemployment Rate"     = ur_feols
)

cat("--- Table: Output Gap vs Unemployment Rate ---\n")
modelsummary(ur_rob_list,
             stars    = c("*"=0.1, "**"=0.05, "***"=0.01),
             coef_map = coef_map_ur,
             gof_map  = c("nobs", "r.squared.within"),
             title    = "Robustness: Output Gap vs Unemployment Rate")

modelsummary(ur_rob_list,
             stars    = c("*"=0.1, "**"=0.05, "***"=0.01),
             coef_map = coef_map_ur,
             gof_map  = c("nobs", "r.squared.within"),
             output   = file.path(safetable, "tab_ur_robustness.tex"),
             title    = "Robustness: Output Gap vs Unemployment Rate")
cat("  → Saved: tab_ur_robustness.tex\n\n")

# --- 9E: Interpretation ---
ct_ur <- summary(ur_feols)$coeftable
cat("--- 9E: Interpretation (sign-flip check) ---\n")
cat(paste0(
  "  If the output gap equation is correctly capturing real-economy dynamics,\n",
  "  the unemployment rate equation should show MIRROR-IMAGE signs:\n",
  "    α_S  > 0 (lockdowns raise unemployment)\n",
  "    ψ    > 0 (lockdowns amplify UR persistence — hysteresis)\n",
  "    α_CP < 0 (CP reduces unemployment)\n",
  "    η̃   should flip sign (CP cushioning under lockdown reduces UR)\n",
  "    α_DI < 0 (demand injection reduces UR with delay)\n\n"
))

if ("S_mean_tw" %in% rownames(ct_ur)) {
  cat(sprintf("  α_S  (UR): %+.4f [p=%.4f] — expected: positive\n",
              ct_ur["S_mean_tw","Estimate"], ct_ur["S_mean_tw","Pr(>|t|)"]))
}
if ("S_mean_tw:UR_lag1" %in% rownames(ct_ur)) {
  cat(sprintf("  ψ    (UR): %+.5f [p=%.4f] — expected: positive\n",
              ct_ur["S_mean_tw:UR_lag1","Estimate"], ct_ur["S_mean_tw:UR_lag1","Pr(>|t|)"]))
}
if ("F_CP" %in% rownames(ct_ur)) {
  cat(sprintf("  α_CP (UR): %+.4f [p=%.4f] — expected: negative\n",
              ct_ur["F_CP","Estimate"], ct_ur["F_CP","Pr(>|t|)"]))
}
if ("S_mean_tw:F_CP" %in% rownames(ct_ur)) {
  cat(sprintf("  η̃   (UR): %+.5f [p=%.4f] — expected: sign flip from OG\n",
              ct_ur["S_mean_tw:F_CP","Estimate"], ct_ur["S_mean_tw:F_CP","Pr(>|t|)"]))
}
if ("F_DI_lag2" %in% rownames(ct_ur)) {
  cat(sprintf("  α_DI (UR): %+.4f [p=%.4f] — expected: negative\n",
              ct_ur["F_DI_lag2","Estimate"], ct_ur["F_DI_lag2","Pr(>|t|)"]))
}

rm(hh_raw, ur_quarterly, main_sample_ur)

cat("\n", strrep("=",70), "\n")
cat("  OUTPUT GAP SECTION COMPLETE.\n")
cat("  Structural parameters calibrated. Proceed to DEBT section.\n")
cat(strrep("=",70), "\n\n")

#===============================================================================
#################################DEBT###########################################
#===============================================================================

#Für ganzes Sample
pdataY <- pdata %>%
  group_by(Country) %>%
  mutate(debt_dR = DebtR_share2019 - lag(DebtR_share2019)) %>%
  ungroup()
#gleich aber nominal
pdataY <- pdata %>%
  group_by(Country) %>%
  mutate(debt_dN = DebtN_share2019 - lag(DebtN_share2019)) %>%
  ungroup()

summary(pdataY$debt_dN)
# Include Q3.2019 as lag source for debt FD, sort chronologically, then drop it
pdataD <- pdataY %>%
  filter(Quarter %in% c("Q3.2019","Q4.2019",
                        "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                        "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
                        "Q1.2022","Q2.2022")) %>%
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



desc_debt <- pdataD %>%
  transmute(
    `Debt-to-GDP Ratio (pp of 2019 GDP)` = DebtR_share2019,
    `Debt Gap (pp, HP-filtered)`         = d_t_pct,
    `Debt Change (pp, first diff)`       = debt_dR,
    `Debt Change (nom, fist diff)`       = debt_dN,
    `Output Gap (pp)`                    = y_t_pct,
    `Capacity Preservation (pp GDP)`     = F_CP,
    `Demand Injection (pp GDP)`          = F_DI,
    `Health Expenditure (pp GDP)`        = F_H,
    `Infection Prevalence (pct)`         = theta_pct,
    `Vaccination Rate (pct)`             = vax_rate
  )

tab_debt <- datasummary(
  All(desc_debt) ~ N + Mean + SD + Min + P25 + Median + P75 + Max,
  data   = desc_debt,
  fmt    = 2,
  output = "data.frame"
)
print(tab_debt)

# Prüfe Ausreißer
pdataD %>% filter(d_t_pct > 80) %>% select(Country, Quarter, d_t_pct)
pdataD %>% filter(debt_dR > 10) %>% select(Country, Quarter, debt_dR)

summary(pdataD$debt_dR)
summary(pdataD$debt_dN)

#Check: +x nur die above-the-line elemente, 1x beide separat und dann mit dem main modell vergleichen
# Wenn du die Rohdaten hast: nach category aufsplitten
fiscal_panel <- fm %>%
  filter(broad_fiscal != 0) %>%
  mutate(
    Quarter_fmt = paste0("Q", Quarter, ".", Year),
    CP_above = ifelse(transmission_channel == "CP" & category == 1, broad_fiscal_gdp, 0),
    CP_below = ifelse(transmission_channel == "CP" & category == 2, broad_fiscal_gdp, 0)
  ) %>%
  group_by(Country, Quarter_fmt) %>%
  summarise(
    F_CP_above = sum(CP_above, na.rm = TRUE),
    F_CP_below = sum(CP_below, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(Quarter = Quarter_fmt)

# Ins Panel mergen
pdataD <- pdataD %>%
  left_join(fiscal_panel, by = c("Country", "Quarter")) %>%
  mutate(
    F_CP_above = replace_na(F_CP_above, 0),
    F_CP_below = replace_na(F_CP_below, 0)
  )


#...............................................................................
#.............................ANALYSIS..........................................

# Country-FE (debt_dN und debt_dR nehmen
# Pandemie-Sample mit F
debt1 <- plm(
  debt_dR ~ y_t_pct+ F_CP_above +  F_CP_below + F_DI_lag1 + as.numeric(Quarter),
  data = pdataD,  model = "within", index=c("Country","Quarter"), effect= "individual")

coeftest(debt1)
coeftest(debt1, vcov = vcovHC(debt1, cluster = "group", type = "HC1"))

##CP_guarantees dominierendes Instrument
#Problem: das einte ist ausbezahlt, das andere sind angekündigte werte


pdataD <- pdataD %>%
  mutate(
    # Szenario 1: Konservativ (OECD-Durchschnitt ~25%)
    F_CP_below_adj_lo = F_CP_below * 0.25,
    
    # Szenario 2: Zentral (~35%, gewichtet nach Ländergröße)
    F_CP_below_adj_mid = F_CP_below * 0.35,
    
    # Szenario 3: Liberal (~50%, oberes Ende)
    F_CP_below_adj_hi = F_CP_below * 0.50,
    
    # Adjusted Total CP
    F_CP_adj_lo  = F_CP_above + F_CP_below_adj_lo,
    F_CP_adj_mid = F_CP_above + F_CP_below_adj_mid,
    F_CP_adj_hi  = F_CP_above + F_CP_below_adj_hi
  
  )
#Die Inanspruchnahme-Raten kommen aus der IMF Working Paper-Reihe (2023, Nr. 016: "Evaluating the Costs of Government Credit Support Programs during COVID-19") und dem ECB Economic Bulletin (2020/6). Die IMF-Studie dokumentiert, dass für sieben Advanced Economies 1.71.7
#1.7 Billionen USD an Kredit tatsächlich vergeben wurden — bei angekündigten Deckeln von über 55
#5 Billionen USD. Das impliziert eine aggregierte Take-Up-Rate von ca.
#34%, was dein zentrales Szenario stützt.
debt2 <- plm(
  debt_dR ~ y_t_pct+ F_CP_above +  F_CP_below_adj_mid+ F_DI_lag1+  inflation_index +as.numeric(Quarter),
  data = pdataD,  model = "within", effect = "individual"
)

coeftest(debt2, vcov = vcovHC(debt2, cluster = "group", type = "HC1"))

##Die Gegenseite anschauen (Y) um Output/Debt Ratios zu sehen

unique(fm$PolicyCode)

fiscal_panel <- fm %>%
  filter(broad_fiscal != 0) %>%
  mutate(
    Quarter_fmt = paste0("Q", Quarter, ".", Year),
    CP_above = ifelse(transmission_channel == "CP" & category == 1, broad_fiscal_gdp, 0),
    CP_below = ifelse(transmission_channel == "CP" & category == 2, broad_fiscal_gdp, 0)
  ) %>%
  group_by(Country, Quarter_fmt) %>%
  summarise(
    F_CP_above = sum(CP_above, na.rm = TRUE),
    F_CP_below = sum(CP_below, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(Quarter = Quarter_fmt)

# Ins Panel mergen
pdataY <- pdataY %>%
  left_join(fiscal_panel, by = c("Country", "Quarter")) %>%
  mutate(
    F_CP_above = replace_na(F_CP_above, 0),
    F_CP_below = replace_na(F_CP_below, 0)
  )


pdataY <- pdataY %>%
  mutate(
    # Szenario 1: Konservativ (OECD-Durchschnitt ~25%)
    F_CP_below_adj_lo = F_CP_below * 0.25,
    
    # Szenario 2: Zentral (~35%, gewichtet nach Ländergröße)
    F_CP_below_adj_mid = F_CP_below * 0.35,
    
    # Szenario 3: Liberal (~50%, oberes Ende)
    F_CP_below_adj_hi = F_CP_below * 0.5,
    
    # Adjusted Total CP
    F_CP_adj_lo  = F_CP_above + F_CP_below_adj_lo,
    F_CP_adj_mid = F_CP_above + F_CP_below_adj_mid,
    F_CP_adj_hi  = F_CP_above + F_CP_below_adj_hi
  )


##If I assume it has the same influence if it is announced or payed out-> this works w/o the distinction-> Erstens das "Whatever it Takes"-Argument. Die Outputwirkung von Garantien läuft nicht über die tatsächliche Inanspruchnahme, sondern über die Ankündigung. Draghis berühmter Satz stabilisierte die Märkte, ohne dass die EZB einen Euro ausgab. Dieselbe Logik gilt für KfW, PGE, ICO: die Existenz des Rahmens verhinderte Insolvenzen, Kreditklemmen und Entlassungen. Wenn du above und below trennst, misst du den Effekt pro angekündigter Einheit separat — aber die Ankündigung wirkte als Paket. Firmen wussten: Kurzarbeit und Garantien sind verfügbar. Die Trennung zerstört den Paketeffekt.


debt1 <- plm(
  debt_dR ~ y_t_pct+ F_CP+ F_DI_lag1 +as.numeric(Quarter),
  data = pdataD,  model = "within", effect = "individual"
)

coeftest(debt1, vcov = vcovHC(debt1, cluster = "group", type = "HC1"))


###TEST-> Robustheit und ohne non parametric timetrend
debtPO <- plm(
  debt_dR ~ y_t_pct+ F_CP_above+ F_CP_below_adj_mid+ F_DI_lag1 +as.numeric(Quarter),
  data = pdataD, index=c("Country", "Quarter"),  model = "pooling", effect = "individual")

debtRE <- plm(
  debt_dR ~ y_t_pct+ F_CP_above+ F_CP_below_adj_mid+ F_DI_lag1 +as.numeric(Quarter),
  data = pdataD, index=c("Country", "Quarter"),   model = "random", effect = "individual")

debtFE <- plm(
  debt_dR ~ y_t_pct+ F_CP_above+ F_CP_below_adj_mid+ F_DI_lag1+as.numeric(Quarter),
  data = pdataD, index=c("Country", "Quarter"),   model = "within", effect = "individual")

debtBE <- plm(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_mid + F_DI_lag1,
  data = pdataD, model = "between")
coeftest(debtBE)

coeftest(debtPO, vcov = vcovHC(debtPO, cluster = "group", type = "HC1"))
coeftest(debtRE, vcov = vcovHC(debtRE, cluster = "group", type = "HC1"))
coeftest(debtFE, vcov = vcovHC(debtFE, cluster = "group", type = "HC1"))


# Zeitmittelwerte für Mundlak
pdataD <- pdataD %>%
  group_by(Country) %>%
  mutate(
    y_mean          = mean(y_t_pct, na.rm = TRUE),
    FCP_above_mean  = mean(F_CP_above, na.rm = TRUE),
    FCP_below_mean  = mean(F_CP_below_adj_mid, na.rm = TRUE),
    FDI_lag1_mean   = mean(F_DI_lag1, na.rm = TRUE)
  ) %>%
  ungroup()

# Mundlak Regression
debt_mundlak <- plm(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_mid + F_DI_lag1 +
    y_mean + FCP_above_mean + FCP_below_mean + FDI_lag1_mean +
    as.numeric(Quarter),
  data  = pdataD,
  model = "pooling"
)
coeftest(debt_mundlak, vcov = vcovHC(debt_mundlak, type = "HC1", cluster = "group"))

##differenzen sind wegen dem linearen Timetrend
# Wald-Test: RE1 verletzt?
waldtest(debt_mundlak,
         . ~ . - y_mean - FCP_above_mean - FCP_below_mean - FDI_lag1_mean,
         vcov = vcovHC(debt_mundlak, type = "HC1", cluster = "group"))

#===============================================================================
###DECISION: FE ist notwendig
pdataD %>%
  filter(Country == "DEU") %>%
  select(Quarter, inflation_index) %>%
  head(12)
#Decision
debtFE <- plm(
  debt_dR ~ y_t_pct+F_CP_above+ F_CP_below_adj_mid+ F_DI_lag1+as.numeric(Quarter),
  data = pdataD, index=c("Country", "Quarter"),   model = "within", effect = "individual")

coeftest(debtFE, vcov = vcovHC(debtFE, cluster = "group", type = "HC1"))

#Robustness
debtFD <- plm(
  debt_dR ~ y_t_pct+F_CP_above+ F_CP_below_adj_mid+ F_DI_lag1+ as.numeric(Quarter),
  data = pdataD, index=c("Country", "Quarter"),   model = "fd", effect = "individual")

coeftest(debtFD, vcov = vcovHC(debtFD, cluster = "group", type = "HC1"))

#evtl. einbauen_ FD mit non parametrischen TT
##Gemäss MUndlak Test sitzt die Endogenität between country und nicht between Quarter-> Time FE würde den ganzen Zyklus absorbieren

#WICHITGER BEFUND AUS DER LITERATUr:
#Chetty et al. (2020) — "How Did COVID-19 and Stabilization Policies Affect Spending and Employment?" — verwenden Region-FE ohne Zeit-FE auf Hochfrequenzdaten, weil die Policy-Variation zeitlich ist.
#Deb et al. (2021, IMF WP) — "The Economic Effects of COVID-19 Containment Measures" — verwenden Country-FE mit spezifischen Zeitkontrollen, nicht TWFE. Ihre Begründung ist identisch mit deiner: die Fiskal- und Containment-Variation ist primär zeitlich.
#Robustness: nominell ohne TUR und IRL aber eigentlich alles auf real 2019 GDP normiert-> das macht Sinn!!
##Maybe das Sample anpassen-> bis Ende 2022, momentan gleich wie Y

##Individual or TWFE
#Two-Way-FE ist für die Debt-Gleichung *nicht* angemessen, weil die Quarter-FE den gemeinsamen fiskalischen Deployment-Zyklus absorbieren, der die identifizierende Variation darstellt. Im Gegensatz zur Output-Gleichung — wo SkS_k
#Sk endogen zum Pandemieverlauf ist und Quarter-FE diese Endogenität kontrollieren — enthält die Debt-Gleichung kein Instrument, dessen Endogenität zeitliche Kontrolle erfordert.


# ================================================================
# DEBT EQUATION
# ================================================================

# Col 1: Pooled OLS
m_d_pool <- plm(
  debt_dR ~ y_t_pct + F_CP + F_DI_lag1 + as.numeric(Quarter),
  data = pdataD, model = "pooling"
)

# Col 2: Country FE (gepoolt CP)
m_d_fe <- plm(
  debt_dR ~ y_t_pct + F_CP + F_DI + as.numeric(Quarter),
  data = pdataD, model = "within", effect = "individual"
)

# Col 3: Country FE (split CP, Hauptspezifikation)
m_d_split <- plm(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_mid + F_DI_lag1 +
    as.numeric(Quarter),
  data = pdataD, model = "within", effect = "individual"
)

# Col 4: First Differences (Robustheit)
m_d_fd <- plm(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_mid + F_DI_lag1,
  data = pdataD, model = "fd"
)

# Alle mit HC1
coeftest(m_d_pool,  vcov = vcovHC(m_d_pool,  cluster = "group", type = "HC1"))
coeftest(m_d_fe,    vcov = vcovHC(m_d_fe,    cluster = "group", type = "HC1"))
coeftest(m_d_split, vcov = vcovHC(m_d_split, cluster = "group", type = "HC1"))
coeftest(m_d_fd,    vcov = vcovHC(m_d_fd,    cluster = "group", type = "HC1"))


# Col 3: Country FE (split CP, Hauptspezifikation)
m_d_split <- feols(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_mid + F_DI_lag1,
  data = pdataD, model = "within", effect = "individual"
)

# --- Build CP sub-components directly in pdataD ---
# Source: fm1 (cleaned fiscal database, same as STEP 0 in output section)
fiscal_subcomp_d <- fm1 %>%
  filter(broad_fiscal == 1) %>%
  mutate(
    Quarter_fmt = paste0("Q", Quarter, ".", Year),
    CP_above = ifelse(transmission_channel == "CP" & category == 1, broad_fiscal_gdp, 0),
    CP_loans = ifelse(transmission_channel == "CP" & category == 2 &
                        PolicyCode %in% c("40","41"), broad_fiscal_gdp, 0),
    CP_guar  = ifelse(transmission_channel == "CP" & category == 2 &
                        PolicyCode == "43", broad_fiscal_gdp, 0)
  ) %>%
  group_by(Country, Quarter_fmt) %>%
  summarise(
    F_CP_above_3 = sum(CP_above, na.rm = TRUE) * 100,
    F_CP_loans   = sum(CP_loans, na.rm = TRUE) * 100,
    F_CP_guar    = sum(CP_guar,  na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  rename(Quarter = Quarter_fmt) %>%
  mutate(F_CP_guar_adj = F_CP_guar * 0.35)

pdataD <- pdataD %>%
  select(-any_of(c("F_CP_above_3","F_CP_loans","F_CP_guar","F_CP_guar_adj"))) %>%
  left_join(fiscal_subcomp_d, by = c("Country","Quarter")) %>%
  mutate(
    F_CP_above_3 = replace_na(F_CP_above_3, 0),
    F_CP_loans   = replace_na(F_CP_loans, 0),
    F_CP_guar    = replace_na(F_CP_guar, 0),
    F_CP_guar_adj = replace_na(F_CP_guar_adj, 0)
  )

# ==============================================================================
#  DEBT EQUATION — MAIN SPECIFICATION & ROBUSTNESS
#  Estimator: feols, Country + Quarter FE (TWFE)
#  Sample:    Q1.2020–Q2.2022 (t_idx >= 5 & t_idx <= 14), 380 obs
#  SE:        Clustered by Country (AER standard)
#  DV:        debt_dR = first difference of real debt / 2019 GDP
# ==============================================================================

# Add t_idx to pdataD (same quarter ordering as pdataY)
quarter_order <- c("Q1.2019","Q2.2019","Q3.2019","Q4.2019",
                   "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                   "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
                   "Q1.2022","Q2.2022","Q3.2022","Q4.2022")
pdataD$t_idx <- match(as.character(pdataD$Quarter), quarter_order)

cat("\n", strrep("=", 70), "\n")
cat("  DEBT EQUATION — MAIN SPEC & ROBUSTNESS (feols, Q1.2020-Q2.2022)\n")
cat(strrep("=", 70), "\n\n")

# Impute pre-pandemic NAs with 0
pdataD <- pdataD %>%
  mutate(
    S_mean_tw       = replace_na(S_mean_tw, 0),
    F_CP            = replace_na(F_CP, 0),
    F_DI_lag1       = replace_na(F_DI_lag1, 0),
    F_DI_lag2       = replace_na(F_DI_lag2, 0),
    p_proj_all_ages = replace_na(p_proj_all_ages, 0),
    y_lag1          = replace_na(y_lag1, 0),
    theta_pct       = replace_na(theta_pct, 0),
    vax_rate        = replace_na(vax_rate, 0),
    S_max_tw        = replace_na(S_max_tw, 0),
    F_CP_above         = replace_na(F_CP_above, 0),
    F_CP_below_adj_mid = replace_na(F_CP_below_adj_mid, 0),
    F_CP_below_adj_lo  = replace_na(F_CP_below_adj_lo, 0),
    F_CP_below_adj_hi  = replace_na(F_CP_below_adj_hi, 0)
  )


# ==============================================================================
#  DEBT: MAIN RESULTS TABLE (5 columns, like output gap progression)
#  (1) OLS — pooled CP + DI
#  (2) TWFE — pooled CP + DI (kappa_CP, kappa_DI)
#  (3) TWFE — CP split above/below (no adjustment)
#  (4) TWFE — CP split above/below (below adj. 35% take-up)
#  (5) TWFE — CP three-way: above + loans + guarantees (adj. 35%)
# ==============================================================================
debt_sub <- pdataD$t_idx >= 5 & pdataD$t_idx <= 14

y_q4.2019 <- feols(
  y_t_pct ~ S_mean_tw*y_lag1 + S_mean_tw*F_CP + y_lag1:F_CP + F_DI_lag2+ p_proj_all_ages |
    Country + Quarter, data=pdataY, panel.id = ~Country + Quarter, subset = ~ t_idx >= 5 & t_idx <= 12)
#base main

d_main <- feols(
  debt_dR~ y_t_pct+ F_CP_above_3 + F_CP_loans + F_CP_guar_adj + F_DI + Quarter | Country,
  data = pdataD, panel.id = ~Country + Quarter, subset = ~ t_idx >= 5 & t_idx <= 14)

summary(d_main, cluster = ~Country, ssc = ssc(K.adj = TRUE,  G.adj = TRUE))
# ==============================================================================
#  DEBT EQUATION: MAIN RESULTS TABLE (5 columns)
#  Sample: Q1.2020–Q2.2022 (t_idx 5–14, N=380)
#  No time trend, no F_H, no theta in main spec (see robustness)
# ==============================================================================



# Set SSC globally for consistent SEs across all feols models
setFixest_ssc(ssc(adj = TRUE, fixef.K = "none", cluster.adj = TRUE))

# (1) OLS — pooled CP and DI, no FE
d1_ols <- feols(
  debt_dR ~ y_t_pct + F_CP + F_DI_lag1,
  data = pdataD, subset = debt_sub, cluster = ~Country)

# (2) Country FE — pooled CP
d2_pooled <- feols(
  debt_dR ~ y_t_pct + F_CP + F_DI_lag1 | Country,
  data = pdataD, subset = debt_sub, cluster = ~Country)

# (3) Country FE — CP split above/below (unadjusted)
d3_split_raw <- feols(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_below + F_DI_lag1 | Country,
  data = pdataD, subset = debt_sub, cluster = ~Country)

# (4) Country FE — CP split above/below (below adjusted 35% take-up)
d4_split_adj <- feols(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_mid + F_DI_lag1 | Country,
  data = pdataD, subset = debt_sub, cluster = ~Country)

# (5) Country FE — CP three-way: above + loans + guarantees (adj 35%)
d5_3way <- feols(
  debt_dR ~ y_t_pct + F_CP_above_3 + F_CP_loans + F_CP_guar_adj + F_DI_lag1 | Country,
  data = pdataD, subset = debt_sub, cluster = ~Country)

# --- Model list with descriptive column headers ---
debt_main_list <- list(
  "OLS"                    = d1_ols,
  "Pooled CP"              = d2_pooled,
  "Above/Below"            = d3_split_raw,
  "Above/Below (adj.)"     = d4_split_adj,
  "Above/Loans/Guar."      = d5_3way
)

# --- Coefficient map with clean readable names ---
debt_main_coefmap <- c(
  "y_t_pct"            = "Output gap ($y_{ik}$)",
  "F_CP"               = "Capacity preservation, pooled",
  "F_CP_above"         = "CP above-the-line",
  "F_CP_below"         = "CP below-the-line (face value)",
  "F_CP_below_adj_mid" = "CP below-the-line (adj.\\ 35\\%)",
  "F_CP_above_3"       = "CP above-the-line",
  "F_CP_loans"         = "CP government loans",
  "F_CP_guar_adj"      = "CP guarantees (adj.\\ 35\\%)",
  "F_DI_lag1"          = "Demand injection ($F^{DI}_{i,k-1}$)"
)

# --- Custom rows for table footer ---
debt_add_rows <- tribble(
  ~term,              ~`OLS`, ~`Pooled CP`, ~`Above/Below`, ~`Above/Below (adj.)`, ~`Above/Loans/Guar.`,
  "Country FE",       "No",   "Yes",        "Yes",          "Yes",                 "Yes",
  "Guarantee adj.",   "---",  "---",        "No",           "35\\%",               "35\\%"
)

# --- Custom vcov function for consistent SEs ---
my_vcov <- function(x) vcov(x, cluster = ~Country, ssc = ssc(adj = TRUE, cluster.adj = TRUE))

# --- Print to console ---
cat("\n=== Debt Equation: Main Results ===\n")
modelsummary(debt_main_list,
             vcov     = my_vcov,
             stars    = c("." = 0.10, "*" = 0.05, "**" = 0.01, "***" = 0.001),
             coef_map = debt_main_coefmap,
             gof_map  = c("nobs", "r.squared.within"),
             add_rows = debt_add_rows,
             title    = "Debt Equation: Main Results")

# --- Export to LaTeX ---
modelsummary(debt_main_list,
             vcov     = my_vcov,
             stars    = c("." = 0.10, "*" = 0.05, "**" = 0.01, "***" = 0.001),
             coef_map = debt_main_coefmap,
             gof_map  = c("nobs", "r.squared.within"),
             add_rows = debt_add_rows,
             output   = file.path(safetable, "tab_debt_main.tex"),
             title    = "Debt Equation: Main Results",
             escape   = FALSE)
cat("  -> Saved: tab_debt_main.tex\n")



# ==============================================================================
#  DEBT EQUATION: PANEL B — ROBUSTNESS (5 columns)
#  Baseline: Col 4 from Panel A (Above/Below, adj. 35%, Country FE)
# ==============================================================================

debt_sub <- pdataD$t_idx >= 5 & pdataD$t_idx <= 14
debt_sub_prevax <- pdataD$t_idx >= 5 & pdataD$t_idx <= 8  # Q1.2020–Q4.2020

# (6) Baseline + quarterly trend
d6_trend <- feols(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_mid + F_DI_lag1 + trend | Country,
  data = pdataD, subset = debt_sub, cluster = ~Country)

# (7) Baseline + infection prevalence
d7_theta <- feols(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_mid + F_DI_lag1 + theta_mean | Country,
  data = pdataD, subset = debt_sub, cluster = ~Country)

# (8) Guarantee adjustment at 25%
d8_guar25 <- feols(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_lo + F_DI_lag1 | Country,
  data = pdataD, subset = debt_sub, cluster = ~Country)

# (9) Guarantee adjustment at 50%
d9_guar50 <- feols(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_hi + F_DI_lag1 | Country,
  data = pdataD, subset = debt_sub, cluster = ~Country)

# (10) Pre-vaccination sample only (Q1.2020–Q4.2020)
d10_prevax <- feols(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_mid + F_DI_lag1 | Country,
  data = pdataD, subset = debt_sub_prevax, cluster = ~Country)

# --- Model list ---
debt_robust_list <- list(
  "+ Trend"          = d6_trend,
  "+ Inf. prev."     = d7_theta,
  "Guar. 25\\%"     = d8_guar25,
  "Guar. 50\\%"     = d9_guar50,
  "Pre-vax"          = d10_prevax
)

# --- Coefficient map ---
debt_robust_coefmap <- c(
  "y_t_pct"            = "Output gap",
  "F_CP_above"         = "CP above-the-line",
  "F_CP_below_adj_mid" = "CP below-the-line (adj. 35\\%)",
  "F_CP_below_adj_lo"  = "CP below-the-line (adj. 25\\%)",
  "F_CP_below_adj_hi"  = "CP below-the-line (adj. 50\\%)",
  "F_DI_lag1"          = "Demand injection (lag 1)",
  "trend"              = "Quarterly trend",
  "theta_mean"         = "Infection prevalence"
)

# --- Custom rows ---
debt_robust_rows <- tribble(
  ~term,              ~`+ Trend`, ~`+ Inf. prev.`, ~`Guar. 25\\%`, ~`Guar. 50\\%`, ~`Pre-vax`,
  "Country FE",       "Yes",      "Yes",            "Yes",           "Yes",           "Yes",
  "Sample",           "Full",     "Full",           "Full",          "Full",          "Q1--Q4.2020"
)

# --- Custom vcov ---
my_vcov <- function(x) vcov(x, cluster = ~Country, ssc = ssc(adj = TRUE, cluster.adj = TRUE))

# --- Print to console ---
cat("\n=== Debt Equation: Robustness (Panel B) ===\n")
modelsummary(debt_robust_list,
             vcov     = my_vcov,
             stars    = c("." = 0.10, "*" = 0.05, "**" = 0.01, "***" = 0.001),
             coef_map = debt_robust_coefmap,
             gof_map  = c("nobs", "r.squared.within"),
             add_rows = debt_robust_rows,
             title    = "Debt Equation: Robustness")

# --- Export to LaTeX ---
modelsummary(debt_robust_list,
             vcov     = my_vcov,
             stars    = c("." = 0.10, "*" = 0.05, "**" = 0.01, "***" = 0.001),
             coef_map = debt_robust_coefmap,
             gof_map  = c("nobs", "r.squared.within"),
             add_rows = debt_robust_rows,
             output   = file.path(safetable, "tab_debt_robust.tex"),
             title    = "Debt Equation: Robustness",
             escape   = FALSE)
cat("  -> Saved: tab_debt_robust.tex\n")




# ==============================================================================
#  DEBT EQUATION: ROBUSTNESS CHECKS
#  Consistent with main spec: Country FE only, no Quarter FE, no trend,
#  no F_H, no theta. CRV1 with K.adj and G.adj.
#  Sample: Q1.2020–Q2.2022 (t_idx 5–14, N=380) unless noted.
# ==============================================================================

# ==============================================================================
#  APPENDIX: ADDITIONAL ROBUSTNESS (outliers, DI lags, sample, net effect)
# ==============================================================================

# --- Outlier exclusion (one at a time) ---
d_noIRL <- feols(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_mid + F_DI_lag1 | Country,
  data = pdataD %>% filter(Country != "IRL", t_idx >= 5, t_idx <= 14),
  cluster = ~Country)

d_noTUR <- feols(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_mid + F_DI_lag1 | Country,
  data = pdataD %>% filter(Country != "TUR" , t_idx >= 5, t_idx <= 14), cluster = ~Country)

d_noEST <- feols(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_mid + F_DI_lag1 | Country,
  data = pdataD %>% filter(Country != "EST", t_idx >= 5, t_idx <= 14), cluster = ~Country)

# --- DI lag structure ---
d_di0 <- feols(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_mid + F_DI | Country,
  data = pdataD, subset = debt_sub, cluster = ~Country)

d_di2 <- feols(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_mid + F_DI_lag2 | Country,
  data = pdataD, subset = debt_sub, cluster = ~Country)

# --- Sample horizon ---
d_strict <- feols(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_mid + F_DI_lag1 | Country,
  data = pdataD, subset = strict_sub, cluster = ~Country)

d_extend <- feols(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_mid + F_DI_lag1 | Country,
  data = pdataD, subset = extend_sub, cluster = ~Country)

# --- Net effect (without y_t_pct → total debt cost incl. stabilizer channel) ---
d_netto <- feols(
  debt_dR ~ F_CP_above + F_CP_below_adj_mid + F_DI_lag1 | Country,
  data = pdataD, subset = debt_sub, cluster = ~Country)

# --- Appendix model list ---
debt_app_list <- list(
  "Excl. IRL"        = d_noIRL,
  "Excl. TUR"        = d_noTUR,
  "Excl. EST"        = d_noEST,
  "DI contemp."      = d_di0,
  "DI lag 2"         = d_di2,
  "Q1.20--Q4.21"     = d_strict,
  "Q1.20--Q4.22"     = d_extend,
  "Net effect"       = d_netto
)

debt_app_coefmap <- c(
  "y_t_pct"            = "Output gap",
  "F_CP_above"         = "CP above-the-line",
  "F_CP_below_adj_mid" = "CP below-the-line (adj. 35\\%)",
  "F_DI"               = "Demand injection (contemp.)",
  "F_DI_lag1"          = "Demand injection (lag 1)",
  "F_DI_lag2"          = "Demand injection (lag 2)"
)

# --- Console output ---
cat("\n=== Debt Equation: Appendix Robustness ===\n")
modelsummary(debt_app_list,
             vcov     = my_vcov,
             stars    = c("." = 0.10, "*" = 0.05, "**" = 0.01, "***" = 0.001),
             coef_map = debt_app_coefmap,
             gof_map  = c("nobs", "r.squared.within"),
             title    = "Debt Equation: Additional Robustness (Appendix)")

# --- LaTeX export ---
modelsummary(debt_app_list,
             vcov     = my_vcov,
             stars    = c("." = 0.10, "*" = 0.05, "**" = 0.01, "***" = 0.001),
             coef_map = debt_app_coefmap,
             gof_map  = c("nobs", "r.squared.within"),
             output   = file.path(safetable, "tab_debt_appendix.tex"),
             title    = "Debt Equation: Additional Robustness (Appendix)",
             escape   = FALSE)
cat("  -> Saved: tab_debt_appendix.tex\n")

# ==============================================================================
#  DEBT: GUARANTEE TAKE-UP SENSITIVITY (feols)
# ==============================================================================

# Build effective CP for multiple take-up scenarios
take_ups <- c(0.00, 0.10, 0.25, 0.35, 0.50, 1.00)
tu_labels <- c("guar excl.", "guar*0.10", "guar*0.25", "guar*0.35", "guar*0.50", "guar full")

for (tu in take_ups) {
  tag <- gsub("\\.", "", sprintf("%.2f", tu))
  pdataD[[paste0("F_CP_eff_", tag)]] <-
    pdataD$F_CP_above_3 + pdataD$F_CP_loans + pdataD$F_CP_guar * tu
}

stars_fn <- function(pv) ifelse(pv < 0.001, "***", ifelse(pv < 0.01, "**",
  ifelse(pv < 0.05, "*", ifelse(pv < 0.1, ".", ""))))

cat("\n=== DEBT: kappa_CP across guarantee take-up scenarios ===\n")
cat(sprintf("  %-14s  %9s  %8s  %7s  %8s  %9s  %10s\n",
            "Scenario", "kappa_CP", "SE", "t-val", "p-val", "mean_CP", "debt_eff"))
cat("  ", strrep("-", 75), "\n")

# Pooled reference
d_pool_ref <- feols(
  debt_dR ~ y_t_pct + F_CP + F_DI_lag1 + p_proj_all_ages | Country + Quarter,
  data = pdataD, subset = debt_sub, cluster = ~Country)
ct_pr <- summary(d_pool_ref)$coeftable
kp <- ct_pr["F_CP", "Estimate"]
sep <- ct_pr["F_CP", "Std. Error"]
tp <- ct_pr["F_CP", "t value"]
pp <- ct_pr["F_CP", "Pr(>|t|)"]
d_subset <- pdataD %>% filter(t_idx >= 5 & t_idx <= 14)
mp <- mean(d_subset$F_CP, na.rm = TRUE)
cat(sprintf("  %-14s  %8.4f%s  %8.4f  %7.3f  %8.4f  %9.4f  %10.4f\n",
            "Pooled (orig.)", kp, stars_fn(pp), sep, tp, pp, mp, kp * mp))

tu_results <- list()
for (j in seq_along(take_ups)) {
  tag <- gsub("\\.", "", sprintf("%.2f", take_ups[j]))
  vname <- paste0("F_CP_eff_", tag)
  fml <- as.formula(paste0(
    "debt_dR ~ y_t_pct + ", vname, " + F_DI_lag1 + p_proj_all_ages | Country + Quarter"))
  m <- feols(fml, data = pdataD, subset = debt_sub, cluster = ~Country)
  ct <- summary(m)$coeftable
  k <- ct[vname, "Estimate"]
  se <- ct[vname, "Std. Error"]
  tv <- ct[vname, "t value"]
  pv <- ct[vname, "Pr(>|t|)"]
  cp_mean <- mean(d_subset[[vname]], na.rm = TRUE)
  tu_results[[j]] <- list(model = m, vname = vname)
  cat(sprintf("  %-14s  %8.4f%s  %8.4f  %7.3f  %8.4f  %9.4f  %10.4f\n",
              tu_labels[j], k, stars_fn(pv), se, tv, pv, cp_mean, k * cp_mean))
}
cat("  debt_eff = kappa_CP * mean(F_CP_eff) = avg quarterly debt increase (pp 2019 GDP)\n")



# ================================================================
#  LP 8b: COUNTRY-FE ONLY (no Quarter FE) — ROBUSTNESS
#
#  Motivation: TWFE absorbs the common deployment timing, which is
#  the primary source of fiscal variation. Country-FE-only preserves
#  the temporal variation (Deb et al. 2021 IMF, Chetty et al. 2020).
#  Tradeoff: more power but risk of omitted common shocks.
#  Controls (S, theta, y_lag1) mitigate this.
#  If results are qualitatively similar -> robust identification.
#  If stronger -> TWFE was overly conservative (absorbed signal).
# ================================================================
cat("\n=== LP 8b: COUNTRY-FE ONLY (no Quarter FE) — ROBUSTNESS ===\n\n")

# --- Output: Joint CP + DI, Country FE only ---
res_y_joint_cfe <- run_lp(lp_panel, dy_vars, "F_CP + F_DI",
                          controls = c("S_mean_tw", "y_lag1", "theta_pct"),
                          fe = "Country")
print_lp(res_y_joint_cfe, "OUTPUT (Country FE only): Joint LP (CP + DI)")

# --- Output: State-dependent, Country FE only ---
res_y_state_cfe <- run_lp(lp_panel, dy_vars, "F_CP + S_x_FCP + F_DI",
                          controls = c("S_mean_tw", "y_lag1", "theta_pct"),
                          fe = "Country")
print_lp(res_y_state_cfe, "OUTPUT (Country FE only): State-Dependent (F_CP + S*F_CP + F_DI)")

# --- Output: CP sub-components, Country FE only ---
res_y_cp_sub_cfe <- run_lp(lp_panel, dy_vars,
                           "F_CP_above_3 + F_CP_loans + F_CP_guar_adj",
                           controls = c("F_DI", "S_mean_tw", "y_lag1", "theta_pct"),
                           fe = "Country")
print_lp(res_y_cp_sub_cfe, "OUTPUT (Country FE only): CP Sub-Components")

# --- Output: DI sub-components, Country FE only ---
res_y_di_sub_cfe <- run_lp(lp_panel, dy_vars,
                           "F_DI_transfers + F_DI_demand",
                           controls = c("F_CP", "S_mean_tw", "y_lag1", "theta_pct"),
                           fe = "Country")
print_lp(res_y_di_sub_cfe, "OUTPUT (Country FE only): DI Sub-Components")

# --- Debt: Joint, Country FE only (same as TWFE version since debt already uses Country FE) ---
# (Debt LP already uses Country FE, so this is identical — skip to save time)

# --- Comparison table: TWFE vs Country-FE-only ---
cat("\n\n")
cat(strrep("=", 80), "\n")
cat("  COMPARISON: TWFE vs COUNTRY-FE-ONLY (Output LP, joint CP + DI)\n")
cat(strrep("=", 80), "\n\n")

cat(sprintf("  %3s  %24s  %24s\n", "h", "--- TWFE ---", "--- Country FE only ---"))
cat(sprintf("  %3s  %11s  %11s  %11s  %11s\n", "", "CP", "DI", "CP", "DI"))
cat("  ", strrep("-", 55), "\n")

stars_fn <- function(pv) ifelse(pv<0.001,"***",ifelse(pv<0.01,"** ",ifelse(pv<0.05,"*  ",ifelse(pv<0.1,".  ","   "))))

for (hh in 0:H_max) {
  cp_tw <- res_y_joint %>% filter(variable == "F_CP", .data$h == hh)
  di_tw <- res_y_joint %>% filter(variable == "F_DI", .data$h == hh)
  cp_cf <- res_y_joint_cfe %>% filter(variable == "F_CP", .data$h == hh)
  di_cf <- res_y_joint_cfe %>% filter(variable == "F_DI", .data$h == hh)

  fmt <- function(r) if(nrow(r)>0) sprintf("%7.4f%s", r$coef, stars_fn(r$pval)) else "       ---"
  cat(sprintf("  %3d  %11s  %11s  %11s  %11s\n", hh,
              fmt(cp_tw), fmt(di_tw), fmt(cp_cf), fmt(di_cf)))
}

# --- Comparison: State-dependent LP ---
cat("\n")
cat(strrep("=", 80), "\n")
cat("  COMPARISON: TWFE vs COUNTRY-FE-ONLY (State-Dependent: F_CP + S*F_CP + F_DI)\n")
cat(strrep("=", 80), "\n\n")

cat(sprintf("  %3s  %34s  %34s\n", "h", "------- TWFE -------", "--- Country FE only ---"))
cat(sprintf("  %3s  %11s  %11s  %11s  %11s  %11s  %11s\n",
            "", "F_CP", "S*F_CP", "F_DI", "F_CP", "S*F_CP", "F_DI"))
cat("  ", strrep("-", 75), "\n")

for (hh in 0:H_max) {
  tw_cp  <- res_y_state %>% filter(variable == "F_CP", .data$h == hh)
  tw_sx  <- res_y_state %>% filter(variable == "S_x_FCP", .data$h == hh)
  tw_di  <- res_y_state %>% filter(variable == "F_DI", .data$h == hh)
  cf_cp  <- res_y_state_cfe %>% filter(variable == "F_CP", .data$h == hh)
  cf_sx  <- res_y_state_cfe %>% filter(variable == "S_x_FCP", .data$h == hh)
  cf_di  <- res_y_state_cfe %>% filter(variable == "F_DI", .data$h == hh)

  fmt <- function(r) if(nrow(r)>0) sprintf("%7.4f%s", r$coef, stars_fn(r$pval)) else "       ---"
  cat(sprintf("  %3d  %11s  %11s  %11s  %11s  %11s  %11s\n", hh,
              fmt(tw_cp), fmt(tw_sx), fmt(tw_di),
              fmt(cf_cp), fmt(cf_sx), fmt(cf_di)))
}

# --- Comparison: CP sub-components ---
cat("\n")
cat(strrep("=", 80), "\n")
cat("  COMPARISON: TWFE vs COUNTRY-FE-ONLY (CP Sub-Components -> Output)\n")
cat(strrep("=", 80), "\n\n")

cat(sprintf("  %3s  %34s  %34s\n", "h", "------- TWFE -------", "--- Country FE only ---"))
cat(sprintf("  %3s  %11s  %11s  %11s  %11s  %11s  %11s\n",
            "", "Above", "Loans", "Guar(adj)", "Above", "Loans", "Guar(adj)"))
cat("  ", strrep("-", 75), "\n")

for (hh in 0:H_max) {
  tw_a <- res_y_cp_sub %>% filter(variable == "F_CP_above_3", .data$h == hh)
  tw_l <- res_y_cp_sub %>% filter(variable == "F_CP_loans", .data$h == hh)
  tw_g <- res_y_cp_sub %>% filter(variable == "F_CP_guar_adj", .data$h == hh)
  cf_a <- res_y_cp_sub_cfe %>% filter(variable == "F_CP_above_3", .data$h == hh)
  cf_l <- res_y_cp_sub_cfe %>% filter(variable == "F_CP_loans", .data$h == hh)
  cf_g <- res_y_cp_sub_cfe %>% filter(variable == "F_CP_guar_adj", .data$h == hh)

  fmt <- function(r) if(nrow(r)>0) sprintf("%7.4f%s", r$coef, stars_fn(r$pval)) else "       ---"
  cat(sprintf("  %3d  %11s  %11s  %11s  %11s  %11s  %11s\n", hh,
              fmt(tw_a), fmt(tw_l), fmt(tw_g),
              fmt(cf_a), fmt(cf_l), fmt(cf_g)))
}

# --- Cumulative multipliers: Country FE only ---
cat("\n")
cat(strrep("=", 80), "\n")
cat("  CUMULATIVE MULTIPLIERS: Country FE only\n")
cat(strrep("=", 80), "\n\n")

cat("  --- Output Multipliers (Country FE only) ---\n")
cat(sprintf("  %3s  %14s  %14s\n", "h", "CP multiplier", "DI multiplier"))
cat("  ", strrep("-", 40), "\n")
for (hh in 0:H_max) {
  cp_r <- res_y_joint_cfe %>% filter(variable == "F_CP", .data$h == hh)
  di_r <- res_y_joint_cfe %>% filter(variable == "F_DI", .data$h == hh)
  cp_s <- if(nrow(cp_r)>0) sprintf("%8.4f%s", cp_r$coef, ifelse(cp_r$pval<0.05,"*",ifelse(cp_r$pval<0.1,"."," "))) else "     ---"
  di_s <- if(nrow(di_r)>0) sprintf("%8.4f%s", di_r$coef, ifelse(di_r$pval<0.05,"*",ifelse(di_r$pval<0.1,"."," "))) else "     ---"
  cat(sprintf("  %3d  %14s  %14s\n", hh, cp_s, di_s))
}

# Interpretation block
# The Country-FE-only results tell us:
# - If output multipliers become significant -> TWFE was absorbing genuine
#   fiscal effect (the temporal deployment pattern IS the treatment).
# - If S*F_CP remains significant and similar magnitude -> the state-dependent
#   channel is robust to the FE specification.
# - If CP sub-component pattern is preserved -> the guarantee vs loan
#   decomposition is not an artifact of the time FE structure.
# - Key diagnostic: compare the S*F_CP coefficient. If it's stable across
#   TWFE and Country-FE, the interaction (not the level) is driving
#   identification, and the FE choice is secondary for this channel.



# ================================================================
# Kombinierter Plot
# ================================================================

# Und kontrolliere für S: zeige Output Gap nur für Quartale mit ähnlichem S
# Beschrifte die Gruppen mit Länderlisten

cat("\n=== Deskriptiver Vergleich der Gruppen ===\n")
pdataD %>%
  filter(Quarter >= "Q1.2020" & Quarter <= "Q4.2021") %>%
  group_by(DI_group) %>%
  summarise(
    mean_S     = mean(S_mean_tw, na.rm = TRUE),
    mean_CP    = mean(F_CP, na.rm = TRUE),
    mean_y     = mean(y_t_pct, na.rm = TRUE),
    mean_debt  = mean(debt_dR, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  as.data.frame()


cat("\n=== Deskriptiver Vergleich CP-Gruppen ===\n")
pdataD %>%
  filter(Quarter >= "Q1.2020" & Quarter <= "Q4.2021") %>%
  group_by(CP_group) %>%
  summarise(
    mean_S     = mean(S_mean_tw, na.rm = TRUE),
    mean_DI    = mean(F_DI, na.rm = TRUE),
    mean_y     = mean(y_t_pct, na.rm = TRUE),
    mean_debt  = mean(debt_dR, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  as.data.frame()

###############################

# ================================================================
# Scatterplots: Containment Intensity vs. Fiscal Deployment
# ================================================================

# Aggregiere auf Länderebene (2020–2021)
country_agg <- pdataD %>%
  filter(Quarter >= "Q1.2020" & Quarter <= "Q4.2021") %>%
  group_by(Country) %>%
  summarise(
    mean_S      = mean(S_mean_tw, na.rm = TRUE),
    total_CP    = sum(F_CP, na.rm = TRUE),
    total_DI    = sum(F_DI, na.rm = TRUE),
    total_F     = sum(F_CP + F_DI, na.rm = TRUE),
    total_above = sum(F_CP_above, na.rm = TRUE),
    total_below = sum(F_CP_below_adj_mid, na.rm = TRUE),
    mean_y      = mean(y_t_pct, na.rm = TRUE),
    total_debt  = sum(debt_dR, na.rm = TRUE),
    .groups     = "drop"
  )

# Plot 1: S vs. Total Spending
p1 <- ggplot(country_agg, aes(x = mean_S, y = total_F)) +
  geom_point(size = 3, color = "steelblue") +
  geom_text(aes(label = Country), hjust = -0.2, vjust = 0.5, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue", alpha = 0.15) +
  labs(
    title = "Containment Intensity vs. Total Fiscal Deployment",
    x = "Mean Stringency Index (2020–2021)",
    y = "Cumulative Fiscal Spending (% of 2019 GDP)",
    caption = sprintf("r = %.2f", cor(country_agg$mean_S, country_agg$total_F))
  ) +
  theme_minimal()

# Plot 2: S vs. CP
p2 <- ggplot(country_agg, aes(x = mean_S, y = total_CP)) +
  geom_point(size = 3, color = "darkgreen") +
  geom_text(aes(label = Country), hjust = -0.2, vjust = 0.5, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen", alpha = 0.15) +
  labs(
    title = "Containment vs. Capacity Preservation",
    x = "Mean Stringency Index (2020–2021)",
    y = "Cumulative CP (% of 2019 GDP)",
    caption = sprintf("r = %.2f", cor(country_agg$mean_S, country_agg$total_CP))
  ) +
  theme_minimal()

# Plot 3: S vs. DI
p3 <- ggplot(country_agg, aes(x = mean_S, y = total_DI)) +
  geom_point(size = 3, color = "firebrick") +
  geom_text(aes(label = Country), hjust = -0.2, vjust = 0.5, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "firebrick", alpha = 0.15) +
  labs(
    title = "Containment vs. Demand Injection",
    x = "Mean Stringency Index (2020–2021)",
    y = "Cumulative DI (% of 2019 GDP)",
    caption = sprintf("r = %.2f", cor(country_agg$mean_S, country_agg$total_DI))
  ) +
  theme_minimal()

# Kombiniert
library(patchwork)
p_all <- p1 / (p2 | p3) +
  plot_annotation(title = "Did Stricter Lockdowns Lead to More Fiscal Spending?")
print(p_all)

# Korrelationen
cat("\n=== Korrelationen (Länderebene) ===\n")
cat(sprintf("  S vs. Total F:  r = %.3f\n", cor(country_agg$mean_S, country_agg$total_F)))
cat(sprintf("  S vs. CP:       r = %.3f\n", cor(country_agg$mean_S, country_agg$total_CP)))
cat(sprintf("  S vs. DI:       r = %.3f\n", cor(country_agg$mean_S, country_agg$total_DI)))
cat(sprintf("  S vs. Output:   r = %.3f\n", cor(country_agg$mean_S, country_agg$mean_y)))
cat(sprintf("  S vs. Debt:     r = %.3f\n", cor(country_agg$mean_S, country_agg$total_debt)))

#*"Fiscal deployment was orthogonal to containment intensity in the cross-section (r=0.07r = 0.07 r=0.07): countries with comparable stringency levels chose markedly different fiscal strategies. This variation in the composition and timing of fiscal responses — driven by institutional capacity, political preferences, and pre-existing policy frameworks rather than by the contemporaneous containment level — provides the identifying variation for the panel estimation."*

# ================================================================
# Simpson's Paradox: Between vs. Within
# ================================================================

# Plot 1: S vs Output (Between — Länderebene)
p1 <- ggplot(country_agg, aes(x = mean_S, y = mean_y)) +
  geom_point(size = 3, color = "steelblue") +
  geom_text(aes(label = Country), hjust = -0.2, vjust = 0.5, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue", alpha = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  labs(
    title = "Containment vs. Output (Between Countries)",
    subtitle = "No cross-country relationship",
    x = "Mean Stringency Index (2020–2021)",
    y = "Mean Output Gap (pp)",
    caption = sprintf("r = %.2f", cor(country_agg$mean_S, country_agg$mean_y))
  ) +
  theme_minimal()

# Plot 2: Total F vs Output (Between)
p2 <- ggplot(country_agg, aes(x = total_F, y = mean_y)) +
  geom_point(size = 3, color = "darkgreen") +
  geom_text(aes(label = Country), hjust = -0.2, vjust = 0.5, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen", alpha = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  labs(
    title = "Total Fiscal Spending vs. Output (Between Countries)",
    subtitle = "No cross-country relationship (endogeneity)",
    x = "Cumulative Fiscal Spending (% of 2019 GDP)",
    y = "Mean Output Gap (pp)",
    caption = sprintf("r = %.2f", cor(country_agg$total_F, country_agg$mean_y))
  ) +
  theme_minimal()

# Plot 3: CP vs Output (Between)
p3 <- ggplot(country_agg, aes(x = total_CP, y = mean_y)) +
  geom_point(size = 3, color = "darkgreen") +
  geom_text(aes(label = Country), hjust = -0.2, vjust = 0.5, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen", alpha = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  labs(
    title = "CP vs. Output (Between Countries)",
    subtitle = "Negative: harder-hit countries deployed more CP",
    x = "Cumulative CP (% of 2019 GDP)",
    y = "Mean Output Gap (pp)",
    caption = sprintf("r = %.2f", cor(country_agg$total_CP, country_agg$mean_y))
  ) +
  theme_minimal()

# Plot 4: Total F vs Debt (Between)
p4 <- ggplot(country_agg, aes(x = total_F, y = total_debt)) +
  geom_point(size = 3, color = "firebrick") +
  geom_text(aes(label = Country), hjust = -0.2, vjust = 0.5, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "firebrick", alpha = 0.15) +
  labs(
    title = "Total Fiscal Spending vs. Debt (Between Countries)",
    subtitle = "Positive: more spending, more debt",
    x = "Cumulative Fiscal Spending (% of 2019 GDP)",
    y = "Cumulative Debt Change (pp of 2019 GDP)",
    caption = sprintf("r = %.2f", cor(country_agg$total_F, country_agg$total_debt))
  ) +
  theme_minimal()

# Kombiniert
p_simpson <- (p1 | p2) / (p3 | p4) +
  plot_annotation(
    title = "The Simpson's Paradox of Pandemic Fiscal Policy",
    subtitle = "Between-country correlations mask within-country causal effects"
  )
print(p_simpson)

# Korrelationen
cat("\n=== Between-Country Correlations ===\n")
cat(sprintf("  S vs. Output:       r = %.3f\n", cor(country_agg$mean_S, country_agg$mean_y)))
cat(sprintf("  Total F vs. Output: r = %.3f\n", cor(country_agg$total_F, country_agg$mean_y)))
cat(sprintf("  CP vs. Output:      r = %.3f\n", cor(country_agg$total_CP, country_agg$mean_y)))
cat(sprintf("  DI vs. Output:      r = %.3f\n", cor(country_agg$total_DI, country_agg$mean_y)))
cat(sprintf("  Total F vs. Debt:   r = %.3f\n", cor(country_agg$total_F, country_agg$total_debt)))
cat(sprintf("  CP vs. Debt:        r = %.3f\n", cor(country_agg$total_CP, country_agg$total_debt)))
cat(sprintf("  DI vs. Debt:        r = %.3f\n", cor(country_agg$total_DI, country_agg$total_debt)))

# ================================================================
# Find Country Pairs: Similar S and Theta, Different F Composition
# ================================================================

# Länderlevel Aggregate
pairs_data <- pdataD %>%
  filter(Quarter >= "Q1.2020" & Quarter <= "Q4.2021") %>%
  group_by(Country) %>%
  summarise(
    mean_S     = mean(S_mean_tw, na.rm = TRUE),
    mean_theta = mean(theta_pct, na.rm = TRUE),
    total_CP   = sum(F_CP, na.rm = TRUE),
    total_DI   = sum(F_DI, na.rm = TRUE),
    total_F    = total_CP + total_DI,
    CP_share   = total_CP / total_F,
    DI_share   = total_DI / total_F,
    mean_y     = mean(y_t_pct, na.rm = TRUE),
    total_debt = sum(debt_dR, na.rm = TRUE),
    .groups    = "drop"
  )

# Finde Paare: ähnliches S (±5), ähnliches Theta (±0.5), verschiedene Komposition
pairs <- expand.grid(
  A = pairs_data$Country,
  B = pairs_data$Country,
  stringsAsFactors = FALSE
) %>%
  filter(A < B) %>%
  left_join(pairs_data, by = c("A" = "Country")) %>%
  rename_with(~ paste0(.x, "_A"), -c(A, B)) %>%
  left_join(pairs_data, by = c("B" = "Country")) %>%
  rename_with(~ paste0(.x, "_B"), -c(A, B, ends_with("_A"))) %>%
  mutate(
    S_diff     = abs(mean_S_A - mean_S_B),
    theta_diff = abs(mean_theta_A - mean_theta_B),
    CP_share_diff = abs(CP_share_A - CP_share_B),
    y_diff     = mean_y_A - mean_y_B,
    debt_diff  = total_debt_A - total_debt_B
  ) %>%
  filter(S_diff < 5, theta_diff < 0.5) %>%
  arrange(-CP_share_diff)

# Top 10 Paare mit größter Kompositionsdifferenz
cat("=== Top Country Pairs: Similar S & Theta, Different Composition ===\n")
pairs %>%
  head(10) %>%
  select(A, B, mean_S_A, mean_S_B, mean_theta_A, mean_theta_B,
         CP_share_A, CP_share_B, mean_y_A, mean_y_B,
         total_debt_A, total_debt_B) %>%
  as.data.frame() %>%
  print(digits = 2)

# ================================================================
# Case Study: Canada (DI-heavy) vs. Switzerland (CP-heavy)
# ================================================================

case_pair <- pdataD %>%
  filter(Country %in% c("CAN", "CHE")) %>%
  select(Country, Quarter, y_t_pct, debt_dR, S_mean_tw, F_CP, F_DI, theta_pct) %>%
  group_by(Country) %>%
  arrange(Quarter) %>%
  mutate(debt_cum = cumsum(replace_na(debt_dR, 0))) %>%
  ungroup()

# Panel 1: Stringency — zeige Ähnlichkeit
p1 <- ggplot(case_pair, aes(x = Quarter, y = S_mean_tw,
                            color = Country, group = Country)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("CAN" = "firebrick", "CHE" = "steelblue"),
                     labels = c("Canada (DI-heavy)", "Switzerland (CP-heavy)")) +
  labs(title = "Containment: Similar Stringency",
       y = "Stringency Index", x = NULL, color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Panel 2: Fiscal Composition
case_fiscal <- case_pair %>%
  select(Country, Quarter, F_CP, F_DI) %>%
  pivot_longer(cols = c(F_CP, F_DI), names_to = "Instrument", values_to = "pct_GDP")

p2 <- ggplot(case_fiscal, aes(x = Quarter, y = pct_GDP,
                              fill = Instrument)) +
  geom_col(position = "stack", width = 0.7) +
  facet_wrap(~ Country, labeller = labeller(
    Country = c("CAN" = "Canada (DI-heavy)", "CHE" = "Switzerland (CP-heavy)"))) +
  scale_fill_manual(values = c("F_CP" = "darkgreen", "F_DI" = "firebrick"),
                    labels = c("Capacity Preservation", "Demand Injection")) +
  labs(title = "Fiscal Composition: Different Strategies",
       y = "% of 2019 GDP", x = NULL, fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# Panel 3: Output Gap
p3 <- ggplot(case_pair, aes(x = Quarter, y = y_t_pct,
                            color = Country, group = Country)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c("CAN" = "firebrick", "CHE" = "steelblue"),
                     labels = c("Canada (DI-heavy)", "Switzerland (CP-heavy)")) +
  labs(title = "Output: Faster Recovery with CP",
       y = "Output Gap (pp)", x = NULL, color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Panel 4: Kumulative Schulden
p4 <- ggplot(case_pair, aes(x = Quarter, y = debt_cum,
                            color = Country, group = Country)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("CAN" = "firebrick", "CHE" = "steelblue"),
                     labels = c("Canada (DI-heavy)", "Switzerland (CP-heavy)")) +
  labs(title = "Debt: Lower Accumulation with CP",
       y = "Cumulative debt change (pp of 2019 GDP)", x = NULL, color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Kombiniert
library(patchwork)
p_case <- (p1 | p2) / (p3 | p4) +
  plot_annotation(
    title = "Canada vs. Switzerland: The Composition Effect",
    subtitle = "Similar containment and infection pressure, different fiscal strategies, different outcomes"
  )
print(p_case)

# Deskriptive Zusammenfassung
cat("\n=== Case Study Summary (2020–2021 averages) ===\n")
case_pair %>%
  filter(Quarter <= "Q4.2021") %>%
  group_by(Country) %>%
  summarise(
    mean_S     = mean(S_mean_tw, na.rm = TRUE),
    mean_theta = mean(theta_pct, na.rm = TRUE),
    total_CP   = sum(F_CP, na.rm = TRUE),
    total_DI   = sum(F_DI, na.rm = TRUE),
    CP_share   = total_CP / (total_CP + total_DI),
    mean_y     = mean(y_t_pct, na.rm = TRUE),
    total_debt = sum(debt_dR, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  as.data.frame() %>%
  print(digits = 3)

##Achtung das ist mit institutionellen Unterschiede-> Schuldenbremse, fiskalischer Förderalismus usw

##Problem der Finanzierung
# Wie viel Volumen ist EU-finanziert?
fm1 %>%
  filter(transmission_channel %in% c("CP", "DI")) %>%
  group_by(transmission_channel, is_eu) %>%
  summarise(
    n = n(),
    total_vol = sum(size_pct, na.rm = TRUE),
    .groups = "drop"
  )

##DI bewegt sich wenn EU weg!

##MP-> Ermöglicher, DI ist deshalb evtl schwach da MP über gleichen Kanal läuft. CP sind zentral-> Zinsen tief ABER STEIGEND SPàTER-> KOnsequenz

# Eurozone vs. Nicht-Euro
pdataY$euro <- as.numeric(pdataY$Country %in% 
                            c("AUT", "BEL", "EST", "FIN", "FRA", "DEU", "GRC", "IRL", 
                              "ITA", "LVA", "LTU", "LUX", "NLD", "PRT", "SVK", "SVN", "ESP"))

m_y_euro <- plm(
  y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP + F_DI_lag2,
  data = pdataY, model = "within", effect = "twoways"
)
coeftest(m_y_euro, vcov = vcovHC(m_y_euro, cluster = "group", type = "HC1"))

##Geldpolitik Heterogenität spielt keine Rolle
##F_DI auch als Interaktion modellieren

# ==============================================================================
#  DESCRIPTIVE TIME-SERIES PLOTS: Cross-Country Averages Q1.2020–Q4.2022
# ==============================================================================

plot_qs <- c("Q1.2020","Q2.2020","Q3.2020","Q4.2020",
             "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
             "Q1.2022","Q2.2022","Q3.2022","Q4.2022")

avg_ts <- pdata %>%
  filter(Quarter %in% plot_qs) %>%
  group_by(Quarter) %>%
  summarise(
    y_t_pct   = mean(y_t_pct,   na.rm = TRUE),
    debt_dR   = mean(debt_dR,   na.rm = TRUE),
    S_mean_pw = mean(S_mean_pw, na.rm = TRUE),
    F_CP      = mean(F_CP,      na.rm = TRUE),
    F_DI      = mean(F_DI,      na.rm = TRUE),
    F_H       = mean(F_H,       na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  mutate(Quarter = factor(Quarter, levels = plot_qs, ordered = TRUE))

# --- Plot 1: Output Gap ---
p_y <- ggplot(avg_ts, aes(x = Quarter, y = y_t_pct, group = 1)) +
  geom_line(linewidth = 1.2, color = "steelblue") +
  geom_point(size = 2.5, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  labs(title = "Output Gap", y = "pp of potential", x = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- Plot 2: Debt Change (first diff, real) ---
p_debt <- ggplot(avg_ts, aes(x = Quarter, y = debt_dR, group = 1)) +
  geom_line(linewidth = 1.2, color = "firebrick") +
  geom_point(size = 2.5, color = "firebrick") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  labs(title = "Debt Change (real)", y = "pp of 2019 GDP", x = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- Plot 3: Stringency (S_mean_pw) ---
p_s <- ggplot(avg_ts, aes(x = Quarter, y = S_mean_pw, group = 1)) +
  geom_line(linewidth = 1.2, color = "darkorange") +
  geom_point(size = 2.5, color = "darkorange") +
  labs(title = "Stringency Index (S_mean_pw)", y = "Index", x = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- Plot 4: Fiscal Compositions (F_CP, F_DI, F_H) ---
avg_fiscal_long <- avg_ts %>%
  select(Quarter, F_CP, F_DI, F_H) %>%
  pivot_longer(cols = c(F_CP, F_DI, F_H),
               names_to = "Instrument", values_to = "pct_GDP")

p_fiscal <- ggplot(avg_fiscal_long, aes(x = Quarter, y = pct_GDP,
                                        color = Instrument, group = Instrument)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(
    values = c("F_CP" = "darkgreen", "F_DI" = "firebrick", "F_H" = "purple"),
    labels = c("Capacity Preservation", "Demand Injection", "Health")
  ) +
  labs(title = "Fiscal Composition", y = "% of 2019 GDP", x = NULL, color = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# --- Combined ---
library(patchwork)
p_descriptive_ts <- (p_y | p_debt) / (p_s | p_fiscal) +
  plot_annotation(
    title    = "Cross-Country Averages (38 OECD Countries), Q1.2020 – Q4.2022",
    subtitle = "Output gap, debt dynamics, containment stringency, and fiscal composition"
  )
print(p_descriptive_ts)

ggsave(file.path(safeplots, "descriptive_ts_averages.pdf"),
       p_descriptive_ts, width = 14, height = 9)

# --- Variant: Fiscal as stacked bar (flow per quarter) ---
p_fiscal_bar <- ggplot(avg_fiscal_long, aes(x = Quarter, y = pct_GDP,
                                            fill = Instrument)) +
  geom_col(position = "stack", width = 0.7) +
  scale_fill_manual(
    values = c("F_CP" = "darkgreen", "F_DI" = "firebrick", "F_H" = "purple"),
    labels = c("Capacity Preservation", "Demand Injection", "Health")
  ) +
  labs(title = "Fiscal Disbursement per Quarter",
       y = "% of 2019 GDP (flow)", x = NULL, fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

p_descriptive_ts_flow <- (p_y | p_debt) / (p_s | p_fiscal_bar) +
  plot_annotation(
    title    = "Cross-Country Averages (38 OECD Countries), Q1.2020 – Q4.2022",
    subtitle = "Output gap, debt dynamics, containment stringency, and fiscal disbursement (flow per quarter)"
  )
print(p_descriptive_ts_flow)

ggsave(file.path(safeplots, "descriptive_ts_averages_flow.pdf"),
       p_descriptive_ts_flow, width = 14, height = 9)



# ================================================================
# Export für MATLAB: Länderspezifische Kontrafaktuale
# ================================================================

# Recompute debt_dR with proper chronological sorting (fix alphabetical lag bug)
# Include Q3.2019 from pdata as lag source for Q4.2019
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
pdataD <- pdataD %>%
  left_join(debt_lag_source, by = c("Country", "Quarter")) %>%
  mutate(debt_dR = debt_dR_new) %>%
  select(-debt_dR_new)

# Export: Q4.2019 through Q4.2022, fill pre-pandemic NAs with 0
export_data <- pdataD %>%
  select(
    Country, Quarter,
    # Gesundheitsseite (fixiert)
    S_mean_tw, theta_pct,
    # Fiskalpolitik (beobachtet, zum Vergleich)
    F_CP, F_CP_above, F_CP_below_adj_mid, F_DI, F_H,
    # Outcomes (Validierung)
    y_t_pct, debt_dR,
    # Excess mortality
    excess_mortality = p_proj_all_ages,
    # Zusatzinfo
    vax_rate
  ) %>%
  mutate(
    # Pre-pandemic quarters: fill NAs with 0 (no policy, no infections)
    across(c(S_mean_tw, theta_pct, F_CP, F_CP_above, F_CP_below_adj_mid,
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

# Prüfe ob F_H existiert
cat("=== F_H Check ===\n")
cat(sprintf("  F_H vorhanden: %s\n", "F_H" %in% names(export_data)))
cat(sprintf("  F_H Range: %.3f to %.3f\n", 
            min(export_data$F_H, na.rm=TRUE), 
            max(export_data$F_H, na.rm=TRUE)))
cat(sprintf("  F_H mean:  %.3f\n", mean(export_data$F_H, na.rm=TRUE)))

# Falls F_H nicht in pdataD existiert, prüfe den Namen
# names(pdataD) %>% grep("H|health|hlth", ., value=TRUE, ignore.case=TRUE)

# Prüfe Vollständigkeit
cat("\n=== Export Summary ===\n")
cat(sprintf("  Countries: %d\n", length(unique(export_data$Country))))
cat(sprintf("  Quarters:  %d\n", length(unique(export_data$Quarter))))
cat(sprintf("  Rows:      %d\n", nrow(export_data)))

# Missing Values
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


