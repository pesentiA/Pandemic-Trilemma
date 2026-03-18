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
    Qpopulation_th
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
# --- Trilemma estimation sample: Q1.2020 – Q4.2021 --------------------------
df_estimation <- df %>%
  filter(Quarter %in% c("Q4.2019","Q1.2020", "Q2.2020", "Q3.2020", "Q4.2020",
                        "Q1.2021", "Q2.2021", "Q3.2021", "Q4.2021", "Q1.2022"))

cat(sprintf("  Estimation sample: %d obs (%d countries × %d quarters)\n",
            nrow(df_estimation),
            n_distinct(df_estimation$Country),
            n_distinct(df_estimation$Quarter)))
# ==============================================================================
#  Modelling based on the theory
#  Q: What is the transmission delay of S_k and F_k on y_k?
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


# --- Construct lagged variables within panel ---------------------------------
pdata <- pdata %>%
  mutate(S_mean_tw= S_mean_pw* 100)

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
    y_lead1     = lead(y_t_pct,   1)
  )




colnames(pdata)

#===============================================================================
##For OUTPUT GAP
#===============================================================================
#restrict sample to main COVID Years plus Q1.2022
pdataY <- pdata %>%
  filter(Quarter %in% c("Q1.2020", "Q2.2020", "Q3.2020", "Q4.2020",
                        "Q1.2021", "Q2.2021", "Q3.2021", "Q4.2021", "Q1.2022"))


##Descriptives
# Variablen auswählen und umbenennen
desc_vars <- pdataY %>%
  transmute(
    `Output Gap (pp)`            = y_t_pct,
    `Stringency Index`           = S_mean_tw,
    `Capacity Preservation (pp GDP)` = F_CP,
    `Demand Injection (pp GDP)`  = F_DI,
    `Infection Prevalence (pct)` = theta_pct,
    `Excess Mortality (pct)`     = d_t_pct,
    `Health Expenditure (pp GDP)` = F_H,
    `Vax Rate`                   = vax_rate,
    
  )

tab <- datasummary(
  All(desc_vars) ~ N + Mean + SD + Min + P25 + Median + P75 + Max,
  data   = desc_vars,
  fmt    = 2,
  output = "data.frame"
)
print(tab)

##Show the differences for the vax rate wehn you shirnk the time horizon

#variables for timetrend analysis
#to check for non parametric time trend-> Koeffizienten und T-Werte fallen wenn genauer für die Zeit kontrolliert wird-> Problem: Omitted Variable Bias of Time-> use either twoways or the Quarter dummys-> yield exact the same koeffizient
#vary sample size, include time trend, include controls and different definitions of Y, do bootstrap, check for outliner-> CP stays significant
#
#Get the whole story out of the parameters and how they change
#wenn ich 2022 inkludiere wird vax_rate hochsignifikant-> SPannend aber vlt aussrrhalb von meiner Forschungsfrage
#===============================================================================
##Base Model

m_main <- plm(y_t_pct~ S_mean_tw*y_lag1+ S_mean_tw*F_CP +  F_DI_lag2 + theta_pct,
  data = pdataY, index=c("Country", "Quarter"),  model = "within", effect = "twoways"
)

coeftest(m_main, vcov = vcovHC(m_main, cluster = "group", type = "HC1"))



#LSDV-> Als Dummys
# LSDV mit expliziten Dummies für Länder und Zeit
m_lsdv_explicit <- lm(y_t_pct ~ S_mean_tw * y_lag1 + 
                        S_mean_tw * F_CP + 
                        F_DI_lag2 + theta_pct + 
                        factor(Country) + factor(Quarter), 
                      data = pdataY)

# Robuste Standardfehler (Clustering auf Länderebene)

coeftest(m_lsdv_explicit, vcov = vcovHC(m_lsdv_explicit, cluster = "Country", type = "HC1"))

# Dokumentiere den Bias für das Paper
cat("=== Nickell Bias Assessment ===\n")
cat(sprintf("  T = %d → O(1/T) = %.1f%%\n", 9, 100/9))
cat(sprintf("  rho_y (FE):          %.3f\n", coef(m_main)["y_lag1"]))
cat(sprintf("  rho_y (bias-adj):    %.3f\n", coef(m_main)["y_lag1"] / (1 - 1/9)))
cat(sprintf("  psi (FE):            %.4f (lower bound)\n", coef(m_main)["S_mean_tw:y_lag1"]))
 

#waldtest-> CP oder DI Besser??

##was ist mit y_lead1->gefahr von Zuunft erklärt die Gegenwart
#S_lag1 nicht aufnehmen, zeigt nur die Persistenz, F_CP_lag2 auch, dS ebenso-> nimmt diw Wirkung von Containment Lockerung und CP Begleitung auf

# Hauptspezifikation: HC1, country-clustered (Stata-äquivalent)
# Robustheit: HC3 (konservativer, für N=38 sinnvoll als Check)
#coeftest(m_main, vcov = vcovHC(m_main, cluster = "group", type = "HC3"))


##Use FEOLS for robustness and bootstrap

m_feols <- feols(
  y_t_pct ~ S_mean_tw*y_lag1+S_mean_tw * F_CP + F_DI_lag2 |
            Country + Quarter, data = pdataY, cluster = ~Country)

# fixest cluster-robust SEs (äquivalent zu HC1)
summary(m_feols)

# Pairs Cluster Bootstrap (in fixest eingebaut)
summary(m_feols, se = "cluster", cluster = ~Country, 
        ssc = ssc(fixef.K = "none", cluster.adj = TRUE))


##vorschlag Claude bootstrap
#######
library(boot)
countries <- unique(pdataY$Country)
country_df <- data.frame(Country = countries, id = seq_along(countries))

boot_feols <- function(country_data, indices) {
  boot_countries <- country_data$Country[indices]
  
  boot_data <- do.call(rbind, lapply(seq_along(boot_countries), function(i) {
    d <- pdataY[pdataY$Country == boot_countries[i], ]
    d$Country <- paste0(d$Country, "_", i)
    d
  }))
  
  tryCatch({
    m <- feols(
      y_t_pct ~ S_mean_tw:y_lag1 + S_mean_tw * F_CP + F_DI_lag2 |
        Country + Quarter,
      data = boot_data
    )
    coef(m)
  }, error = function(e) rep(NA, 5))
}

set.seed(42)
boot_result <- boot(
  data      = country_df,
  statistic = boot_feols,
  R         = 99
)

# Ergebnisse
coef_names <- names(boot_result$t0)
cat("=== Bootstrap SE vs. HC1 ===\n\n")
for (i in seq_along(coef_names)) {
  boot_se <- sd(boot_result$t[, i], na.rm = TRUE)
  boot_p  <- 2 * min(mean(boot_result$t[, i] >= 0, na.rm = TRUE),
                     mean(boot_result$t[, i] <= 0, na.rm = TRUE))
  cat(sprintf("  %-25s  Coef: %8.5f  Boot-SE: %8.5f  p: %.4f\n",
              coef_names[i], boot_result$t0[i], boot_se, boot_p))
}

# Was enthält boot_result?
boot_result$t0
ncol(boot_result$t)
head(boot_result$t)


#Check: nur die above-the-line elemente, 1x beide separat und dann mit dem main modell vergleichen
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
pdataY_CP <- pdataY %>%
  left_join(fiscal_panel, by = c("Country", "Quarter")) %>%
  mutate(
    F_CP_above = replace_na(F_CP_above, 0),
    F_CP_below = replace_na(F_CP_below, 0)
  )





##Robust gegen: 
#Controls-> theta_mean, y_lag1,F_H, CP_share, S_max (wackelt etwas)
#Andere Variationen von Y: QReal.GDP.Growth_gr, y_t, 
#STRUKUTR? Anders in Transitionsgleichung-> aber auch wenn wie in der Theory-> Effekt genau gleich
#Theta ändert Parameter Alpha_s praktisch nicht-> FE absorbieren die endogenen Elemente
##Argument das DI von CP abhängt: + F_DI_lag1:F_CP
#===============================================================================
#################################DEBT###########################################
#===============================================================================

#Für ganzes Sample
pdata <- pdata %>%
  group_by(Country) %>%
  mutate(debt_dR = DebtR_share2019 - lag(DebtR_share2019)) %>%
  ungroup()
#gleich aber nominal
pdata <- pdata %>%
  group_by(Country) %>%
  mutate(debt_dN = DebtN_share2019 - lag(DebtN_share2019)) %>%
  ungroup()


pdataD <- pdata %>%
  filter(Quarter %in% c("Q1.2020", "Q2.2020", "Q3.2020", "Q4.2020",
                        "Q1.2021", "Q2.2021", "Q3.2021", "Q4.2021", "Q1.2022" ))

#First Differences von den normierten realen Staatsschulden des 2019 GDP
pdataD <- pdataD %>%
  group_by(Country) %>%
  mutate(debt_dR = DebtR_share2019 - lag(DebtR_share2019)) %>%
  ungroup()

#gleich aber nominal
pdataD <- pdataD %>%
  group_by(Country) %>%
  mutate(debt_dN = DebtN_share2019 - lag(DebtN_share2019)) %>%
  ungroup()



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
#.............................ANALYSIS.........................................
#1) Pre and Post vergleichen
##zwei samples um die kollabierte gleichun gzu zeigen
# Breites Sample: 2015–2022
m_gamma_pre <- plm(
  debt_dN ~ y_t_pct,
  data = pdata %>% filter(year_only < 2020),
  model = "within", effect = "twoways"
)

summary(m_gamma_pre)

# Pandemie-Sample mit F
m_gamma_pan <- plm(
  debt_dR ~ y_t_pct + F_CP + F_DI + F_H + theta_pct + inflation_index,
  data = pdataD %>% filter(year_only >= 2019),
  model = "within", effect = "twoways"
)

summary(m_gamma_pan)


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

##MAAAAIN MODELL OUPUT
m_output_adj <- plm(y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP+ F_DI_lag2 + theta_mean,
  data = pdataY, model = "within", effect = "twoways")

coeftest(m_output_adj, vcov = vcovHC(m_output_adj, cluster = "group", type = "HC1"))

# ==============================================================================
#  FEAR CHANNEL: OUTPUT GAP PLM WITH EXCESS MORTALITY
#  Excess mortality (P-score) captures fear-driven voluntary behavioural
#  suppression beyond S_k mandates (Eichenbaum et al. 2020 channel).
#  Main:       p_proj_all_ages  — projected-baseline P-score (avoids ageing bias)
#  Robustness: p_avg_all_ages   — 2015-2019 avg P-score (upper bound)
# ==============================================================================

# Pull quarterly excess mortality already aggregated in qdata
pdataY <- pdataY %>%
  left_join(
    qdata %>%
      select(Country, Quarter, p_proj_all_ages, p_avg_all_ages,
             cum_excess_per_million_proj_all_ages) %>%
      distinct(Country, Quarter, .keep_all = TRUE),
    by = c("Country", "Quarter")
  )

# Main: projected P-score (correct specification — avoids ageing-society upward bias)
m_y_fear <- plm(
  y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP + F_DI_lag2 + p_proj_all_ages,
  data = pdataY, index = c("Country", "Quarter"),
  model = "within", effect = "twoways"
)
ct_y_fear <- coeftest(m_y_fear, vcov = vcovHC(m_y_fear, cluster = "group", type = "HC1"))
cat("\n=== OUTPUT GAP + FEAR TERM (p_proj_all_ages — projected P-score) ===\n")
print(ct_y_fear)

# Robustness: 2015-2019 avg baseline P-score (upward biased in ageing societies)
m_y_fear_avg <- plm(
  y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP + F_DI_lag2 + p_avg_all_ages,
  data = pdataY, index = c("Country", "Quarter"),
  model = "within", effect = "twoways"
)
cat("\n=== OUTPUT GAP + FEAR TERM (p_avg_all_ages — 2015-2019 avg, robustness) ===\n")
print(coeftest(m_y_fear_avg, vcov = vcovHC(m_y_fear_avg, cluster = "group", type = "HC1")))

# Robustness: cumulative excess deaths per million (level measure)
m_y_fear_cum <- plm(
  y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP + F_DI_lag2 +
    cum_excess_per_million_proj_all_ages,
  data = pdataY, index = c("Country", "Quarter"),
  model = "within", effect = "twoways"
)
cat("\n=== OUTPUT GAP + FEAR TERM (cum_excess_per_million — cumulative stock, robustness) ===\n")
print(coeftest(m_y_fear_cum, vcov = vcovHC(m_y_fear_cum, cluster = "group", type = "HC1")))

#Hier lag2-> Kette aufzeigen-> DI ist additiv, CP ist also identifiziert, Sample ist jetzt restricted auf Pandemieperiode
#CP und DI wirken aber DI mit Verzögerung
#Die Konsequenz ist das DI viel teurer war vor allem wenn wir CP splitten und Guarantees betrachten
#Genau das was ich wollte-> NEXT STEPS
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
# TABLE 1: OUTPUT EQUATION
# ================================================================

# Col 1: Pooled OLS
m_y_pool <- plm(
  y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP + F_DI_lag2,
  data = pdataY, model = "pooling"
)

# Col 2: Country FE
m_y_fe1 <- plm(
  y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP + F_DI_lag2,
  data = pdataY, model = "within", effect = "individual"
)

# Col 3: Two-Way FE (Hauptspezifikation)
m_y_fe2 <- plm(
  y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP + F_DI_lag2,
  data = pdataY, model = "within", effect = "twoways"
)

# Col 4: Two-Way FE mit Lag 1 (Robustheit)
m_y_lag1 <- plm(
  y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP + F_DI_lag1,
  data = pdataY, model = "within", effect = "twoways"
)

# Alle mit HC1
coeftest(m_y_pool, vcov = vcovHC(m_y_pool, cluster = "group", type = "HC1"))
coeftest(m_y_fe1,  vcov = vcovHC(m_y_fe1,  cluster = "group", type = "HC1"))
coeftest(m_y_fe2,  vcov = vcovHC(m_y_fe2,  cluster = "group", type = "HC1"))
coeftest(m_y_lag1, vcov = vcovHC(m_y_lag1,  cluster = "group", type = "HC1"))


# ================================================================
# TABLE 2: DEBT EQUATION
# ================================================================

# Col 1: Pooled OLS
m_d_pool <- plm(
  debt_dR ~ y_t_pct + F_CP + F_DI_lag1 + as.numeric(Quarter),
  data = pdataD, model = "pooling"
)

# Col 2: Country FE (gepoolt CP)
m_d_fe <- plm(
  debt_dR ~ y_t_pct + F_CP + F_DI_lag1 + as.numeric(Quarter),
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



###########################Final Descriptive####################################

# ================================================================
# COMBINED DESCRIPTIVE STATISTICS
# ================================================================

# Verwende das breitere Debt-Sample als Basis
# Variablen die nur im Output-Sample existieren separat

desc_all <- pdataD %>%
  transmute(
    `Output Gap (pp)`                      = y_t_pct,
    `Debt Change, real (pp of 2019 GDP)`   = debt_dR,
    `Stringency Index (0--100)`            = S_mean_tw,
    `Capacity Preservation (\\% GDP)`      = F_CP,
    `CP Above-the-line (\\% GDP)`          = F_CP_above,
    `CP Below-the-line, adj. (\\% GDP)`    = F_CP_below_adj_mid,
    `Demand Injection (\\% GDP)`           = F_DI,
    `DI Lag 1 (\\% GDP)`                   = F_DI_lag1,
    `DI Lag 2 (\\% GDP)`                   = F_DI_lag2,
    `Infection Prevalence (\\%)`           = theta_pct,
    `Vaccination Rate`                     = vax_rate,
    `Inflation Index (2015=1)`             = inflation_index
  )

tab_all <- datasummary(
  All(desc_all) ~ N + Mean + SD + Min + P25 + Median + P75 + Max,
  data = desc_all, fmt = 2, output = "data.frame"
)
print(tab_all)


#########################ROBUSTNESS############################################

#===============================================================================
#############################Output GAP#########################################
#===============================================================================

##Functional Form

#Non linearity
m_quad <- plm(
  y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP + I(F_CP^2) + F_DI_lag2,
  data = pdataY, model = "within", effect = "twoways"
)
coeftest(m_quad, vcov = vcovHC(m_quad, cluster = "group", type = "HC1"))

##Knapp nicht signifikant, negativ-> macht sinn, abnehmender Grenznutzen-> Interaktionsterm absoribiert nicht-linearität
##Rechtsschiefe von CP ist durch Interaktion mit S teilweise kontrolliert-> Log wäre iene Lösung aber viele 0-> Extreme Massnahmen hatten auch massives S-> das hilft

##Additive Separabilität

pdataY$S_high <- as.numeric(pdataY$S_mean_tw < 50)

m_threshold <- plm(
  y_t_pct ~ S_mean_tw * y_lag1 + F_CP * S_high + F_DI_lag2,
  data = pdataY, model = "within", effect = "twoways"
)
coeftest(m_threshold, vcov = vcovHC(m_threshold, cluster = "group", type = "HC1"))

##STandardfehler

# HC3 (bias-corrected)
coeftest(m_y_fe2, vcov = vcovHC(m_y_fe2, cluster = "group", type = "HC3"))
#Passt, evtl noch bootstrap


##Ausreiser
pdataY %>%
  filter(Country == "IRL") %>%
  select(Quarter, y_t_pct) %>%
  as.data.frame()
# Ohne Irland (Leprechaun Economics, y_gap bis +11%)
m_y_noIRL <- plm(
  y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP + F_DI_lag2,
  data = pdataY %>% filter(Country != "IRL"),
  model = "within", effect = "twoways"
)
coeftest(m_y_noIRL, vcov = vcovHC(m_y_noIRL, cluster = "group", type = "HC1"))

# Ohne Türkei (hohe Inflation, idiosynkratische Dynamik)
m_y_noTUR <- plm(
  y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP + F_DI_lag2,
  data = pdataY %>% filter(Country != "TUR"),
  model = "within", effect = "twoways"
)
coeftest(m_y_noTUR, vcov = vcovHC(m_y_noTUR, cluster = "group", type = "HC1"))

# Ohne beide
m_y_noOutliers <- plm(
  y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP + F_DI_lag2,
  data = pdataY %>% filter(!Country %in% c("IRL", "TUR")),
  model = "within", effect = "twoways"
)
coeftest(m_y_noOutliers, vcov = vcovHC(m_y_noOutliers, cluster = "group", type = "HC1"))

##IRL bewegt DI, reporten im Anhang oder/und Variable checken

##Sample Horizont

# Strikt pre-Omicron (Q1.2020–Q4.2021)
m_y_preOmi <- plm(
  y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP + F_DI_lag2,
  data = pdataY %>% filter(Quarter <= "Q4.2021"),
  model = "within", effect = "twoways"
)
coeftest(m_y_preOmi, vcov = vcovHC(m_y_preOmi, cluster = "group", type = "HC1"))

# Nur akute Phase (Q1.2020–Q2.2021)
m_y_acute <- plm(
  y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP + F_DI_lag2,
  data = pdataY %>% filter(Quarter <= "Q2.2021"),
  model = "within", effect = "twoways"
)
coeftest(m_y_acute, vcov = vcovHC(m_y_acute, cluster = "group", type = "HC1"))

#ALles andere stabil, Ende des Samples nicht problematisch
#DI Lag 2 ist weg aber das ist gut: Das stärkt ironischerweise die Timing-Narrative: DI wirkt nur mit erheblicher Verzögerung. In der akuten Phase (wo der Schock passiert und DI deployed wird) ist kein Outputeffekt sichtbar. Erst im erweiterten Sample (das die Erholung 2021 einschließt) materialisiert sich der Lag-2-Effekt.

##Andere COntainment Messung
pdataY$S_max_pw<-(pdataY$S_max_pw*100)
# Maximum Stringency statt Mean
m_y_maxS <- plm(
  y_t_pct ~ S_max_pw * y_lag1 + S_max_pw * F_CP + F_DI_lag2,
  data = pdataY, model = "within", effect = "twoways"
)
coeftest(m_y_maxS, vcov = vcovHC(m_y_maxS, cluster = "group", type = "HC1"))
##Especially das mit S_max ist super nice-> Die Differenz Smean∗=42S^*_{mean} = 42Smean∗​=42 vs. Smax∗=73S^*_{max} = 73 Smax∗​=73 hat eine saubere ökonomische Interpretation: CP verliert seine Wirksamkeit bei einem *durchschnittlichen* Containment von 42, aber toleriert *Spitzenwerte* bis 73 — weil intermittierende Lockerungsphasen innerhalb eines Quartals der Wirtschaft erlauben, die von CP geschützten Strukturen zu nutzen.
##Stärkere Wirkung und löst den Aggregatiosnkonflikt etwas

##Andere FE Struktur
# Half-year FE statt Quarter FE
pdataY$half_year <- ifelse(
  as.numeric(gsub("Q([0-9]).*", "\\1", pdataY$Quarter)) <= 2,
  paste0("H1.", gsub(".*\\.", "", pdataY$Quarter)),
  paste0("H2.", gsub(".*\\.", "", pdataY$Quarter))
)

m_y_halfyr <- plm(
  y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP + F_DI_lag2 + half_year,
  data = pdataY, model = "within", effect = "individual"
)
coeftest(m_y_halfyr, vcov = vcovHC(m_y_halfyr, cluster = "group", type = "HC1"))
##Zu grob

##Nicht so sinnvoll

# Above vs Below
m_y_split <- plm(
  y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP_above +
    S_mean_tw * F_CP_below_adj_mid + F_DI_lag2,
  data = pdataY, model = "within", effect = "twoways"
)
coeftest(m_y_split, vcov = vcovHC(m_y_split, cluster = "group", type = "HC1"))
##evtl berichten aber die Wirkung ist schon gemeinsam


##Vax als KOntrolle
m_y_vax <- plm(
  y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP + F_DI_lag2 + vax_rate,
  data = pdataY, model = "within", effect = "twoways"
)
coeftest(m_y_vax, vcov = vcovHC(m_y_vax, cluster = "group", type = "HC1"))
##Kein Problem: Schon gesehen



##DI-CP Komplementarität

m_y_compl <- plm(
  y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP + F_DI_lag2 * F_CP,
  data = pdataY, model = "within", effect = "twoways"
)
coeftest(m_y_compl, vcov = vcovHC(m_y_compl, cluster = "group", type = "HC1"))
##WICHTIGES ARGUMENT: Das ist exakt die Guerrieri et al. (2022)-Logik: Nachfragestimulus braucht eine intakte Angebotsseite. DI wirkt nur wenn CP die Produktionsstrukturen erhalten hat — Transfers an Haushalte können nur ausgegeben werden wenn Geschäfte offen und Firmen operationsfähig sind. Ohne CP versickert DI in Ersparnissen.

##Lagged Cp
m_y_cplag <- plm(
  y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP + F_CP_lag1 + F_DI_lag2,
  data = pdataY, model = "within", effect = "twoways"
)
coeftest(m_y_cplag, vcov = vcovHC(m_y_cplag, cluster = "group", type = "HC1"))
##Lag macht keinen Sinn-> Timing bestätigt

#Maybe noch die Bewegungsdaten hinzufügen-> würde Kompositionseffekt lösen-> S_max und auch mobility-> nimm nur S_max und diskutiere!! FE nehmen weg usw. eher unterschätzt wenn dann-> zeig ePlot mit Mobility


#===============================================================================
#############################Debt#########################################
#===============================================================================



# HC3
coeftest(m_d_split, vcov = vcovHC(m_d_split, cluster = "group", type = "HC3"))

# Bootstrap
countries_d <- unique(pdataD$Country)
country_df_d <- data.frame(Country = countries_d)

boot_debt <- function(country_data, indices) {
  boot_countries <- country_data$Country[indices]
  boot_data <- do.call(rbind, lapply(seq_along(boot_countries), function(i) {
    d <- pdataD[pdataD$Country == boot_countries[i], ]
    d$Country <- paste0(d$Country, "_", i)
    d
  }))
  tryCatch({
    m <- feols(
      debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_mid + F_DI_lag1 +
        as.numeric(Quarter) | Country,
      data = boot_data
    )
    coef(m)
  }, error = function(e) rep(NA, 5))
}

set.seed(42)
boot_d <- boot(data = country_df_d, statistic = boot_debt, R = 999)

coef_names_d <- names(boot_d$t0)
cat("=== Debt Bootstrap SE ===\n")
for (i in seq_along(coef_names_d)) {
  boot_se <- sd(boot_d$t[, i], na.rm = TRUE)
  boot_p  <- 2 * min(mean(boot_d$t[, i] >= 0, na.rm = TRUE),
                     mean(boot_d$t[, i] <= 0, na.rm = TRUE))
  cat(sprintf("  %-25s  Coef: %8.4f  Boot-SE: %8.4f  p: %.4f\n",
              coef_names_d[i], boot_d$t0[i], boot_se, boot_p))
}

print(coef_names_d)

##evtl. zum laufen bringen


# Ohne Irland
m_d_noIRL <- plm(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_mid + F_DI_lag1 +
    as.numeric(Quarter),
  data = pdataD %>% filter(Country != "IRL"),
  model = "within", effect = "individual"
)
coeftest(m_d_noIRL, vcov = vcovHC(m_d_noIRL, cluster = "group", type = "HC1"))

# Ohne Türkei
m_d_noTUR <- plm(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_mid + F_DI_lag1 +
    as.numeric(Quarter),
  data = pdataD %>% filter(Country != "TUR"),
  model = "within", effect = "individual"
)
coeftest(m_d_noTUR, vcov = vcovHC(m_d_noTUR, cluster = "group", type = "HC1"))

# Ohne Estland (Debt Gap Ausreißer +95 pp)
m_d_noEST <- plm(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_mid + F_DI_lag1 +
    as.numeric(Quarter),
  data = pdataD %>% filter(Country != "EST"),
  model = "within", effect = "individual"
)
coeftest(m_d_noEST, vcov = vcovHC(m_d_noEST, cluster = "group", type = "HC1"))

##gleiches Muster mit IRL und DI

##Sample Horizont
# Strikt Pandemie (Q1.2020–Q4.2021)
m_d_pan <- plm(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_mid + F_DI_lag1 +
    as.numeric(Quarter),
  data = pdataD %>% filter(Quarter <= "Q4.2021"),
  model = "within", effect = "individual"
)
coeftest(m_d_pan, vcov = vcovHC(m_d_pan, cluster = "group", type = "HC1"))

# Breiter (Q1.2020–Q4.2022, ohne Trend)
m_d_notrend <- plm(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_mid + F_DI_lag1,
  data = pdataD, model = "within", effect = "individual"
)
coeftest(m_d_notrend, vcov = vcovHC(m_d_notrend, cluster = "group", type = "HC1"))


##Alternative Debt Messung

# Nominal + Inflationsrate

# Gepoolt CP (Hauptspezifikation ohne Split)
m_d_poolcp <- plm(
  debt_dR ~ y_t_pct + F_CP + F_DI_lag1 + as.numeric(Quarter),
  data = pdataD, model = "within", effect = "individual"
)
coeftest(m_d_poolcp, vcov = vcovHC(m_d_poolcp, cluster = "group", type = "HC1"))


##Alternativer 
# Two-Way FE (erwartet: tötet alles)
m_d_twfe <- plm(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_mid + F_DI_lag1,
  data = pdataD, model = "within", effect = "twoways"
)
coeftest(m_d_twfe, vcov = vcovHC(m_d_twfe, cluster = "group", type = "HC1"))

# Pooled OLS (Benchmark ohne FE)
m_d_pool2 <- plm(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_mid + F_DI_lag1 +
    as.numeric(Quarter),
  data = pdataD, model = "pooling"
)
coeftest(m_d_pool2, vcov = vcovHC(m_d_pool2, cluster = "group", type = "HC1"))


##take up sensititivität

# Konservativ (25%)
m_d_lo <- plm(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_lo + F_DI_lag1 +
    as.numeric(Quarter),
  data = pdataD, model = "within", effect = "individual"
)
coeftest(m_d_lo, vcov = vcovHC(m_d_lo, cluster = "group", type = "HC1"))

# Liberal (50%)
m_d_hi <- plm(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_hi + F_DI_lag1 +
    as.numeric(Quarter),
  data = pdataD, model = "within", effect = "individual"
)
coeftest(m_d_hi, vcov = vcovHC(m_d_hi, cluster = "group", type = "HC1"))


##LAG DI
# Zeitgenössisch
m_d_di0 <- plm(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_mid + F_DI +
    as.numeric(Quarter),
  data = pdataD, model = "within", effect = "individual"
)
coeftest(m_d_di0, vcov = vcovHC(m_d_di0, cluster = "group", type = "HC1"))

# Lag 2
m_d_di2 <- plm(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_mid + F_DI_lag2 +
    as.numeric(Quarter),
  data = pdataD, model = "within", effect = "individual"
)
coeftest(m_d_di2, vcov = vcovHC(m_d_di2, cluster = "group", type = "HC1"))

##Nettoeffekt anstatt Bruttokosten

m_d_netto <- plm(
  debt_dR ~ F_CP_above + F_CP_below_adj_mid + F_DI_lag1 +
    as.numeric(Quarter),
  data = pdataD, model = "within", effect = "individual"
)
coeftest(m_d_netto, vcov = vcovHC(m_d_netto, cluster = "group", type = "HC1"))

#Fünftens: Brutto vs. Netto bestätigt Selbstfinanzierung. Ohne yky_k yk​ steigen die CP-Koeffizienten massiv (above: 0.52→0.880.52 \to 0.880.52→0.88; below: 0.47→0.790.47 \to 0.79


##was mit dem??             
#################################################################################

##Strategie 3-> Kummulative Cross Section
cross_section <- pdataD %>%
  filter(year_only %in% c(2020, 2021)) %>%
  group_by(Country) %>%
  summarise(
    total_debt_change = last(DebtR_share2019) - first(DebtR_share2019),
    total_CP          = sum(F_CP, na.rm = TRUE),
    total_CP_above    = sum(F_CP_above, na.rm = TRUE),
    total_CP_below    = sum(F_CP_below_adj_mid, na.rm = TRUE),
    total_DI          = sum(F_DI, na.rm = TRUE),
    total_FH          = sum(F_H, na.rm = TRUE),
    mean_S            = mean(S_mean_tw, na.rm = TRUE),
    mean_y            = mean(y_t_pct, na.rm = TRUE),
    mean_theta        = mean(theta_pct, na.rm = TRUE),
    gdp_pc_2019       = first(rGDP_pc_2019),
    debt_2019         = first(debt_2019)
  )

# Querschnittsregression
m_cross <- lm(
  total_debt_change ~  total_CP_above + total_CP_below + total_DI + mean_S + mean_theta,
  data = cross_section)

coeftest(m_cross)


###########################################
##Write dataset for matlab iLQR

# ================================================================
# Export für MATLAB: Länderspezifische Kontrafaktuale
# ================================================================

export_data <- pdataD %>%
  select(
    Country, Quarter,
    # Gesundheitsseite (fixiert)
    S_mean_tw, theta_pct,
    # Fiskalpolitik (beobachtet, zum Vergleich)
    F_CP, F_CP_above, F_CP_below_adj_mid, F_DI, F_H,
    # Outcomes (Validierung)
    y_t_pct, debt_dR,
    # Zusatzinfo
    vax_rate
  ) %>%
  arrange(Country, Quarter) %>%
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


















# ================================================================
#  SUB-COMPONENT ANALYSIS: CP, DI, AND H DECOMPOSITION
#  Motivation: The pooled CP/DI/H coefficients mask heterogeneous
#  transmission mechanisms across instrument types. This section
#  decomposes each channel into economically meaningful sub-categories
#  and estimates separate effects.
# ================================================================

# ==============================================================================
#  BLOCK A: CP SUB-COMPONENT CONSTRUCTION
#  Split below-the-line CP into loans (Code 40+41) vs guarantees (Code 43)
#  Rationale: Loans are actual disbursements (create debt), guarantees are
#  contingent liabilities (preserve output via announcement, low fiscal cost)
# ==============================================================================

fiscal_panel_3way <- fm1 %>%
  filter(broad_fiscal != 0) %>%
  mutate(
    Quarter_fmt = paste0("Q", Quarter, ".", Year),
    CP_above = ifelse(transmission_channel == "CP" & category == 1, broad_fiscal_gdp, 0),
    CP_loans = ifelse(transmission_channel == "CP" & category == 2 &
                        PolicyCode %in% c("40", "41"), broad_fiscal_gdp, 0),
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
  rename(Quarter = Quarter_fmt)

# Merge into output panel
pdataY <- pdataY %>%
  left_join(fiscal_panel_3way, by = c("Country", "Quarter")) %>%
  mutate(
    F_CP_above_3  = replace_na(F_CP_above_3, 0),
    F_CP_loans    = replace_na(F_CP_loans, 0),
    F_CP_guar     = replace_na(F_CP_guar, 0),
    F_CP_guar_adj = F_CP_guar * 0.35,
    F_CP_below_new = F_CP_loans + F_CP_guar * 0.35
  )

# Merge into debt panel
pdataD <- pdataD %>%
  left_join(fiscal_panel_3way, by = c("Country", "Quarter")) %>%
  mutate(
    F_CP_above_3  = replace_na(F_CP_above_3, 0),
    F_CP_loans    = replace_na(F_CP_loans, 0),
    F_CP_guar     = replace_na(F_CP_guar, 0),
    F_CP_guar_adj = F_CP_guar * 0.35,
    F_CP_below_new = F_CP_loans + F_CP_guar * 0.35
  )

cat("\n=== CP Sub-Component Means (pdataY, pp GDP) ===\n")
cat(sprintf("  F_CP_above (grants/subsidies): %.4f\n", mean(pdataY$F_CP_above_3, na.rm = TRUE)))
cat(sprintf("  F_CP_loans (Code 40+41):       %.4f\n", mean(pdataY$F_CP_loans, na.rm = TRUE)))
cat(sprintf("  F_CP_guar  (Code 43, full):    %.4f\n", mean(pdataY$F_CP_guar, na.rm = TRUE)))
cat(sprintf("  F_CP_guar  (Code 43, adj 35%%): %.4f\n", mean(pdataY$F_CP_guar_adj, na.rm = TRUE)))

# ==============================================================================
#  BLOCK B: DI SUB-COMPONENT CONSTRUCTION
#  DI_transfers: Codes 35,36,37,38 (direct cash, unemployment, benefits, ad hoc)
#  DI_demand:    Codes 27,28,29 (infrastructure, green investment, tourism)
#  DI_tax:       Codes 17-22,25,26 (individual tax relief, consumption tax cuts)
# ==============================================================================

df_fiscal_di <- fm1 %>%
  filter(broad_fiscal == 1, transmission_channel == "DI") %>%
  mutate(
    Quarter = as.character(paste0("Q", Quarter, ".", Year)),
    DI_sub = case_when(
      PolicyCode %in% c("35","36","37","38") ~ "transfers",
      PolicyCode %in% c("27","28","29")      ~ "demand",
      PolicyCode %in% c("17","18","19","20","21","22","25","26") ~ "tax",
      TRUE ~ "other_di"
    )
  ) %>%
  filter(Quarter %in% pandemic_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(
    F_DI_transfers = sum(broad_fiscal_gdp[DI_sub == "transfers"], na.rm = TRUE) * 100,
    F_DI_demand    = sum(broad_fiscal_gdp[DI_sub == "demand"],    na.rm = TRUE) * 100,
    F_DI_tax       = sum(broad_fiscal_gdp[DI_sub == "tax"],       na.rm = TRUE) * 100,
    .groups = "drop"
  )

pdataY <- pdataY %>%
  left_join(df_fiscal_di, by = c("Country", "Quarter")) %>%
  mutate(
    F_DI_transfers = replace_na(F_DI_transfers, 0),
    F_DI_demand    = replace_na(F_DI_demand, 0),
    F_DI_tax       = replace_na(F_DI_tax, 0),
    F_DI_transfers_lag2 = lag(F_DI_transfers, 2),
    F_DI_demand_lag2    = lag(F_DI_demand, 2),
    F_DI_tax_lag2       = lag(F_DI_tax, 2)
  )

cat("\n=== DI Sub-Component Means (pdataY, pp GDP) ===\n")
cat(sprintf("  DI_transfers (73%%): %.4f\n", mean(pdataY$F_DI_transfers, na.rm = TRUE)))
cat(sprintf("  DI_demand    (19%%): %.4f\n", mean(pdataY$F_DI_demand, na.rm = TRUE)))
cat(sprintf("  DI_tax       ( 8%%): %.4f\n", mean(pdataY$F_DI_tax, na.rm = TRUE)))

# ==============================================================================
#  BLOCK C: H SUB-COMPONENT CONSTRUCTION
#  H_supply: Codes 30,31,32 (medical items procurement)
#  H_infra:  Codes 33,34,general (health infrastructure, workforce)
# ==============================================================================

df_fiscal_h <- fm1 %>%
  filter(broad_fiscal == 1, transmission_channel == "H") %>%
  mutate(
    Quarter = as.character(paste0("Q", Quarter, ".", Year)),
    H_sub = case_when(
      PolicyCode %in% c("30","31","32")      ~ "supply",
      PolicyCode %in% c("33","34","general") ~ "infra",
      TRUE ~ "other_h"
    )
  ) %>%
  filter(Quarter %in% pandemic_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(
    F_H_supply = sum(broad_fiscal_gdp[H_sub == "supply"], na.rm = TRUE) * 100,
    F_H_infra  = sum(broad_fiscal_gdp[H_sub == "infra"],  na.rm = TRUE) * 100,
    .groups = "drop"
  )

pdataY <- pdataY %>%
  left_join(df_fiscal_h, by = c("Country", "Quarter")) %>%
  mutate(
    F_H_supply = replace_na(F_H_supply, 0),
    F_H_infra  = replace_na(F_H_infra, 0)
  )

cat("\n=== H Sub-Component Means (pdataY, pp GDP) ===\n")
cat(sprintf("  H_supply (38%%): %.4f\n", mean(pdataY$F_H_supply, na.rm = TRUE)))
cat(sprintf("  H_infra  (62%%): %.4f\n", mean(pdataY$F_H_infra, na.rm = TRUE)))


# ================================================================
#  OUTPUT GAP: CP INSTRUMENT DECOMPOSITION
#  Key finding: Guarantees drive the S*CP interaction (output
#  preservation via announcement), loans have no output effect.
# ================================================================

# Three-way: Above + Loans(full) + Guarantees(adj 0.35), each interacted with S
m_y_cp3way <- plm(
  y_t_pct ~ S_mean_tw * y_lag1 +
    S_mean_tw * F_CP_above_3 + S_mean_tw * F_CP_loans +
    S_mean_tw * F_CP_guar_adj + F_DI_lag2,
  data = pdataY, model = "within", effect = "twoways"
)
ct_y_cp3way <- coeftest(m_y_cp3way, vcov = vcovHC(m_y_cp3way, cluster = "group", type = "HC1"))
cat("\n=== OUTPUT: CP Three-Way Split (Above + Loans + Guar*0.35, each * S) ===\n")
print(ct_y_cp3way)
# Interpretation: Guarantees are the active ingredient of CP for output preservation.
# F_CP_guar_adj is significant at the 1% level both in level (0.98*) and interaction
# with S (-0.025**). This is the "whatever it takes" channel: announced guarantees
# prevent insolvency cascades and preserve firm-worker matches during lockdowns.
# Loans show no output effect — they are disbursed too late and create repayment
# obligations that dampen their multiplier.
# Above-the-line CP (grants/subsidies) is individually insignificant when separated,
# suggesting its effect is captured by the guarantee-stringency interaction.

# Levels only (no S interaction on sub-components) — confirms guarantee channel
m_y_cp_levels <- plm(
  y_t_pct ~ S_mean_tw * y_lag1 +
    F_CP_above_3 + F_CP_loans + F_CP_guar_adj + F_DI_lag2,
  data = pdataY, model = "within", effect = "twoways"
)
ct_y_cp_levels <- coeftest(m_y_cp_levels, vcov = vcovHC(m_y_cp_levels, cluster = "group", type = "HC1"))
cat("\n=== OUTPUT: CP Three-Way (levels only, no S*CP interaction) ===\n")
print(ct_y_cp_levels)
# Without the S interaction, loans become significant (0.23**) but guarantees are
# insignificant. This confirms that the guarantee effect operates *through* the
# interaction with stringency — guarantees only matter when lockdowns are active,
# which is exactly the capacity preservation mechanism in the model.


# ================================================================
#  OUTPUT GAP: DI INSTRUMENT DECOMPOSITION
#  Key finding: Only direct transfers (Codes 35-38) have a
#  significant output effect, and only at lag 2.
# ================================================================

# Three-way DI split (lag 2)
m_y_di3way <- plm(
  y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP +
    F_DI_transfers_lag2 + F_DI_demand_lag2 + F_DI_tax_lag2,
  data = pdataY, model = "within", effect = "twoways"
)
ct_y_di3way <- coeftest(m_y_di3way, vcov = vcovHC(m_y_di3way, cluster = "group", type = "HC1"))
cat("\n=== OUTPUT: DI Three-Way Split (lag 2) ===\n")
print(ct_y_di3way)
# Interpretation: Direct cash transfers to households (Codes 35-38) are the only DI
# sub-component with a significant output effect (0.29*, lag 2). Infrastructure
# spending (Codes 27-29) and individual tax relief (Codes 17-22, 25-26) show zero
# effect. Infrastructure projects have implementation lags beyond the pandemic window;
# individual tax cuts are too small (8% of DI volume) to register.
# This sharpens the narrative: the DI multiplier channel operates exclusively through
# direct transfers, not through demand stimulus or tax relief.


# ================================================================
#  OUTPUT GAP: HEALTH EXPENDITURE DECOMPOSITION
#  Key finding: Pooled H has no output effect, but H_supply is
#  significantly negative (reverse causality: harder-hit countries
#  spent more on medical procurement). Confirms H as endogenous
#  pandemic cost, not a policy instrument.
# ================================================================

# Baseline + pooled H (confirming no direct effect)
m_y_h <- plm(
  y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP + F_DI_lag2 + F_H,
  data = pdataY, model = "within", effect = "twoways"
)
ct_y_h <- coeftest(m_y_h, vcov = vcovHC(m_y_h, cluster = "group", type = "HC1"))
cat("\n=== OUTPUT: Baseline + F_H (pooled) ===\n")
print(ct_y_h)
# F_H is insignificant (-0.04, p=0.88): health spending has no net output effect.

# H split: supply vs infrastructure
m_y_hsplit <- plm(
  y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP + F_DI_lag2 +
    F_H_supply + F_H_infra,
  data = pdataY, model = "within", effect = "twoways"
)
ct_y_hsplit <- coeftest(m_y_hsplit, vcov = vcovHC(m_y_hsplit, cluster = "group", type = "HC1"))
cat("\n=== OUTPUT: H Split — Supply vs Infrastructure ===\n")
print(ct_y_hsplit)
# H_supply is significantly negative (-0.63**): countries that spent more on medical
# procurement (ventilators, PPE, vaccines) had worse output gaps. This is NOT causal —
# it reflects reverse causality: countries hit harder by COVID needed more medical
# supplies. This confirms H as an endogenous cost driven by theta_k (the
# epidemiological state), exactly as the model posits (c_H * theta_k).
# H_infra is positive but insignificant (+0.47): health infrastructure investment
# may have a modest positive externality (construction activity), but not identifiable.

# H with S interaction
m_y_hS <- plm(
  y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP + F_DI_lag2 +
    S_mean_tw * F_H,
  data = pdataY, model = "within", effect = "twoways"
)
ct_y_hS <- coeftest(m_y_hS, vcov = vcovHC(m_y_hS, cluster = "group", type = "HC1"))
cat("\n=== OUTPUT: Baseline + S*F_H (interaction) ===\n")
print(ct_y_hS)
# S*F_H is marginally significant (-0.025.): when interacted with stringency, health
# spending shows a CP-like pattern — but this captures the correlation between high
# stringency and high health spending during severe waves, not a causal channel.


# ================================================================
#  DEBT: CP INSTRUMENT DECOMPOSITION
#  Key finding: Loans drive the debt coefficient (actual
#  disbursements), guarantees have no debt effect (most never drawn).
#  This is the mirror image of the output result.
# ================================================================

# Three-way: Above + Loans(full) + Guarantees(adj 0.35)
m_d_cp3way <- plm(
  debt_dR ~ y_t_pct + F_CP_above_3 + F_CP_loans + F_CP_guar_adj + F_DI_lag1 +
    as.numeric(Quarter),
  data = pdataD, model = "within", effect = "individual"
)
ct_d_cp3way <- coeftest(m_d_cp3way, vcov = vcovHC(m_d_cp3way, cluster = "group", type = "HC1"))
cat("\n=== DEBT: CP Three-Way Split (Above + Loans + Guar*0.35) ===\n")
print(ct_d_cp3way)
# Interpretation: Loans (Code 40+41) are highly significant (0.34***) — actual
# disbursements that directly increase government debt. Guarantees (Code 43) are
# insignificant (0.19, p=0.48) — consistent with the "whatever it takes" logic:
# guarantees preserve capacity via announcement but most are never called.
# This is the mirror image of the output result: guarantees preserve output but
# don't create debt; loans create debt but don't preserve output.

# Three-way: all at full scale (for comparison)
m_d_cp3way_full <- plm(
  debt_dR ~ y_t_pct + F_CP_above_3 + F_CP_loans + F_CP_guar + F_DI_lag1 +
    as.numeric(Quarter),
  data = pdataD, model = "within", effect = "individual"
)
ct_d_cp3way_full <- coeftest(m_d_cp3way_full, vcov = vcovHC(m_d_cp3way_full, cluster = "group", type = "HC1"))
cat("\n=== DEBT: CP Three-Way Split (all full scale) ===\n")
print(ct_d_cp3way_full)
# Even at full scale, guarantees remain insignificant (0.065) while loans stay
# significant (0.34***). The adjustment factor is irrelevant for guarantees.

# Loans full + Guarantees*0.35 as combined below-the-line
m_d_blw_new <- plm(
  debt_dR ~ y_t_pct + F_CP_above_3 + F_CP_below_new + F_DI_lag1 +
    as.numeric(Quarter),
  data = pdataD, model = "within", effect = "individual"
)
ct_d_blw_new <- coeftest(m_d_blw_new, vcov = vcovHC(m_d_blw_new, cluster = "group", type = "HC1"))
cat("\n=== DEBT: Above + Below(Loans full + Guar*0.35) ===\n")
print(ct_d_blw_new)


# ================================================================
#  DEBT: COMBINED CP WITH GUARANTEE TAKE-UP ADJUSTMENT
#  F_CP_eff = F_CP_above + F_CP_loans + F_CP_guar * take_up_rate
#  Key finding: kappa_CP nearly doubles when adjusted for effective
#  deployment (0.19 pooled -> 0.33 at 35% take-up), but total debt
#  effect is stable (~0.28-0.34 pp/quarter) because kappa and volume
#  move inversely.
# ================================================================

# Build effective CP for multiple take-up scenarios
take_ups <- c(0.00, 0.10, 0.25, 0.35, 0.50, 1.00)
tu_labels <- c("guar excl.", "guar*0.10", "guar*0.25", "guar*0.35", "guar*0.50", "guar full")

for (tu in take_ups) {
  tag <- gsub("\\.", "", sprintf("%.2f", tu))
  pdataD[[paste0("F_CP_eff_", tag)]] <-
    pdataD$F_CP_above_3 + pdataD$F_CP_loans + pdataD$F_CP_guar * tu
}

cat("\n=== Effective CP Means by Guarantee Take-Up Scenario ===\n")
for (j in seq_along(take_ups)) {
  tag <- gsub("\\.", "", sprintf("%.2f", take_ups[j]))
  cat(sprintf("  %-12s: %.4f pp GDP\n", tu_labels[j],
              mean(pdataD[[paste0("F_CP_eff_", tag)]], na.rm = TRUE)))
}

# Run debt model for each scenario
cat("\n=== DEBT: kappa_CP across guarantee take-up scenarios ===\n")
cat(sprintf("  %-14s  %9s  %8s  %7s  %8s  %9s  %10s\n",
            "Scenario", "kappa_CP", "SE", "t-val", "p-val", "mean_CP", "debt_eff"))
cat("  ", strrep("-", 75), "\n")

# Pooled (original)
m_pool_ref <- plm(debt_dR ~ y_t_pct + F_CP + F_DI_lag1 + as.numeric(Quarter),
                  data = pdataD, model = "within", effect = "individual")
ct_pool_ref <- coeftest(m_pool_ref, vcov = vcovHC(m_pool_ref, cluster = "group", type = "HC1"))
idx_p <- which(rownames(ct_pool_ref) == "F_CP")
pv_p <- ct_pool_ref[idx_p, 4]
stars_fn <- function(pv) ifelse(pv < 0.001, "***", ifelse(pv < 0.01, "**", ifelse(pv < 0.05, "*", ifelse(pv < 0.1, ".", ""))))
total_p <- ct_pool_ref[idx_p, 1] * mean(pdataD$F_CP, na.rm = TRUE)
cat(sprintf("  %-14s  %8.4f%s  %8.4f  %7.3f  %8.4f  %9.4f  %10.4f\n",
            "Pooled (orig.)",
            ct_pool_ref[idx_p, 1], stars_fn(pv_p), ct_pool_ref[idx_p, 2],
            ct_pool_ref[idx_p, 3], pv_p,
            mean(pdataD$F_CP, na.rm = TRUE), total_p))

tu_results <- list()
for (j in seq_along(take_ups)) {
  tag <- gsub("\\.", "", sprintf("%.2f", take_ups[j]))
  vname <- paste0("F_CP_eff_", tag)
  fml <- as.formula(paste0("debt_dR ~ y_t_pct + ", vname, " + F_DI_lag1 + as.numeric(Quarter)"))
  m <- plm(fml, data = pdataD, model = "within", effect = "individual")
  ct <- coeftest(m, vcov = vcovHC(m, cluster = "group", type = "HC1"))
  idx <- which(rownames(ct) == vname)
  cp_mean <- mean(pdataD[[vname]], na.rm = TRUE)
  total <- ct[idx, 1] * cp_mean
  pv <- ct[idx, 4]
  tu_results[[j]] <- list(ct = ct, model = m, vname = vname)
  cat(sprintf("  %-14s  %8.4f%s  %8.4f  %7.3f  %8.4f  %9.4f  %10.4f\n",
              tu_labels[j], ct[idx, 1], stars_fn(pv), ct[idx, 2],
              ct[idx, 3], pv, cp_mean, total))
}
cat("  debt_eff = kappa_CP * mean(F_CP_eff) = avg quarterly debt increase (pp 2019 GDP)\n")

# Central scenario interpretation
r35 <- tu_results[[4]]
idx35 <- which(rownames(r35$ct) == r35$vname)
kappa35 <- r35$ct[idx35, 1]
mean35 <- mean(pdataD[[r35$vname]], na.rm = TRUE)
cat(sprintf("\n  Central scenario (IMF 35%% take-up):\n"))
cat(sprintf("    kappa_CP_eff       = %.4f\n", kappa35))
cat(sprintf("    Mean F_CP_eff      = %.4f pp GDP per quarter\n", mean35))
cat(sprintf("    Quarterly debt     = %.4f pp of 2019 GDP\n", kappa35 * mean35))
cat(sprintf("    Over 8 quarters    = %.2f pp of 2019 GDP\n", kappa35 * mean35 * 8))
cat(sprintf("    vs. Pooled: %.2f pp (ratio adjusted/pooled = %.2f)\n",
            total_p * 8, (kappa35 * mean35) / total_p))

# Interpretation summary:
# 1. kappa_CP nearly doubles when adjusted for effective deployment (0.19 -> 0.33).
#    The pooled estimate is attenuated because guarantees inflate the denominator
#    without proportionally increasing debt.
# 2. The total debt effect is remarkably stable across scenarios (0.28-0.34 pp/quarter)
#    because kappa and mean_CP move inversely: discounting guarantees raises the
#    coefficient but lowers the volume. The product converges.
# 3. Over 8 pandemic quarters: ~2.7 pp of 2019 GDP (adjusted) vs ~2.2 pp (pooled),
#    about 22% higher. The naive estimate underestimates CP's true fiscal cost.
# 4. Significance improves with adjustment (t-stat peaks at 3.58*** when guarantees
#    excluded, vs 2.87** pooled): removing measurement noise from undrawn guarantees
#    sharpens the estimate.
# 5. Combined with the output results: the same below-the-line CP instrument has
#    opposite signatures in output vs debt — guarantees preserve output (announcement
#    effect, S*guar significant) but don't create debt (most never drawn); loans
#    create debt (actual disbursement, kappa_loans=0.34***) but don't preserve
#    output (too slow, repayment burden).


# ================================================================
#  LOCAL PROJECTIONS (Jordà 2005) — Dynamic Fiscal Transmission
#  Panel LP: y_{i,t+h} - y_{i,t-1} = alpha_ih + beta_h * F_it + X + FE
#
#  Design:
#  - Uses wider panel (Q1.2019 – Q4.2022) for LP leads
#  - Pre-pandemic (2019): baseline with F = 0, anchors counterfactual
#  - Post-pandemic (2022): provides response horizons for late shocks
#  - Horizons: h = 0,...,5
#  - SEs: Country-clustered (robust to LP-induced serial correlation)
# ================================================================

# --- LP panel: need wider time range for leads ---
# pdata already spans Q1.2019 – Q4.2022 (16 quarters)
# Create leads of dependent variables

H_max <- 5

lp_panel <- pdata %>%
  arrange(Country, year_only, quarter_only) %>%
  group_by(Country) %>%
  mutate(
    # Cumulative output change: y_{t+h} - y_{t-1}
    dy_h0 = y_t_pct - dplyr::lag(y_t_pct, 1),
    dy_h1 = dplyr::lead(y_t_pct, 1) - dplyr::lag(y_t_pct, 1),
    dy_h2 = dplyr::lead(y_t_pct, 2) - dplyr::lag(y_t_pct, 1),
    dy_h3 = dplyr::lead(y_t_pct, 3) - dplyr::lag(y_t_pct, 1),
    dy_h4 = dplyr::lead(y_t_pct, 4) - dplyr::lag(y_t_pct, 1),
    dy_h5 = dplyr::lead(y_t_pct, 5) - dplyr::lag(y_t_pct, 1),
    # Cumulative debt change: debt_{t+h} - debt_{t-1}
    dd_h0 = DebtR_share2019 - dplyr::lag(DebtR_share2019, 1),
    dd_h1 = dplyr::lead(DebtR_share2019, 1) - dplyr::lag(DebtR_share2019, 1),
    dd_h2 = dplyr::lead(DebtR_share2019, 2) - dplyr::lag(DebtR_share2019, 1),
    dd_h3 = dplyr::lead(DebtR_share2019, 3) - dplyr::lag(DebtR_share2019, 1),
    dd_h4 = dplyr::lead(DebtR_share2019, 4) - dplyr::lag(DebtR_share2019, 1),
    dd_h5 = dplyr::lead(DebtR_share2019, 5) - dplyr::lag(DebtR_share2019, 1)
  ) %>%
  ungroup()

# Merge in CP sub-components and DI sub-components (already built above)
# F_CP_above_3, F_CP_loans, F_CP_guar, F_CP_guar_adj already in pdataY/pdataD
# Need them in lp_panel too
lp_panel <- lp_panel %>%
  left_join(fiscal_panel_3way, by = c("Country", "Quarter")) %>%
  mutate(
    F_CP_above_3  = replace_na(F_CP_above_3, 0),
    F_CP_loans    = replace_na(F_CP_loans, 0),
    F_CP_guar     = replace_na(F_CP_guar, 0),
    F_CP_guar_adj = F_CP_guar * 0.35,
    S_x_FCP       = S_mean_tw * F_CP
  )

# Also merge DI sub-components
lp_panel <- lp_panel %>%
  left_join(df_fiscal_di, by = c("Country", "Quarter")) %>%
  mutate(
    F_DI_transfers = replace_na(F_DI_transfers, 0),
    F_DI_demand    = replace_na(F_DI_demand, 0)
  )

cat(sprintf("\nLP panel: %d obs, %d countries\n", nrow(lp_panel), n_distinct(lp_panel$Country)))

# --- LP estimation function using fixest ---
run_lp <- function(data, dep_vars, shock_var, controls, fe = "Country + Quarter",
                   cluster = "Country", H = H_max) {
  results <- tibble()
  for (h in 0:H) {
    dv <- dep_vars[h + 1]
    if (!dv %in% names(data)) next
    fml_str <- paste0(dv, " ~ ", shock_var)
    if (length(controls) > 0 && controls[1] != "") {
      fml_str <- paste0(fml_str, " + ", paste(controls, collapse = " + "))
    }
    fml_str <- paste0(fml_str, " | ", fe)
    fml <- as.formula(fml_str)
    m <- tryCatch(feols(fml, data = data, cluster = cluster), error = function(e) NULL)
    if (is.null(m)) next
    ct <- summary(m)$coeftable
    shock_names <- strsplit(shock_var, " \\+ ")[[1]]
    for (sn in shock_names) {
      sn_clean <- trimws(sn)
      if (sn_clean %in% rownames(ct)) {
        results <- bind_rows(results, tibble(
          h = h, variable = sn_clean,
          coef = ct[sn_clean, "Estimate"], se = ct[sn_clean, "Std. Error"],
          tval = ct[sn_clean, "t value"], pval = ct[sn_clean, "Pr(>|t|)"],
          ci90_lo = ct[sn_clean, "Estimate"] - 1.645 * ct[sn_clean, "Std. Error"],
          ci90_hi = ct[sn_clean, "Estimate"] + 1.645 * ct[sn_clean, "Std. Error"],
          ci95_lo = ct[sn_clean, "Estimate"] - 1.96  * ct[sn_clean, "Std. Error"],
          ci95_hi = ct[sn_clean, "Estimate"] + 1.96  * ct[sn_clean, "Std. Error"],
          nobs = m$nobs
        ))
      }
    }
  }
  return(results)
}

# --- LP printer ---
print_lp <- function(res, title) {
  cat(sprintf("\n%s\n%s\n", title, strrep("=", nchar(title))))
  for (v in unique(res$variable)) {
    cat(sprintf("\n  %s:\n", v))
    sub <- res %>% filter(variable == v)
    cat(sprintf("  %3s  %9s  %8s  %7s  %8s  %5s\n", "h","coef","SE","t-val","p-val","N"))
    for (i in 1:nrow(sub)) {
      stars <- ifelse(sub$pval[i]<0.001,"***",ifelse(sub$pval[i]<0.01,"** ",
               ifelse(sub$pval[i]<0.05,"*  ",ifelse(sub$pval[i]<0.1,".  ","   "))))
      cat(sprintf("  %3d  %8.4f%s %8.4f  %7.3f  %8.4f  %5d\n",
                  sub$h[i], sub$coef[i], stars, sub$se[i], sub$tval[i],
                  sub$pval[i], sub$nobs[i]))
    }
  }
}

# --- IRF plot function ---
plot_irf <- function(res, varname, title, ylab, color = "steelblue") {
  sub <- res %>% filter(variable == varname)
  if (nrow(sub) == 0) return(ggplot() + ggtitle(paste("No data for", varname)))
  anchor <- tibble(h = -1, coef = 0, ci90_lo = 0, ci90_hi = 0, ci95_lo = 0, ci95_hi = 0)
  sub <- bind_rows(anchor, sub)
  ggplot(sub, aes(x = h, y = coef)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_ribbon(aes(ymin = ci95_lo, ymax = ci95_hi), alpha = 0.15, fill = color) +
    geom_ribbon(aes(ymin = ci90_lo, ymax = ci90_hi), alpha = 0.25, fill = color) +
    geom_line(linewidth = 1.1, color = color) +
    geom_point(size = 2.5, color = color) +
    scale_x_continuous(breaks = -1:H_max) +
    labs(title = title, x = "Quarters after shock", y = ylab) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 11))
}

print(plot_irf)
dy_vars <- paste0("dy_h", 0:H_max)
dd_vars <- paste0("dd_h", 0:H_max)


# ================================================================
#  LP 1: OUTPUT — Joint CP + DI
# ================================================================
cat("\n=== LP 1: OUTPUT — Joint CP + DI ===\n")

res_y_joint <- run_lp(lp_panel, dy_vars, "F_CP + F_DI",
                      controls = c("S_mean_tw", "y_lag1", "theta_pct"),
                      fe = "Country + Quarter")
print_lp(res_y_joint, "OUTPUT: Joint LP (CP + DI)")


# ================================================================
#  LP 2: OUTPUT — State-Dependent (S * F_CP)
#  Key result: S*F_CP interaction significant at h=0, confirming
#  CP's output effect operates through the stringency channel
# ================================================================
cat("\n=== LP 2: OUTPUT — State-Dependent (S * F_CP) ===\n")

res_y_state <- run_lp(lp_panel, dy_vars, "F_CP + S_x_FCP + F_DI",
                      controls = c("S_mean_tw", "y_lag1", "theta_pct"),
                      fe = "Country + Quarter")
print_lp(res_y_state, "OUTPUT: State-Dependent LP (F_CP + S*F_CP + F_DI)")
# Interpretation: The S*F_CP interaction is significant at h=0 (-0.006*),
# confirming that CP's output-preserving effect is contemporaneous and
# conditional on lockdown intensity. The level effect of F_CP is positive
# and significant at h=0 (0.24**). This validates the static panel finding
# dynamically: CP works immediately during lockdowns, not with a lag.


# ================================================================
#  LP 3: OUTPUT — CP Sub-Components
# ================================================================
cat("\n=== LP 3: OUTPUT — CP Sub-Components ===\n")

res_y_cp_sub <- run_lp(lp_panel, dy_vars,
                       "F_CP_above_3 + F_CP_loans + F_CP_guar_adj",
                       controls = c("F_DI", "S_mean_tw", "y_lag1", "theta_pct"),
                       fe = "Country + Quarter")
print_lp(res_y_cp_sub, "OUTPUT: CP Sub-Components (Above / Loans / Guar)")
# Interpretation: Loans show immediate, transitory output effect (0.25** at h=0).
# Above-the-line builds slowly, significant at h=4-5 (grants have delayed effect).
# Guarantees are insignificant throughout — their effect operates through the
# S*F_CP interaction (announcement channel), not as a direct LP shock.


# ================================================================
#  LP 4: OUTPUT — DI Sub-Components
# ================================================================
cat("\n=== LP 4: OUTPUT — DI Sub-Components ===\n")

res_y_di_sub <- run_lp(lp_panel, dy_vars,
                       "F_DI_transfers + F_DI_demand",
                       controls = c("F_CP", "S_mean_tw", "y_lag1", "theta_pct"),
                       fe = "Country + Quarter")
print_lp(res_y_di_sub, "OUTPUT: DI Sub-Components (Transfers / Demand)")


# ================================================================
#  LP 5: DEBT — Joint CP + DI
#  Key result: CP debt is monotonically increasing and highly
#  significant at every horizon. No mean reversion.
# ================================================================
cat("\n=== LP 5: DEBT — Joint CP + DI ===\n")

res_d_joint <- run_lp(lp_panel, dd_vars, "F_CP + F_DI",
                      controls = c("y_t_pct", "S_mean_tw"),
                      fe = "Country")
print_lp(res_d_joint, "DEBT: Joint LP (CP + DI)")
# Interpretation: Each 1 pp GDP of CP creates 0.16** debt at h=0, rising
# monotonically to 0.63*** at h=5. No mean reversion — the debt effect of
# CP is permanent. DI debt builds more slowly (significant from h=3).


# ================================================================
#  LP 6: DEBT — CP Sub-Components
# ================================================================
cat("\n=== LP 6: DEBT — CP Sub-Components ===\n")

res_d_cp_sub <- run_lp(lp_panel, dd_vars,
                       "F_CP_above_3 + F_CP_loans + F_CP_guar_adj",
                       controls = c("F_DI", "y_t_pct", "S_mean_tw"),
                       fe = "Country")
print_lp(res_d_cp_sub, "DEBT: CP Sub-Components (Above / Loans / Guar)")
# Interpretation: Above-the-line is the costliest (0.41* at h=0, 1.53*** at h=5).
# Loans are significant at every horizon (0.35*** to 0.68**).
# Guarantees create debt only with delay (significant from h=3), consistent
# with gradual calling of contingent liabilities.


# ================================================================
#  LP 7: DEBT — DI Sub-Components
# ================================================================
cat("\n=== LP 7: DEBT — DI Sub-Components ===\n")

res_d_di_sub <- run_lp(lp_panel, dd_vars,
                       "F_DI_transfers + F_DI_demand",
                       controls = c("F_CP", "y_t_pct", "S_mean_tw"),
                       fe = "Country")
print_lp(res_d_di_sub, "DEBT: DI Sub-Components (Transfers / Demand)")


# ================================================================
#  LP 8: CUMULATIVE MULTIPLIERS
# ================================================================
cat("\n=== LP 8: CUMULATIVE FISCAL MULTIPLIERS ===\n\n")
cat("  Note: LP dep. var. is y_{t+h} - y_{t-1} (cumulative by construction).\n")
cat("  beta_h directly gives the cumulative multiplier.\n\n")

cat("  --- Output Multipliers ---\n")
cat(sprintf("  %3s  %14s  %14s\n", "h", "CP multiplier", "DI multiplier"))
cat("  ", strrep("-", 40), "\n")
for (hh in 0:H_max) {
  cp_r <- res_y_joint %>% filter(variable == "F_CP", .data$h == hh)
  di_r <- res_y_joint %>% filter(variable == "F_DI", .data$h == hh)
  cp_s <- if(nrow(cp_r)>0) sprintf("%8.4f%s", cp_r$coef, ifelse(cp_r$pval<0.05,"*",ifelse(cp_r$pval<0.1,"."," "))) else "     ---"
  di_s <- if(nrow(di_r)>0) sprintf("%8.4f%s", di_r$coef, ifelse(di_r$pval<0.05,"*",ifelse(di_r$pval<0.1,"."," "))) else "     ---"
  cat(sprintf("  %3d  %14s  %14s\n", hh, cp_s, di_s))
}

cat("\n  --- Debt Multipliers ---\n")
cat(sprintf("  %3s  %14s  %14s\n", "h", "CP -> debt", "DI -> debt"))
cat("  ", strrep("-", 40), "\n")
for (hh in 0:H_max) {
  cp_r <- res_d_joint %>% filter(variable == "F_CP", .data$h == hh)
  di_r <- res_d_joint %>% filter(variable == "F_DI", .data$h == hh)
  cp_s <- if(nrow(cp_r)>0) sprintf("%8.4f%s", cp_r$coef, ifelse(cp_r$pval<0.05,"*",ifelse(cp_r$pval<0.1,"."," "))) else "     ---"
  di_s <- if(nrow(di_r)>0) sprintf("%8.4f%s", di_r$coef, ifelse(di_r$pval<0.05,"*",ifelse(di_r$pval<0.1,"."," "))) else "     ---"
  cat(sprintf("  %3d  %14s  %14s\n", hh, cp_s, di_s))
}

# Efficiency ratios
cat("\n  --- CP Efficiency: Output per Unit Debt ---\n")
for (hh in 0:H_max) {
  y_cp <- res_y_joint %>% filter(variable == "F_CP", .data$h == hh)
  d_cp <- res_d_joint %>% filter(variable == "F_CP", .data$h == hh)
  if (nrow(y_cp) > 0 && nrow(d_cp) > 0 && d_cp$coef != 0) {
    cat(sprintf("    h=%d: %.3f pp output / %.3f pp debt = %.2f ratio\n",
                hh, y_cp$coef, d_cp$coef, y_cp$coef / d_cp$coef))
  }
}

cat("\n  --- DI Efficiency: Output per Unit Debt ---\n")
for (hh in 0:H_max) {
  y_di <- res_y_joint %>% filter(variable == "F_DI", .data$h == hh)
  d_di <- res_d_joint %>% filter(variable == "F_DI", .data$h == hh)
  if (nrow(y_di) > 0 && nrow(d_di) > 0 && d_di$coef != 0) {
    cat(sprintf("    h=%d: %.3f pp output / %.3f pp debt = %.2f ratio\n",
                hh, y_di$coef, d_di$coef, y_di$coef / d_di$coef))
  }
}


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
#  LP 9: IRF PLOTS
# ================================================================
cat("\n=== LP 9: GENERATING IRF PLOTS ===\n")

# Output: CP vs DI
p_y_cp <- plot_irf(res_y_joint, "F_CP", "CP -> Output Gap", "pp output", "darkgreen")
p_y_di <- plot_irf(res_y_joint, "F_DI", "DI -> Output Gap", "pp output", "firebrick")
p_output_lp <- (p_y_cp | p_y_di) + plot_annotation(
  title = "Local Projections: Fiscal Policy -> Output Gap",
  subtitle = "Cumulative response per 1 pp GDP fiscal shock")
ggsave(file.path(safeplots, "lp_output_cp_di.pdf"), p_output_lp, width = 12, height = 5)

# Output: CP sub-components
p_y_above <- plot_irf(res_y_cp_sub, "F_CP_above_3", "Above-the-line", "pp output", "darkgreen")
p_y_loans <- plot_irf(res_y_cp_sub, "F_CP_loans", "Loans", "pp output", "steelblue")
p_y_guar  <- plot_irf(res_y_cp_sub, "F_CP_guar_adj", "Guarantees (adj)", "pp output", "darkorange")
p_cp_sub_lp <- (p_y_above | p_y_loans | p_y_guar) + plot_annotation(
  title = "LP: CP Sub-Components -> Output Gap")
ggsave(file.path(safeplots, "lp_output_cp_sub.pdf"), p_cp_sub_lp, width = 15, height = 5)

# Output: State-dependent
p_sfcp <- plot_irf(res_y_state, "S_x_FCP", "S * F_CP interaction", "pp output", "darkorange")
p_fcp  <- plot_irf(res_y_state, "F_CP", "F_CP (level)", "pp output", "darkgreen")
p_fdi  <- plot_irf(res_y_state, "F_DI", "F_DI", "pp output", "firebrick")
p_state_lp <- (p_fcp | p_sfcp | p_fdi) + plot_annotation(
  title = "State-Dependent LP: CP conditional on Stringency")
ggsave(file.path(safeplots, "lp_output_state_dep.pdf"), p_state_lp, width = 15, height = 5)

# Debt: CP vs DI
p_d_cp <- plot_irf(res_d_joint, "F_CP", "CP -> Debt", "pp debt/2019GDP", "darkgreen")
p_d_di <- plot_irf(res_d_joint, "F_DI", "DI -> Debt", "pp debt/2019GDP", "firebrick")
p_debt_lp <- (p_d_cp | p_d_di) + plot_annotation(
  title = "Local Projections: Fiscal Policy -> Government Debt",
  subtitle = "Cumulative response per 1 pp GDP fiscal shock")
ggsave(file.path(safeplots, "lp_debt_cp_di.pdf"), p_debt_lp, width = 12, height = 5)

# Debt: CP sub-components
p_d_above <- plot_irf(res_d_cp_sub, "F_CP_above_3", "Above-the-line", "pp debt", "darkgreen")
p_d_loans <- plot_irf(res_d_cp_sub, "F_CP_loans", "Loans", "pp debt", "steelblue")
p_d_guar  <- plot_irf(res_d_cp_sub, "F_CP_guar_adj", "Guarantees (adj)", "pp debt", "darkorange")
p_debt_sub_lp <- (p_d_above | p_d_loans | p_d_guar) + plot_annotation(
  title = "LP: CP Sub-Components -> Government Debt")
ggsave(file.path(safeplots, "lp_debt_cp_sub.pdf"), p_debt_sub_lp, width = 15, height = 5)

# Combined 2x2: Main result
p_main_lp <- (p_y_cp | p_y_di) / (p_d_cp | p_d_di) + plot_annotation(
  title = "Local Projections: Fiscal Transmission Channels",
  subtitle = "Top: Output | Bottom: Debt | Per 1 pp GDP shock")
ggsave(file.path(safeplots, "lp_main_2x2.pdf"), p_main_lp, width = 13, height = 10)

cat("  LP plots saved to:", safeplots, "\n")

# --- Country-FE-only comparison plots ---
cat("  Generating Country-FE-only comparison plots...\n")

# Output: TWFE vs Country-FE, CP
p_y_cp_tw  <- plot_irf(res_y_joint, "F_CP", "CP -> Output (TWFE)", "pp output", "darkgreen")
p_y_cp_cfe <- plot_irf(res_y_joint_cfe, "F_CP", "CP -> Output (Country FE)", "pp output", "darkgreen")
p_y_di_tw  <- plot_irf(res_y_joint, "F_DI", "DI -> Output (TWFE)", "pp output", "firebrick")
p_y_di_cfe <- plot_irf(res_y_joint_cfe, "F_DI", "DI -> Output (Country FE)", "pp output", "firebrick")

p_fe_compare <- (p_y_cp_tw | p_y_cp_cfe) / (p_y_di_tw | p_y_di_cfe) +
  plot_annotation(
    title = "LP Robustness: TWFE vs Country-FE-Only",
    subtitle = "Left: TWFE (conservative) | Right: Country FE only (preserves temporal variation)")
ggsave(file.path(safeplots, "lp_fe_comparison.pdf"), p_fe_compare, width = 13, height = 10)

# State-dependent comparison
p_sfcp_tw  <- plot_irf(res_y_state, "S_x_FCP", "S*F_CP (TWFE)", "pp output", "darkorange")
p_sfcp_cfe <- plot_irf(res_y_state_cfe, "S_x_FCP", "S*F_CP (Country FE)", "pp output", "darkorange")
p_fcp_tw   <- plot_irf(res_y_state, "F_CP", "F_CP level (TWFE)", "pp output", "darkgreen")
p_fcp_cfe  <- plot_irf(res_y_state_cfe, "F_CP", "F_CP level (Country FE)", "pp output", "darkgreen")

p_state_compare <- (p_fcp_tw | p_fcp_cfe) / (p_sfcp_tw | p_sfcp_cfe) +
  plot_annotation(
    title = "State-Dependent LP: TWFE vs Country-FE-Only",
    subtitle = "Top: F_CP level | Bottom: S*F_CP interaction")
ggsave(file.path(safeplots, "lp_state_dep_fe_comparison.pdf"), p_state_compare, width = 13, height = 10)

cat("  FE comparison plots saved.\n")


# ================================================================
#  LP — INTERPRETATION AND CONCLUSIONS
# ================================================================
#
# The Local Projections provide dynamic validation of the static panel
# results and reveal the full transmission profile of fiscal instruments.
#
# --- OUTPUT GAP ---
#
# 1. POOLED CP AND DI: The joint LP (LP 1) shows no significant cumulative
#    output effects for either CP or DI at conventional horizons. This is
#    expected: TWFE absorbs the time-series variation that LP requires.
#    The identifying variation for CP's output effect lies in the interaction
#    with stringency, not in the level.
#
# 2. STATE-DEPENDENT LP (LP 2 — the key output result):
#    - F_CP level: 0.24** at h=0, then insignificant — CP's direct output
#      effect is immediate and fully contemporaneous.
#    - S * F_CP: -0.006* at h=0, -0.004. at h=2 — the stringency interaction
#      is significant, confirming that CP preserves output DURING lockdowns.
#      The effect decays as stringency relaxes in later quarters.
#    - F_DI: insignificant at all horizons in the state-dependent specification.
#    - This validates the static panel dynamically: CP operates through the
#      S*CP channel, is contemporaneous, and does not require a lag.
#
# 3. CP SUB-COMPONENTS (LP 3):
#    - Loans (Code 40+41): Immediate effect (0.25** at h=0, 0.25* at h=1),
#      then fades. Loans inject liquidity that sustains operations on impact.
#    - Above-the-line (grants/subsidies): Slow build, significant at h=4
#      (0.31.) and h=5 (0.22.). Grants take time to disburse and transmit.
#    - Guarantees: Insignificant at all horizons as a direct shock. This is
#      consistent with the static panel: the guarantee channel operates via
#      the announcement effect (S*F_CP interaction), not as a dose-response.
#      Firms respond to the existence of the guarantee framework, not to the
#      volume deployed in any given quarter.
#
# 4. DI SUB-COMPONENTS (LP 4):
#    - Transfers: Peak effect at h=2 (0.21, p=0.24) — directionally consistent
#      with the lag-2 finding in the static panel but not statistically
#      significant in the LP. Precision is limited by TWFE absorption.
#    - Demand stimulus: Large point estimates but high variance — too few
#      infrastructure measures with meaningful volume for LP identification.
#
# --- GOVERNMENT DEBT ---
#
# 5. JOINT DEBT LP (LP 5 — the strongest LP result):
#    - CP -> Debt: Monotonically increasing and highly significant at EVERY
#      horizon: 0.16** (h=0) -> 0.36*** (h=1) -> 0.48*** (h=2) -> 0.52***
#      (h=3) -> 0.57*** (h=4) -> 0.63*** (h=5).
#      NO MEAN REVERSION. Each 1 pp GDP of CP creates permanent, accumulating
#      debt. This is the fiscal cost of capacity preservation.
#    - DI -> Debt: Builds more slowly (insignificant until h=2-3, then 0.81*
#      at h=3, 1.00* at h=5). DI is ultimately more expensive per unit than
#      CP but takes longer to materialize in the debt stock. The delay reflects
#      the lag between DI authorization and budgetary recording.
#
# 6. CP SUB-COMPONENTS -> DEBT (LP 6):
#    - Above-the-line: The costliest instrument. 0.41* at h=0, rising to
#      1.53*** at h=5. Grants and wage subsidies (Kurzarbeit) create large,
#      persistent fiscal obligations. These are the workhorses of CP.
#    - Loans (Code 40+41): Significant at every horizon (0.35*** to 0.68**).
#      Actual disbursements that directly increase government liabilities.
#    - Guarantees: Insignificant until h=3 (0.60*), reaching 1.00** at h=5.
#      This is the contingent liability channel: guarantees are initially
#      off-balance-sheet, but a fraction gets called as borrowers default,
#      creating a DELAYED debt effect. The 3-quarter lag to significance is
#      consistent with typical guarantee call horizons.
#
# 7. DI SUB-COMPONENTS -> DEBT (LP 7):
#    - Transfers: Builds slowly (0.96. at h=3, 1.01* at h=5). Cash transfers
#      are expensive but their debt impact is spread over time.
#    - Demand stimulus: Large but imprecise — too noisy for LP identification.
#
# --- CUMULATIVE MULTIPLIERS AND EFFICIENCY ---
#
# 8. The output-per-debt efficiency ratio (LP 8) is low for both CP and DI
#    in the unconditional LP. This reflects two facts:
#    (a) Output effects are absorbed by TWFE, depressing the numerator.
#    (b) Debt effects are highly significant, inflating the denominator.
#    The correct interpretation is that CP's output effect operates through
#    a state-dependent channel (S*CP) that the unconditional multiplier
#    misses. The static panel is better suited for output identification;
#    the LP is better suited for tracing debt dynamics.
#
# --- OVERALL CONCLUSION ---
#
# The LP analysis adds three results to the static panel:
#
# (a) TIMING: CP acts immediately (h=0), DI with a 2-quarter lag. This
#     confirms the static lag structure is not an artifact of specification.
#
# (b) PERSISTENCE: CP debt accumulates monotonically with no mean reversion
#     over 5 quarters. The fiscal cost of capacity preservation is permanent.
#     Guarantees create debt only with a 3-quarter delay (contingent calling).
#
# (c) ASYMMETRY: The same below-the-line instrument has opposite dynamic
#     signatures — guarantees: no output shock response but delayed debt
#     (contingent); loans: immediate output response but immediate debt
#     (actual disbursement). This decomposition is only visible in the LP,
#     not in the static panel which pools them.
#
# These findings support the paper's trilemma narrative: governments faced
# a tradeoff where capacity preservation was effective but costly, demand
# injection was delayed but ultimately more expensive per unit, and the
# instrument choice (grants vs loans vs guarantees) shaped both the output
# recovery path and the debt accumulation trajectory.
#


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
