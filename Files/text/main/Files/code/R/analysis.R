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
options(max.print = 9999, scipen = 999, na.print = "")

dir <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/output r"
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

safedata <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/Database/Analyse"
load(file.path(safedata, "dataforanalysis.RData"))

#Output Location Plots und Table
safeplots <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/output r/Plots"
safetable <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/output r/Table Descriptives"

#Create one Analysis Dataset with main specifications
##Load modified FM Dataset V.15 instead of 1.4
fm1 <- readxl::read_excel("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/Database/fiscal_classified_v1_5.xlsx")

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
