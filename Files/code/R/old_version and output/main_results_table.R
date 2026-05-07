suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(lubridate)
  library(readxl); library(fixest); library(modelsummary); library(data.table)
})

conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflicts_prefer(dplyr::lag)
conflicted::conflicts_prefer(dplyr::first)
conflicted::conflicts_prefer(dplyr::coalesce)
conflicted::conflicts_prefer(dplyr::recode)
conflicted::conflicts_prefer(lubridate::quarter)
conflicted::conflicts_prefer(lubridate::year)
conflicted::conflicts_prefer(dplyr::last)
conflicted::conflicts_prefer(data.table::isoweek)
conflicted::conflicts_prefer(data.table::month)
conflicted::conflicts_prefer(dplyr::lead)
conflicted::conflicts_prefer(lubridate::union)
conflicted::conflicts_prefer(modelsummary::SD)

set.seed(1234)
options(scipen = 999)

# --- Paths ---
safedata  <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/processed"
safetable <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/output/tables"

load(file.path(safedata, "dataforanalysis.RData"))

fm1 <- read_excel("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/raw/fiscal measures/fiscal_classified_v1_7.xlsx")
fm1 <- fm1 %>%
  mutate(YQ = paste0("Q", Quarter, ".", Year),
         YQ_ord = factor(YQ, levels = sort(unique(paste0("Q", Quarter, ".", Year))), ordered = TRUE)) %>%
  filter(!PolicyCode %in% c(5, 6, 11, 12, 15, 16))

pandemic_qs <- c("Q1.2019","Q2.2019","Q3.2019","Q4.2019",
                 "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                 "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
                 "Q1.2022","Q2.2022","Q3.2022","Q4.2022")

# --- Data build (same as analysis.R) ---
df_qdata <- qdata %>% filter(Quarter %in% pandemic_qs) %>%
  select(Country, Quarter, y_t_pct, p_proj_all_ages, p_avg_all_ages, Qpopulation_th)
pop_2019 <- df_qdata[df_qdata$Quarter=="Q4.2019", c("Country","Qpopulation_th")]
names(pop_2019)[2] <- "pop_2019"
df_qdata <- merge(df_qdata, pop_2019, by="Country")

df_theta <- theta_quarterly_full %>%
  mutate(Quarter = as.character(YQ)) %>%
  filter(Quarter %in% pandemic_qs) %>%
  select(Country, Quarter, theta_mean, S_mean, S_max)

df_fiscal <- fm1 %>% filter(broad_fiscal == 1) %>%
  mutate(Quarter = as.character(YQ_ord)) %>%
  filter(Quarter %in% pandemic_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(F_CP = sum(broad_fiscal_gdp[transmission_channel == "CP"], na.rm=TRUE),
            F_DI = sum(broad_fiscal_gdp[transmission_channel == "DI"], na.rm=TRUE),
            .groups="drop")

df_stringency <- panel_w %>% filter(!is.na(S_mean)) %>%
  mutate(year = year(date), quarter = quarter(date),
         Quarter = paste0("Q", quarter, ".", year)) %>%
  filter(Quarter %in% pandemic_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(S_mean_pw = mean(S_mean, na.rm=TRUE), .groups="drop")

df <- df_qdata %>%
  left_join(df_theta, by=c("Country","Quarter")) %>%
  left_join(df_fiscal, by=c("Country","Quarter")) %>%
  left_join(df_stringency, by=c("Country","Quarter")) %>%
  mutate(Quarter = factor(Quarter, levels = pandemic_qs, ordered = TRUE),
         F_CP = replace_na(F_CP, 0), F_DI = replace_na(F_DI, 0),
         S_mean_pw = replace_na(S_mean_pw, 0),
         p_proj_all_ages = replace_na(p_proj_all_ages, 0),
         theta_mean = replace_na(theta_mean, 0)) %>%
  arrange(Country, Quarter)

# Scale
pdata <- df %>%
  filter(Quarter %in% c("Q4.2019","Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                         "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
                         "Q1.2022","Q2.2022")) %>%
  mutate(S_mean_tw = S_mean_pw * 100,
         F_CP = F_CP * 100, F_DI = F_DI * 100,
         theta_pct = theta_mean * 100)

# Lags
pdata <- pdata %>%
  group_by(Country) %>%
  arrange(Quarter) %>%
  mutate(y_lag1 = lag(y_t_pct, 1),
         F_DI_lag2 = lag(F_DI, 2)) %>%
  ungroup()

# t_idx and impute zeros for pre-pandemic NAs
pdata$t_idx <- match(as.character(pdata$Quarter), pandemic_qs)
pdata <- pdata %>%
  mutate(
    S_mean_tw       = replace_na(S_mean_tw, 0),
    F_CP            = replace_na(F_CP, 0),
    F_DI_lag2       = replace_na(F_DI_lag2, 0),
    p_proj_all_ages = replace_na(p_proj_all_ages, 0),
    y_lag1          = replace_na(y_lag1, 0)
  )

pdataY <- pdata

# Main sample: Q1.2020-Q2.2022 (t_idx 5-14)
main_sub <- ~ t_idx >= 5 & t_idx <= 14

cat(sprintf("Sample: %d obs in main period\n\n",
            nrow(pdataY %>% filter(t_idx >= 5 & t_idx <= 14))))

# =============================================================================
# 5 MODELS: OLS → Country FE → TWFE → TWFE + psi → Full specification
# =============================================================================

# (1) Pooled OLS — no fixed effects
m1_ols <- feols(
  y_t_pct ~ S_mean_tw + y_lag1 + F_CP + F_DI_lag2 + p_proj_all_ages,
  data = pdataY, subset = main_sub, cluster = ~Country)

# (2) Country FE only — absorbs cross-country heterogeneity
m2_cfe <- feols(
  y_t_pct ~ S_mean_tw + y_lag1 + F_CP + F_DI_lag2 + p_proj_all_ages | Country,
  data = pdataY, subset = main_sub, cluster = ~Country)

# (3) TWFE additive — country + quarter FE, no interactions
m3_twfe <- feols(
  y_t_pct ~ S_mean_tw + y_lag1 + F_CP + F_DI_lag2 + p_proj_all_ages | Country + Quarter,
  data = pdataY, subset = main_sub, cluster = ~Country)

# (4) TWFE + lockdown hysteresis (psi) — adds S × y_lag1
m4_psi <- feols(
  y_t_pct ~ S_mean_tw * y_lag1 + F_CP + F_DI_lag2 + p_proj_all_ages | Country + Quarter,
  data = pdataY, subset = main_sub, cluster = ~Country)

# (5) Full specification — adds S × F_CP and F_CP × y_lag1
m5_full <- feols(
  y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP + F_CP * y_lag1 +
    F_DI_lag2 + p_proj_all_ages | Country + Quarter,
  data = pdataY, subset = main_sub, cluster = ~Country)

# =============================================================================
# TABLE: Main Results
# =============================================================================

model_list <- list(
  "(1) OLS"         = m1_ols,
  "(2) Country FE"  = m2_cfe,
  "(3) TWFE"        = m3_twfe,
  "(4) + Hysteresis" = m4_psi,
  "(5) Full"        = m5_full
)

coef_map <- c(
  "S_mean_tw"          = "$S_{ik}$",
  "y_lag1"             = "$y_{i,k-1}$",
  "F_CP"               = "$F^{CP}_{ik}$",
  "F_DI_lag2"          = "$F^{DI}_{i,k-2}$",
  "p_proj_all_ages"    = "$d_{ik}$ (fear)",
  "S_mean_tw:y_lag1"   = "$S_{ik} \\times y_{i,k-1}$ [$\\psi$]",
  "S_mean_tw:F_CP"     = "$S_{ik} \\times F^{CP}_{ik}$ [$\\tilde{\\eta}$]",
  "y_lag1:F_CP"        = "$F^{CP}_{ik} \\times y_{i,k-1}$ [$-\\eta_p$]"
)

gof_map <- tribble(
  ~raw,                ~clean,              ~fmt,
  "nobs",             "Observations",       0,
  "r.squared.within", "Within $R^2$",       3,
  "FE: Country",      "Country FE",         0,
  "FE: Quarter",      "Quarter FE",         0
)

# Print to console
cat("=== Main Results Table ===\n\n")
modelsummary(model_list,
  vcov     = ~Country,
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_map = coef_map,
  gof_map  = gof_map,
  title    = "Main Results: Output Gap Equation")

# Export to LaTeX
modelsummary(model_list,
  vcov     = ~Country,
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_map = coef_map,
  gof_map  = gof_map,
  output   = file.path(safetable, "tab_main_results.tex"),
  title    = "Main Results: Output Gap Equation")

cat(sprintf("\nSaved: %s\n", file.path(safetable, "tab_main_results.tex")))

# Also print individual model summaries for verification
cat("\n\n=== (5) Full Specification Details ===\n")
summary(m5_full, cluster = ~Country)
