suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(lubridate)
  library(readxl); library(fixest); library(data.table)
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

safedata <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/processed"
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

# --- Data build ---
df_qdata <- qdata %>% filter(Quarter %in% pandemic_qs) %>%
  select(Country, Quarter, y_t_pct, DebtR_share2019, p_proj_all_ages, Qpopulation_th)

df_theta <- theta_quarterly_full %>% mutate(Quarter = as.character(YQ)) %>%
  filter(Quarter %in% pandemic_qs) %>% select(Country, Quarter, theta_mean, S_mean)

df_fiscal <- fm1 %>% filter(broad_fiscal == 1) %>%
  mutate(Quarter = as.character(YQ_ord)) %>% filter(Quarter %in% pandemic_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(F_CP = sum(broad_fiscal_gdp[transmission_channel == "CP"], na.rm=TRUE),
            F_DI = sum(broad_fiscal_gdp[transmission_channel == "DI"], na.rm=TRUE),
            F_total = sum(broad_fiscal_gdp, na.rm=TRUE),
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
         F_total = replace_na(F_total, 0),
         S_mean_pw = replace_na(S_mean_pw, 0),
         p_proj_all_ages = replace_na(p_proj_all_ages, 0)) %>%
  arrange(Country, Quarter)

# Scale x100
df <- df %>%
  mutate(S_mean_tw = S_mean_pw * 100,
         F_CP = F_CP * 100, F_DI = F_DI * 100, F_total = F_total * 100)

# Lags
df <- df %>%
  group_by(Country) %>% arrange(Quarter) %>%
  mutate(y_lag1 = lag(y_t_pct, 1),
         F_DI_lag2 = lag(F_DI, 2),
         F_total_lag1 = lag(F_total, 1),
         F_total_lag2 = lag(F_total, 2)) %>%
  ungroup()

df$t_idx <- match(as.character(df$Quarter), pandemic_qs)

# Impute NAs
df <- df %>%
  mutate(across(c(S_mean_tw, F_CP, F_DI_lag2, p_proj_all_ages, y_lag1,
                  F_total, F_total_lag1, F_total_lag2), ~ replace_na(.x, 0)))

# --- OUTPUT GAP ---
cat("=" |> strrep(70), "\n")
cat("  OUTPUT GAP: Total Fiscal vs Disaggregated\n")
cat("=" |> strrep(70), "\n\n")

# Main spec (disaggregated)
y_disagg <- feols(
  y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP + F_CP * y_lag1 +
    F_DI_lag2 + p_proj_all_ages | Country + Quarter,
  data = df, subset = ~ t_idx >= 5 & t_idx <= 14, cluster = ~Country)

# Total fiscal (contemporaneous)
y_total_0 <- feols(
  y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_total + F_total * y_lag1 +
    p_proj_all_ages | Country + Quarter,
  data = df, subset = ~ t_idx >= 5 & t_idx <= 14, cluster = ~Country)

# Total fiscal (lag 1)
y_total_1 <- feols(
  y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_total_lag1 + F_total_lag1 * y_lag1 +
    p_proj_all_ages | Country + Quarter,
  data = df, subset = ~ t_idx >= 5 & t_idx <= 14, cluster = ~Country)

# Total fiscal (lag 2)
y_total_2 <- feols(
  y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_total_lag2 + F_total_lag2 * y_lag1 +
    p_proj_all_ages | Country + Quarter,
  data = df, subset = ~ t_idx >= 5 & t_idx <= 14, cluster = ~Country)

# Just total fiscal, no interactions
y_total_add <- feols(
  y_t_pct ~ S_mean_tw + y_lag1 + F_total + p_proj_all_ages | Country + Quarter,
  data = df, subset = ~ t_idx >= 5 & t_idx <= 14, cluster = ~Country)

cat("--- (A) Disaggregated (main spec) ---\n")
print(summary(y_disagg))

cat("\n--- (B) Total Fiscal (contemporaneous, with interactions) ---\n")
print(summary(y_total_0))

cat("\n--- (C) Total Fiscal (lag 1, with interactions) ---\n")
print(summary(y_total_1))

cat("\n--- (D) Total Fiscal (lag 2, with interactions) ---\n")
print(summary(y_total_2))

cat("\n--- (E) Total Fiscal (additive, no interactions) ---\n")
print(summary(y_total_add))

# --- DEBT ---
cat("\n\n", "=" |> strrep(70), "\n")
cat("  DEBT: Total Fiscal vs Disaggregated\n")
cat("=" |> strrep(70), "\n\n")

# Build pdataD with debt FD
pdataD <- df %>%
  filter(Quarter %in% c("Q3.2019","Q4.2019",
                        "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                        "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
                        "Q1.2022","Q2.2022")) %>%
  arrange(Country, t_idx) %>%
  group_by(Country) %>%
  mutate(debt_dR = DebtR_share2019 - lag(DebtR_share2019, 1)) %>%
  ungroup() %>%
  filter(Quarter != "Q3.2019")

debt_sub <- pdataD$t_idx >= 5 & pdataD$t_idx <= 14

# Disaggregated (main spec)
d_disagg <- feols(
  debt_dR ~ y_t_pct + F_CP + F_DI_lag2 | Country,
  data = pdataD, subset = debt_sub, cluster = ~Country)

# Total fiscal (contemporaneous)
d_total_0 <- feols(
  debt_dR ~ y_t_pct + F_total | Country,
  data = pdataD, subset = debt_sub, cluster = ~Country)

# Total fiscal (lag 1)
d_total_1 <- feols(
  debt_dR ~ y_t_pct + F_total_lag1 | Country,
  data = pdataD, subset = debt_sub, cluster = ~Country)

cat("--- (A) Disaggregated (CP + DI) ---\n")
print(summary(d_disagg))

cat("\n--- (B) Total Fiscal (contemporaneous) ---\n")
print(summary(d_total_0))

cat("\n--- (C) Total Fiscal (lag 1) ---\n")
print(summary(d_total_1))
