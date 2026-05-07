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

set.seed(1234)
options(max.print = 999, scipen = 999)

# --- Load data ---
safedata <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/processed"
load(file.path(safedata, "dataforanalysis.RData"))

fm1 <- read_excel("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/raw/fiscal measures/fiscal_classified_v1_7.xlsx")
fm1 <- fm1 %>%
  mutate(YQ = paste0("Q", Quarter, ".", Year),
         YQ_ord = factor(YQ, levels = sort(unique(paste0("Q", Quarter, ".", Year))), ordered = TRUE)) %>%
  filter(!PolicyCode %in% c(5, 6, 11, 12, 15, 16))

# Include Q2.2019 onward so all lags are available
all_qs <- c("Q2.2019","Q3.2019","Q4.2019",
            "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
            "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
            "Q1.2022")

# --- Build blocks ---
df_qdata <- qdata %>% filter(Quarter %in% all_qs) %>%
  select(Country, Quarter, y_t_pct, p_proj_all_ages, Qpopulation_th)

df_theta <- theta_quarterly_full %>%
  mutate(Quarter = as.character(YQ)) %>%
  filter(Quarter %in% all_qs) %>%
  select(Country, Quarter, theta_mean, S_mean)

df_fiscal <- fm1 %>% filter(broad_fiscal == 1) %>%
  mutate(Quarter = as.character(YQ_ord)) %>%
  filter(Quarter %in% all_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(F_CP = sum(broad_fiscal_gdp[transmission_channel == "CP"], na.rm=TRUE),
            F_DI = sum(broad_fiscal_gdp[transmission_channel == "DI"], na.rm=TRUE),
            .groups="drop")

df_stringency <- panel_w %>% filter(!is.na(S_mean)) %>%
  mutate(year = year(date), quarter = quarter(date),
         Quarter = paste0("Q", quarter, ".", year)) %>%
  filter(Quarter %in% all_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(S_mean_pw = mean(S_mean, na.rm=TRUE), .groups="drop")

# --- Merge ---
df <- df_qdata %>%
  left_join(df_theta, by=c("Country","Quarter")) %>%
  left_join(df_fiscal, by=c("Country","Quarter")) %>%
  left_join(df_stringency, by=c("Country","Quarter")) %>%
  mutate(
    Quarter = factor(Quarter, levels = all_qs, ordered = TRUE),
    # Fill pre-pandemic NAs with 0 (no policy, no infections, no excess mortality)
    F_CP          = replace_na(F_CP, 0),
    F_DI          = replace_na(F_DI, 0),
    S_mean_pw     = replace_na(S_mean_pw, 0),
    p_proj_all_ages = replace_na(p_proj_all_ages, 0),
    theta_mean    = replace_na(theta_mean, 0)
  ) %>%
  arrange(Country, Quarter)

# --- Scale (same as analysis.R) ---
df <- df %>%
  mutate(S_mean_tw = S_mean_pw * 100,
         F_CP = F_CP * 100,
         F_DI = F_DI * 100,
         theta_pct = theta_mean * 100)

# --- Lags (chronological order guaranteed by factor levels) ---
df <- df %>%
  group_by(Country) %>%
  arrange(Quarter) %>%
  mutate(
    y_lag1   = lag(y_t_pct, 1),
    F_DI_lag2 = lag(F_DI, 2)
  ) %>%
  ungroup()

# --- Estimation sample: Q4.2019 - Q1.2022 ---
est_qs <- c("Q4.2019","Q1.2020","Q2.2020","Q3.2020","Q4.2020",
            "Q1.2021","Q2.2021","Q3.2021","Q4.2021","Q1.2022")

main <- df %>%
  filter(Quarter %in% est_qs) %>%
  mutate(Quarter = factor(Quarter, levels = est_qs))

cat(sprintf("Sample: %d obs, %d countries, %d quarters\n",
            nrow(main), n_distinct(main$Country), n_distinct(main$Quarter)))

# NA check
cat("\n=== NA check ===\n")
main %>%
  group_by(Quarter) %>%
  summarise(n = n(),
            na_S = sum(is.na(S_mean_tw)),
            na_y_lag = sum(is.na(y_lag1)),
            na_FCP = sum(is.na(F_CP)),
            na_FDI2 = sum(is.na(F_DI_lag2)),
            na_fear = sum(is.na(p_proj_all_ages)),
            .groups = "drop") %>%
  print(n = 20)

# --- feols: TWFE with Q4.2019 as reference ---
m <- feols(
  y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP + F_DI_lag2 +
    F_CP * y_lag1 + p_proj_all_ages | Country + Quarter,
  data = main,
  cluster = ~Country
)

cat("\n=== Main specification (feols, TWFE) ===\n\n")
summary(m)

# --- Extract Quarter FE ---
qfe <- fixef(m)$Quarter

cat("\n=== Quarter Fixed Effects (all quarters) ===\n")
cat("  Reference: Q4.2019 (pre-pandemic steady state)\n\n")
cat(sprintf("  %-10s %10s\n", "Quarter", "FE"))
cat("  ", strrep("-", 22), "\n")
for (i in seq_along(qfe)) {
  cat(sprintf("  %-10s %10.4f\n", names(qfe)[i], qfe[i]))
}
cat(sprintf("\n  Number of quarter FEs: %d\n", length(qfe)))
