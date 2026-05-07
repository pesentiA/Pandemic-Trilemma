suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(lubridate)
  library(readxl); library(plm); library(lmtest); library(sandwich)
  library(data.table)
})

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

pandemic_qs <- c("Q1.2019","Q2.2019","Q3.2019","Q4.2019",
                 "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                 "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
                 "Q1.2022","Q2.2022","Q3.2022","Q4.2022")

# Blocks
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
         F_CP = replace_na(F_CP, 0), F_DI = replace_na(F_DI, 0)) %>%
  arrange(Country, Quarter)

# Scale
pdata <- df %>%
  filter(Quarter %in% c("Q4.2019","Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                         "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
                         "Q1.2022")) %>%
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

# Main sample: Q4.2019 - Q1.2022 (include Q4.2019 so it gets a quarter FE)
quarter_order <- pandemic_qs
pdata$t_idx <- match(as.character(pdata$Quarter), quarter_order)
main_sample <- pdata %>% filter(t_idx >= 4 & t_idx <= 13)

cat(sprintf("Main sample: %d obs, %d countries, %d quarters\n",
            nrow(main_sample), n_distinct(main_sample$Country),
            n_distinct(main_sample$Quarter)))
cat(sprintf("Quarters: %s\n\n", paste(sort(unique(as.character(main_sample$Quarter))), collapse=", ")))

# --- Check: how many NAs per quarter in regressors? ---
cat("=== NA check per quarter ===\n")
main_sample %>%
  group_by(Quarter) %>%
  summarise(
    n = n(),
    na_S = sum(is.na(S_mean_tw)),
    na_y_lag = sum(is.na(y_lag1)),
    na_FCP = sum(is.na(F_CP)),
    na_FDI2 = sum(is.na(F_DI_lag2)),
    na_fear = sum(is.na(p_proj_all_ages)),
    .groups = "drop"
  ) %>%
  print(n = 20)

# Drop rows with any NA in regressors (same as plm does internally)
main_complete <- main_sample %>%
  filter(!is.na(S_mean_tw) & !is.na(y_lag1) & !is.na(F_CP) &
         !is.na(F_DI_lag2) & !is.na(p_proj_all_ages))

cat(sprintf("\nComplete cases: %d obs, %d quarters: %s\n\n",
            nrow(main_complete), n_distinct(main_complete$Quarter),
            paste(sort(unique(as.character(main_complete$Quarter))), collapse=", ")))

# --- Run main spec ---
m_twfe <- plm(
  y_t_pct ~ S_mean_tw*y_lag1 + S_mean_tw*F_CP + F_DI_lag2 + F_CP*y_lag1 + p_proj_all_ages,
  data = main_sample, index = c("Country","Quarter"),
  model = "within", effect = "twoways"
)

# --- Extract ALL Quarter FE (level intercepts) ---
qfe <- fixef(m_twfe, effect = "time", type = "level")

cat("=== Quarter Fixed Effects (level, all quarters) ===\n\n")
cat(sprintf("  %-10s %10s\n", "Quarter", "FE"))
cat("  ", strrep("-", 22), "\n")
for (i in seq_along(qfe)) {
  cat(sprintf("  %-10s %10.4f\n", names(qfe)[i], qfe[i]))
}
cat(sprintf("\n  Number of quarter FEs: %d\n", length(qfe)))

# Also show relative to Q4.2019 = 0 (first differences from plm)
qfe_d <- fixef(m_twfe, effect = "time", type = "dmean")
cat("\n=== Quarter FE (deviation from mean) ===\n\n")
cat(sprintf("  %-10s %10s\n", "Quarter", "FE"))
cat("  ", strrep("-", 22), "\n")
for (i in seq_along(qfe_d)) {
  cat(sprintf("  %-10s %10.4f\n", names(qfe_d)[i], qfe_d[i]))
}

# Country FE as well for completeness
cfe <- fixef(m_twfe, effect = "individual", type = "level")
cat("\n=== Country Fixed Effects (level) ===\n\n")
cat(sprintf("  %-6s %10s\n", "Country", "FE"))
cat("  ", strrep("-", 18), "\n")
for (i in seq_along(cfe)) {
  cat(sprintf("  %-6s %10.4f\n", names(cfe)[i], cfe[i]))
}
