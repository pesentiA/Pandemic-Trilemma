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

# --- Load data (replicate analysis.R data prep) ---
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
  select(Country, Quarter, year_only, quarter_only, y_t_pct,
         p_proj_all_ages, p_avg_all_ages, StringencyIndex_PopWeighted, Qpopulation_th)
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

# Scale (same as analysis.R)
pdata <- df %>%
  filter(Quarter %in% c("Q4.2019","Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                         "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
                         "Q1.2022","Q2.2022","Q3.2022","Q4.2022")) %>%
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

# Main sample: include Q4.2019 as reference quarter (t_idx 4-13)
quarter_order <- pandemic_qs
pdata$t_idx <- match(as.character(pdata$Quarter), quarter_order)
main_sample <- pdata %>% filter(t_idx >= 4 & t_idx <= 13)

cat(sprintf("Main sample: %d obs, %d countries, %d quarters\n\n",
            nrow(main_sample), n_distinct(main_sample$Country),
            n_distinct(main_sample$Quarter)))

# =============================================================================
# METHOD 1: Extract quarter FE from plm twoways model via fixef()
# =============================================================================

cat("=== METHOD 1: plm twoways + fixef() ===\n\n")

m_twfe <- plm(
  y_t_pct ~ S_mean_tw*y_lag1 + S_mean_tw*F_CP + F_DI_lag2 + F_CP*y_lag1 + p_proj_all_ages,
  data = main_sample, index = c("Country","Quarter"),
  model = "within", effect = "twoways"
)

cat("--- Structural coefficients ---\n")
ct <- coeftest(m_twfe, vcov = vcovHC(m_twfe, type="HC1", cluster="group"))
print(ct)

# Extract time (quarter) fixed effects
qfe <- fixef(m_twfe, effect = "time")
cat("\n--- Quarter Fixed Effects (time FE from plm) ---\n")
cat("  These represent the common shock delta_k absorbed by the model.\n\n")
cat(sprintf("  %-10s %10s\n", "Quarter", "FE"))
cat("  ", strrep("-", 22), "\n")
for (i in seq_along(qfe)) {
  cat(sprintf("  %-10s %10.4f\n", names(qfe)[i], qfe[i]))
}

# =============================================================================
# METHOD 2: LSDV with explicit quarter dummies (for verification + SEs)
# =============================================================================

cat("\n\n=== METHOD 2: LSDV with explicit country + quarter dummies (lm) ===\n\n")

# Full LSDV via lm() — Q4.2019 as explicit reference
main_sample$Quarter_f <- relevel(factor(as.character(main_sample$Quarter)), ref = "Q4.2019")
main_sample$Country_f <- factor(main_sample$Country)

m_lsdv <- lm(
  y_t_pct ~ S_mean_tw*y_lag1 + S_mean_tw*F_CP + F_DI_lag2 + F_CP*y_lag1 +
    p_proj_all_ages + Country_f + Quarter_f,
  data = main_sample
)

ct_lsdv <- coeftest(m_lsdv, vcov = vcovCL(m_lsdv, cluster = main_sample$Country))

cat("--- Quarter dummy coefficients (relative to Q4.2019) ---\n\n")
q_idx <- grep("Quarter_f", rownames(ct_lsdv))
q_coefs <- ct_lsdv[q_idx, , drop = FALSE]
print(q_coefs)

cat("\n  Reference (omitted) quarter: Q4.2019\n")
cat("  All coefficients are relative to the pre-pandemic steady state.\n")

# =============================================================================
# Summary table for MATLAB: Quarter FE as common shock sequence
# =============================================================================

cat("\n\n=== Quarter FE for MATLAB (common shock epsilon_k) ===\n")
cat("  These values represent the average pandemic shock to output\n")
cat("  in each quarter, after controlling for S, F_CP, F_DI, fear, and lags.\n\n")

# Use fixef (Method 1) — these are the level effects, not relative to a base
qfe_df <- data.frame(
  Quarter = names(qfe),
  FE = as.numeric(qfe)
)
# Normalize: subtract the mean so they represent deviations
qfe_df$FE_centered <- qfe_df$FE - mean(qfe_df$FE)

cat(sprintf("  %-10s %10s %10s\n", "Quarter", "FE (level)", "FE (centered)"))
cat("  ", strrep("-", 32), "\n")
for (i in 1:nrow(qfe_df)) {
  cat(sprintf("  %-10s %10.4f %10.4f\n",
              qfe_df$Quarter[i], qfe_df$FE[i], qfe_df$FE_centered[i]))
}

cat("\n  Mean FE: ", round(mean(qfe_df$FE), 4), "\n")
cat("  The centered FE sum to zero by construction.\n")

cat("\nDone.\n")
