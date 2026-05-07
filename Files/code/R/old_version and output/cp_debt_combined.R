library(readxl)
library(dplyr)
library(plm)
library(lmtest)
library(sandwich)
library(tidyr)
library(stringr)
library(lubridate)
library(conflicted)

conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflicts_prefer(dplyr::lag)
conflicted::conflicts_prefer(dplyr::lead)
conflicted::conflicts_prefer(lubridate::quarter)
conflicted::conflicts_prefer(lubridate::year)

safedata <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/processed"
load(file.path(safedata, "dataforanalysis.RData"))
set.seed(1234)

fm_path <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/raw/fiscal measures/fiscal_classified_v1_7.xlsx"

pandemic_qs <- c(
  "Q1.2019","Q2.2019","Q3.2019","Q4.2019",
  "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
  "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
  "Q1.2022","Q2.2022","Q3.2022","Q4.2022"
)

fm1 <- readxl::read_excel(fm_path)
fm1 <- fm1 %>%
  mutate(
    YQ     = paste0("Q", Quarter, ".", Year),
    YQ_ord = factor(YQ, levels = sort(unique(paste0("Q", Quarter, ".", Year))), ordered = TRUE)
  ) %>%
  filter(!PolicyCode %in% c(5, 6, 11, 12, 15, 16))

df_qdata <- qdata %>%
  filter(Quarter %in% pandemic_qs) %>%
  select(Country, Quarter, year_only, quarter_only,
         y_t_pct, DebtR_share2019, Qpopulation_th, inflation_index,
         vax_rate, d_t_pct)

pop_2019 <- df_qdata[df_qdata$Quarter == "Q4.2019", c("Country", "Qpopulation_th")]
names(pop_2019)[2] <- "pop_2019"
df_qdata <- merge(df_qdata, pop_2019, by = "Country")

df_theta <- theta_quarterly_full %>%
  mutate(Quarter = as.character(YQ)) %>%
  filter(Quarter %in% pandemic_qs) %>%
  select(Country, Quarter, theta_mean)

df_fiscal <- fm1 %>%
  filter(broad_fiscal == 1) %>%
  mutate(Quarter = as.character(YQ_ord)) %>%
  filter(Quarter %in% pandemic_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(
    F_CP = sum(broad_fiscal_gdp[transmission_channel == "CP"], na.rm = TRUE),
    F_DI = sum(broad_fiscal_gdp[transmission_channel == "DI"], na.rm = TRUE),
    F_H  = sum(broad_fiscal_gdp[transmission_channel == "H"],  na.rm = TRUE),
    .groups = "drop"
  )

df_stringency <- panel_w %>%
  filter(!is.na(S_mean)) %>%
  mutate(year = year(date), quarter = quarter(date),
         Quarter = paste0("Q", quarter, ".", year)) %>%
  filter(Quarter %in% pandemic_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(S_mean_pw = mean(S_mean, na.rm = TRUE), .groups = "drop")

df <- df_qdata %>%
  left_join(df_theta,      by = c("Country", "Quarter")) %>%
  left_join(df_fiscal,     by = c("Country", "Quarter")) %>%
  left_join(df_stringency, by = c("Country", "Quarter")) %>%
  mutate(
    Quarter = factor(Quarter, levels = pandemic_qs, ordered = TRUE),
    F_CP = replace_na(F_CP, 0),
    F_DI = replace_na(F_DI, 0),
    F_H  = replace_na(F_H, 0)
  ) %>%
  arrange(Country, Quarter)

pdata <- df %>%
  mutate(S_mean_tw = S_mean_pw * 100,
         F_CP = F_CP * 100, F_DI = F_DI * 100, F_H = F_H * 100,
         theta_pct = theta_mean * 100) %>%
  mutate(F_DI_lag1 = lag(F_DI, 1), y_lag1 = lag(y_t_pct, 1))

pdata <- pdata %>%
  group_by(Country) %>%
  mutate(debt_dR = DebtR_share2019 - lag(DebtR_share2019)) %>%
  ungroup()

pdataD <- pdata %>%
  filter(Quarter %in% c("Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                         "Q1.2021","Q2.2021","Q3.2021","Q4.2021","Q1.2022"))
pdataD <- pdataD %>%
  group_by(Country) %>%
  mutate(debt_dR = DebtR_share2019 - lag(DebtR_share2019)) %>%
  ungroup()

# Three-way CP split
fiscal_3way <- fm1 %>%
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
    F_CP_above = sum(CP_above, na.rm = TRUE) * 100,
    F_CP_loans = sum(CP_loans, na.rm = TRUE) * 100,
    F_CP_guar  = sum(CP_guar,  na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  rename(Quarter = Quarter_fmt)

pdataD <- pdataD %>%
  left_join(fiscal_3way, by = c("Country", "Quarter")) %>%
  mutate(
    F_CP_above = replace_na(F_CP_above, 0),
    F_CP_loans = replace_na(F_CP_loans, 0),
    F_CP_guar  = replace_na(F_CP_guar, 0)
  )

# =============================================================================
#  Build effective CP for multiple take-up scenarios
# =============================================================================
cat(strrep("=", 70), "\n")
cat("  COMBINED CP WITH GUARANTEE ADJUSTMENT\n")
cat("  F_CP_eff = F_CP_above + F_CP_loans + F_CP_guar * take_up_rate\n")
cat(strrep("=", 70), "\n\n")

cat("Component means (pp GDP):\n")
cat(sprintf("  F_CP_above:  %.4f\n", mean(pdataD$F_CP_above, na.rm=TRUE)))
cat(sprintf("  F_CP_loans:  %.4f\n", mean(pdataD$F_CP_loans, na.rm=TRUE)))
cat(sprintf("  F_CP_guar:   %.4f\n", mean(pdataD$F_CP_guar, na.rm=TRUE)))
cat(sprintf("  F_CP total:  %.4f\n\n", mean(pdataD$F_CP, na.rm=TRUE)))

take_ups <- c(0.00, 0.10, 0.25, 0.35, 0.50, 1.00)
tu_labels <- c("guar excl.", "guar*0.10", "guar*0.25", "guar*0.35", "guar*0.50", "guar full")

for (tu in take_ups) {
  tag <- gsub("\\.", "", sprintf("%.2f", tu))
  pdataD[[paste0("F_CP_eff_", tag)]] <-
    pdataD$F_CP_above + pdataD$F_CP_loans + pdataD$F_CP_guar * tu
}

cat("Effective CP means by scenario:\n")
for (j in seq_along(take_ups)) {
  tag <- gsub("\\.", "", sprintf("%.2f", take_ups[j]))
  cat(sprintf("  %-12s: %.4f pp GDP\n", tu_labels[j],
              mean(pdataD[[paste0("F_CP_eff_", tag)]], na.rm=TRUE)))
}

# =============================================================================
#  Run debt models for each scenario
# =============================================================================
cat("\n\n")
cat(strrep("=", 70), "\n")
cat("  DEBT MODELS: debt_dR ~ y_t_pct + F_CP_eff + F_DI_lag1 + t\n")
cat(strrep("=", 70), "\n")

# Pooled (original)
m_pool <- plm(debt_dR ~ y_t_pct + F_CP + F_DI_lag1 + as.numeric(Quarter),
              data = pdataD, model = "within", effect = "individual")
ct_pool <- coeftest(m_pool, vcov = vcovHC(m_pool, cluster = "group", type = "HC1"))

cat("\n--- Pooled CP (no adjustment) ---\n")
print(ct_pool)

results <- list()
for (j in seq_along(take_ups)) {
  tag <- gsub("\\.", "", sprintf("%.2f", take_ups[j]))
  vname <- paste0("F_CP_eff_", tag)
  fml <- as.formula(paste0("debt_dR ~ y_t_pct + ", vname, " + F_DI_lag1 + as.numeric(Quarter)"))
  m <- plm(fml, data = pdataD, model = "within", effect = "individual")
  ct <- coeftest(m, vcov = vcovHC(m, cluster = "group", type = "HC1"))
  results[[j]] <- list(label = tu_labels[j], ct = ct, model = m, vname = vname, tu = take_ups[j])
  cat(sprintf("\n--- F_CP_eff: %s ---\n", tu_labels[j]))
  print(ct)
}

# =============================================================================
#  SUMMARY TABLE
# =============================================================================
cat("\n\n")
cat(strrep("=", 70), "\n")
cat("  SUMMARY: kappa_CP ACROSS GUARANTEE TAKE-UP SCENARIOS\n")
cat(strrep("=", 70), "\n\n")

cat(sprintf("  %-14s  %9s  %8s  %7s  %8s  %9s  %10s\n",
            "Scenario", "kappa_CP", "SE", "t-val", "p-val", "mean_CP", "debt_eff"))
cat("  ", strrep("-", 75), "\n")

# Pooled
idx_p <- which(rownames(ct_pool) == "F_CP")
total_p <- ct_pool[idx_p, 1] * mean(pdataD$F_CP, na.rm=TRUE)
pv_p <- ct_pool[idx_p, 4]
stars_p <- ifelse(pv_p<0.001,"***",ifelse(pv_p<0.01,"**",ifelse(pv_p<0.05,"*",ifelse(pv_p<0.1,".",""))))
cat(sprintf("  %-14s  %8.4f%s  %8.4f  %7.3f  %8.4f  %9.4f  %10.4f\n",
            "Pooled (orig.)",
            ct_pool[idx_p, 1], stars_p, ct_pool[idx_p, 2], ct_pool[idx_p, 3],
            ct_pool[idx_p, 4], mean(pdataD$F_CP, na.rm=TRUE), total_p))

for (r in results) {
  idx <- which(rownames(r$ct) == r$vname)
  cp_mean <- mean(pdataD[[r$vname]], na.rm = TRUE)
  total <- r$ct[idx, 1] * cp_mean
  pv <- r$ct[idx, 4]
  stars <- ifelse(pv<0.001,"***",ifelse(pv<0.01,"**",ifelse(pv<0.05,"*",ifelse(pv<0.1,".",""))))
  cat(sprintf("  %-14s  %8.4f%s  %8.4f  %7.3f  %8.4f  %9.4f  %10.4f\n",
              r$label, r$ct[idx, 1], stars, r$ct[idx, 2], r$ct[idx, 3],
              pv, cp_mean, total))
}

cat("\n  debt_eff = kappa_CP * mean(F_CP_eff) = avg quarterly debt increase (pp of 2019 GDP)\n")

# =============================================================================
#  KEY ECONOMIC INTERPRETATION
# =============================================================================
cat("\n\n")
cat(strrep("=", 70), "\n")
cat("  ECONOMIC INTERPRETATION\n")
cat(strrep("=", 70), "\n\n")

# Central scenario (0.35)
r35 <- results[[4]]
idx35 <- which(rownames(r35$ct) == r35$vname)
kappa35 <- r35$ct[idx35, 1]
mean35 <- mean(pdataD[[r35$vname]], na.rm=TRUE)

cat("  Central scenario (guarantees at 35% take-up, IMF estimate):\n")
cat(sprintf("    kappa_CP_eff       = %.4f\n", kappa35))
cat(sprintf("    Mean F_CP_eff      = %.4f pp GDP per quarter\n", mean35))
cat(sprintf("    Quarterly debt     = %.4f pp of 2019 GDP\n", kappa35 * mean35))
cat(sprintf("    Over 8 quarters    = %.4f pp of 2019 GDP (~%.1f pp)\n",
            kappa35 * mean35 * 8, kappa35 * mean35 * 8))

# Compare: pooled vs adjusted
cat(sprintf("\n  Pooled (unadjusted):\n"))
cat(sprintf("    kappa_CP           = %.4f\n", ct_pool[idx_p, 1]))
cat(sprintf("    Mean F_CP          = %.4f pp GDP per quarter\n", mean(pdataD$F_CP, na.rm=TRUE)))
cat(sprintf("    Quarterly debt     = %.4f pp of 2019 GDP\n", total_p))
cat(sprintf("    Over 8 quarters    = %.4f pp (~%.1f pp)\n", total_p * 8, total_p * 8))

cat(sprintf("\n  Difference (adjusted vs pooled):\n"))
cat(sprintf("    Quarterly: %.4f vs %.4f (ratio: %.2f)\n",
            kappa35 * mean35, total_p, (kappa35 * mean35) / total_p))

cat("\n========== DONE ==========\n")
