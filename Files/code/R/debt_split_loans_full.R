# =============================================================================
#  Debt Split: Guarantees adjusted (0.35), Loans/Equity at full scale
# =============================================================================

rm(list = ls())

packages_vector <- c("dplyr", "fixest", "plm", "lmtest", "sandwich",
                      "tidyr", "stringr", "readxl", "lubridate",
                      "conflicted", "modelsummary")
lapply(packages_vector, require, character.only = TRUE)

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

# =============================================================================
#  Build panel (same as analysis.R)
# =============================================================================
fm1 <- readxl::read_excel(fm_path)
fm1 <- fm1 %>%
  mutate(
    YQ     = paste0("Q", Quarter, ".", Year),
    YQ_ord = factor(YQ, levels = sort(unique(paste0("Q", Quarter, ".", Year))), ordered = TRUE),
    size_pct = broad_fiscal_gdp * 100
  ) %>%
  filter(!PolicyCode %in% c(5, 6, 11, 12, 15, 16))

df_qdata <- qdata %>%
  filter(Quarter %in% pandemic_qs) %>%
  select(Country, Quarter, year_only, quarter_only,
         y_t_pct, y_t, DebtN_share2019, DebtR_share2019,
         Qpopulation_th, nGDP_2019_an, inflation_index,
         vax_rate, rGDP_pc_2019, debt_2019,
         StringencyIndex_PopWeighted, d_t_pct)

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
    F_CP    = sum(broad_fiscal_gdp[transmission_channel == "CP"], na.rm = TRUE),
    F_DI    = sum(broad_fiscal_gdp[transmission_channel == "DI"], na.rm = TRUE),
    F_H     = sum(broad_fiscal_gdp[transmission_channel == "H"],  na.rm = TRUE),
    F_total = sum(broad_fiscal_gdp, na.rm = TRUE),
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
    quarter_num = as.integer(str_sub(Quarter, 2, 2)),
    Quarter     = factor(Quarter, levels = pandemic_qs, ordered = TRUE),
    F_CP    = replace_na(F_CP, 0),
    F_DI    = replace_na(F_DI, 0),
    F_H     = replace_na(F_H, 0),
    F_total = replace_na(F_total, 0)
  ) %>%
  arrange(Country, Quarter)

pdata <- df %>%
  mutate(S_mean_tw = S_mean_pw * 100,
         F_CP = F_CP * 100, F_DI = F_DI * 100, F_H = F_H * 100,
         vax_rate = vax_rate * 100,
         theta_pct = theta_mean * 100) %>%
  mutate(
    F_DI_lag1 = lag(F_DI, 1),
    F_DI_lag2 = lag(F_DI, 2),
    y_lag1    = lag(y_t_pct, 1)
  )

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

# =============================================================================
#  NEW: Three-way split of CP — above / loans+equity / guarantees
# =============================================================================
fiscal_panel_3way <- fm1 %>%
  filter(broad_fiscal != 0) %>%
  mutate(
    Quarter_fmt = paste0("Q", Quarter, ".", Year),
    # Above-the-line CP (category 1)
    CP_above = ifelse(transmission_channel == "CP" & category == 1, broad_fiscal_gdp, 0),
    # Below-the-line: Loans (Code 40) + Equity (Code 41)
    CP_loans = ifelse(transmission_channel == "CP" & category == 2 &
                        PolicyCode %in% c("40", "41"), broad_fiscal_gdp, 0),
    # Below-the-line: Guarantees (Code 43)
    CP_guar  = ifelse(transmission_channel == "CP" & category == 2 &
                        PolicyCode == "43", broad_fiscal_gdp, 0)
  ) %>%
  group_by(Country, Quarter_fmt) %>%
  summarise(
    F_CP_above = sum(CP_above, na.rm = TRUE) * 100,
    F_CP_loans = sum(CP_loans, na.rm = TRUE) * 100,
    F_CP_guar  = sum(CP_guar,  na.rm = TRUE) * 100,
    # Also build the original split for comparison
    F_CP_below = (sum(CP_loans, na.rm = TRUE) + sum(CP_guar, na.rm = TRUE)) * 100,
    .groups = "drop"
  ) %>%
  rename(Quarter = Quarter_fmt)

pdataD <- pdataD %>%
  left_join(fiscal_panel_3way, by = c("Country", "Quarter")) %>%
  mutate(
    F_CP_above = replace_na(F_CP_above, 0),
    F_CP_loans = replace_na(F_CP_loans, 0),
    F_CP_guar  = replace_na(F_CP_guar, 0),
    F_CP_below = replace_na(F_CP_below, 0),
    # Original: all below-the-line adjusted at 0.35
    F_CP_below_adj_mid = F_CP_below * 0.35,
    # NEW: Guarantees adjusted at 0.35, Loans/Equity at full scale
    F_CP_below_new = F_CP_loans + F_CP_guar * 0.35
  )

# =============================================================================
#  DESCRIPTIVES
# =============================================================================
cat(strrep("=", 70), "\n")
cat("  BELOW-THE-LINE CP DESCRIPTIVES (pp GDP, panel means)\n")
cat(strrep("=", 70), "\n\n")

cat(sprintf("  F_CP_above (grants/subsidies):     %.4f\n", mean(pdataD$F_CP_above, na.rm = TRUE)))
cat(sprintf("  F_CP_loans (Code 40+41, full):     %.4f\n", mean(pdataD$F_CP_loans, na.rm = TRUE)))
cat(sprintf("  F_CP_guar  (Code 43, full):        %.4f\n", mean(pdataD$F_CP_guar, na.rm = TRUE)))
cat(sprintf("  F_CP_guar  (Code 43, adj 0.35):    %.4f\n", mean(pdataD$F_CP_guar * 0.35, na.rm = TRUE)))
cat(sprintf("  F_CP_below_adj_mid (all BLW*0.35): %.4f\n", mean(pdataD$F_CP_below_adj_mid, na.rm = TRUE)))
cat(sprintf("  F_CP_below_new (loans full+guar*0.35): %.4f\n", mean(pdataD$F_CP_below_new, na.rm = TRUE)))

# =============================================================================
#  MODEL A: Original — all below-the-line adjusted at 0.35 (baseline)
# =============================================================================
cat("\n\n")
cat(strrep("=", 70), "\n")
cat("  MODEL A: Original — F_CP_below * 0.35 (all BLW adjusted)\n")
cat(strrep("=", 70), "\n\n")

mA <- plm(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_mid + F_DI_lag1 +
    as.numeric(Quarter),
  data = pdataD, model = "within", effect = "individual"
)
ctA <- coeftest(mA, vcov = vcovHC(mA, cluster = "group", type = "HC1"))
print(ctA)

# =============================================================================
#  MODEL B: Loans/Equity full scale, Guarantees adjusted at 0.35
# =============================================================================
cat("\n\n")
cat(strrep("=", 70), "\n")
cat("  MODEL B: Loans full + Guarantees * 0.35\n")
cat("  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_new + F_DI_lag1 + t\n")
cat(strrep("=", 70), "\n\n")

mB <- plm(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_new + F_DI_lag1 +
    as.numeric(Quarter),
  data = pdataD, model = "within", effect = "individual"
)
ctB <- coeftest(mB, vcov = vcovHC(mB, cluster = "group", type = "HC1"))
print(ctB)

# =============================================================================
#  MODEL C: Three-way split — Above / Loans (full) / Guarantees (adj 0.35)
# =============================================================================
cat("\n\n")
cat(strrep("=", 70), "\n")
cat("  MODEL C: Three-way — Above + Loans(full) + Guarantees(adj 0.35)\n")
cat("  debt_dR ~ y_t_pct + F_CP_above + F_CP_loans + F_CP_guar_adj + F_DI_lag1 + t\n")
cat(strrep("=", 70), "\n\n")

pdataD$F_CP_guar_adj <- pdataD$F_CP_guar * 0.35

mC <- plm(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_loans + F_CP_guar_adj + F_DI_lag1 +
    as.numeric(Quarter),
  data = pdataD, model = "within", effect = "individual"
)
ctC <- coeftest(mC, vcov = vcovHC(mC, cluster = "group", type = "HC1"))
print(ctC)

# =============================================================================
#  MODEL D: Three-way with Guarantees UNadjusted (for comparison)
# =============================================================================
cat("\n\n")
cat(strrep("=", 70), "\n")
cat("  MODEL D: Three-way — Above + Loans(full) + Guarantees(full)\n")
cat(strrep("=", 70), "\n\n")

mD <- plm(
  debt_dR ~ y_t_pct + F_CP_above + F_CP_loans + F_CP_guar + F_DI_lag1 +
    as.numeric(Quarter),
  data = pdataD, model = "within", effect = "individual"
)
ctD <- coeftest(mD, vcov = vcovHC(mD, cluster = "group", type = "HC1"))
print(ctD)

# =============================================================================
#  COMPARISON TABLE
# =============================================================================
cat("\n\n")
cat(strrep("=", 70), "\n")
cat("  COEFFICIENT COMPARISON ACROSS MODELS\n")
cat(strrep("=", 70), "\n\n")

cat("  Variable                  Model A        Model B        Model C        Model D\n")
cat("                           (BLW*0.35)    (L+G*0.35)    (3-way adj)    (3-way full)\n")
cat("  ", strrep("-", 80), "\n")

# Get coefficients
get_coef_str <- function(ct, varname) {
  idx <- which(rownames(ct) == varname)
  if (length(idx) == 0) return("       ---    ")
  est <- ct[idx, 1]
  pv  <- ct[idx, 4]
  stars <- ifelse(pv < 0.001, "***", ifelse(pv < 0.01, "** ", ifelse(pv < 0.05, "*  ", ifelse(pv < 0.1, ".  ", "   "))))
  sprintf("%8.4f%s", est, stars)
}

rows <- c("y_t_pct", "F_CP_above", "F_CP_below_adj_mid", "F_CP_below_new",
           "F_CP_loans", "F_CP_guar_adj", "F_CP_guar", "F_DI_lag1", "as.numeric(Quarter)")
labels <- c("y_t_pct", "F_CP_above", "F_CP_below (all*0.35)", "F_CP_below (L+G*0.35)",
            "F_CP_loans (full)", "F_CP_guar (adj 0.35)", "F_CP_guar (full)", "F_DI_lag1", "Quarter trend")

for (i in seq_along(rows)) {
  cat(sprintf("  %-25s  %s  %s  %s  %s\n",
              labels[i],
              get_coef_str(ctA, rows[i]),
              get_coef_str(ctB, rows[i]),
              get_coef_str(ctC, rows[i]),
              get_coef_str(ctD, rows[i])))
}

cat("\n========== DONE ==========\n")
