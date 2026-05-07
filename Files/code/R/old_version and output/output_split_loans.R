# =============================================================================
#  Output Gap: Above / Loans / Guarantees split
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
#  Build panel
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

pdataY <- pdata %>%
  filter(Quarter %in% c("Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                         "Q1.2021","Q2.2021","Q3.2021","Q4.2021","Q1.2022"))

# =============================================================================
#  Three-way fiscal split for output panel
# =============================================================================
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
    F_CP_above = sum(CP_above, na.rm = TRUE) * 100,
    F_CP_loans = sum(CP_loans, na.rm = TRUE) * 100,
    F_CP_guar  = sum(CP_guar,  na.rm = TRUE) * 100,
    F_CP_below = (sum(CP_loans, na.rm = TRUE) + sum(CP_guar, na.rm = TRUE)) * 100,
    .groups = "drop"
  ) %>%
  rename(Quarter = Quarter_fmt)

pdataY <- pdataY %>%
  left_join(fiscal_panel_3way, by = c("Country", "Quarter")) %>%
  mutate(
    F_CP_above = replace_na(F_CP_above, 0),
    F_CP_loans = replace_na(F_CP_loans, 0),
    F_CP_guar  = replace_na(F_CP_guar, 0),
    F_CP_below = replace_na(F_CP_below, 0),
    F_CP_below_adj_mid = F_CP_below * 0.35,
    F_CP_below_new     = F_CP_loans + F_CP_guar * 0.35,
    F_CP_guar_adj      = F_CP_guar * 0.35
  )

# =============================================================================
#  MODEL 0: Baseline — pooled F_CP (main spec from analysis.R)
# =============================================================================
cat(strrep("=", 70), "\n")
cat("  MODEL 0: Baseline — pooled F_CP (main specification)\n")
cat("  y_t_pct ~ S*y_lag1 + S*F_CP + F_DI_lag2  |  TWFE\n")
cat(strrep("=", 70), "\n\n")

m0 <- plm(y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP + F_DI_lag2,
           data = pdataY, model = "within", effect = "twoways")
ct0 <- coeftest(m0, vcov = vcovHC(m0, cluster = "group", type = "HC1"))
print(ct0)

# =============================================================================
#  MODEL 1: Above vs Below (all*0.35)
# =============================================================================
cat("\n\n")
cat(strrep("=", 70), "\n")
cat("  MODEL 1: Above + Below(all*0.35) with S interaction\n")
cat(strrep("=", 70), "\n\n")

m1 <- plm(y_t_pct ~ S_mean_tw * y_lag1 +
             S_mean_tw * F_CP_above + S_mean_tw * F_CP_below_adj_mid + F_DI_lag2,
           data = pdataY, model = "within", effect = "twoways")
ct1 <- coeftest(m1, vcov = vcovHC(m1, cluster = "group", type = "HC1"))
print(ct1)

# =============================================================================
#  MODEL 2: Above + Below(Loans full + Guar*0.35) with S interaction
# =============================================================================
cat("\n\n")
cat(strrep("=", 70), "\n")
cat("  MODEL 2: Above + Below(Loans full + Guar*0.35) with S interaction\n")
cat(strrep("=", 70), "\n\n")

m2 <- plm(y_t_pct ~ S_mean_tw * y_lag1 +
             S_mean_tw * F_CP_above + S_mean_tw * F_CP_below_new + F_DI_lag2,
           data = pdataY, model = "within", effect = "twoways")
ct2 <- coeftest(m2, vcov = vcovHC(m2, cluster = "group", type = "HC1"))
print(ct2)

# =============================================================================
#  MODEL 3: Three-way — Above + Loans(full) + Guar(adj 0.35), each with S
# =============================================================================
cat("\n\n")
cat(strrep("=", 70), "\n")
cat("  MODEL 3: Three-way — Above + Loans(full) + Guar(adj), each * S\n")
cat(strrep("=", 70), "\n\n")

m3 <- plm(y_t_pct ~ S_mean_tw * y_lag1 +
             S_mean_tw * F_CP_above + S_mean_tw * F_CP_loans +
             S_mean_tw * F_CP_guar_adj + F_DI_lag2,
           data = pdataY, model = "within", effect = "twoways")
ct3 <- coeftest(m3, vcov = vcovHC(m3, cluster = "group", type = "HC1"))
print(ct3)

# =============================================================================
#  MODEL 4: Three-way — Above + Loans(full) + Guar(full), each with S
# =============================================================================
cat("\n\n")
cat(strrep("=", 70), "\n")
cat("  MODEL 4: Three-way — Above + Loans(full) + Guar(full), each * S\n")
cat(strrep("=", 70), "\n\n")

m4 <- plm(y_t_pct ~ S_mean_tw * y_lag1 +
             S_mean_tw * F_CP_above + S_mean_tw * F_CP_loans +
             S_mean_tw * F_CP_guar + F_DI_lag2,
           data = pdataY, model = "within", effect = "twoways")
ct4 <- coeftest(m4, vcov = vcovHC(m4, cluster = "group", type = "HC1"))
print(ct4)

# =============================================================================
#  MODEL 5: Simpler — no S interaction on CP sub-components, only levels
# =============================================================================
cat("\n\n")
cat(strrep("=", 70), "\n")
cat("  MODEL 5: Above + Loans + Guar(adj) — level effects only (no S*CP)\n")
cat("  y_t_pct ~ S*y_lag1 + F_CP_above + F_CP_loans + F_CP_guar_adj + F_DI_lag2\n")
cat(strrep("=", 70), "\n\n")

m5 <- plm(y_t_pct ~ S_mean_tw * y_lag1 +
             F_CP_above + F_CP_loans + F_CP_guar_adj + F_DI_lag2,
           data = pdataY, model = "within", effect = "twoways")
ct5 <- coeftest(m5, vcov = vcovHC(m5, cluster = "group", type = "HC1"))
print(ct5)

# =============================================================================
#  COMPARISON TABLE
# =============================================================================
cat("\n\n")
cat(strrep("=", 70), "\n")
cat("  COEFFICIENT COMPARISON — OUTPUT GAP MODELS\n")
cat(strrep("=", 70), "\n\n")

get_coef_str <- function(ct, varname) {
  idx <- which(rownames(ct) == varname)
  if (length(idx) == 0) return("         ---   ")
  est <- ct[idx, 1]
  pv  <- ct[idx, 4]
  stars <- ifelse(pv < 0.001, "***", ifelse(pv < 0.01, "** ", ifelse(pv < 0.05, "*  ", ifelse(pv < 0.1, ".  ", "   "))))
  sprintf("%9.5f%s", est, stars)
}

cat("                                M0(base)     M1(A+B*.35)  M2(A+LG*.35) M3(3way adj) M4(3way full) M5(levels)\n")
cat(strrep("-", 110), "\n")

vars <- c("S_mean_tw", "y_lag1", "F_CP", "F_CP_above", "F_CP_below_adj_mid",
          "F_CP_below_new", "F_CP_loans", "F_CP_guar_adj", "F_CP_guar",
          "F_DI_lag2",
          "S_mean_tw:y_lag1", "S_mean_tw:F_CP",
          "S_mean_tw:F_CP_above", "S_mean_tw:F_CP_below_adj_mid",
          "S_mean_tw:F_CP_below_new",
          "S_mean_tw:F_CP_loans", "S_mean_tw:F_CP_guar_adj", "S_mean_tw:F_CP_guar")

labels <- c("S", "y_lag1", "F_CP (pooled)", "F_CP_above", "F_CP_below (all*0.35)",
            "F_CP_below (L+G*0.35)", "F_CP_loans (full)", "F_CP_guar (adj 0.35)", "F_CP_guar (full)",
            "F_DI_lag2",
            "S * y_lag1", "S * F_CP (pooled)",
            "S * F_CP_above", "S * F_CP_below(all*.35)",
            "S * F_CP_below(L+G*.35)",
            "S * F_CP_loans", "S * F_CP_guar(adj)", "S * F_CP_guar(full)")

for (i in seq_along(vars)) {
  s0 <- get_coef_str(ct0, vars[i])
  s1 <- get_coef_str(ct1, vars[i])
  s2 <- get_coef_str(ct2, vars[i])
  s3 <- get_coef_str(ct3, vars[i])
  s4 <- get_coef_str(ct4, vars[i])
  s5 <- get_coef_str(ct5, vars[i])
  # Only print if at least one model has it
  if (!all(grepl("---", c(s0,s1,s2,s3,s4,s5)))) {
    cat(sprintf("%-26s %s %s %s %s %s %s\n", labels[i], s0, s1, s2, s3, s4, s5))
  }
}

cat("\n\nAdj R2:                    ")
cat(sprintf("%.4f       ", summary(m0)$r.squared["adjrsq"]))
cat(sprintf("%.4f       ", summary(m1)$r.squared["adjrsq"]))
cat(sprintf("%.4f       ", summary(m2)$r.squared["adjrsq"]))
cat(sprintf("%.4f       ", summary(m3)$r.squared["adjrsq"]))
cat(sprintf("%.4f       ", summary(m4)$r.squared["adjrsq"]))
cat(sprintf("%.4f", summary(m5)$r.squared["adjrsq"]))
cat("\n")

cat("\n========== DONE ==========\n")
