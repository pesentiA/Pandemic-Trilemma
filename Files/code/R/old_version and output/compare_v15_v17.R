# =============================================================================
#  Compare v1.5 vs v1.7 fiscal classification: Output Gap & Debt models
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

# Load base data
safedata <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/processed"
load(file.path(safedata, "dataforanalysis.RData"))

set.seed(1234)

# Fiscal file paths
fm_path_v15 <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/raw/fiscal measures/fiscal_classified_v1_5.xlsx"
fm_path_v17 <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/raw/fiscal measures/fiscal_classified_v1_7.xlsx"

pandemic_qs <- c(
  "Q1.2019", "Q2.2019", "Q3.2019", "Q4.2019",
  "Q1.2020", "Q2.2020", "Q3.2020", "Q4.2020",
  "Q1.2021", "Q2.2021", "Q3.2021", "Q4.2021",
  "Q1.2022", "Q2.2022", "Q3.2022", "Q4.2022"
)

# =============================================================================
#  FUNCTION: Build the full panel from a given fiscal xlsx
# =============================================================================
build_panel <- function(fm_path) {

  fm1 <- readxl::read_excel(fm_path)
  fm1 <- fm1 %>%
    mutate(
      YQ       = paste0("Q", Quarter, ".", Year),
      YQ_ord   = factor(YQ, levels = sort(unique(paste0("Q", Quarter, ".", Year))), ordered = TRUE),
      size_pct = broad_fiscal_gdp * 100
    )

  # Exclude deferral codes
  fm1 <- fm1 %>% filter(!PolicyCode %in% c(5, 6, 11, 12, 15, 16))

  # --- Block 1: qdata ---
  df_qdata <- qdata %>%
    filter(Quarter %in% pandemic_qs) %>%
    select(Country, Quarter, year_only, quarter_only,
           y_t_pct, y_t, QReal.GDP.Growth_gr,
           d_t_pct, d_t_pct_r, d_t,
           DebtN_share2019, DebtR_share2019,
           DebtN_share2019_growth, DebtR_share2019_growth,
           Qpopulation_th, nGDP_2019_an, inflation_index,
           vax_rate, rGDP_pc_2019, debt_2019,
           StringencyIndex_PopWeighted)

  pop_2019 <- df_qdata[df_qdata$Quarter == "Q4.2019", c("Country", "Qpopulation_th")]
  names(pop_2019)[2] <- "pop_2019"
  df_qdata <- merge(df_qdata, pop_2019, by = "Country")

  # --- Block 2: theta ---
  df_theta <- theta_quarterly_full %>%
    mutate(Quarter = as.character(YQ)) %>%
    filter(Quarter %in% pandemic_qs) %>%
    select(Country, Quarter, theta_mean, theta_mean_l2, theta_mean_l4,
           theta_mean_lo, theta_mean_hi, wave_dominant, freq_source)

  # --- Block 3: fiscal ---
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
      CP_share = ifelse(F_total > 0, F_CP / F_total, NA_real_),
      DI_share = ifelse(F_total > 0, F_DI / F_total, NA_real_),
      n_measures_CP = sum(transmission_channel == "CP", na.rm = TRUE),
      n_measures_DI = sum(transmission_channel == "DI", na.rm = TRUE),
      .groups = "drop"
    )

  # --- Block 4: stringency ---
  df_stringency <- panel_w %>%
    filter(!is.na(S_mean)) %>%
    mutate(year = year(date), quarter = quarter(date),
           Quarter = paste0("Q", quarter, ".", year)) %>%
    filter(Quarter %in% pandemic_qs) %>%
    group_by(Country, Quarter) %>%
    summarise(S_mean_pw = mean(S_mean, na.rm = TRUE),
              S_max_pw  = max(S_max,   na.rm = TRUE),
              S_sd      = sd(S_mean,   na.rm = TRUE),
              .groups = "drop")

  # --- Block 5: hosp ---
  df_hosp <- hosp_d %>%
    mutate(date = as.Date(date)) %>%
    filter(date >= as.Date("2020-01-01"), date <= as.Date("2021-12-31")) %>%
    mutate(year = year(date), quarter = quarter(date),
           Quarter = paste0("Q", quarter, ".", year)) %>%
    filter(Quarter %in% pandemic_qs) %>%
    group_by(Country, Quarter) %>%
    summarise(hosp_new_pm = mean(Weekly.new.hospital.admissions.per.million, na.rm = TRUE),
              icu_occ_pm  = mean(Daily.ICU.occupancy.per.million, na.rm = TRUE),
              hosp_occ_pm = mean(Daily.hospital.occupancy.per.million, na.rm = TRUE),
              .groups = "drop")

  # --- Merge ---
  df <- df_qdata %>%
    left_join(df_theta,      by = c("Country", "Quarter")) %>%
    left_join(df_fiscal,     by = c("Country", "Quarter")) %>%
    left_join(df_stringency, by = c("Country", "Quarter")) %>%
    left_join(df_hosp,       by = c("Country", "Quarter")) %>%
    mutate(
      quarter_num = as.integer(str_sub(Quarter, 2, 2)),
      Quarter     = factor(Quarter, levels = pandemic_qs, ordered = TRUE),
      date        = ymd(paste0(year_only, "-", (quarter_num - 1L) * 3L + 1L, "-01")),
      F_CP    = replace_na(F_CP, 0),
      F_DI    = replace_na(F_DI, 0),
      F_H     = replace_na(F_H, 0),
      F_total = replace_na(F_total, 0)
    ) %>%
    arrange(Country, Quarter)

  # --- Build pdata ---
  pdata <- df %>%
    filter(Quarter %in% c("Q1.2019","Q2.2019","Q3.2019","Q4.2019",
                           "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                           "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
                           "Q1.2022","Q2.2022","Q3.2022","Q4.2022"))

  pdata <- pdata %>%
    mutate(S_mean_tw = S_mean_pw * 100,
           F_CP = F_CP * 100, F_DI = F_DI * 100, F_H = F_H * 100,
           vax_rate = vax_rate * 100,
           theta_pct = theta_mean * 100,
           Qpopulation_th = Qpopulation_th / 1000)

  pdata <- pdata %>%
    mutate(
      S_lag1     = lag(S_mean_tw, 1),
      S_lag2     = lag(S_mean_tw, 2),
      F_DI_lag1  = lag(F_DI, 1),
      F_DI_lag2  = lag(F_DI, 2),
      F_CP_lag1  = lag(F_CP, 1),
      F_CP_lag2  = lag(F_CP, 2),
      theta_lag1 = lag(theta_mean, 1),
      y_lag1     = lag(y_t_pct, 1)
    )

  # --- Output Gap sample ---
  pdataY <- pdata %>%
    filter(Quarter %in% c("Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                           "Q1.2021","Q2.2021","Q3.2021","Q4.2021","Q1.2022"))

  # --- Debt sample ---
  pdata <- pdata %>%
    group_by(Country) %>%
    mutate(debt_dR = DebtR_share2019 - lag(DebtR_share2019),
           debt_dN = DebtN_share2019 - lag(DebtN_share2019)) %>%
    ungroup()

  pdataD <- pdata %>%
    filter(Quarter %in% c("Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                           "Q1.2021","Q2.2021","Q3.2021","Q4.2021","Q1.2022"))
  pdataD <- pdataD %>%
    group_by(Country) %>%
    mutate(debt_dR = DebtR_share2019 - lag(DebtR_share2019),
           debt_dN = DebtN_share2019 - lag(DebtN_share2019)) %>%
    ungroup()

  # Build above/below split from fm object in environment
  fiscal_panel <- fm1 %>%
    filter(broad_fiscal != 0) %>%
    mutate(
      Quarter_fmt = paste0("Q", Quarter, ".", Year),
      CP_above = ifelse(transmission_channel == "CP" & category == 1, broad_fiscal_gdp, 0),
      CP_below = ifelse(transmission_channel == "CP" & category == 2, broad_fiscal_gdp, 0)
    ) %>%
    group_by(Country, Quarter_fmt) %>%
    summarise(
      F_CP_above = sum(CP_above, na.rm = TRUE) * 100,
      F_CP_below = sum(CP_below, na.rm = TRUE) * 100,
      .groups = "drop"
    ) %>%
    rename(Quarter = Quarter_fmt)

  pdataD <- pdataD %>%
    left_join(fiscal_panel, by = c("Country", "Quarter")) %>%
    mutate(
      F_CP_above = replace_na(F_CP_above, 0),
      F_CP_below = replace_na(F_CP_below, 0),
      F_CP_below_adj_mid = F_CP_below * 0.35
    )

  list(pdataY = pdataY, pdataD = pdataD, fm1 = fm1)
}

# =============================================================================
#  BUILD BOTH PANELS
# =============================================================================
cat("\n========== Building v1.5 panel ==========\n")
p15 <- build_panel(fm_path_v15)

cat("\n========== Building v1.7 panel ==========\n")
p17 <- build_panel(fm_path_v17)

# =============================================================================
#  FISCAL AGGREGATES COMPARISON
# =============================================================================
cat("\n\n")
cat(strrep("=", 70), "\n")
cat("  FISCAL AGGREGATES COMPARISON (broad_fiscal == 1, excl. deferrals)\n")
cat(strrep("=", 70), "\n")

agg15 <- p15$fm1 %>% filter(broad_fiscal == 1, !PolicyCode %in% c(5,6,11,12,15,16)) %>%
  group_by(transmission_channel) %>%
  summarise(n = n(), total_gdp = sum(broad_fiscal_gdp, na.rm = TRUE), .groups = "drop")
agg17 <- p17$fm1 %>% filter(broad_fiscal == 1, !PolicyCode %in% c(5,6,11,12,15,16)) %>%
  group_by(transmission_channel) %>%
  summarise(n = n(), total_gdp = sum(broad_fiscal_gdp, na.rm = TRUE), .groups = "drop")

cat("\n  v1.5:\n")
print(as.data.frame(agg15))
cat("\n  v1.7:\n")
print(as.data.frame(agg17))

cat("\n  F_CP / F_DI / F_H means in panel (pdataY, in pp GDP):\n")
cat(sprintf("  v1.5: F_CP=%.4f  F_DI=%.4f  F_H=%.4f\n",
            mean(p15$pdataY$F_CP, na.rm=T), mean(p15$pdataY$F_DI, na.rm=T), mean(p15$pdataY$F_H, na.rm=T)))
cat(sprintf("  v1.7: F_CP=%.4f  F_DI=%.4f  F_H=%.4f\n",
            mean(p17$pdataY$F_CP, na.rm=T), mean(p17$pdataY$F_DI, na.rm=T), mean(p17$pdataY$F_H, na.rm=T)))

# =============================================================================
#  OUTPUT GAP MODELS
# =============================================================================
cat("\n\n")
cat(strrep("=", 70), "\n")
cat("  OUTPUT GAP EQUATION: TWFE (main specification)\n")
cat("  y_t_pct ~ S_mean_tw*y_lag1 + S_mean_tw*F_CP + F_DI_lag2\n")
cat(strrep("=", 70), "\n")

run_output_model <- function(pdataY) {
  m <- plm(y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP + F_DI_lag2,
           data = pdataY, model = "within", effect = "twoways")
  ct <- coeftest(m, vcov = vcovHC(m, cluster = "group", type = "HC1"))
  list(model = m, ct = ct)
}

y15 <- run_output_model(p15$pdataY)
y17 <- run_output_model(p17$pdataY)

cat("\n  --- v1.5 ---\n")
print(y15$ct)
cat(sprintf("  Adj. R2: %.4f  |  N: %d\n", summary(y15$model)$r.squared["adjrsq"], nobs(y15$model)))

cat("\n  --- v1.7 ---\n")
print(y17$ct)
cat(sprintf("  Adj. R2: %.4f  |  N: %d\n", summary(y17$model)$r.squared["adjrsq"], nobs(y17$model)))

cat("\n  --- Coefficient differences (v1.7 - v1.5) ---\n")
vars <- names(coef(y15$model))
for (v in vars) {
  d <- coef(y17$model)[v] - coef(y15$model)[v]
  pct <- d / abs(coef(y15$model)[v]) * 100
  cat(sprintf("  %-25s  v1.5: %8.5f  v1.7: %8.5f  diff: %+8.5f (%+.1f%%)\n",
              v, coef(y15$model)[v], coef(y17$model)[v], d, pct))
}

# =============================================================================
#  DEBT MODELS
# =============================================================================
cat("\n\n")
cat(strrep("=", 70), "\n")
cat("  DEBT EQUATION: Country FE (main specification)\n")
cat("  debt_dR ~ y_t_pct + F_CP + F_DI_lag1 + as.numeric(Quarter)\n")
cat(strrep("=", 70), "\n")

run_debt_model <- function(pdataD) {
  m <- plm(debt_dR ~ y_t_pct + F_CP + F_DI_lag1 + as.numeric(Quarter),
           data = pdataD, model = "within", effect = "individual")
  ct <- coeftest(m, vcov = vcovHC(m, cluster = "group", type = "HC1"))
  list(model = m, ct = ct)
}

d15 <- run_debt_model(p15$pdataD)
d17 <- run_debt_model(p17$pdataD)

cat("\n  --- v1.5 ---\n")
print(d15$ct)

cat("\n  --- v1.7 ---\n")
print(d17$ct)

cat("\n  --- Coefficient differences (v1.7 - v1.5) ---\n")
vars_d <- names(coef(d15$model))
for (v in vars_d) {
  d <- coef(d17$model)[v] - coef(d15$model)[v]
  pct <- d / abs(coef(d15$model)[v]) * 100
  cat(sprintf("  %-25s  v1.5: %8.5f  v1.7: %8.5f  diff: %+8.5f (%+.1f%%)\n",
              v, coef(d15$model)[v], coef(d17$model)[v], d, pct))
}

# =============================================================================
#  DEBT SPLIT MODEL (above/below)
# =============================================================================
cat("\n\n")
cat(strrep("=", 70), "\n")
cat("  DEBT EQUATION: Split CP (above/below, adj_mid)\n")
cat("  debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_mid + F_DI_lag1 + t\n")
cat(strrep("=", 70), "\n")

run_debt_split <- function(pdataD) {
  m <- plm(debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_mid + F_DI_lag1 +
             as.numeric(Quarter),
           data = pdataD, model = "within", effect = "individual")
  ct <- coeftest(m, vcov = vcovHC(m, cluster = "group", type = "HC1"))
  list(model = m, ct = ct)
}

ds15 <- run_debt_split(p15$pdataD)
ds17 <- run_debt_split(p17$pdataD)

cat("\n  --- v1.5 ---\n")
print(ds15$ct)

cat("\n  --- v1.7 ---\n")
print(ds17$ct)

cat("\n  --- Coefficient differences (v1.7 - v1.5) ---\n")
vars_ds <- names(coef(ds15$model))
for (v in vars_ds) {
  d <- coef(ds17$model)[v] - coef(ds15$model)[v]
  pct <- d / abs(coef(ds15$model)[v]) * 100
  cat(sprintf("  %-25s  v1.5: %8.5f  v1.7: %8.5f  diff: %+8.5f (%+.1f%%)\n",
              v, coef(ds15$model)[v], coef(ds17$model)[v], d, pct))
}

cat("\n\n========== COMPARISON COMPLETE ==========\n")
