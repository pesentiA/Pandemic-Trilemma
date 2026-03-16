# =============================================================================
#  Output Gap: DI and H sub-component splits
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

# --- Standard fiscal aggregates (CP, DI, H) ---
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

# --- DI sub-components ---
# DI_transfers: Codes 35,36,37,38 (direct cash, unemployment, benefit expansion, ad hoc)
# DI_demand:    Codes 27,28,29 (infrastructure, green investment, tourism stimulus)
# DI_tax:       Codes 17-22,25,26 (individual tax relief + consumption tax cuts)
df_fiscal_di <- fm1 %>%
  filter(broad_fiscal == 1, transmission_channel == "DI") %>%
  mutate(
    Quarter = as.character(YQ_ord),
    DI_sub = case_when(
      PolicyCode %in% c("35","36","37","38") ~ "transfers",
      PolicyCode %in% c("27","28","29")      ~ "demand",
      PolicyCode %in% c("17","18","19","20","21","22","25","26") ~ "tax",
      TRUE ~ "other_di"
    )
  ) %>%
  filter(Quarter %in% pandemic_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(
    F_DI_transfers = sum(broad_fiscal_gdp[DI_sub == "transfers"], na.rm = TRUE),
    F_DI_demand    = sum(broad_fiscal_gdp[DI_sub == "demand"],    na.rm = TRUE),
    F_DI_tax       = sum(broad_fiscal_gdp[DI_sub == "tax"],       na.rm = TRUE),
    .groups = "drop"
  )

# --- H sub-components ---
# H_supply: Codes 30,31,32 (medical items — tax relief, low-cost, high-cost)
# H_infra:  Codes 33,34,general (health infrastructure, workforce, general spending)
# H_other:  Codes 23,24 (health-adjacent)
df_fiscal_h <- fm1 %>%
  filter(broad_fiscal == 1, transmission_channel == "H") %>%
  mutate(
    Quarter = as.character(YQ_ord),
    H_sub = case_when(
      PolicyCode %in% c("30","31","32")          ~ "supply",
      PolicyCode %in% c("33","34","general")     ~ "infra",
      PolicyCode %in% c("23","24")               ~ "adjacent",
      TRUE ~ "other_h"
    )
  ) %>%
  filter(Quarter %in% pandemic_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(
    F_H_supply   = sum(broad_fiscal_gdp[H_sub == "supply"],   na.rm = TRUE),
    F_H_infra    = sum(broad_fiscal_gdp[H_sub == "infra"],    na.rm = TRUE),
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
  left_join(df_fiscal_di,  by = c("Country", "Quarter")) %>%
  left_join(df_fiscal_h,   by = c("Country", "Quarter")) %>%
  left_join(df_stringency, by = c("Country", "Quarter")) %>%
  mutate(
    quarter_num = as.integer(str_sub(Quarter, 2, 2)),
    Quarter     = factor(Quarter, levels = pandemic_qs, ordered = TRUE),
    across(starts_with("F_"), ~replace_na(., 0))
  ) %>%
  arrange(Country, Quarter)

pdata <- df %>%
  mutate(S_mean_tw = S_mean_pw * 100,
         across(starts_with("F_"), ~. * 100),
         vax_rate = vax_rate * 100,
         theta_pct = theta_mean * 100) %>%
  mutate(
    F_DI_lag1 = lag(F_DI, 1),
    F_DI_lag2 = lag(F_DI, 2),
    F_DI_transfers_lag1 = lag(F_DI_transfers, 1),
    F_DI_transfers_lag2 = lag(F_DI_transfers, 2),
    F_DI_demand_lag1    = lag(F_DI_demand, 1),
    F_DI_demand_lag2    = lag(F_DI_demand, 2),
    F_DI_tax_lag1       = lag(F_DI_tax, 1),
    F_DI_tax_lag2       = lag(F_DI_tax, 2),
    y_lag1 = lag(y_t_pct, 1)
  )

pdataY <- pdata %>%
  filter(Quarter %in% c("Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                         "Q1.2021","Q2.2021","Q3.2021","Q4.2021","Q1.2022"))

# =============================================================================
#  DESCRIPTIVES
# =============================================================================
cat(strrep("=", 70), "\n")
cat("  PANEL MEANS (pdataY, pp GDP)\n")
cat(strrep("=", 70), "\n\n")

cat("  DI total:       ", sprintf("%.4f", mean(pdataY$F_DI, na.rm=T)), "\n")
cat("    DI_transfers: ", sprintf("%.4f", mean(pdataY$F_DI_transfers, na.rm=T)),
    sprintf(" (%.0f%%)\n", mean(pdataY$F_DI_transfers, na.rm=T)/mean(pdataY$F_DI, na.rm=T)*100))
cat("    DI_demand:    ", sprintf("%.4f", mean(pdataY$F_DI_demand, na.rm=T)),
    sprintf(" (%.0f%%)\n", mean(pdataY$F_DI_demand, na.rm=T)/mean(pdataY$F_DI, na.rm=T)*100))
cat("    DI_tax:       ", sprintf("%.4f", mean(pdataY$F_DI_tax, na.rm=T)),
    sprintf(" (%.0f%%)\n", mean(pdataY$F_DI_tax, na.rm=T)/mean(pdataY$F_DI, na.rm=T)*100))

cat("\n  H total:        ", sprintf("%.4f", mean(pdataY$F_H, na.rm=T)), "\n")
cat("    H_supply:     ", sprintf("%.4f", mean(pdataY$F_H_supply, na.rm=T)),
    sprintf(" (%.0f%%)\n", mean(pdataY$F_H_supply, na.rm=T)/mean(pdataY$F_H, na.rm=T)*100))
cat("    H_infra:      ", sprintf("%.4f", mean(pdataY$F_H_infra, na.rm=T)),
    sprintf(" (%.0f%%)\n", mean(pdataY$F_H_infra, na.rm=T)/mean(pdataY$F_H, na.rm=T)*100))

# =============================================================================
#  MODEL 0: Baseline (pooled CP, pooled DI)
# =============================================================================
cat("\n\n")
cat(strrep("=", 70), "\n")
cat("  MODEL 0: Baseline\n")
cat("  y ~ S*y_lag1 + S*F_CP + F_DI_lag2  |  TWFE\n")
cat(strrep("=", 70), "\n\n")

m0 <- plm(y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP + F_DI_lag2,
           data = pdataY, model = "within", effect = "twoways")
ct0 <- coeftest(m0, vcov = vcovHC(m0, cluster = "group", type = "HC1"))
print(ct0)

# =============================================================================
#  DI SPLIT MODELS
# =============================================================================

# --- DI Model 1: Split DI into transfers vs demand vs tax (lag 2) ---
cat("\n\n")
cat(strrep("=", 70), "\n")
cat("  DI MODEL 1: Three-way DI split (all lag 2)\n")
cat("  y ~ S*y_lag1 + S*F_CP + F_DI_transfers_lag2 + F_DI_demand_lag2 + F_DI_tax_lag2\n")
cat(strrep("=", 70), "\n\n")

m_di1 <- plm(y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP +
                F_DI_transfers_lag2 + F_DI_demand_lag2 + F_DI_tax_lag2,
              data = pdataY, model = "within", effect = "twoways")
ct_di1 <- coeftest(m_di1, vcov = vcovHC(m_di1, cluster = "group", type = "HC1"))
print(ct_di1)

# --- DI Model 2: Split DI (lag 1) ---
cat("\n\n")
cat(strrep("=", 70), "\n")
cat("  DI MODEL 2: Three-way DI split (all lag 1)\n")
cat(strrep("=", 70), "\n\n")

m_di2 <- plm(y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP +
                F_DI_transfers_lag1 + F_DI_demand_lag1 + F_DI_tax_lag1,
              data = pdataY, model = "within", effect = "twoways")
ct_di2 <- coeftest(m_di2, vcov = vcovHC(m_di2, cluster = "group", type = "HC1"))
print(ct_di2)

# --- DI Model 3: Transfers vs rest (lag 2) ---
cat("\n\n")
cat(strrep("=", 70), "\n")
cat("  DI MODEL 3: Transfers vs non-transfers (lag 2)\n")
cat(strrep("=", 70), "\n\n")

pdataY$F_DI_nontransfer_lag2 <- pdataY$F_DI_demand_lag2 + pdataY$F_DI_tax_lag2

m_di3 <- plm(y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP +
                F_DI_transfers_lag2 + F_DI_nontransfer_lag2,
              data = pdataY, model = "within", effect = "twoways")
ct_di3 <- coeftest(m_di3, vcov = vcovHC(m_di3, cluster = "group", type = "HC1"))
print(ct_di3)

# =============================================================================
#  H IN THE OUTPUT EQUATION
# =============================================================================

# --- H Model 1: Add pooled H to baseline ---
cat("\n\n")
cat(strrep("=", 70), "\n")
cat("  H MODEL 1: Baseline + F_H\n")
cat(strrep("=", 70), "\n\n")

m_h1 <- plm(y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP + F_DI_lag2 + F_H,
             data = pdataY, model = "within", effect = "twoways")
ct_h1 <- coeftest(m_h1, vcov = vcovHC(m_h1, cluster = "group", type = "HC1"))
print(ct_h1)

# --- H Model 2: H split into supply vs infra ---
cat("\n\n")
cat(strrep("=", 70), "\n")
cat("  H MODEL 2: Baseline + H_supply + H_infra\n")
cat(strrep("=", 70), "\n\n")

m_h2 <- plm(y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP + F_DI_lag2 +
               F_H_supply + F_H_infra,
             data = pdataY, model = "within", effect = "twoways")
ct_h2 <- coeftest(m_h2, vcov = vcovHC(m_h2, cluster = "group", type = "HC1"))
print(ct_h2)

# --- H Model 3: H with S interaction (does health spending interact with stringency?) ---
cat("\n\n")
cat(strrep("=", 70), "\n")
cat("  H MODEL 3: Baseline + S*F_H\n")
cat(strrep("=", 70), "\n\n")

m_h3 <- plm(y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP + F_DI_lag2 +
               S_mean_tw * F_H,
             data = pdataY, model = "within", effect = "twoways")
ct_h3 <- coeftest(m_h3, vcov = vcovHC(m_h3, cluster = "group", type = "HC1"))
print(ct_h3)

# =============================================================================
#  FULL MODEL: CP + DI split + H
# =============================================================================
cat("\n\n")
cat(strrep("=", 70), "\n")
cat("  FULL MODEL: S*F_CP + DI_transfers_lag2 + DI_demand_lag2 + F_H\n")
cat(strrep("=", 70), "\n\n")

m_full <- plm(y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP +
                F_DI_transfers_lag2 + F_DI_demand_lag2 + F_DI_tax_lag2 + F_H,
              data = pdataY, model = "within", effect = "twoways")
ct_full <- coeftest(m_full, vcov = vcovHC(m_full, cluster = "group", type = "HC1"))
print(ct_full)
cat(sprintf("  Adj R2: %.4f\n", summary(m_full)$r.squared["adjrsq"]))

# =============================================================================
#  SUMMARY TABLE
# =============================================================================
cat("\n\n")
cat(strrep("=", 70), "\n")
cat("  SUMMARY: KEY COEFFICIENTS ACROSS MODELS\n")
cat(strrep("=", 70), "\n\n")

get_str <- function(ct, v) {
  idx <- which(rownames(ct) == v)
  if (length(idx) == 0) return("        ---   ")
  est <- ct[idx, 1]; pv <- ct[idx, 4]
  stars <- ifelse(pv<0.001,"***",ifelse(pv<0.01,"** ",ifelse(pv<0.05,"*  ",ifelse(pv<0.1,".  ","   "))))
  sprintf("%8.4f%s", est, stars)
}

cat("                             M0(base)   DI-split(L2)  DI-split(L1)  DI 2way(L2)  +H         +H split    +S*H        Full\n")
cat(strrep("-", 130), "\n")

all_vars <- c("S_mean_tw","y_lag1","F_CP","F_DI_lag2",
              "F_DI_transfers_lag2","F_DI_demand_lag2","F_DI_tax_lag2",
              "F_DI_transfers_lag1","F_DI_demand_lag1","F_DI_tax_lag1",
              "F_DI_nontransfer_lag2",
              "F_H","F_H_supply","F_H_infra",
              "S_mean_tw:y_lag1","S_mean_tw:F_CP","S_mean_tw:F_H")

all_labels <- c("S","y_lag1","F_CP","F_DI (pooled, L2)",
                "DI_transfers (L2)","DI_demand (L2)","DI_tax (L2)",
                "DI_transfers (L1)","DI_demand (L1)","DI_tax (L1)",
                "DI_nontransfer (L2)",
                "F_H (pooled)","H_supply","H_infra",
                "S*y_lag1","S*F_CP","S*F_H")

cts <- list(ct0, ct_di1, ct_di2, ct_di3, ct_h1, ct_h2, ct_h3, ct_full)

for (i in seq_along(all_vars)) {
  vals <- sapply(cts, function(ct) get_str(ct, all_vars[i]))
  if (!all(grepl("---", vals))) {
    cat(sprintf("%-24s %s\n", all_labels[i], paste(vals, collapse=" ")))
  }
}

cat("\nAdj R2:                  ")
models <- list(m0, m_di1, m_di2, m_di3, m_h1, m_h2, m_h3, m_full)
cat(paste(sapply(models, function(m) sprintf("%.4f       ", summary(m)$r.squared["adjrsq"])), collapse=" "), "\n")

cat("\n========== DONE ==========\n")
