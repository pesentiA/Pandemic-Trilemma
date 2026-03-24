library(contdid)
library(dplyr)
library(tidyr)
library(ggplot2)
library(fixest)
library(readxl)
library(patchwork)
library(lmtest)
library(sandwich)

conflicted::conflict_prefer("select",   "dplyr")
conflicted::conflict_prefer("filter",   "dplyr")
conflicted::conflicts_prefer(dplyr::lag)
conflicted::conflicts_prefer(lubridate::year)
conflicted::conflicts_prefer(lubridate::quarter)
conflicted::conflicts_prefer(fixest::pvalue)

base_dir  <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files"
safeplots <- file.path(base_dir, "output/figures")
load(file.path(base_dir, "data/processed/dataforanalysis.RData"))
ox <- readr::read_csv(file.path(base_dir, "data/raw/oxford stringency/Oxcnat.csv"),
                       show_col_types = FALSE)
fm1 <- read_excel(file.path(base_dir, "data/raw/fiscal measures/fiscal_classified_v1_7.xlsx"))
fm1 <- fm1 %>%
  mutate(YQ = paste0("Q", Quarter, ".", Year),
         YQ_ord = factor(YQ, levels = sort(unique(paste0("Q", Quarter, ".", Year))),
                          ordered = TRUE)) %>%
  filter(!PolicyCode %in% c(5, 6, 11, 12, 15, 16))

set.seed(1234)

oecd <- c("AUS","AUT","BEL","CAN","CHL","COL","CRI","CZE","DNK","EST",
  "FIN","FRA","DEU","GRC","HUN","ISL","IRL","ISR","ITA","JPN",
  "LVA","LTU","LUX","MEX","NLD","NZL","NOR","POL","PRT","SVK",
  "SVN","KOR","ESP","SWE","CHE","TUR","GBR","USA")

quarter_order <- c(
  "Q1.2019","Q2.2019","Q3.2019","Q4.2019",
  "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
  "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
  "Q1.2022","Q2.2022","Q3.2022","Q4.2022"
)

# ==============================================================================
#  DATA PREPARATION (same as test_contdid.R)
# ==============================================================================

ox2 <- ox %>%
  filter(CountryCode %in% oecd, Jurisdiction == "NAT_TOTAL") %>%
  mutate(date = lubridate::ymd(Date), Country = CountryCode) %>%
  filter(date >= "2020-01-01", date <= "2021-12-31",
         !is.na(StringencyIndex_Average))

dose <- ox2 %>%
  group_by(Country) %>%
  summarise(dose_mean = mean(StringencyIndex_Average, na.rm = TRUE),
            .groups = "drop")

# Fiscal composition: cumulative CP and DI over 2020-2021
fiscal_cum <- fm1 %>%
  filter(broad_fiscal == 1) %>%
  mutate(Quarter_fmt = paste0("Q", Quarter, ".", Year)) %>%
  filter(Quarter_fmt %in% quarter_order[5:12]) %>%  # Q1.2020-Q4.2021
  group_by(Country) %>%
  summarise(
    F_CP_cum = sum(broad_fiscal_gdp[transmission_channel == "CP"], na.rm = TRUE) * 100,
    F_DI_cum = sum(broad_fiscal_gdp[transmission_channel == "DI"], na.rm = TRUE) * 100,
    F_H_cum  = sum(broad_fiscal_gdp[transmission_channel == "H"],  na.rm = TRUE) * 100,
    F_tot_cum = sum(broad_fiscal_gdp, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    CP_share = ifelse(F_tot_cum > 0, F_CP_cum / F_tot_cum, NA_real_),
    DI_share = ifelse(F_tot_cum > 0, F_DI_cum / F_tot_cum, NA_real_)
  )

# Quarterly fiscal panel
df_fiscal <- fm1 %>%
  filter(broad_fiscal == 1) %>%
  mutate(Quarter = as.character(YQ_ord)) %>%
  filter(Quarter %in% quarter_order) %>%
  group_by(Country, Quarter) %>%
  summarise(
    F_CP = sum(broad_fiscal_gdp[transmission_channel == "CP"], na.rm = TRUE) * 100,
    F_DI = sum(broad_fiscal_gdp[transmission_channel == "DI"], na.rm = TRUE) * 100,
    F_H  = sum(broad_fiscal_gdp[transmission_channel == "H"],  na.rm = TRUE) * 100,
    .groups = "drop"
  )

# Build panel
panel <- qdata %>%
  filter(Quarter %in% quarter_order) %>%
  select(Country, Quarter, y_t_pct, DebtR_share2019, rGDP_pc_2019, debt_2019) %>%
  mutate(t_idx = match(Quarter, quarter_order)) %>%
  left_join(dose, by = "Country") %>%
  left_join(fiscal_cum, by = "Country") %>%
  left_join(df_fiscal, by = c("Country", "Quarter")) %>%
  mutate(
    F_CP = replace_na(F_CP, 0),
    F_DI = replace_na(F_DI, 0),
    F_H  = replace_na(F_H, 0)
  )

# G assignment (same as working specification)
low_threshold <- quantile(dose$dose_mean, 0.20)
g_df <- dose %>% mutate(G = ifelse(dose_mean <= low_threshold, 0L, 6L))

panel <- panel %>%
  left_join(g_df %>% select(Country, G), by = "Country")

ids <- panel %>% distinct(Country) %>% arrange(Country) %>% mutate(id = row_number())
panel <- panel %>% left_join(ids, by = "Country")

cat("\n", strrep("=", 70), "\n")
cat("  FISCAL COMPOSITION IN CONTINUOUS DiD\n")
cat(strrep("=", 70), "\n\n")

# ==============================================================================
#  DIAGNOSTIC: Orthogonality of Stringency and Fiscal Composition
# ==============================================================================

cat("--- Orthogonality Check: Stringency vs Fiscal Composition ---\n\n")

orth_data <- dose %>%
  left_join(fiscal_cum, by = "Country")

r_S_CP    <- cor(orth_data$dose_mean, orth_data$F_CP_cum,  use = "complete.obs")
r_S_DI    <- cor(orth_data$dose_mean, orth_data$F_DI_cum,  use = "complete.obs")
r_S_CPsh  <- cor(orth_data$dose_mean, orth_data$CP_share,  use = "complete.obs")
r_S_Ftot  <- cor(orth_data$dose_mean, orth_data$F_tot_cum, use = "complete.obs")

cat(sprintf("  r(dose_mean, F_CP_cum)  = %+.3f\n", r_S_CP))
cat(sprintf("  r(dose_mean, F_DI_cum)  = %+.3f\n", r_S_DI))
cat(sprintf("  r(dose_mean, CP_share)  = %+.3f\n", r_S_CPsh))
cat(sprintf("  r(dose_mean, F_total)   = %+.3f\n", r_S_Ftot))

cat(paste0(
  "\n  IMPLICATION: If r(S, fiscal) ~ 0, fiscal composition is approximately\n",
  "  orthogonal to stringency dose. This means:\n",
  "    (a) Fiscal vars are NOT confounders of the S->Y relationship\n",
  "    (b) We CAN study them as INDEPENDENT treatment channels\n",
  "    (c) Subsample splits by fiscal composition are valid\n\n"
))


# ==============================================================================
#  APPROACH 1: SUBSAMPLE SPLIT BY FISCAL COMPOSITION
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("  APPROACH 1: SUBSAMPLE SPLIT (High-CP vs Low-CP countries)\n")
cat(strrep("=", 70), "\n\n")

cat(paste0(
  "  LOGIC: If CP cushions the output cost of stringency, then:\n",
  "    - High-CP countries: dose-response should be WEAKER (less negative ACRT)\n",
  "    - Low-CP countries: dose-response should be STRONGER (more negative ACRT)\n",
  "  For debt: High-CP countries should show MORE debt accumulation per unit dose\n",
  "    (they spent more to preserve capacity)\n\n"
))

# Split at median CP share
med_cp <- median(fiscal_cum$CP_share, na.rm = TRUE)
cat(sprintf("  Median CP share: %.3f\n", med_cp))

cp_groups <- fiscal_cum %>%
  mutate(cp_group = ifelse(CP_share >= med_cp, "High_CP", "Low_CP")) %>%
  select(Country, cp_group, CP_share, F_CP_cum, F_DI_cum, F_tot_cum)

cat("  High-CP countries: ", paste(sort(cp_groups$Country[cp_groups$cp_group == "High_CP"]),
                                    collapse = ", "), "\n")
cat("  Low-CP countries:  ", paste(sort(cp_groups$Country[cp_groups$cp_group == "Low_CP"]),
                                    collapse = ", "), "\n\n")

panel_split <- panel %>%
  left_join(cp_groups %>% select(Country, cp_group), by = "Country")

# --- Run cont_did for each subsample ---
run_contdid_sub <- function(data, label, outcome = "y_t_pct") {
  df_sub <- data %>%
    filter(!is.na(.data[[outcome]])) %>%
    select(id, t_idx, Y = all_of(outcome), D = dose_mean, G) %>%
    as.data.frame()
  df_sub$D[df_sub$G == 0] <- 0

  n_treated  <- n_distinct(df_sub$id[df_sub$G > 0])
  n_control  <- n_distinct(df_sub$id[df_sub$G == 0])
  cat(sprintf("  %s: %d treated, %d comparison\n", label, n_treated, n_control))

  if (n_control < 2 || n_treated < 5) {
    cat("    -> Too few units, skipping.\n")
    return(NULL)
  }

  tryCatch(
    cont_did(
      yname = "Y", tname = "t_idx", idname = "id",
      dname = "D", gname = "G", data = df_sub,
      target_parameter = "slope", aggregation = "eventstudy",
      treatment_type = "continuous", control_group = "notyettreated",
      degree = 1, num_knots = 0,
      biters = 1000, cband = TRUE, print_details = FALSE
    ),
    error = function(e) { cat(sprintf("    -> ERROR: %s\n", e$message)); NULL }
  )
}

# Output gap by CP group
cat("\n--- Output Gap: High-CP subsample ---\n")
cd_y_hicp <- run_contdid_sub(
  panel_split %>% filter(cp_group == "High_CP"), "High-CP (y)"
)
if (!is.null(cd_y_hicp)) summary(cd_y_hicp)

cat("\n--- Output Gap: Low-CP subsample ---\n")
cd_y_locp <- run_contdid_sub(
  panel_split %>% filter(cp_group == "Low_CP"), "Low-CP (y)"
)
if (!is.null(cd_y_locp)) summary(cd_y_locp)

# Debt by CP group
cat("\n--- Debt: High-CP subsample ---\n")
cd_d_hicp <- run_contdid_sub(
  panel_split %>% filter(cp_group == "High_CP"), "High-CP (debt)",
  outcome = "DebtR_share2019"
)
if (!is.null(cd_d_hicp)) summary(cd_d_hicp)

cat("\n--- Debt: Low-CP subsample ---\n")
cd_d_locp <- run_contdid_sub(
  panel_split %>% filter(cp_group == "Low_CP"), "Low-CP (debt)",
  outcome = "DebtR_share2019"
)
if (!is.null(cd_d_locp)) summary(cd_d_locp)


# ==============================================================================
#  APPROACH 2: FISCAL COMPOSITION AS THE DOSE
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  APPROACH 2: FISCAL COMPOSITION AS CONTINUOUS TREATMENT\n")
cat(strrep("=", 70), "\n\n")

cat(paste0(
  "  LOGIC: Instead of stringency as the dose, use FISCAL COMPOSITION.\n",
  "  The 'treatment' is the fiscal package mix (CP vs DI).\n",
  "  D = cumulative CP (pp GDP) — higher D = more capacity preservation\n",
  "  G = Q2.2020 for countries that deployed fiscal measures (virtually all)\n",
  "  Comparison: low-fiscal countries (bottom 20% of F_total)\n\n"
))

# G for fiscal: low-fiscal countries as comparison
fiscal_low_thresh <- quantile(fiscal_cum$F_tot_cum, 0.20, na.rm = TRUE)
g_fiscal <- fiscal_cum %>%
  mutate(G_fiscal = ifelse(F_tot_cum <= fiscal_low_thresh, 0L, 6L))

cat(sprintf("  Fiscal threshold (20th pct): %.2f pp GDP\n", fiscal_low_thresh))
cat(sprintf("  Comparison (G=0): %d countries\n", sum(g_fiscal$G_fiscal == 0)))
cat("  Comparison countries: ",
    paste(g_fiscal$Country[g_fiscal$G_fiscal == 0], collapse = ", "), "\n\n")

# --- 2A: CP as dose -> Output Gap ---
cat("--- 2A: F_CP as dose -> Output Gap ---\n")

df_cp_y <- panel %>%
  filter(!is.na(y_t_pct)) %>%
  left_join(g_fiscal %>% select(Country, G_fiscal), by = "Country") %>%
  select(id, t_idx, Y = y_t_pct, D = F_CP_cum, G = G_fiscal) %>%
  as.data.frame()
df_cp_y$D[df_cp_y$G == 0] <- 0

cd_cp_y <- tryCatch(
  cont_did(
    yname = "Y", tname = "t_idx", idname = "id",
    dname = "D", gname = "G", data = df_cp_y,
    target_parameter = "slope", aggregation = "dose",
    treatment_type = "continuous", control_group = "notyettreated",
    degree = 1, num_knots = 0,
    biters = 1000, cband = TRUE, print_details = FALSE
  ),
  error = function(e) { cat(sprintf("  ERROR: %s\n", e$message)); NULL }
)
if (!is.null(cd_cp_y)) {
  cat("\n  F_CP dose -> Output Gap (ACRT):\n")
  summary(cd_cp_y)
}

# --- 2B: DI as dose -> Output Gap ---
cat("\n--- 2B: F_DI as dose -> Output Gap ---\n")

df_di_y <- panel %>%
  filter(!is.na(y_t_pct)) %>%
  left_join(g_fiscal %>% select(Country, G_fiscal), by = "Country") %>%
  select(id, t_idx, Y = y_t_pct, D = F_DI_cum, G = G_fiscal) %>%
  as.data.frame()
df_di_y$D[df_di_y$G == 0] <- 0

cd_di_y <- tryCatch(
  cont_did(
    yname = "Y", tname = "t_idx", idname = "id",
    dname = "D", gname = "G", data = df_di_y,
    target_parameter = "slope", aggregation = "dose",
    treatment_type = "continuous", control_group = "notyettreated",
    degree = 1, num_knots = 0,
    biters = 1000, cband = TRUE, print_details = FALSE
  ),
  error = function(e) { cat(sprintf("  ERROR: %s\n", e$message)); NULL }
)
if (!is.null(cd_di_y)) {
  cat("\n  F_DI dose -> Output Gap (ACRT):\n")
  summary(cd_di_y)
}

# --- 2C: CP as dose -> Debt ---
cat("\n--- 2C: F_CP as dose -> Debt ---\n")

df_cp_d <- panel %>%
  filter(!is.na(DebtR_share2019)) %>%
  left_join(g_fiscal %>% select(Country, G_fiscal), by = "Country") %>%
  select(id, t_idx, Y = DebtR_share2019, D = F_CP_cum, G = G_fiscal) %>%
  as.data.frame()
df_cp_d$D[df_cp_d$G == 0] <- 0

cd_cp_d <- tryCatch(
  cont_did(
    yname = "Y", tname = "t_idx", idname = "id",
    dname = "D", gname = "G", data = df_cp_d,
    target_parameter = "slope", aggregation = "dose",
    treatment_type = "continuous", control_group = "notyettreated",
    degree = 1, num_knots = 0,
    biters = 1000, cband = TRUE, print_details = FALSE
  ),
  error = function(e) { cat(sprintf("  ERROR: %s\n", e$message)); NULL }
)
if (!is.null(cd_cp_d)) {
  cat("\n  F_CP dose -> Debt (ACRT):\n")
  summary(cd_cp_d)
}

# --- 2D: DI as dose -> Debt ---
cat("\n--- 2D: F_DI as dose -> Debt ---\n")

df_di_d <- panel %>%
  filter(!is.na(DebtR_share2019)) %>%
  left_join(g_fiscal %>% select(Country, G_fiscal), by = "Country") %>%
  select(id, t_idx, Y = DebtR_share2019, D = F_DI_cum, G = G_fiscal) %>%
  as.data.frame()
df_di_d$D[df_di_d$G == 0] <- 0

cd_di_d <- tryCatch(
  cont_did(
    yname = "Y", tname = "t_idx", idname = "id",
    dname = "D", gname = "G", data = df_di_d,
    target_parameter = "slope", aggregation = "dose",
    treatment_type = "continuous", control_group = "notyettreated",
    degree = 1, num_knots = 0,
    biters = 1000, cband = TRUE, print_details = FALSE
  ),
  error = function(e) { cat(sprintf("  ERROR: %s\n", e$message)); NULL }
)
if (!is.null(cd_di_d)) {
  cat("\n  F_DI dose -> Debt (ACRT):\n")
  summary(cd_di_d)
}


# ==============================================================================
#  APPROACH 3: RESIDUALIZED OUTCOMES
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  APPROACH 3: RESIDUALIZED OUTCOMES (Partial Out Fiscal Effects)\n")
cat(strrep("=", 70), "\n\n")

cat(paste0(
  "  LOGIC: Remove variation in Y explained by fiscal policy.\n",
  "  Step 1: Regress Y on F_CP + F_DI + F_H + quarter FE (post only)\n",
  "  Step 2: Use residuals as fiscal-adjusted outcome\n",
  "  Step 3: Re-estimate cont_did with stringency dose\n",
  "  If the ACRT changes substantially, fiscal composition confounds the\n",
  "  stringency dose-response. If stable, the effects are separable.\n\n"
))

# Residualize output gap
panel_post <- panel %>% filter(t_idx >= 5)
m_resid_y <- lm(y_t_pct ~ F_CP + F_DI + F_H + factor(t_idx), data = panel_post)
cat("  Fiscal regression (y_t_pct ~ F_CP + F_DI + F_H + quarter FE):\n")
print(coeftest(m_resid_y, vcov = vcovHC(m_resid_y, type = "HC1"))[1:3, ])

panel$y_resid <- panel$y_t_pct
panel$y_resid[panel$t_idx >= 5] <- residuals(m_resid_y) +
  mean(panel_post$y_t_pct, na.rm = TRUE)

# Residualize debt
m_resid_d <- lm(DebtR_share2019 ~ F_CP + F_DI + F_H + factor(t_idx),
                 data = panel_post %>% filter(!is.na(DebtR_share2019)))
cat("\n  Fiscal regression (Debt ~ F_CP + F_DI + F_H + quarter FE):\n")
print(coeftest(m_resid_d, vcov = vcovHC(m_resid_d, type = "HC1"))[1:3, ])

panel$d_resid <- panel$DebtR_share2019
valid_d <- panel$t_idx >= 5 & !is.na(panel$DebtR_share2019)
panel$d_resid[valid_d] <- residuals(m_resid_d) +
  mean(panel$DebtR_share2019[valid_d], na.rm = TRUE)

# Re-estimate with residualized outcomes
cat("\n--- Residualized Output Gap (stringency dose) ---\n")
df_yr <- panel %>%
  filter(!is.na(y_resid)) %>%
  select(id, t_idx, Y = y_resid, D = dose_mean, G) %>%
  as.data.frame()
df_yr$D[df_yr$G == 0] <- 0

cd_yr <- tryCatch(
  cont_did(
    yname = "Y", tname = "t_idx", idname = "id",
    dname = "D", gname = "G", data = df_yr,
    target_parameter = "slope", aggregation = "dose",
    treatment_type = "continuous", control_group = "notyettreated",
    degree = 1, num_knots = 0,
    biters = 1000, cband = TRUE, print_details = FALSE
  ),
  error = function(e) { cat(sprintf("  ERROR: %s\n", e$message)); NULL }
)
if (!is.null(cd_yr)) {
  cat("\n  Residualized ACRT (Output Gap):\n")
  summary(cd_yr)
}

cat("\n--- Residualized Debt (stringency dose) ---\n")
df_dr <- panel %>%
  filter(!is.na(d_resid)) %>%
  select(id, t_idx, Y = d_resid, D = dose_mean, G) %>%
  as.data.frame()
df_dr$D[df_dr$G == 0] <- 0

cd_dr <- tryCatch(
  cont_did(
    yname = "Y", tname = "t_idx", idname = "id",
    dname = "D", gname = "G", data = df_dr,
    target_parameter = "slope", aggregation = "dose",
    treatment_type = "continuous", control_group = "notyettreated",
    degree = 1, num_knots = 0,
    biters = 1000, cband = TRUE, print_details = FALSE
  ),
  error = function(e) { cat(sprintf("  ERROR: %s\n", e$message)); NULL }
)
if (!is.null(cd_dr)) {
  cat("\n  Residualized ACRT (Debt):\n")
  summary(cd_dr)
}


# ==============================================================================
#  APPROACH 4: REGRESSION-BASED INTERACTION (supplementary)
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  APPROACH 4: REGRESSION INTERACTION (Dose x Fiscal Composition)\n")
cat(strrep("=", 70), "\n\n")

cat(paste0(
  "  LOGIC: Use a standard panel regression with dose x CP_share interaction.\n",
  "  This tests whether fiscal composition MODERATES the dose-response.\n",
  "  Not a formal DiD estimator, but captures the interaction cleanly.\n\n"
))

# Base period outcome
base_y <- panel %>%
  filter(Quarter == "Q4.2019") %>%
  select(Country, y_base = y_t_pct, d_base = DebtR_share2019)

reg_data <- panel %>%
  filter(t_idx >= 5) %>%
  left_join(base_y, by = "Country") %>%
  left_join(cp_groups %>% select(Country, CP_share, F_CP_cum, F_DI_cum), by = "Country") %>%
  mutate(
    dy = y_t_pct - y_base,
    dd = DebtR_share2019 - d_base
  )

# Output gap: dose x CP_share interaction
cat("--- Output Gap: dose x CP_share ---\n")
m_int_y <- feols(dy ~ dose_mean * CP_share + dose_mean * F_DI_cum | t_idx,
                 data = reg_data, cluster = ~Country)
print(summary(m_int_y))

cat("\n--- Debt: dose x CP_share ---\n")
m_int_d <- feols(dd ~ dose_mean * CP_share + dose_mean * F_DI_cum | t_idx,
                 data = reg_data %>% filter(!is.na(dd)), cluster = ~Country)
print(summary(m_int_d))

# Marginal effect of dose at different CP shares
cp_vals <- c(0.3, 0.5, 0.7)
cat("\n  Marginal effect of stringency dose on output gap at different CP shares:\n")
for (cp in cp_vals) {
  marg <- coef(m_int_y)["dose_mean"] + coef(m_int_y)["dose_mean:CP_share"] * cp
  cat(sprintf("    CP_share = %.1f: dY/dDose = %+.4f\n", cp, marg))
}

cat("\n  Marginal effect of stringency dose on debt at different CP shares:\n")
for (cp in cp_vals) {
  marg <- coef(m_int_d)["dose_mean"] + coef(m_int_d)["dose_mean:CP_share"] * cp
  cat(sprintf("    CP_share = %.1f: dDebt/dDose = %+.4f\n", cp, marg))
}


# ==============================================================================
#  SUMMARY
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SUMMARY: FISCAL COMPOSITION EFFECTS\n")
cat(strrep("=", 70), "\n\n")

cat(paste0(
  "  COMPARISON TABLE:\n",
  "  ----------------------------------------------------------------\n",
  "  Specification                 | ACRT (y_k)  | ACRT (b_k)\n",
  "  ----------------------------------------------------------------\n"
))

# Collect results
specs <- list()
if (!is.null(cd_yr)) specs[["Residualized (fiscal-adj)"]] <- c(cd_yr, cd_dr)

cat(paste0(
  "  See above for detailed results per approach.\n\n",
  "  KEY QUESTIONS ANSWERED:\n",
  "  1. Does fiscal composition confound the stringency dose-response?\n",
  "     -> Compare raw vs residualized ACRT. If similar, NO confounding.\n\n",
  "  2. Does CP cushion the output cost of stringency?\n",
  "     -> Subsample: High-CP ACRT should be LESS negative than Low-CP.\n",
  "     -> Interaction: dose x CP_share should be POSITIVE (CP mitigates).\n\n",
  "  3. Does CP drive debt accumulation?\n",
  "     -> F_CP dose-ACRT on debt should be POSITIVE and significant.\n",
  "     -> Subsample: High-CP should show MORE debt per unit stringency.\n\n",
  "  4. Why might more S NOT lead to more debt?\n",
  "     -> Higher stringency may reduce GDP (automatic stabilizer costs rise)\n",
  "       but also reduce virus spread -> faster recovery -> more revenue.\n",
  "     -> Countries with high S that also deployed CP spent more,\n",
  "       but preserved capacity -> faster recovery offset initial debt.\n",
  "     -> The net effect is ambiguous, explaining the insignificant ACRT.\n\n"
))

cat("DONE.\n")
