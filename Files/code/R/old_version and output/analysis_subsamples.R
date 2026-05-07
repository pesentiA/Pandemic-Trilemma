# =============================================================================
#  SUBSAMPLE ROBUSTNESS ANALYSIS
#  Runs the main output gap and debt specifications on meaningful splits
#  to test whether fiscal transmission mechanisms differ across country types.
#
#  Splits:
#  1. Advanced (high income) vs Emerging (low income) — GDP pc median
#  2. Strong vs Weak social safety nets — welfare state typology
#  3. High vs Low pre-COVID stringency — tests CP relevance
#  4. High vs Low pre-COVID debt — fiscal space constraint
# =============================================================================

rm(list = ls())

packages_vector <- c("dplyr", "fixest", "plm", "lmtest", "sandwich",
                      "tidyr", "stringr", "readxl", "lubridate",
                      "conflicted", "ggplot2", "patchwork")
lapply(packages_vector, require, character.only = TRUE)

conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflicts_prefer(dplyr::lag)
conflicted::conflicts_prefer(dplyr::lead)
conflicted::conflicts_prefer(dplyr::first)
conflicted::conflicts_prefer(dplyr::last)
conflicted::conflicts_prefer(dplyr::coalesce)
conflicted::conflicts_prefer(dplyr::recode)
conflicted::conflicts_prefer(lubridate::intersect)
conflicted::conflicts_prefer(lubridate::quarter)
conflicted::conflicts_prefer(lubridate::year)
conflicted::conflicts_prefer(lubridate::wday)
conflicted::conflicts_prefer(data.table::isoweek)
conflicted::conflicts_prefer(data.table::month)
conflicted::conflicts_prefer(dplyr::lead)
conflicted::conflicts_prefer(dplyr::between)

safedata <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/processed"
load(file.path(safedata, "dataforanalysis.RData"))

safeplots <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/output/figures"

set.seed(1234)

fm_path <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/raw/fiscal measures/fiscal_classified_v1_7.xlsx"

pandemic_qs <- c(
  "Q1.2019","Q2.2019","Q3.2019","Q4.2019",
  "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
  "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
  "Q1.2022","Q2.2022","Q3.2022","Q4.2022"
)

# =============================================================================
#  PANEL CONSTRUCTION (replicates analysis.R)
# =============================================================================

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
         y_t_pct, DebtR_share2019, DebtN_share2019,
         Qpopulation_th, inflation_index, vax_rate,
         rGDP_pc_2019, debt_2019, d_t_pct,
         StringencyIndex_PopWeighted)

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
    F_total = sum(broad_fiscal_gdp, na.rm = TRUE),
    .groups = "drop"
  )

# CP sub-components
df_fiscal_cp <- fm1 %>%
  filter(broad_fiscal != 0) %>%
  mutate(
    Quarter = paste0("Q", Quarter, ".", Year),
    CP_above = ifelse(transmission_channel == "CP" & category == 1, broad_fiscal_gdp, 0),
    CP_loans = ifelse(transmission_channel == "CP" & category == 2 &
                        PolicyCode %in% c("40", "41"), broad_fiscal_gdp, 0),
    CP_guar  = ifelse(transmission_channel == "CP" & category == 2 &
                        PolicyCode == "43", broad_fiscal_gdp, 0)
  ) %>%
  filter(Quarter %in% pandemic_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(
    F_CP_above = sum(CP_above, na.rm = TRUE),
    F_CP_loans = sum(CP_loans, na.rm = TRUE),
    F_CP_guar  = sum(CP_guar,  na.rm = TRUE),
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
  left_join(df_fiscal_cp,  by = c("Country", "Quarter")) %>%
  left_join(df_stringency, by = c("Country", "Quarter")) %>%
  mutate(
    quarter_num = as.integer(str_sub(Quarter, 2, 2)),
    Quarter     = factor(Quarter, levels = pandemic_qs, ordered = TRUE),
    across(c(starts_with("F_"), "S_mean_pw"), ~replace_na(., 0))
  ) %>%
  arrange(Country, Quarter)

pdata <- df %>%
  mutate(S_mean_tw = S_mean_pw * 100,
         F_CP = F_CP * 100, F_DI = F_DI * 100, F_H = F_H * 100,
         F_CP_above = F_CP_above * 100, F_CP_loans = F_CP_loans * 100,
         F_CP_guar = F_CP_guar * 100,
         vax_rate = vax_rate * 100,
         theta_pct = theta_mean * 100,
         Qpopulation_th = Qpopulation_th / 1000) %>%
  mutate(
    F_DI_lag1 = lag(F_DI, 1),
    F_DI_lag2 = lag(F_DI, 2),
    F_CP_lag1 = lag(F_CP, 1),
    y_lag1    = lag(y_t_pct, 1),
    F_CP_below_adj_mid = (F_CP_loans + F_CP_guar) * 0.35
  )

# Output panel
pdataY <- pdata %>%
  filter(Quarter %in% c("Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                         "Q1.2021","Q2.2021","Q3.2021","Q4.2021","Q1.2022"))

# Debt panel
pdata <- pdata %>%
  group_by(Country) %>%
  mutate(debt_dR = DebtR_share2019 - dplyr::lag(DebtR_share2019, 1)) %>%
  ungroup()

pdataD <- pdata %>%
  filter(Quarter %in% c("Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                         "Q1.2021","Q2.2021","Q3.2021","Q4.2021","Q1.2022")) %>%
  group_by(Country) %>%
  mutate(debt_dR = DebtR_share2019 - dplyr::lag(DebtR_share2019, 1)) %>%
  ungroup()


# =============================================================================
#  DEFINE SUBSAMPLE SPLITS
# =============================================================================

# Country-level characteristics (from Q4.2019)
cty_chars <- pdata %>%
  filter(Quarter == "Q4.2019") %>%
  select(Country, rGDP_pc_2019, debt_2019) %>%
  distinct()

# Average pandemic stringency
avg_S <- pdata %>%
  filter(Quarter %in% c("Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                         "Q1.2021","Q2.2021","Q3.2021","Q4.2021")) %>%
  group_by(Country) %>%
  summarise(avg_stringency = mean(S_mean_tw, na.rm = TRUE), .groups = "drop")

cty_chars <- cty_chars %>% left_join(avg_S, by = "Country")

# --- SPLIT 1: Advanced vs Emerging (GDP per capita median) ---
gdp_median <- median(cty_chars$rGDP_pc_2019, na.rm = TRUE)
cty_chars$income_group <- ifelse(cty_chars$rGDP_pc_2019 >= gdp_median, "AE", "EM")

# --- SPLIT 2: Welfare State Typology ---
# Nordic/Continental (strong safety nets) vs Liberal/Southern/Eastern (weaker)
# Based on Esping-Andersen (1990) + extensions for OECD
strong_safety <- c("AUT","BEL","CHE","DEU","DNK","FIN","FRA","ISL",
                   "LUX","NLD","NOR","SWE")
weak_safety   <- c("AUS","CAN","CHL","COL","CRI","CZE","ESP","EST",
                    "GBR","GRC","HUN","IRL","ISR","ITA","JPN","KOR",
                    "LTU","LVA","MEX","NZL","POL","PRT","SVK","SVN",
                    "TUR","USA")
cty_chars$safety_net <- ifelse(cty_chars$Country %in% strong_safety,
                                "Strong", "Weak")

# --- SPLIT 3: High vs Low pandemic stringency ---
s_median <- median(cty_chars$avg_stringency, na.rm = TRUE)
cty_chars$stringency_group <- ifelse(cty_chars$avg_stringency >= s_median,
                                      "High_S", "Low_S")

# --- SPLIT 4: High vs Low pre-COVID debt (debt/GDP ratio) ---
# Use debt_2019 normalized by GDP — but debt_2019 is in local currency levels
# Use the debt_2019 share from the panel instead
debt_share_2019 <- pdata %>%
  filter(Quarter == "Q4.2019") %>%
  select(Country, DebtR_share2019) %>%
  distinct()
cty_chars <- cty_chars %>% left_join(debt_share_2019, by = "Country")
debt_median <- median(cty_chars$DebtR_share2019, na.rm = TRUE)
cty_chars$debt_group <- ifelse(cty_chars$DebtR_share2019 >= debt_median,
                                "High_Debt", "Low_Debt")

# Print groups
cat(strrep("=", 70), "\n")
cat("  SUBSAMPLE DEFINITIONS\n")
cat(strrep("=", 70), "\n\n")

cat(sprintf("  SPLIT 1 — Income (median GDP_pc = %.0f):\n", gdp_median))
cat("    AE (", sum(cty_chars$income_group == "AE"), "): ",
    paste(sort(cty_chars$Country[cty_chars$income_group == "AE"]), collapse = ", "), "\n")
cat("    EM (", sum(cty_chars$income_group == "EM"), "): ",
    paste(sort(cty_chars$Country[cty_chars$income_group == "EM"]), collapse = ", "), "\n\n")

cat("  SPLIT 2 — Social Safety Net (Esping-Andersen typology):\n")
cat("    Strong (", sum(cty_chars$safety_net == "Strong"), "): ",
    paste(sort(cty_chars$Country[cty_chars$safety_net == "Strong"]), collapse = ", "), "\n")
cat("    Weak   (", sum(cty_chars$safety_net == "Weak"), "): ",
    paste(sort(cty_chars$Country[cty_chars$safety_net == "Weak"]), collapse = ", "), "\n\n")

cat(sprintf("  SPLIT 3 — Pandemic Stringency (median = %.1f):\n", s_median))
cat("    High_S (", sum(cty_chars$stringency_group == "High_S"), "): ",
    paste(sort(cty_chars$Country[cty_chars$stringency_group == "High_S"]), collapse = ", "), "\n")
cat("    Low_S  (", sum(cty_chars$stringency_group == "Low_S"), "): ",
    paste(sort(cty_chars$Country[cty_chars$stringency_group == "Low_S"]), collapse = ", "), "\n\n")

cat(sprintf("  SPLIT 4 — Pre-COVID Debt (median = %.1f%% of 2019 GDP):\n", debt_median * 100))
cat("    High (", sum(cty_chars$debt_group == "High_Debt"), "): ",
    paste(sort(cty_chars$Country[cty_chars$debt_group == "High_Debt"]), collapse = ", "), "\n")
cat("    Low  (", sum(cty_chars$debt_group == "Low_Debt"), "): ",
    paste(sort(cty_chars$Country[cty_chars$debt_group == "Low_Debt"]), collapse = ", "), "\n\n")

# Merge group labels into panels
pdataY <- pdataY %>% left_join(cty_chars %>% select(Country, income_group, safety_net, stringency_group, debt_group), by = "Country")
pdataD <- pdataD %>% left_join(cty_chars %>% select(Country, income_group, safety_net, stringency_group, debt_group), by = "Country")


# =============================================================================
#  ESTIMATION FUNCTIONS
# =============================================================================

run_output_model <- function(data, label) {
  n_cty <- n_distinct(data$Country)
  if (n_cty < 5) { cat(sprintf("  SKIP %s: only %d countries\n", label, n_cty)); return(NULL) }
  m <- tryCatch(
    plm(y_t_pct ~ S_mean_tw * y_lag1 + S_mean_tw * F_CP + F_DI_lag2,
        data = data, model = "within", effect = "twoways"),
    error = function(e) NULL)
  if (is.null(m)) { cat(sprintf("  FAIL %s\n", label)); return(NULL) }
  ct <- coeftest(m, vcov = vcovHC(m, cluster = "group", type = "HC1"))
  list(label = label, ct = ct, model = m, n_cty = n_cty, nobs = nobs(m))
}

run_debt_model <- function(data, label) {
  n_cty <- n_distinct(data$Country)
  if (n_cty < 5) { cat(sprintf("  SKIP %s: only %d countries\n", label, n_cty)); return(NULL) }
  m <- tryCatch(
    plm(debt_dR ~ y_t_pct + F_CP + F_DI_lag1 + as.numeric(Quarter),
        data = data, model = "within", effect = "individual"),
    error = function(e) NULL)
  if (is.null(m)) { cat(sprintf("  FAIL %s\n", label)); return(NULL) }
  ct <- coeftest(m, vcov = vcovHC(m, cluster = "group", type = "HC1"))
  list(label = label, ct = ct, model = m, n_cty = n_cty, nobs = nobs(m))
}

# Formatted coefficient extraction
get_coef <- function(res, varname) {
  if (is.null(res)) return(list(est = NA, se = NA, pv = NA, str = "         ---"))
  idx <- which(rownames(res$ct) == varname)
  if (length(idx) == 0) return(list(est = NA, se = NA, pv = NA, str = "         ---"))
  est <- res$ct[idx, 1]; se <- res$ct[idx, 2]; pv <- res$ct[idx, 4]
  stars <- ifelse(pv<0.001,"***",ifelse(pv<0.01,"** ",ifelse(pv<0.05,"*  ",ifelse(pv<0.1,".  ","   "))))
  list(est = est, se = se, pv = pv, str = sprintf("%8.4f%s", est, stars))
}


# =============================================================================
#  RUN ALL SUBSAMPLES
# =============================================================================

splits <- list(
  list(name = "Income",     var = "income_group",    levels = c("AE", "EM")),
  list(name = "Safety Net",  var = "safety_net",      levels = c("Strong", "Weak")),
  list(name = "Stringency",  var = "stringency_group", levels = c("High_S", "Low_S")),
  list(name = "Debt",        var = "debt_group",      levels = c("High_Debt", "Low_Debt"))
)

# Store all results
all_output <- list()
all_debt   <- list()

# Full sample baseline
all_output[["Full"]] <- run_output_model(pdataY, "Full Sample")
all_debt[["Full"]]   <- run_debt_model(pdataD, "Full Sample")

for (sp in splits) {
  for (lv in sp$levels) {
    lab <- paste0(sp$name, ": ", lv)
    sub_Y <- pdataY %>% filter(.data[[sp$var]] == lv)
    sub_D <- pdataD %>% filter(.data[[sp$var]] == lv)
    all_output[[lab]] <- run_output_model(sub_Y, lab)
    all_debt[[lab]]   <- run_debt_model(sub_D, lab)
  }
}


# =============================================================================
#  OUTPUT GAP RESULTS TABLE
# =============================================================================
cat("\n\n")
cat(strrep("#", 80), "\n")
cat("  OUTPUT GAP EQUATION: y ~ S*y_lag1 + S*F_CP + F_DI_lag2 | TWFE\n")
cat(strrep("#", 80), "\n\n")

output_vars <- c("F_CP", "S_mean_tw:F_CP", "F_DI_lag2", "S_mean_tw", "S_mean_tw:y_lag1")
output_labels <- c("F_CP (level)", "S * F_CP", "F_DI (lag 2)", "S (level)", "S * y_lag1")

cat(sprintf("%-22s  %4s  %4s", "Subsample", "N_c", "N"))
for (vl in output_labels) cat(sprintf("  %13s", vl))
cat("\n", strrep("-", 22 + 10 + 13 * length(output_vars)), "\n")

for (nm in names(all_output)) {
  res <- all_output[[nm]]
  if (is.null(res)) next
  cat(sprintf("%-22s  %4d  %4d", nm, res$n_cty, res$nobs))
  for (v in output_vars) cat(sprintf("  %13s", get_coef(res, v)$str))
  cat("\n")
}


# =============================================================================
#  DEBT EQUATION RESULTS TABLE
# =============================================================================
cat("\n\n")
cat(strrep("#", 80), "\n")
cat("  DEBT EQUATION: debt_dR ~ y + F_CP + F_DI_lag1 + t | Country FE\n")
cat(strrep("#", 80), "\n\n")

debt_vars <- c("y_t_pct", "F_CP", "F_DI_lag1", "as.numeric(Quarter)")
debt_labels <- c("y_t_pct", "F_CP", "F_DI (lag 1)", "Quarter trend")

cat(sprintf("%-22s  %4s  %4s", "Subsample", "N_c", "N"))
for (vl in debt_labels) cat(sprintf("  %13s", vl))
cat("\n", strrep("-", 22 + 10 + 13 * length(debt_vars)), "\n")

for (nm in names(all_debt)) {
  res <- all_debt[[nm]]
  if (is.null(res)) next
  cat(sprintf("%-22s  %4d  %4d", nm, res$n_cty, res$nobs))
  for (v in debt_vars) cat(sprintf("  %13s", get_coef(res, v)$str))
  cat("\n")
}


# =============================================================================
#  DETAILED SUBSAMPLE COMPARISON: PRINT FULL COEFTEST FOR EACH
# =============================================================================
cat("\n\n")
cat(strrep("#", 80), "\n")
cat("  DETAILED RESULTS BY SUBSAMPLE\n")
cat(strrep("#", 80), "\n")

for (sp in splits) {
  cat(sprintf("\n\n%s\n  SPLIT: %s\n%s\n",
              strrep("=", 70), sp$name, strrep("=", 70)))

  for (lv in sp$levels) {
    lab <- paste0(sp$name, ": ", lv)

    cat(sprintf("\n  --- OUTPUT: %s (%d countries) ---\n", lab,
                ifelse(is.null(all_output[[lab]]), 0, all_output[[lab]]$n_cty)))
    if (!is.null(all_output[[lab]])) print(all_output[[lab]]$ct)

    cat(sprintf("\n  --- DEBT: %s (%d countries) ---\n", lab,
                ifelse(is.null(all_debt[[lab]]), 0, all_debt[[lab]]$n_cty)))
    if (!is.null(all_debt[[lab]])) print(all_debt[[lab]]$ct)
  }
}


# =============================================================================
#  LP IRFs BY SUBSAMPLE (debt only — strongest LP result)
# =============================================================================
cat("\n\n")
cat(strrep("#", 80), "\n")
cat("  LP DEBT IRFs BY SUBSAMPLE\n")
cat(strrep("#", 80), "\n")

H_max <- 5

# LP panel from pdata (wider range)
lp_base <- pdata %>%
  arrange(Country, year_only, quarter_only) %>%
  group_by(Country) %>%
  mutate(
    dd_h0 = DebtR_share2019 - dplyr::lag(DebtR_share2019, 1),
    dd_h1 = dplyr::lead(DebtR_share2019, 1) - dplyr::lag(DebtR_share2019, 1),
    dd_h2 = dplyr::lead(DebtR_share2019, 2) - dplyr::lag(DebtR_share2019, 1),
    dd_h3 = dplyr::lead(DebtR_share2019, 3) - dplyr::lag(DebtR_share2019, 1),
    dd_h4 = dplyr::lead(DebtR_share2019, 4) - dplyr::lag(DebtR_share2019, 1),
    dd_h5 = dplyr::lead(DebtR_share2019, 5) - dplyr::lag(DebtR_share2019, 1)
  ) %>%
  ungroup() %>%
  left_join(cty_chars %>% select(Country, income_group, safety_net, stringency_group, debt_group),
            by = "Country")

dd_vars <- paste0("dd_h", 0:H_max)

run_lp_debt <- function(data, label) {
  results <- tibble()
  for (h in 0:H_max) {
    dv <- dd_vars[h + 1]
    if (!dv %in% names(data)) next
    fml <- as.formula(paste0(dv, " ~ F_CP + F_DI + y_t_pct + S_mean_tw | Country"))
    m <- tryCatch(feols(fml, data = data, cluster = "Country"), error = function(e) NULL)
    if (is.null(m)) next
    ct <- summary(m)$coeftable
    for (v in c("F_CP", "F_DI")) {
      if (v %in% rownames(ct)) {
        results <- bind_rows(results, tibble(
          h = h, variable = v, coef = ct[v, "Estimate"],
          se = ct[v, "Std. Error"], pval = ct[v, "Pr(>|t|)"],
          label = label
        ))
      }
    }
  }
  return(results)
}

# Run LP for all subsamples
lp_results <- tibble()
lp_results <- bind_rows(lp_results, run_lp_debt(lp_base, "Full Sample"))

for (sp in splits) {
  for (lv in sp$levels) {
    lab <- paste0(sp$name, ": ", lv)
    sub <- lp_base %>% filter(.data[[sp$var]] == lv)
    if (n_distinct(sub$Country) >= 5) {
      lp_results <- bind_rows(lp_results, run_lp_debt(sub, lab))
    }
  }
}

# Print LP debt IRF comparison
for (v in c("F_CP", "F_DI")) {
  cat(sprintf("\n  --- LP Debt IRF: %s ---\n", v))
  sub <- lp_results %>% filter(variable == v)
  labels_lp <- unique(sub$label)

  cat(sprintf("  %3s", "h"))
  for (lb in labels_lp) cat(sprintf("  %18s", substr(lb, 1, 18)))
  cat("\n  ", strrep("-", 3 + 20 * length(labels_lp)), "\n")

  for (h in 0:H_max) {
    cat(sprintf("  %3d", h))
    for (lb in labels_lp) {
      row <- sub %>% filter(h == !!h, label == lb)
      if (nrow(row) == 0) { cat(sprintf("  %18s", "---")); next }
      stars <- ifelse(row$pval<0.01,"**",ifelse(row$pval<0.05,"* ",ifelse(row$pval<0.1,". ","  ")))
      cat(sprintf("  %8.4f%s %s", row$coef, stars, strrep(" ", 7)))
    }
    cat("\n")
  }
}


# =============================================================================
#  INTERPRETATION AND CONCLUSIONS
# =============================================================================
cat("\n\n")
cat(strrep("#", 80), "\n")
cat("  INTERPRETATION: SUBSAMPLE HETEROGENEITY\n")
cat(strrep("#", 80), "\n\n")

cat("
  1. INCOME SPLIT (AE vs EM):
     - CP effectiveness (S*F_CP): Compare the interaction coefficient across
       groups. If CP is more effective in AE, this suggests that institutional
       capacity (rule of law, efficient bureaucracy) amplifies the capacity
       preservation channel. If similar, the mechanism is universal.
     - DI lag structure: EM countries may show different DI timing due to
       weaker transfer infrastructure (slower disbursement, lower coverage).
     - Debt: AE countries typically have deeper capital markets, so the debt
       effect may be larger (more borrowing capacity) or smaller (lower
       risk premia allow cheaper financing).

  2. SOCIAL SAFETY NET SPLIT (Strong vs Weak):
     - The key hypothesis: countries with strong pre-existing safety nets
       (Nordic/Continental) should show WEAKER DI effects because the
       automatic stabilizers already provide income support, making
       discretionary DI redundant. Conversely, CP should be equally
       effective regardless of safety net strength because it operates
       through a different channel (firm-side, not household-side).
     - If DI is significant only in weak-safety-net countries, this supports
       the complementarity argument: DI substitutes for missing automatic
       stabilizers.
     - If CP is significant in both groups, this confirms it as a genuinely
       new policy channel distinct from traditional social protection.

  3. STRINGENCY SPLIT (High vs Low):
     - By construction, CP should matter MORE in high-stringency countries
       (the S*F_CP interaction is the mechanism). This split tests whether
       CP has any effect in low-stringency environments.
     - If CP is insignificant in low-stringency countries, this confirms
       that capacity preservation is a lockdown-specific policy tool.
     - DI should be less sensitive to stringency (it operates through the
       income channel regardless of lockdown intensity).

  4. DEBT SPLIT (High vs Low pre-COVID debt):
     - Tests the fiscal space hypothesis: do high-debt countries face a
       higher marginal cost of fiscal intervention?
     - If kappa_CP is higher in high-debt countries, this suggests that
       pre-existing fiscal stress amplifies the debt cost of crisis spending
       (risk premium channel, rollover costs).
     - If the output effect of CP is weaker in high-debt countries, this
       suggests a fiscal credibility constraint: markets discount the
       effectiveness of fiscal support when the sovereign is already
       stressed.
")

cat("\n========== SUBSAMPLE ANALYSIS COMPLETE ==========\n")
