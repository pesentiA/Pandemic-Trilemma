# ============================================================================
# CONTINUOUS DIFFERENCE-IN-DIFFERENCES USING THE contdid PACKAGE
# Callaway, Goodman-Bacon & Sant'Anna (2025)
# "Difference-in-Differences with a Continuous Treatment"
# ============================================================================
#
# PACKAGE:   contdid (Callaway, Goodman-Bacon & Sant'Anna)
# TREATMENT: StringencyIndex intensity (continuous dose, time-invariant)
# TIMING:    Staggered adoption — first quarter exceeding a stringency threshold
# OUTCOMES:  y_t_pct (Output Gap), debt_dR (Debt change, real)
# CONTROLS:  Fiscal Composition (F_CP, F_DI) via residualization
# ============================================================================

rm(list = ls())

# ==============================================================================
#  SECTION 0: SETUP
# ==============================================================================

packages <- c(
  "dplyr", "tidyr", "ggplot2", "fixest", "modelsummary",
  "data.table", "lubridate", "stringr", "purrr", "sandwich",
  "lmtest", "KernSmooth", "broom", "scales", "patchwork",
  "readr", "countrycode", "readxl"
)
lapply(packages, require, character.only = TRUE)

# Install contdid if not available
if (!requireNamespace("contdid", quietly = TRUE)) {
  devtools::install_github("bcallaway11/contdid")
}
library(contdid)

options(max.print = 99, scipen = 999)
set.seed(1234)

# Conflict resolution
conflicted::conflict_prefer("select",   "dplyr")
conflicted::conflict_prefer("filter",   "dplyr")
conflicted::conflicts_prefer(dplyr::lag)
conflicted::conflicts_prefer(dplyr::first)
conflicted::conflicts_prefer(dplyr::last)
conflicted::conflicts_prefer(lubridate::year)
conflicted::conflicts_prefer(lubridate::quarter)
conflicted::conflicts_prefer(lubridate::month)
conflicted::conflicts_prefer(fixest::pvalue)

# --- Paths ---
base_dir  <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files"
data_proc <- file.path(base_dir, "data/processed")
data_raw  <- file.path(base_dir, "data/raw")
safeplots <- file.path(base_dir, "output/figures")
safetable <- file.path(base_dir, "output/tables")

# --- Load main analysis data ---
load(file.path(data_proc, "dataforanalysis.RData"))

# --- Load fiscal measures ---
fm1 <- read_excel(file.path(data_raw, "fiscal measures/fiscal_classified_v1_7.xlsx"))
fm1 <- fm1 %>%
  mutate(
    YQ       = paste0("Q", Quarter, ".", Year),
    YQ_ord   = factor(YQ, levels = sort(unique(paste0("Q", Quarter, ".", Year))),
                       ordered = TRUE),
    size_pct = broad_fiscal_gdp * 100
  ) %>%
  filter(!PolicyCode %in% c(5, 6, 11, 12, 15, 16))

cat("\n", strrep("=", 70), "\n")
cat("  CONTINUOUS DiD — contdid package\n")
cat("  Callaway, Goodman-Bacon & Sant'Anna (2025)\n")
cat(strrep("=", 70), "\n\n")


# ==============================================================================
#  SECTION 1: DOSE CONSTRUCTION FROM DAILY OXFORD STRINGENCY DATA
# ==============================================================================

cat("--- SECTION 1: Dose Construction ---\n\n")

# --- 1A: Load daily Oxford stringency data ---
ox_daily <- read_csv(
  file.path(data_raw, "oxford stringency/Oxcnat.csv"),
  show_col_types = FALSE
)

# OECD ISO3 codes
oecd_iso3 <- c(
  "AUS","AUT","BEL","CAN","CHL","COL","CRI","CZE","DNK","EST",
  "FIN","FRA","DEU","GRC","HUN","ISL","IRL","ISR","ITA","JPN",
  "LVA","LTU","LUX","MEX","NLD","NZL","NOR","POL","PRT","SVK",
  "SVN","KOR","ESP","SWE","CHE","TUR","GBR","USA"
)

ox_daily <- ox_daily %>%
  filter(CountryCode %in% oecd_iso3, Jurisdiction == "NAT_TOTAL") %>%
  mutate(date = ymd(Date), Country = CountryCode) %>%
  select(Country, date, StringencyIndex_Average) %>%
  filter(!is.na(StringencyIndex_Average))

cat(sprintf("  Oxford daily data: %d obs, %d countries, %s to %s\n",
            nrow(ox_daily), n_distinct(ox_daily$Country),
            min(ox_daily$date), max(ox_daily$date)))

# --- 1B: Dose measures (country-level, time-invariant) ---
dose_start <- as.Date("2020-01-01")
dose_end   <- as.Date("2021-12-31")

dose_data <- ox_daily %>%
  filter(date >= dose_start, date <= dose_end) %>%
  group_by(Country) %>%
  summarise(
    dose_mean       = mean(StringencyIndex_Average, na.rm = TRUE),
    dose_cumul      = sum(StringencyIndex_Average, na.rm = TRUE) / 100,
    dose_peak       = max(StringencyIndex_Average, na.rm = TRUE),
    dose_first_wave = mean(StringencyIndex_Average[
      date <= as.Date("2020-06-30")], na.rm = TRUE),
    n_days = n(),
    .groups = "drop"
  )

# Dose duration: days above OECD-wide daily median
daily_median <- ox_daily %>%
  filter(date >= dose_start, date <= dose_end) %>%
  pull(StringencyIndex_Average) %>%
  median(na.rm = TRUE)

dose_dur <- ox_daily %>%
  filter(date >= dose_start, date <= dose_end) %>%
  group_by(Country) %>%
  summarise(dose_duration = sum(StringencyIndex_Average > daily_median,
                                na.rm = TRUE), .groups = "drop")
dose_data <- dose_data %>% left_join(dose_dur, by = "Country")

cat(sprintf("  OECD daily median stringency: %.1f\n", daily_median))
cat("  Dose summary:\n")
summary(dose_data$dose_mean)


# ==============================================================================
#  SECTION 2: TREATMENT TIMING (G) — STAGGERED ADOPTION
# ==============================================================================

cat("\n--- SECTION 2: Treatment Timing (G) ---\n\n")

# G = first QUARTER where the country's mean daily stringency exceeds
# a threshold. We try three thresholds to generate different amounts of
# staggering. Countries that never exceed the threshold are coded G = 0
# (never-treated → comparison group).

quarter_order <- c(
  "Q1.2019","Q2.2019","Q3.2019","Q4.2019",
  "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
  "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
  "Q1.2022","Q2.2022","Q3.2022","Q4.2022"
)

# Compute quarterly mean stringency from daily data
ox_quarterly <- ox_daily %>%
  mutate(
    yr = year(date),
    qt = quarter(date),
    Quarter = paste0("Q", qt, ".", yr)
  ) %>%
  filter(Quarter %in% quarter_order) %>%
  group_by(Country, Quarter) %>%
  summarise(S_q = mean(StringencyIndex_Average, na.rm = TRUE), .groups = "drop") %>%
  mutate(t_idx = match(Quarter, quarter_order))

# Function: compute G for a given threshold
compute_G <- function(threshold, label) {
  first_q <- ox_quarterly %>%
    filter(S_q >= threshold) %>%
    group_by(Country) %>%
    summarise(G = min(t_idx), .groups = "drop")

  # Countries that never exceed threshold → G = 0
  all_countries <- tibble(Country = oecd_iso3)
  g_df <- all_countries %>%
    left_join(first_q, by = "Country") %>%
    mutate(G = replace_na(G, 0L))

  cat(sprintf("  Threshold %d (%s):\n", threshold, label))
  cat(sprintf("    Never-treated (G=0): %d countries\n", sum(g_df$G == 0)))
  tab <- table(g_df$G)
  for (g in sort(unique(g_df$G[g_df$G > 0]))) {
    cat(sprintf("    G=%d (%s): %d countries\n", g, quarter_order[g], tab[as.character(g)]))
  }
  cat("\n")

  g_df[[paste0("G_", threshold)]] <- g_df$G
  return(g_df)
}

g25 <- compute_G(25, "any substantial policy")
g50 <- compute_G(50, "lockdown-level")
g60 <- compute_G(60, "heavy lockdown")

# Merge all G definitions
g_all <- g25 %>%
  select(Country, G_25) %>%
  left_join(g50 %>% select(Country, G_50), by = "Country") %>%
  left_join(g60 %>% select(Country, G_60), by = "Country")

cat("  Countries never reaching G=60 threshold (comparison group):\n")
cat("  ", paste(g_all$Country[g_all$G_60 == 0], collapse = ", "), "\n\n")


# ==============================================================================
#  SECTION 3: PANEL CONSTRUCTION FOR contdid
# ==============================================================================

cat("--- SECTION 3: Panel Construction ---\n\n")

# --- Build quarterly panel from analysis data ---
pandemic_qs <- quarter_order

df_qdata <- qdata %>%
  filter(Quarter %in% pandemic_qs) %>%
  select(
    Country, Quarter, year_only, quarter_only,
    y_t_pct, d_t_pct,
    DebtR_share2019, DebtN_share2019,
    Qpopulation_th, rGDP_pc_2019, debt_2019,
    StringencyIndex_PopWeighted, inflation_index,
    p_proj_all_ages, p_avg_all_ages
  )

# Population 2019
pop_2019 <- df_qdata %>%
  filter(Quarter == "Q4.2019") %>%
  select(Country, pop_2019 = Qpopulation_th) %>%
  distinct()
df_qdata <- df_qdata %>% left_join(pop_2019, by = "Country")

# Fiscal measures (quarterly)
df_fiscal <- fm1 %>%
  filter(broad_fiscal == 1) %>%
  mutate(Quarter = as.character(YQ_ord)) %>%
  filter(Quarter %in% pandemic_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(
    F_CP    = sum(broad_fiscal_gdp[transmission_channel == "CP"], na.rm = TRUE) * 100,
    F_DI    = sum(broad_fiscal_gdp[transmission_channel == "DI"], na.rm = TRUE) * 100,
    F_H     = sum(broad_fiscal_gdp[transmission_channel == "H"],  na.rm = TRUE) * 100,
    F_total = sum(broad_fiscal_gdp, na.rm = TRUE) * 100,
    .groups = "drop"
  )

# Merge
panel <- df_qdata %>%
  left_join(df_fiscal, by = c("Country", "Quarter")) %>%
  mutate(
    F_CP    = replace_na(F_CP, 0),
    F_DI    = replace_na(F_DI, 0),
    F_H     = replace_na(F_H,  0),
    F_total = replace_na(F_total, 0)
  ) %>%
  arrange(Country, Quarter)

# Time index and debt first-difference
panel <- panel %>%
  mutate(t_idx = match(Quarter, quarter_order)) %>%
  group_by(Country) %>%
  mutate(
    debt_dR = DebtR_share2019 - dplyr::lag(DebtR_share2019),
    debt_dN = DebtN_share2019 - dplyr::lag(DebtN_share2019)
  ) %>%
  ungroup()

# Merge dose and treatment timing
panel <- panel %>%
  left_join(dose_data, by = "Country") %>%
  left_join(g_all,     by = "Country")

# Numeric country ID (required by contdid)
country_ids <- panel %>%
  distinct(Country) %>%
  arrange(Country) %>%
  mutate(id = row_number())
panel <- panel %>% left_join(country_ids, by = "Country")

# Check balance
cat(sprintf("  Panel: %d obs, %d countries, %d quarters\n",
            nrow(panel), n_distinct(panel$Country), n_distinct(panel$Quarter)))
obs_per <- table(panel$Country)
cat(sprintf("  Balanced: min=%d, max=%d per country\n", min(obs_per), max(obs_per)))

# Drop countries with incomplete panels
full_T <- max(table(panel$Country))
complete_countries <- names(obs_per[obs_per == full_T])
panel_bal <- panel %>% filter(Country %in% complete_countries)
cat(sprintf("  Balanced panel: %d obs, %d countries\n",
            nrow(panel_bal), n_distinct(panel_bal$Country)))


# ==============================================================================
#  SECTION 4: ASSUMPTION CHECKS
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 4: ASSUMPTION CHECKS\n")
cat(strrep("=", 70), "\n\n")

# ---- 4A: Pre-trend parallel trends by dose group ----
cat("--- 4A: Pre-Trend Parallel Trends Test ---\n\n")

# Tercile groups for visual check
dose_terciles <- dose_data %>%
  mutate(dose_group = cut(dose_mean,
                          breaks = quantile(dose_mean, c(0, 1/3, 2/3, 1)),
                          labels = c("Low", "Medium", "High"),
                          include.lowest = TRUE)) %>%
  select(Country, dose_group)

pre_panel <- panel_bal %>%
  filter(t_idx <= 4) %>%
  left_join(dose_terciles, by = "Country")

# Visual: mean y_t_pct by quarter and dose group
pre_trends_y <- pre_panel %>%
  group_by(Quarter, dose_group) %>%
  summarise(mean_y = mean(y_t_pct, na.rm = TRUE),
            se_y   = sd(y_t_pct, na.rm = TRUE) / sqrt(n()),
            .groups = "drop")

fig_pretrend <- ggplot(pre_trends_y,
                       aes(x = Quarter, y = mean_y, color = dose_group,
                           group = dose_group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = mean_y - 1.96 * se_y,
                     ymax = mean_y + 1.96 * se_y), width = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = c("Low" = "#2166ac", "Medium" = "#f4a582",
                                "High" = "#b2182b"),
                     name = "Stringency\nDose Group") +
  labs(title    = "Pre-Trend Check: Output Gap by Stringency Dose (2019)",
       subtitle = "Parallel pre-trends required for Continuous DiD validity",
       x = NULL, y = "Output gap (pp of potential GDP)") +
  theme_bw(base_size = 11)

ggsave(file.path(safeplots, "cdid_pretrend_y.pdf"), fig_pretrend, width = 9, height = 5)
print(fig_pretrend)
cat("  -> Saved: cdid_pretrend_y.pdf\n")

# Formal test: regress pre-treatment DeltaY on dose
base_y <- panel_bal %>%
  filter(Quarter == "Q4.2019") %>%
  select(Country, y_base = y_t_pct)

pre_test <- panel_bal %>%
  filter(t_idx %in% 1:3) %>%
  left_join(base_y, by = "Country") %>%
  mutate(dy_pre = y_t_pct - y_base)

pt_test <- feols(dy_pre ~ dose_mean | t_idx, data = pre_test, cluster = ~Country)
cat("\n  Formal pre-trend test (dy_pre ~ dose_mean | quarter FE):\n")
print(summary(pt_test))
cat(sprintf("  dose_mean coefficient: %.4f (p = %.4f) — %s\n\n",
            coef(pt_test)["dose_mean"],
            pvalue(pt_test)["dose_mean"],
            ifelse(pvalue(pt_test)["dose_mean"] > 0.10,
                   "PASS: no differential pre-trend",
                   "CAUTION: differential pre-trend detected")))

# ---- 4B: Dose distribution ----
cat("--- 4B: Dose Distribution ---\n\n")

fig_dose <- dose_data %>%
  ggplot(aes(x = dose_mean)) +
  geom_histogram(aes(y = after_stat(density)), bins = 15,
                 fill = "#2166ac", alpha = 0.5, color = "white") +
  geom_density(linewidth = 1, color = "#b2182b") +
  geom_rug(alpha = 0.5) +
  labs(title = "Distribution of Stringency Dose (Mean Daily Index, 2020-2021)",
       x = "Mean Stringency Index (0-100)", y = "Density") +
  theme_bw(base_size = 11)

ggsave(file.path(safeplots, "cdid_dose_distribution.pdf"), fig_dose, width = 8, height = 5)
print(fig_dose)
cat("  -> Saved: cdid_dose_distribution.pdf\n\n")


# ==============================================================================
#  SECTION 5: contdid ESTIMATION — OUTPUT GAP (y_k)
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 5: contdid — OUTPUT GAP (y_k)\n")
cat(strrep("=", 70), "\n\n")

# ---- Helper: prepare contdid data ----
# contdid requires: yname, dname, gname, tname, idname
# D must be time-invariant; G = first treated period (0 = never-treated)

prep_contdid <- function(panel_data, outcome_var, dose_var, g_var,
                         fiscal_adjust = FALSE) {
  df <- panel_data %>%
    select(id, t_idx, Country,
           Y = all_of(outcome_var),
           D = all_of(dose_var),
           G = all_of(g_var),
           F_CP, F_DI, F_H) %>%
    filter(!is.na(Y), !is.na(D))

  # Optional: residualize Y w.r.t. fiscal variables (post-treatment only)
  if (fiscal_adjust) {
    post_rows <- df$t_idx >= 5  # Q1.2020+
    if (sum(post_rows) > 0) {
      m_resid <- lm(Y ~ F_CP + F_DI + F_H + factor(t_idx),
                     data = df[post_rows, ])
      df$Y[post_rows] <- residuals(m_resid) + mean(df$Y[post_rows], na.rm = TRUE)
    }
  }

  # Ensure D is truly time-invariant (take country-level value)
  d_country <- df %>%
    group_by(id) %>%
    summarise(D_fix = mean(D, na.rm = TRUE), .groups = "drop")
  df <- df %>%
    select(-D) %>%
    left_join(d_country, by = "id") %>%
    rename(D = D_fix)

  # contdid requires D = 0 for never-treated units (G = 0)
  df$D[df$G == 0] <- 0

  as.data.frame(df)
}

# ---- PRIMARY SPECIFICATION: G_60 (6 never-treated comparison countries) ----
# G_50 yields only 1 comparison unit — insufficient for B-spline estimation.
# G_60 provides 6 never-treated countries (EST, FIN, ISL, JPN, LVA, LUX)
# with genuine staggering among treated units (28 in Q2.2020, 1 Q4.2020,
# 2 Q1.2021, 1 Q4.2021).
# ALTERNATIVE: G_25 with control_group="notyettreated" exploits staggered
# timing (5 countries in Q1.2020, 33 in Q2.2020) without needing G=0 units.

# ---- 5A: ATT by dose (G_60, dose_mean) ----
cat("--- 5A: Primary — ATT by dose (G_60, dose_mean) ---\n\n")

df_y60 <- prep_contdid(panel_bal, "y_t_pct", "dose_mean", "G_60")

cat(sprintf("  Data: %d obs, %d units, G=0 (comparison): %d units\n",
            nrow(df_y60), n_distinct(df_y60$id),
            n_distinct(df_y60$id[df_y60$G == 0])))
cat(sprintf("  Treated groups: %s\n",
            paste(names(table(df_y60$G[df_y60$G > 0])), collapse = ", ")))

cd_y_att <- cont_did(
  yname            = "Y",
  tname            = "t_idx",
  idname           = "id",
  dname            = "D",
  gname            = "G",
  data             = df_y60,
  target_parameter = "level",
  aggregation      = "dose",
  treatment_type   = "continuous",
  control_group    = "notyettreated",
  dose_est_method  = "parametric",
  degree           = 1,
  num_knots        = 0,
  biters           = 1000,
  cband            = TRUE,
  print_details    = TRUE
)

cat("\n  === ATT by Dose (Output Gap) ===\n")
summary(cd_y_att)

fig_y_att <- ggcont_did(cd_y_att)
ggsave(file.path(safeplots, "cdid_contdid_att_y_dose.pdf"),
       fig_y_att, width = 9, height = 6)
cat("  -> Saved: cdid_contdid_att_y_dose.pdf\n\n")


# ---- 5B: ACRT by dose ----
cat("--- 5B: ACRT by dose (Output Gap) ---\n\n")

cd_y_acrt <- cont_did(
  yname            = "Y",
  tname            = "t_idx",
  idname           = "id",
  dname            = "D",
  gname            = "G",
  data             = df_y60,
  target_parameter = "slope",
  aggregation      = "dose",
  treatment_type   = "continuous",
  control_group    = "notyettreated",
  dose_est_method  = "parametric",
  degree           = 1,
  num_knots        = 0,
  biters           = 1000,
  cband            = TRUE,
  print_details    = TRUE
)

cat("\n  === ACRT by Dose (Output Gap) ===\n")
summary(cd_y_acrt)

fig_y_acrt <- ggcont_did(cd_y_acrt)
ggsave(file.path(safeplots, "cdid_contdid_acrt_y_dose.pdf"),
       fig_y_acrt, width = 9, height = 6)
cat("  -> Saved: cdid_contdid_acrt_y_dose.pdf\n\n")


# ---- 5C: Event study — ATT by exposure length ----
cat("--- 5C: Event Study ATT (Output Gap) ---\n\n")

cd_y_es_att <- cont_did(
  yname            = "Y",
  tname            = "t_idx",
  idname           = "id",
  dname            = "D",
  gname            = "G",
  data             = df_y60,
  target_parameter = "level",
  aggregation      = "eventstudy",
  treatment_type   = "continuous",
  control_group    = "notyettreated",
  dose_est_method  = "parametric",
  degree           = 1,
  num_knots        = 0,
  biters           = 1000,
  cband            = TRUE,
  print_details    = TRUE
)

cat("\n  === Event Study ATT (Output Gap) ===\n")
summary(cd_y_es_att)

fig_y_es_att <- ggcont_did(cd_y_es_att)
ggsave(file.path(safeplots, "cdid_contdid_es_att_y.pdf"),
       fig_y_es_att, width = 10, height = 6)
cat("  -> Saved: cdid_contdid_es_att_y.pdf\n\n")


# ---- 5D: Event study — ACRT by exposure length ----
cat("--- 5D: Event Study ACRT (Output Gap) ---\n\n")

cd_y_es_acrt <- cont_did(
  yname            = "Y",
  tname            = "t_idx",
  idname           = "id",
  dname            = "D",
  gname            = "G",
  data             = df_y60,
  target_parameter = "slope",
  aggregation      = "eventstudy",
  treatment_type   = "continuous",
  control_group    = "notyettreated",
  dose_est_method  = "parametric",
  degree           = 1,
  num_knots        = 0,
  biters           = 1000,
  cband            = TRUE,
  print_details    = TRUE
)

cat("\n  === Event Study ACRT (Output Gap) ===\n")
summary(cd_y_es_acrt)

fig_y_es_acrt <- ggcont_did(cd_y_es_acrt)
ggsave(file.path(safeplots, "cdid_contdid_es_acrt_y.pdf"),
       fig_y_es_acrt, width = 10, height = 6)
cat("  -> Saved: cdid_contdid_es_acrt_y.pdf\n\n")


# ---- 5E: Alternative — G_25 with not-yet-treated comparison ----
cat("--- 5E: Alternative — G_25, notyettreated (Output Gap ATT) ---\n\n")

df_y25 <- prep_contdid(panel_bal, "y_t_pct", "dose_mean", "G_25")
cat(sprintf("  G_25 staggering: %s\n",
            paste(names(table(df_y25$G)), table(df_y25$G), sep="=", collapse=", ")))

cd_y_att_g25 <- tryCatch({
  cont_did(
    yname = "Y", tname = "t_idx", idname = "id", dname = "D", gname = "G",
    data = df_y25,
    target_parameter = "level", aggregation = "dose",
    treatment_type = "continuous", control_group = "notyettreated",
    degree = 1, num_knots = 0,
    biters = 1000, cband = TRUE, print_details = TRUE
  )
}, error = function(e) {
  cat(sprintf("  G_25 ATT failed: %s\n", e$message))
  NULL
})

if (!is.null(cd_y_att_g25)) {
  cat("\n  === ATT by Dose (Output Gap, G_25) ===\n")
  summary(cd_y_att_g25)
  fig_y_att_g25 <- ggcont_did(cd_y_att_g25)
  ggsave(file.path(safeplots, "cdid_contdid_att_y_g25.pdf"),
         fig_y_att_g25, width = 9, height = 6)
  cat("  -> Saved: cdid_contdid_att_y_g25.pdf\n\n")
}


# ---- 5F: Nonparametric CCK ----
cat("--- 5F: Nonparametric ACRT (CCK method, Output Gap) ---\n\n")

cd_y_cck <- tryCatch({
  cont_did(
    yname = "Y", tname = "t_idx", idname = "id", dname = "D", gname = "G",
    data = df_y60,
    target_parameter = "slope", aggregation = "dose",
    treatment_type = "continuous", dose_est_method = "cck",
    control_group = "notyettreated",
    biters = 1000, cband = TRUE, print_details = TRUE
  )
}, error = function(e) {
  cat(sprintf("  CCK estimation failed: %s\n", e$message))
  cat("  (CCK requires larger samples. Proceeding with parametric.)\n\n")
  NULL
})

if (!is.null(cd_y_cck)) {
  cat("\n  === Nonparametric ACRT (Output Gap) ===\n")
  summary(cd_y_cck)
  fig_y_cck <- ggcont_did(cd_y_cck)
  ggsave(file.path(safeplots, "cdid_contdid_cck_y.pdf"),
         fig_y_cck, width = 9, height = 6)
  cat("  -> Saved: cdid_contdid_cck_y.pdf\n\n")
}


# ---- 5F: Spline flexibility robustness ----
cat("--- 5F: Spline Flexibility Robustness (Output Gap) ---\n\n")

spline_specs <- list(
  "Linear (deg=1, k=0)"    = list(degree = 1, num_knots = 0),
  "Quadratic (deg=2, k=0)" = list(degree = 2, num_knots = 0),
  "Cubic (deg=3, k=0)"     = list(degree = 3, num_knots = 0),
  "Cubic + 1 knot"         = list(degree = 3, num_knots = 1),
  "Cubic + 2 knots"        = list(degree = 3, num_knots = 2)
)

spline_results_y <- list()
for (nm in names(spline_specs)) {
  sp <- spline_specs[[nm]]
  cat(sprintf("  Estimating: %s ... ", nm))
  res <- tryCatch({
    cont_did(
      yname = "Y", tname = "t_idx", idname = "id", dname = "D", gname = "G",
      data = df_y25,
      target_parameter = "slope",
      aggregation      = "dose",
      treatment_type   = "continuous",
      control_group    = "notyettreated",
      dose_est_method  = "parametric",
      degree           = sp$degree,
      num_knots        = sp$num_knots,
      biters           = 500,
      cband            = FALSE,
      print_details    = FALSE
    )
  }, error = function(e) { cat(sprintf("FAILED: %s\n", e$message)); NULL })

  if (!is.null(res)) {
    spline_results_y[[nm]] <- res
    cat("OK\n")
  }
}
cat("\n")


# ==============================================================================
#  SECTION 6: contdid ESTIMATION — DEBT (b_k)
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 6: contdid — DEBT (b_k)\n")
cat(strrep("=", 70), "\n\n")

# For debt: use DebtR_share2019 (level, real debt as % of 2019 GDP)
# DiD computes DeltaY internally, so we can pass the level

# ---- 6A: ATT by dose (Debt) ----
cat("--- 6A: ATT by dose (Debt) ---\n\n")

df_d60 <- prep_contdid(panel_bal, "DebtR_share2019", "dose_mean", "G_60")

cd_d_att <- tryCatch({
  cont_did(
    yname            = "Y",
    tname            = "t_idx",
    idname           = "id",
    dname            = "D",
    gname            = "G",
    data             = df_d60,
    target_parameter = "level",
    aggregation      = "dose",
    treatment_type   = "continuous",
    control_group    = "notyettreated",
    dose_est_method  = "parametric",
    degree           = 3,
    num_knots        = 0,
    biters           = 1000,
    cband            = TRUE,
    print_details    = TRUE
  )
}, error = function(e) { cat(sprintf("  Error: %s\n", e$message)); NULL })

if (!is.null(cd_d_att)) {
  cat("\n  === ATT by Dose (Debt) ===\n")
  summary(cd_d_att)
  fig_d_att <- ggcont_did(cd_d_att)
  ggsave(file.path(safeplots, "cdid_contdid_att_d_dose.pdf"),
         fig_d_att, width = 9, height = 6)
  cat("  -> Saved: cdid_contdid_att_d_dose.pdf\n\n")
}


# ---- 6B: ACRT by dose (Debt) ----
cat("--- 6B: ACRT by dose (Debt) ---\n\n")

cd_d_acrt <- tryCatch({
  cont_did(
    yname            = "Y",
    tname            = "t_idx",
    idname           = "id",
    dname            = "D",
    gname            = "G",
    data             = df_d60,
    target_parameter = "slope",
    aggregation      = "dose",
    treatment_type   = "continuous",
    control_group    = "notyettreated",
    dose_est_method  = "parametric",
    degree           = 3,
    num_knots        = 0,
    biters           = 1000,
    cband            = TRUE,
    print_details    = TRUE
  )
}, error = function(e) { cat(sprintf("  Error: %s\n", e$message)); NULL })

if (!is.null(cd_d_acrt)) {
  cat("\n  === ACRT by Dose (Debt) ===\n")
  summary(cd_d_acrt)
  fig_d_acrt <- ggcont_did(cd_d_acrt)
  ggsave(file.path(safeplots, "cdid_contdid_acrt_d_dose.pdf"),
         fig_d_acrt, width = 9, height = 6)
  cat("  -> Saved: cdid_contdid_acrt_d_dose.pdf\n\n")
}


# ---- 6C: Event study (Debt) ----
cat("--- 6C: Event Study ATT (Debt) ---\n\n")

cd_d_es <- tryCatch({
  cont_did(
    yname            = "Y",
    tname            = "t_idx",
    idname           = "id",
    dname            = "D",
    gname            = "G",
    data             = df_d60,
    target_parameter = "level",
    aggregation      = "eventstudy",
    treatment_type   = "continuous",
    control_group    = "notyettreated",
    dose_est_method  = "parametric",
    degree           = 3,
    num_knots        = 0,
    biters           = 1000,
    cband            = TRUE,
    print_details    = TRUE
  )
}, error = function(e) { cat(sprintf("  Error: %s\n", e$message)); NULL })

if (!is.null(cd_d_es)) {
  cat("\n  === Event Study ATT (Debt) ===\n")
  summary(cd_d_es)
  fig_d_es <- ggcont_did(cd_d_es)
  ggsave(file.path(safeplots, "cdid_contdid_es_att_d.pdf"),
         fig_d_es, width = 10, height = 6)
  cat("  -> Saved: cdid_contdid_es_att_d.pdf\n\n")
}

# Event study ACRT (Debt)
cat("--- 6D: Event Study ACRT (Debt) ---\n\n")

cd_d_es_acrt <- tryCatch({
  cont_did(
    yname            = "Y",
    tname            = "t_idx",
    idname           = "id",
    dname            = "D",
    gname            = "G",
    data             = df_d60,
    target_parameter = "slope",
    aggregation      = "eventstudy",
    treatment_type   = "continuous",
    control_group    = "notyettreated",
    dose_est_method  = "parametric",
    degree           = 3,
    num_knots        = 0,
    biters           = 1000,
    cband            = TRUE,
    print_details    = TRUE
  )
}, error = function(e) { cat(sprintf("  Error: %s\n", e$message)); NULL })

if (!is.null(cd_d_es_acrt)) {
  cat("\n  === Event Study ACRT (Debt) ===\n")
  summary(cd_d_es_acrt)
  fig_d_es_acrt <- ggcont_did(cd_d_es_acrt)
  ggsave(file.path(safeplots, "cdid_contdid_es_acrt_d.pdf"),
         fig_d_es_acrt, width = 10, height = 6)
  cat("  -> Saved: cdid_contdid_es_acrt_d.pdf\n\n")
}


# ==============================================================================
#  SECTION 7: FISCAL COMPOSITION CONTROLS
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 7: FISCAL COMPOSITION CONTROLS\n")
cat(strrep("=", 70), "\n\n")

cat(paste0(
  "  The contdid package does not yet support covariates (xformula = ~1).\n",
  "  We control for fiscal composition via RESIDUALIZATION:\n",
  "    1. Regress Y on F_CP + F_DI + F_H + quarter FE (post-treatment only)\n",
  "    2. Replace Y with residuals + mean(Y) to preserve level\n",
  "    3. Re-estimate cont_did on the fiscal-adjusted outcome\n\n",
  "  JUSTIFICATION: Near-orthogonality of S and F (r ~ 0, see analysis.R)\n",
  "  means fiscal composition is approximately exogenous to stringency.\n",
  "  Residualization partials out any remaining fiscal confounding.\n\n"
))

# ---- 7A: Output Gap — fiscal-adjusted ----
cat("--- 7A: Fiscal-Adjusted ATT (Output Gap) ---\n\n")

df_y60_fa <- prep_contdid(panel_bal, "y_t_pct", "dose_mean", "G_60",
                           fiscal_adjust = TRUE)

cd_y_fa_att <- tryCatch({
  cont_did(
    yname = "Y", tname = "t_idx", idname = "id", dname = "D", gname = "G",
    data             = df_y60_fa,
    target_parameter = "level",
    aggregation      = "dose",
    treatment_type   = "continuous",
    control_group    = "notyettreated",
    degree = 3, num_knots = 0,
    biters = 1000, cband = TRUE, print_details = TRUE
  )
}, error = function(e) { cat(sprintf("  Error: %s\n", e$message)); NULL })

if (!is.null(cd_y_fa_att)) {
  cat("\n  === Fiscal-Adjusted ATT by Dose (Output Gap) ===\n")
  summary(cd_y_fa_att)
  fig_y_fa <- ggcont_did(cd_y_fa_att)
  ggsave(file.path(safeplots, "cdid_contdid_att_y_fiscal_adj.pdf"),
         fig_y_fa, width = 9, height = 6)
  cat("  -> Saved: cdid_contdid_att_y_fiscal_adj.pdf\n\n")
}

# ACRT fiscal-adjusted
cd_y_fa_acrt <- tryCatch({
  cont_did(
    yname = "Y", tname = "t_idx", idname = "id", dname = "D", gname = "G",
    data             = df_y60_fa,
    target_parameter = "slope",
    aggregation      = "dose",
    treatment_type   = "continuous",
    control_group    = "notyettreated",
    degree = 3, num_knots = 0,
    biters = 1000, cband = TRUE, print_details = TRUE
  )
}, error = function(e) { cat(sprintf("  Error: %s\n", e$message)); NULL })

if (!is.null(cd_y_fa_acrt)) {
  cat("\n  === Fiscal-Adjusted ACRT (Output Gap) ===\n")
  summary(cd_y_fa_acrt)
}


# ---- 7B: Debt — fiscal-adjusted ----
cat("\n--- 7B: Fiscal-Adjusted ATT (Debt) ---\n\n")

df_d60_fa <- prep_contdid(panel_bal, "DebtR_share2019", "dose_mean", "G_60",
                           fiscal_adjust = TRUE)

cd_d_fa_att <- tryCatch({
  cont_did(
    yname = "Y", tname = "t_idx", idname = "id", dname = "D", gname = "G",
    data             = df_d60_fa,
    target_parameter = "level",
    aggregation      = "dose",
    treatment_type   = "continuous",
    control_group    = "notyettreated",
    degree = 3, num_knots = 0,
    biters = 1000, cband = TRUE, print_details = TRUE
  )
}, error = function(e) { cat(sprintf("  Error: %s\n", e$message)); NULL })

if (!is.null(cd_d_fa_att)) {
  cat("\n  === Fiscal-Adjusted ATT by Dose (Debt) ===\n")
  summary(cd_d_fa_att)
  fig_d_fa <- ggcont_did(cd_d_fa_att)
  ggsave(file.path(safeplots, "cdid_contdid_att_d_fiscal_adj.pdf"),
         fig_d_fa, width = 9, height = 6)
  cat("  -> Saved: cdid_contdid_att_d_fiscal_adj.pdf\n\n")
}


# ==============================================================================
#  SECTION 8: ROBUSTNESS — ALTERNATIVE G THRESHOLDS & DOSE MEASURES
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 8: ROBUSTNESS\n")
cat(strrep("=", 70), "\n\n")

# ---- 8A: Different G thresholds ----
cat("--- 8A: Robustness — Alternative G Thresholds (Output Gap ACRT) ---\n\n")

g_thresholds <- c("G_25", "G_50", "G_60")
g_results_y <- list()

for (gv in g_thresholds) {
  cat(sprintf("  G threshold: %s ... ", gv))

  df_tmp <- prep_contdid(panel_bal, "y_t_pct", "dose_mean", gv)
  n_control <- n_distinct(df_tmp$id[df_tmp$G == 0])

  if (n_control < 2) {
    cat(sprintf("SKIPPED (only %d comparison units)\n", n_control))
    next
  }

  res <- tryCatch({
    cont_did(
      yname = "Y", tname = "t_idx", idname = "id", dname = "D", gname = "G",
      data = df_tmp,
      target_parameter = "slope", aggregation = "dose",
      treatment_type = "continuous", control_group = "notyettreated",
      degree = 3, num_knots = 0, biters = 500, cband = FALSE,
      print_details = FALSE
    )
  }, error = function(e) { cat(sprintf("FAILED: %s\n", e$message)); NULL })

  if (!is.null(res)) {
    g_results_y[[gv]] <- res
    cat("OK\n")
  }
}
cat("\n")

# Print comparison
for (gv in names(g_results_y)) {
  cat(sprintf("  --- %s ---\n", gv))
  summary(g_results_y[[gv]])
  cat("\n")
}


# ---- 8B: Alternative dose measures ----
cat("--- 8B: Robustness — Alternative Dose Measures (ACRT, Output Gap) ---\n\n")

dose_vars <- c("dose_mean", "dose_cumul", "dose_peak",
               "dose_first_wave", "dose_duration")

dose_results_y <- list()
for (dv in dose_vars) {
  cat(sprintf("  Dose: %s ... ", dv))

  df_tmp <- prep_contdid(panel_bal, "y_t_pct", dv, "G_60")

  res <- tryCatch({
    cont_did(
      yname = "Y", tname = "t_idx", idname = "id", dname = "D", gname = "G",
      data = df_tmp,
      target_parameter = "slope", aggregation = "dose",
      treatment_type = "continuous", control_group = "notyettreated",
      degree = 3, num_knots = 0, biters = 500, cband = FALSE,
      print_details = FALSE
    )
  }, error = function(e) { cat(sprintf("FAILED: %s\n", e$message)); NULL })

  if (!is.null(res)) {
    dose_results_y[[dv]] <- res
    cat("OK\n")
  }
}
cat("\n")

for (dv in names(dose_results_y)) {
  cat(sprintf("  --- %s ---\n", dv))
  summary(dose_results_y[[dv]])
  cat("\n")
}


# ---- 8C: Alternative dose measures (Debt) ----
cat("--- 8C: Robustness — Alternative Dose Measures (ACRT, Debt) ---\n\n")

dose_results_d <- list()
for (dv in dose_vars) {
  cat(sprintf("  Dose: %s ... ", dv))

  df_tmp <- prep_contdid(panel_bal, "DebtR_share2019", dv, "G_60")

  res <- tryCatch({
    cont_did(
      yname = "Y", tname = "t_idx", idname = "id", dname = "D", gname = "G",
      data = df_tmp,
      target_parameter = "slope", aggregation = "dose",
      treatment_type = "continuous", control_group = "notyettreated",
      degree = 3, num_knots = 0, biters = 500, cband = FALSE,
      print_details = FALSE
    )
  }, error = function(e) { cat(sprintf("FAILED: %s\n", e$message)); NULL })

  if (!is.null(res)) {
    dose_results_d[[dv]] <- res
    cat("OK\n")
  }
}
cat("\n")

for (dv in names(dose_results_d)) {
  cat(sprintf("  --- %s ---\n", dv))
  summary(dose_results_d[[dv]])
  cat("\n")
}


# ==============================================================================
#  SECTION 9: ASSUMPTION DISCUSSION
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 9: ASSUMPTION DISCUSSION\n")
cat(strrep("=", 70), "\n\n")

cat(paste0(
  "  CRITICAL ASSUMPTIONS FOR CONTINUOUS DiD (Callaway et al. 2025):\n\n",

  "  1. STRONG PARALLEL TRENDS (Assumption 1):\n",
  "     E[Y_t(0) - Y_{t-1}(0) | D=d, G=g] = E[Y_t(0) - Y_{t-1}(0) | G=g]\n",
  "     Absent treatment, outcome trends are independent of the dose received.\n",
  "     TESTED: Section 4A pre-trend test. If dose_mean is insignificant in\n",
  "     predicting pre-COVID outcome changes, the assumption is supported.\n",
  "     CAVEAT: Richer countries may both lock down more and have different\n",
  "     output trajectories. Conditional parallel trends (on pre-COVID X)\n",
  "     addresses this — but contdid v0.1 does not yet support covariates.\n",
  "     We mitigate via pre-trend testing and fiscal residualization.\n\n",

  "  2. NO ANTICIPATION (Assumption 2):\n",
  "     Y_t(d) = Y_t(0) for all t < G (treatment onset).\n",
  "     PLAUSIBLE: COVID-19 stringency was not anticipated before 2020.\n",
  "     Minor caveat: late Q4.2019 news from China may have affected\n",
  "     expectations for trade-exposed economies. Pre-trend test captures this.\n\n",

  "  3. OVERLAP / COMMON SUPPORT (Assumption 3):\n",
  "     The dose D must have sufficient variation across the support.\n",
  "     With 38 OECD countries, extreme doses have limited observations.\n",
  "     The parametric B-spline approach in contdid extrapolates smoothly.\n",
  "     Section 4B dose histogram visualizes support.\n\n",

  "  4. TREATMENT EFFECT HOMOGENEITY:\n",
  "     Callaway et al. distinguish between ATT(d|d) (effect at d for units\n",
  "     that received d) and ACRT (the slope/derivative of the dose-response).\n",
  "     The ACRT is the key parameter: it measures the marginal causal effect\n",
  "     of an additional unit of stringency on outcomes. Nonlinearity is\n",
  "     accommodated via the B-spline specification.\n\n",

  "  5. SUTVA:\n",
  "     Country i's outcome depends only on its own stringency dose.\n",
  "     CONCERN: Trade linkages create spillovers. A country's output gap\n",
  "     depends on trading partners' lockdowns, not just domestic policy.\n",
  "     This is a known limitation of cross-country pandemic DiD designs.\n\n",

  "  6. FISCAL COMPOSITION (Post-Treatment Variable):\n",
  "     F_CP and F_DI are fiscal responses deployed after stringency.\n",
  "     Including them directly as controls would create 'bad control' bias\n",
  "     if stringency -> fiscal policy -> outcome.\n",
  "     MITIGATION:\n",
  "       (a) Near-orthogonality of S and F (r ~ 0) attenuates this concern.\n",
  "       (b) Residualization (Section 7) partials out fiscal effects.\n",
  "       (c) Comparison of unadjusted vs fiscal-adjusted results reveals\n",
  "           the sensitivity of the dose-response to fiscal composition.\n\n",

  "  7. STAGGERED TIMING (G) DEFINITION:\n",
  "     We define G as the first quarter where mean daily stringency exceeds\n",
  "     a threshold (G_25, G_50, G_60). Countries below the threshold are\n",
  "     never-treated (G=0) and form the comparison group.\n",
  "     ROBUSTNESS: Section 8A varies the threshold. Higher thresholds create\n",
  "     more 'never-treated' units but reduce the treated sample. The results\n",
  "     should be qualitatively stable across threshold choices.\n\n"
))


# ==============================================================================
#  SECTION 10: OUTPUT SUMMARY
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  CONTINUOUS DiD ANALYSIS COMPLETE (contdid package)\n")
cat(strrep("=", 70), "\n\n")

cat(paste0(
  "  OUTPUT FILES:\n",
  "  Figures:\n",
  "    cdid_pretrend_y.pdf           — Pre-trend parallel trends check\n",
  "    cdid_dose_distribution.pdf    — Dose support histogram\n",
  "    cdid_contdid_att_y_dose.pdf   — ATT by dose (Output Gap)\n",
  "    cdid_contdid_acrt_y_dose.pdf  — ACRT by dose (Output Gap)\n",
  "    cdid_contdid_es_att_y.pdf     — Event study ATT (Output Gap)\n",
  "    cdid_contdid_es_acrt_y.pdf    — Event study ACRT (Output Gap)\n",
  "    cdid_contdid_att_d_dose.pdf   — ATT by dose (Debt)\n",
  "    cdid_contdid_acrt_d_dose.pdf  — ACRT by dose (Debt)\n",
  "    cdid_contdid_es_att_d.pdf     — Event study ATT (Debt)\n",
  "    cdid_contdid_es_acrt_d.pdf    — Event study ACRT (Debt)\n",
  "    cdid_contdid_att_y_fiscal_adj.pdf — Fiscal-adjusted ATT (Output Gap)\n",
  "    cdid_contdid_att_d_fiscal_adj.pdf — Fiscal-adjusted ATT (Debt)\n",
  "    cdid_contdid_cck_y.pdf        — Nonparametric ACRT (if available)\n"
))
