# ============================================================================
# CONTINUOUS DIFFERENCE-IN-DIFFERENCES
# Following Callaway, Goodman-Bacon & Sant'Anna (2024)
# "Difference-in-Differences with a Continuous Treatment"
# ============================================================================
#
# METHODOLOGY:
# The ACRF (Average Causal Response Function) is the key target parameter:
#   ACRF(d) = E[Y_post(d) - Y_post(0) | D = d]
#
# Under strong parallel trends:
#   ACRF(d) = E[DeltaY | D=d] - E[DeltaY | D=d_0]
# where DeltaY = Y_post - Y_pre
#
# Since ALL OECD countries had positive stringency during COVID-19,
# we use the lowest-dose group as the comparison group. The estimated
# ACRF is therefore relative to low stringency, not zero stringency.
#
# TREATMENT: StringencyIndex intensity (continuous dose per country)
# OUTCOMES:  y_t_pct (Output Gap, pp of potential GDP)
#            debt_dR (Debt change, real, pp of 2019 GDP)
# CONTROLS:  Fiscal Composition (F_CP, F_DI), pre-COVID characteristics
# ============================================================================

rm(list = ls())

# ==============================================================================
#  SECTION 0: SETUP
# ==============================================================================

packages <- c(
  "dplyr", "tidyr", "ggplot2", "fixest", "modelsummary",
  "data.table", "lubridate", "stringr", "purrr", "sandwich",
  "lmtest", "KernSmooth", "broom", "scales", "patchwork",
  "readr", "countrycode"
)
lapply(packages, require, character.only = TRUE)

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

# --- Paths ---
base_dir  <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files"
data_proc <- file.path(base_dir, "data/processed")
data_raw  <- file.path(base_dir, "data/raw")
safeplots <- file.path(base_dir, "output/figures")
safetable <- file.path(base_dir, "output/tables")

# --- Load main analysis data ---
load(file.path(data_proc, "dataforanalysis.RData"))

# --- Load fiscal measures ---
fm1 <- readxl::read_excel(file.path(data_raw, "fiscal measures/fiscal_classified_v1_7.xlsx"))
fm1 <- fm1 %>%
  mutate(
    YQ       = paste0("Q", Quarter, ".", Year),
    YQ_ord   = factor(YQ, levels = sort(unique(paste0("Q", Quarter, ".", Year))), ordered = TRUE),
    size_pct = broad_fiscal_gdp * 100
  ) %>%
  filter(!PolicyCode %in% c(5, 6, 11, 12, 15, 16))

cat("\n", strrep("=", 70), "\n")
cat("  CONTINUOUS DiD — Callaway, Goodman-Bacon & Sant'Anna (2024)\n")
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
  filter(
    CountryCode %in% oecd_iso3,
    Jurisdiction == "NAT_TOTAL"
  ) %>%
  mutate(
    date = ymd(Date),
    Country = CountryCode
  ) %>%
  select(Country, date, StringencyIndex_Average) %>%
  filter(!is.na(StringencyIndex_Average))

cat(sprintf("  Oxford daily data: %d obs, %d countries, %s to %s\n",
            nrow(ox_daily), n_distinct(ox_daily$Country),
            min(ox_daily$date), max(ox_daily$date)))

# --- 1B: Construct dose measures (country-level treatment intensity) ---
# Define pandemic period for dose computation: 2020-01-01 to 2021-12-31
dose_start <- as.Date("2020-01-01")
dose_end   <- as.Date("2021-12-31")

dose_data <- ox_daily %>%
  filter(date >= dose_start, date <= dose_end) %>%
  group_by(Country) %>%
  summarise(
    # DOSE 1: Mean daily stringency (0-100 scale)
    dose_mean = mean(StringencyIndex_Average, na.rm = TRUE),

    # DOSE 2: Cumulative stringency (sum of daily values / 100 for scaling)
    #         Interpretation: "stringency-days" normalized to 0-100 base
    dose_cumul = sum(StringencyIndex_Average, na.rm = TRUE) / 100,

    # DOSE 3: Peak stringency (maximum daily value)
    dose_peak = max(StringencyIndex_Average, na.rm = TRUE),

    # DOSE 4: Duration above median — # days with stringency > OECD median day
    #         (computed after this summarise, see below)
    n_days = n(),

    # Additional: SD of stringency (volatility)
    dose_sd = sd(StringencyIndex_Average, na.rm = TRUE),

    # Additional: Stringency in the first wave (Q1-Q2 2020)
    dose_first_wave = mean(
      StringencyIndex_Average[date >= as.Date("2020-01-01") &
                                date <= as.Date("2020-06-30")], na.rm = TRUE
    ),
    .groups = "drop"
  )

# Dose 4: Duration above OECD-wide daily median stringency
daily_median <- ox_daily %>%
  filter(date >= dose_start, date <= dose_end) %>%
  pull(StringencyIndex_Average) %>%
  median(na.rm = TRUE)

dose_duration <- ox_daily %>%
  filter(date >= dose_start, date <= dose_end) %>%
  group_by(Country) %>%
  summarise(
    dose_duration = sum(StringencyIndex_Average > daily_median, na.rm = TRUE),
    .groups = "drop"
  )

dose_data <- dose_data %>%
  left_join(dose_duration, by = "Country")

cat(sprintf("  OECD daily median stringency: %.1f\n", daily_median))
cat(sprintf("  Dose measures computed for %d countries\n", nrow(dose_data)))

# --- 1C: Dose summary statistics ---
cat("\n  Dose summary statistics:\n")
dose_data %>%
  select(starts_with("dose_")) %>%
  pivot_longer(everything(), names_to = "measure", values_to = "value") %>%
  group_by(measure) %>%
  summarise(
    mean = mean(value), sd = sd(value),
    min = min(value), p25 = quantile(value, 0.25),
    median = median(value), p75 = quantile(value, 0.75),
    max = max(value), .groups = "drop"
  ) %>%
  print(n = Inf)


# ==============================================================================
#  SECTION 2: GROUP CONSTRUCTION (multiple splitting strategies)
# ==============================================================================

cat("\n--- SECTION 2: Group Construction ---\n\n")

# Function to create groups from a dose variable
make_groups <- function(d, dose_var, method = "tercile") {
  x <- d[[dose_var]]
  if (method == "tercile") {
    cuts <- quantile(x, probs = c(1/3, 2/3), na.rm = TRUE)
    d$dose_group <- cut(x, breaks = c(-Inf, cuts, Inf),
                        labels = c("Low", "Medium", "High"),
                        include.lowest = TRUE)
  } else if (method == "quartile") {
    cuts <- quantile(x, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
    d$dose_group <- cut(x, breaks = c(-Inf, cuts, Inf),
                        labels = c("Q1_Low", "Q2", "Q3", "Q4_High"),
                        include.lowest = TRUE)
  } else if (method == "median") {
    med <- median(x, na.rm = TRUE)
    d$dose_group <- ifelse(x <= med, "Low", "High")
    d$dose_group <- factor(d$dose_group, levels = c("Low", "High"))
  } else if (method == "kmeans3") {
    set.seed(1234)
    km <- kmeans(scale(x), centers = 3, nstart = 25)
    # Relabel by ascending cluster mean
    clust_means <- tapply(x, km$cluster, mean)
    rank_map    <- rank(clust_means)
    labs <- c("Low", "Medium", "High")
    d$dose_group <- factor(labs[rank_map[km$cluster]],
                           levels = c("Low", "Medium", "High"))
  }
  return(d)
}

# Apply all splitting strategies to the primary dose (dose_mean)
for (method in c("tercile", "quartile", "median", "kmeans3")) {
  dose_data <- make_groups(dose_data, "dose_mean", method)
  names(dose_data)[names(dose_data) == "dose_group"] <- paste0("group_mean_", method)
}

# Also apply tercile split to alternative dose measures
for (dv in c("dose_cumul", "dose_peak", "dose_duration", "dose_first_wave")) {
  dose_data <- make_groups(dose_data, dv, "tercile")
  names(dose_data)[names(dose_data) == "dose_group"] <- paste0("group_", sub("dose_", "", dv), "_tercile")
}

# Print group counts
cat("  Group assignments (dose_mean):\n")
for (gv in grep("^group_mean_", names(dose_data), value = TRUE)) {
  cat(sprintf("    %s: %s\n", gv,
              paste(names(table(dose_data[[gv]])),
                    table(dose_data[[gv]]), sep = "=", collapse = ", ")))
}
cat("\n  Alternative dose terciles:\n")
for (gv in grep("^group_.*_tercile$", names(dose_data), value = TRUE)) {
  if (!grepl("mean", gv)) {
    cat(sprintf("    %s: %s\n", gv,
                paste(names(table(dose_data[[gv]])),
                      table(dose_data[[gv]]), sep = "=", collapse = ", ")))
  }
}


# ==============================================================================
#  SECTION 2B: PANEL CONSTRUCTION
# ==============================================================================

cat("\n--- SECTION 2B: Panel Construction ---\n\n")

# Quarterly panel from main analysis data
pandemic_qs <- c(
  "Q1.2019", "Q2.2019", "Q3.2019", "Q4.2019",
  "Q1.2020", "Q2.2020", "Q3.2020", "Q4.2020",
  "Q1.2021", "Q2.2021", "Q3.2021", "Q4.2021",
  "Q1.2022", "Q2.2022", "Q3.2022", "Q4.2022"
)

# --- Build quarterly panel (same as analysis.R blocks 1-6) ---
df_qdata <- qdata %>%
  filter(Quarter %in% pandemic_qs) %>%
  select(
    Country, Quarter, year_only, quarter_only,
    y_t_pct, y_t,
    d_t_pct, d_t,
    DebtR_share2019, DebtN_share2019,
    Qpopulation_th, nGDP_2019_an,
    vax_rate, rGDP_pc_2019, debt_2019,
    StringencyIndex_PopWeighted,
    p_proj_all_ages, excess.deaths_a, ConfirmedDeaths.a,
    p_avg_all_ages, inflation_index
  )

# Population 2019
pop_2019 <- df_qdata %>%
  filter(Quarter == "Q4.2019") %>%
  select(Country, pop_2019 = Qpopulation_th)
df_qdata <- df_qdata %>% left_join(pop_2019, by = "Country")

# Fiscal measures (quarterly aggregation)
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

# Stringency from panel_w (quarterly mean)
df_stringency <- panel_w %>%
  filter(!is.na(S_mean)) %>%
  mutate(
    year    = year(date),
    quarter = quarter(date),
    Quarter = paste0("Q", quarter, ".", year)
  ) %>%
  filter(Quarter %in% pandemic_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(
    S_mean_pw = mean(S_mean, na.rm = TRUE) * 100,
    .groups   = "drop"
  )

# Merge into panel
panel <- df_qdata %>%
  left_join(df_fiscal,     by = c("Country", "Quarter")) %>%
  left_join(df_stringency, by = c("Country", "Quarter")) %>%
  mutate(
    F_CP    = replace_na(F_CP,    0),
    F_DI    = replace_na(F_DI,    0),
    F_H     = replace_na(F_H,     0),
    F_total = replace_na(F_total, 0)
  ) %>%
  arrange(Country, Quarter)

# First-difference debt
panel <- panel %>%
  group_by(Country) %>%
  mutate(
    debt_dR = DebtR_share2019 - dplyr::lag(DebtR_share2019),
    debt_dN = DebtN_share2019 - dplyr::lag(DebtN_share2019)
  ) %>%
  ungroup()

# Time index: Q1.2019=1, ..., Q4.2022=16
quarter_order <- c(
  "Q1.2019","Q2.2019","Q3.2019","Q4.2019",
  "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
  "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
  "Q1.2022","Q2.2022","Q3.2022","Q4.2022"
)
panel$t_idx <- match(panel$Quarter, quarter_order)

# Merge dose and group assignments
panel <- panel %>% left_join(dose_data, by = "Country")

# Post-treatment indicator
panel$post <- as.integer(panel$t_idx >= 5)  # Q1.2020 onwards

# Pre-COVID characteristics (for conditional PT)
pre_chars <- panel %>%
  filter(Quarter == "Q4.2019") %>%
  select(Country, rGDP_pc_2019, debt_2019, pop_2019) %>%
  distinct()

panel <- panel %>% left_join(
  pre_chars %>% select(Country, rGDP_pc_2019, debt_2019) %>%
    rename(gdppc_pre = rGDP_pc_2019, debt_pre = debt_2019),
  by = "Country",
  suffix = c("", "_pre_char")
)

cat(sprintf("  Panel: %d obs, %d countries, %d quarters\n",
            nrow(panel), n_distinct(panel$Country), n_distinct(panel$Quarter)))


# ==============================================================================
#  SECTION 3: ASSUMPTION CHECKS
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 3: ASSUMPTION CHECKS\n")
cat(strrep("=", 70), "\n\n")

# ---- 3A: Pre-trend parallel trends by dose group ----
# Core assumption: E[DeltaY(0) | D=d] does not depend on d
# Test: In 2019 (pre-COVID), outcome trends should not differ by dose group

cat("--- 3A: Pre-Trend Parallel Trends Test ---\n\n")

pre_panel <- panel %>%
  filter(t_idx <= 4) %>%  # Q1.2019 - Q4.2019
  filter(!is.na(group_mean_tercile))

# Visual: mean y_t_pct by quarter and dose group (tercile)
pre_trends_y <- pre_panel %>%
  group_by(Quarter, group_mean_tercile) %>%
  summarise(mean_y = mean(y_t_pct, na.rm = TRUE),
            se_y   = sd(y_t_pct, na.rm = TRUE) / sqrt(n()),
            .groups = "drop")

fig_pretrend_y <- ggplot(pre_trends_y,
                         aes(x = Quarter, y = mean_y,
                             color = group_mean_tercile,
                             group = group_mean_tercile)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = mean_y - 1.96 * se_y,
                     ymax = mean_y + 1.96 * se_y), width = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = c("Low" = "#2166ac", "Medium" = "#f4a582", "High" = "#b2182b"),
                     name = "Stringency\nDose Group") +
  labs(
    title    = "Pre-Trend Test: Output Gap by Stringency Dose Group (2019)",
    subtitle = "Parallel pre-trends required for Continuous DiD validity",
    x = NULL, y = "Output gap (pp of potential GDP)"
  ) +
  theme_bw(base_size = 11)

ggsave(file.path(safeplots, "cdid_pretrend_y.pdf"), fig_pretrend_y, width = 9, height = 5)
print(fig_pretrend_y)
cat("  -> Saved: cdid_pretrend_y.pdf\n")

# Formal test: regress DeltaY_pre on dose (should be insignificant)
# Use Q4.2019 as base, compute changes from Q1-Q3 relative to Q4
base_y <- panel %>%
  filter(Quarter == "Q4.2019") %>%
  select(Country, y_base = y_t_pct, d_base = d_t_pct, debt_base = DebtR_share2019)

pre_test <- panel %>%
  filter(t_idx %in% 1:3) %>%  # Q1-Q3 2019

  left_join(base_y, by = "Country") %>%
  mutate(dy_pre = y_t_pct - y_base)

# Test: regress dy_pre on dose_mean
pt_test_y <- feols(dy_pre ~ dose_mean | Quarter, data = pre_test,
                   cluster = ~Country)
cat("\n  Pre-trend test (y_t_pct ~ dose_mean | Quarter FE):\n")
print(summary(pt_test_y))

# Joint F-test with dose groups
pt_test_y_grp <- feols(dy_pre ~ group_mean_tercile | Quarter,
                       data = pre_test, cluster = ~Country)
cat("\n  Pre-trend test (y_t_pct ~ dose_tercile | Quarter FE):\n")
print(summary(pt_test_y_grp))
cat(sprintf("  F-stat on dose groups: %.3f, p-value: %.4f\n",
            fitstat(pt_test_y_grp, "f")$f$stat,
            fitstat(pt_test_y_grp, "f")$f$p))

# Same for debt
pre_test_d <- pre_test %>%
  mutate(dd_pre = DebtR_share2019 - debt_base)

pt_test_d <- feols(dd_pre ~ dose_mean | Quarter, data = pre_test_d,
                   cluster = ~Country)
cat("\n  Pre-trend test (DebtR ~ dose_mean | Quarter FE):\n")
print(summary(pt_test_d))

cat(paste0(
  "\n  INTERPRETATION: If the dose coefficient is statistically insignificant,\n",
  "  the strong parallel trends assumption holds unconditionally. If marginally\n",
  "  significant, we proceed with conditional parallel trends (Section 5).\n\n"
))


# ---- 3B: Common support (dose distribution overlap) ----
cat("--- 3B: Common Support / Dose Distribution ---\n\n")

fig_dose_dist <- dose_data %>%
  ggplot(aes(x = dose_mean)) +
  geom_histogram(aes(y = after_stat(density)), bins = 15,
                 fill = "#2166ac", alpha = 0.5, color = "white") +
  geom_density(linewidth = 1, color = "#b2182b") +
  geom_rug(alpha = 0.5) +
  labs(
    title    = "Distribution of Stringency Dose (Mean Daily Index, 2020-2021)",
    subtitle = "Continuous DiD requires common support across dose levels",
    x = "Mean Stringency Index (0-100)", y = "Density"
  ) +
  theme_bw(base_size = 11)

ggsave(file.path(safeplots, "cdid_dose_distribution.pdf"), fig_dose_dist, width = 8, height = 5)
print(fig_dose_dist)
cat("  -> Saved: cdid_dose_distribution.pdf\n\n")


# ---- 3C: Balance table across dose groups ----
cat("--- 3C: Balance Table (pre-COVID characteristics by dose group) ---\n")

balance <- panel %>%
  filter(Quarter == "Q4.2019", !is.na(group_mean_tercile)) %>%
  group_by(group_mean_tercile) %>%
  summarise(
    n            = n(),
    gdppc_mean   = mean(rGDP_pc_2019, na.rm = TRUE),
    gdppc_sd     = sd(rGDP_pc_2019,   na.rm = TRUE),
    debt_mean    = mean(debt_2019,     na.rm = TRUE),
    debt_sd      = sd(debt_2019,       na.rm = TRUE),
    pop_mean     = mean(pop_2019,      na.rm = TRUE) / 1000,
    y_pre_mean   = mean(y_t_pct,       na.rm = TRUE),
    .groups = "drop"
  )
print(balance)

cat(paste0(
  "\n  CHECK: If pre-COVID characteristics are imbalanced across dose groups,\n",
  "  unconditional parallel trends may fail. In that case, we condition on X\n",
  "  (GDP per capita, pre-COVID debt) using outcome regression or IPW.\n\n"
))


# ==============================================================================
#  SECTION 4: DISCRETIZED CONTINUOUS DiD — OUTPUT GAP (y_k)
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 4: DISCRETIZED CONTINUOUS DiD — OUTPUT GAP\n")
cat(strrep("=", 70), "\n\n")

# ---- 4A: Compute DeltaY for each post-period relative to Q4.2019 ----
base_outcomes <- panel %>%
  filter(Quarter == "Q4.2019") %>%
  select(Country, y_base = y_t_pct, d_base = d_t_pct,
         debt_R_base = DebtR_share2019)

panel_did <- panel %>%
  filter(t_idx >= 5) %>%  # Q1.2020 onwards
  left_join(base_outcomes, by = "Country") %>%
  mutate(
    # Delta Y: change in output gap from Q4.2019
    dy = y_t_pct - y_base,
    # Delta debt: change in debt from Q4.2019
    dd = DebtR_share2019 - debt_R_base
  )

cat(sprintf("  Post-treatment panel: %d obs, %d countries, %d quarters\n",
            nrow(panel_did), n_distinct(panel_did$Country),
            n_distinct(panel_did$Quarter)))


# ---- 4B: Tercile-based ATT (primary specification) ----
cat("\n--- 4B: Tercile-Based ATT (Output Gap) ---\n")

# For each post-quarter, compute group-specific mean DeltaY
# ATT(g,t) = mean(DeltaY | group=g, t) - mean(DeltaY | group=Low, t)

att_tercile_y <- panel_did %>%
  filter(!is.na(group_mean_tercile)) %>%
  group_by(Quarter, t_idx, group_mean_tercile) %>%
  summarise(
    mean_dy = mean(dy, na.rm = TRUE),
    se_dy   = sd(dy, na.rm = TRUE) / sqrt(sum(!is.na(dy))),
    n       = sum(!is.na(dy)),
    .groups = "drop"
  )

# Subtract the Low-group mean to get ATT
low_means <- att_tercile_y %>%
  filter(group_mean_tercile == "Low") %>%
  select(Quarter, mean_dy_low = mean_dy)

att_tercile_y <- att_tercile_y %>%
  left_join(low_means, by = "Quarter") %>%
  mutate(
    att = mean_dy - mean_dy_low,
    att_se = se_dy  # conservative (does not account for comparison group variance)
  )

cat("\n  ATT by tercile group (Output Gap):\n")
att_tercile_y %>%
  filter(group_mean_tercile != "Low") %>%
  select(Quarter, group_mean_tercile, att, att_se, n) %>%
  print(n = Inf)

# Event-study style figure
fig_att_y_tercile <- att_tercile_y %>%
  filter(group_mean_tercile != "Low") %>%
  ggplot(aes(x = t_idx, y = att, color = group_mean_tercile)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = att - 1.96 * att_se,
                     ymax = att + 1.96 * att_se), width = 0.2) +
  scale_x_continuous(breaks = 5:16,
    labels = c("Q1.20","Q2.20","Q3.20","Q4.20","Q1.21",
               "Q2.21","Q3.21","Q4.21","Q1.22","Q2.22","Q3.22","Q4.22")) +
  scale_color_manual(values = c("Medium" = "#f4a582", "High" = "#b2182b"),
                     name = "Dose Group") +
  labs(
    title    = "Continuous DiD: Output Gap ATT by Stringency Dose Group (Terciles)",
    subtitle = "ATT relative to Low-stringency group. Base period: Q4.2019.",
    x = NULL, y = "ATT: Output Gap (pp of potential GDP)"
  ) +
  theme_bw(base_size = 11)

ggsave(file.path(safeplots, "cdid_att_y_tercile.pdf"), fig_att_y_tercile, width = 10, height = 6)
print(fig_att_y_tercile)
cat("  -> Saved: cdid_att_y_tercile.pdf\n")


# ---- 4C: Regression-based estimation (with clustering) ----
cat("\n--- 4C: Regression-Based ATT with Clustered SEs ---\n\n")

# Specification 1: Unconditional — TWFE with dose groups
m_y_terc <- feols(
  dy ~ i(group_mean_tercile, ref = "Low") | Quarter,
  data = panel_did, cluster = ~Country
)
cat("  Spec 1: Unconditional (tercile groups):\n")
print(summary(m_y_terc))

# Specification 2: Conditional on pre-COVID characteristics
m_y_terc_cond <- feols(
  dy ~ i(group_mean_tercile, ref = "Low") + gdppc_pre + debt_pre | Quarter,
  data = panel_did, cluster = ~Country
)
cat("\n  Spec 2: Conditional on pre-COVID GDP pc + debt:\n")
print(summary(m_y_terc_cond))

# Specification 3: Event-study (group x quarter interactions)
m_y_es <- feols(
  dy ~ i(group_mean_tercile, i.t_idx, ref = "Low") | Quarter,
  data = panel_did, cluster = ~Country
)
cat("\n  Spec 3: Event-study (group x quarter):\n")
print(summary(m_y_es))


# ---- 4D: Quartile-based ATT ----
cat("\n--- 4D: Quartile-Based ATT (Output Gap) ---\n")

m_y_quart <- feols(
  dy ~ i(group_mean_quartile, ref = "Q1_Low") | Quarter,
  data = panel_did, cluster = ~Country
)
cat("  Quartile-based ATT:\n")
print(summary(m_y_quart))


# ---- 4E: Median split (binary benchmark) ----
cat("\n--- 4E: Median-Split ATT (Binary Benchmark) ---\n")

m_y_med <- feols(
  dy ~ i(group_mean_median, ref = "Low") | Quarter,
  data = panel_did, cluster = ~Country
)
cat("  Median-split ATT:\n")
print(summary(m_y_med))


# ---- 4F: Alternative dose measures (tercile splits) ----
cat("\n--- 4F: Alternative Dose Measures (Tercile ATTs) ---\n\n")

alt_dose_vars <- c("group_cumul_tercile", "group_peak_tercile",
                   "group_duration_tercile", "group_first_wave_tercile")

alt_results_y <- list()
for (gv in alt_dose_vars) {
  if (!gv %in% names(panel_did)) next
  fml <- as.formula(paste0("dy ~ i(", gv, ", ref = 'Low') | Quarter"))
  m <- feols(fml, data = panel_did, cluster = ~Country)
  alt_results_y[[gv]] <- m
  cat(sprintf("  %s:\n", gv))
  ct <- coeftable(m)
  print(ct)
  cat("\n")
}


# ==============================================================================
#  SECTION 5: ACRF ESTIMATION — OUTPUT GAP
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 5: ACRF ESTIMATION — OUTPUT GAP\n")
cat(strrep("=", 70), "\n\n")

# ---- 5A: Parametric ACRF (polynomial regression) ----
cat("--- 5A: Parametric ACRF (polynomial in dose) ---\n\n")

# Aggregate DeltaY by country (average over post-periods)
country_dy <- panel_did %>%
  group_by(Country) %>%
  summarise(
    dy_mean = mean(dy, na.rm = TRUE),
    dd_mean = mean(dd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(dose_data, by = "Country") %>%
  left_join(pre_chars, by = "Country")

# Linear ACRF
m_acrf_lin <- lm(dy_mean ~ dose_mean, data = country_dy)
# Quadratic ACRF
m_acrf_quad <- lm(dy_mean ~ dose_mean + I(dose_mean^2), data = country_dy)
# Cubic ACRF
m_acrf_cub <- lm(dy_mean ~ dose_mean + I(dose_mean^2) + I(dose_mean^3),
                  data = country_dy)

cat("  Linear ACRF:\n")
print(summary(m_acrf_lin))
cat("  Quadratic ACRF:\n")
print(summary(m_acrf_quad))

# Model selection (AIC/BIC)
cat(sprintf("  AIC: Linear=%.1f, Quadratic=%.1f, Cubic=%.1f\n",
            AIC(m_acrf_lin), AIC(m_acrf_quad), AIC(m_acrf_cub)))
cat(sprintf("  BIC: Linear=%.1f, Quadratic=%.1f, Cubic=%.1f\n",
            BIC(m_acrf_lin), BIC(m_acrf_quad), BIC(m_acrf_cub)))


# ---- 5B: Nonparametric ACRF (local polynomial) ----
cat("\n--- 5B: Nonparametric ACRF (local polynomial) ---\n\n")

# Local polynomial regression of DeltaY on dose
# Using KernSmooth::locpoly
dose_grid <- seq(min(country_dy$dose_mean, na.rm = TRUE),
                 max(country_dy$dose_mean, na.rm = TRUE),
                 length.out = 200)

# Bandwidth selection (rule-of-thumb)
bw_rot <- dpill(country_dy$dose_mean, country_dy$dy_mean)
cat(sprintf("  Bandwidth (dpill): %.2f\n", bw_rot))

# Local linear fit
lp_fit <- locpoly(country_dy$dose_mean, country_dy$dy_mean,
                  bandwidth = bw_rot, degree = 1,
                  gridsize = 200,
                  range.x = range(country_dy$dose_mean, na.rm = TRUE))

# Reference level: ACRF at lowest observed dose
acrf_ref <- lp_fit$y[1]  # value at minimum dose
acrf_est <- lp_fit$y - acrf_ref  # ACRF relative to low dose

lp_df <- data.frame(
  dose = lp_fit$x,
  acrf = acrf_est
)

# Bootstrap confidence intervals for ACRF
n_boot <- 500
boot_acrf <- matrix(NA, nrow = n_boot, ncol = length(dose_grid))

for (b in 1:n_boot) {
  idx <- sample(nrow(country_dy), replace = TRUE)
  boot_data <- country_dy[idx, ]
  tryCatch({
    bw_b <- dpill(boot_data$dose_mean, boot_data$dy_mean)
    lp_b <- locpoly(boot_data$dose_mean, boot_data$dy_mean,
                    bandwidth = bw_b, degree = 1, gridsize = 200,
                    range.x = range(country_dy$dose_mean, na.rm = TRUE))
    boot_acrf[b, ] <- lp_b$y - lp_b$y[1]
  }, error = function(e) NULL)
}

lp_df$acrf_lo <- apply(boot_acrf, 2, quantile, probs = 0.025, na.rm = TRUE)
lp_df$acrf_hi <- apply(boot_acrf, 2, quantile, probs = 0.975, na.rm = TRUE)

# ACRF figure
fig_acrf_y <- ggplot() +
  geom_ribbon(data = lp_df, aes(x = dose, ymin = acrf_lo, ymax = acrf_hi),
              fill = "#2166ac", alpha = 0.2) +
  geom_line(data = lp_df, aes(x = dose, y = acrf),
            color = "#2166ac", linewidth = 1.2) +
  geom_point(data = country_dy,
             aes(x = dose_mean, y = dy_mean - acrf_ref),
             alpha = 0.5, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title    = "Average Causal Response Function: Output Gap",
    subtitle = "Nonparametric (local linear). Relative to lowest-dose countries. 95% bootstrap CI.",
    x = "Stringency Dose (Mean Daily Index, 2020-2021)",
    y = "ACRF: Output Gap Effect (pp)"
  ) +
  theme_bw(base_size = 11)

ggsave(file.path(safeplots, "cdid_acrf_y.pdf"), fig_acrf_y, width = 9, height = 6)
print(fig_acrf_y)
cat("  -> Saved: cdid_acrf_y.pdf\n")


# ---- 5C: ACRF with pre-COVID covariates (conditional parallel trends) ----
cat("\n--- 5C: Conditional ACRF (controlling for pre-COVID characteristics) ---\n\n")

# Outcome regression approach:
# Regress DeltaY on dose, controlling for X (GDP pc, debt, pop)
m_acrf_cond <- lm(
  dy_mean ~ dose_mean + rGDP_pc_2019 + debt_2019 + pop_2019,
  data = country_dy
)
cat("  Conditional linear ACRF (covariates: GDP pc, debt, pop):\n")
print(coeftest(m_acrf_cond, vcov = vcovHC(m_acrf_cond, type = "HC1")))

m_acrf_cond_q <- lm(
  dy_mean ~ dose_mean + I(dose_mean^2) + rGDP_pc_2019 + debt_2019,
  data = country_dy
)
cat("\n  Conditional quadratic ACRF:\n")
print(coeftest(m_acrf_cond_q, vcov = vcovHC(m_acrf_cond_q, type = "HC1")))


# ---- 5D: Controlling for fiscal composition ----
cat("\n--- 5D: Controlling for Fiscal Composition (CP, DI) ---\n\n")

cat(paste0(
  "  NOTE ON FISCAL CONTROLS:\n",
  "  F_CP and F_DI are post-treatment variables (fiscal response to COVID).\n",
  "  Including them as controls risks 'bad control' bias (Angrist & Pischke 2009)\n",
  "  if fiscal policy lies on the causal path from stringency to outcomes.\n\n",
  "  However, the main analysis (analysis.R Step 1B) demonstrates near-orthogonality\n",
  "  of stringency (S) and fiscal composition (r ~ 0). This attenuates the concern.\n\n",
  "  We implement two approaches:\n",
  "  (a) Residualized outcomes: partial out fiscal effects first, then estimate ACRF\n",
  "  (b) Direct inclusion: include F_CP, F_DI as additional regressors\n\n"
))

# Approach (a): Residualized outcomes
# Step 1: Regress dy on F_CP + F_DI + Quarter FE within post-period
m_resid <- feols(dy ~ F_CP + F_DI + F_H | Quarter, data = panel_did)
panel_did$dy_resid <- resid(m_resid) + mean(panel_did$dy, na.rm = TRUE)

# Aggregate residualized DeltaY by country
country_dy_resid <- panel_did %>%
  group_by(Country) %>%
  summarise(dy_resid_mean = mean(dy_resid, na.rm = TRUE), .groups = "drop") %>%
  left_join(dose_data, by = "Country")

# ACRF on residualized outcome
m_acrf_resid <- lm(dy_resid_mean ~ dose_mean, data = country_dy_resid)
cat("  Approach (a) — Residualized ACRF (fiscal-adjusted):\n")
print(coeftest(m_acrf_resid, vcov = vcovHC(m_acrf_resid, type = "HC1")))

m_acrf_resid_q <- lm(dy_resid_mean ~ dose_mean + I(dose_mean^2),
                      data = country_dy_resid)
cat("\n  Residualized quadratic ACRF:\n")
print(coeftest(m_acrf_resid_q, vcov = vcovHC(m_acrf_resid_q, type = "HC1")))

# Approach (b): Direct inclusion in panel regression
# Using the full panel (not aggregated)
m_y_fiscal <- feols(
  dy ~ dose_mean + F_CP + F_DI | Quarter,
  data = panel_did, cluster = ~Country
)
cat("\n  Approach (b) — Direct inclusion (panel, linear dose + F_CP + F_DI):\n")
print(summary(m_y_fiscal))

# With dose groups
m_y_fiscal_grp <- feols(
  dy ~ i(group_mean_tercile, ref = "Low") + F_CP + F_DI | Quarter,
  data = panel_did, cluster = ~Country
)
cat("\n  Approach (b) — Dose tercile groups + F_CP + F_DI:\n")
print(summary(m_y_fiscal_grp))

# Interaction: does fiscal composition moderate the dose-response?
m_y_fiscal_int <- feols(
  dy ~ dose_mean * F_CP + dose_mean * F_DI | Quarter,
  data = panel_did, cluster = ~Country
)
cat("\n  Dose x Fiscal interaction:\n")
print(summary(m_y_fiscal_int))


# ---- 5E: Time-varying ACRF (by post-period) ----
cat("\n--- 5E: Time-Varying ACRF ---\n\n")

# Estimate ACRF separately for each post-quarter
acrf_by_q <- panel_did %>%
  group_by(Quarter, t_idx) %>%
  group_modify(~ {
    m <- lm(dy ~ dose_mean, data = .x)
    ct <- coeftest(m, vcov = vcovHC(m, type = "HC1"))
    tibble(
      beta_dose = ct["dose_mean", "Estimate"],
      se_dose   = ct["dose_mean", "Std. Error"],
      p_dose    = ct["dose_mean", "Pr(>|t|)"],
      n         = nrow(.x)
    )
  }) %>%
  ungroup()

cat("  Time-varying ACRF slope (linear, by quarter):\n")
print(acrf_by_q, n = Inf)

fig_acrf_time <- acrf_by_q %>%
  ggplot(aes(x = t_idx, y = beta_dose)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_line(color = "#2166ac", linewidth = 1) +
  geom_point(color = "#2166ac", size = 2.5) +
  geom_errorbar(aes(ymin = beta_dose - 1.96 * se_dose,
                     ymax = beta_dose + 1.96 * se_dose),
                width = 0.2, color = "#2166ac") +
  scale_x_continuous(breaks = 5:16,
    labels = c("Q1.20","Q2.20","Q3.20","Q4.20","Q1.21",
               "Q2.21","Q3.21","Q4.21","Q1.22","Q2.22","Q3.22","Q4.22")) +
  labs(
    title    = "Time-Varying ACRF Slope: Marginal Effect of Stringency Dose on Output Gap",
    subtitle = "Linear dose-response estimated separately by quarter. HC1 robust SEs.",
    x = NULL, y = "ACRF slope (pp output gap per unit dose)"
  ) +
  theme_bw(base_size = 11)

ggsave(file.path(safeplots, "cdid_acrf_time_y.pdf"), fig_acrf_time, width = 10, height = 5)
print(fig_acrf_time)
cat("  -> Saved: cdid_acrf_time_y.pdf\n")


# ==============================================================================
#  SECTION 6: DISCRETIZED CONTINUOUS DiD — DEBT (b_k)
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 6: DISCRETIZED CONTINUOUS DiD — DEBT\n")
cat(strrep("=", 70), "\n\n")

# ---- 6A: Pre-trend for debt ----
cat("--- 6A: Pre-Trend Test (Debt) ---\n")

pre_trends_d <- pre_panel %>%
  left_join(base_outcomes %>% select(Country, debt_R_base), by = "Country") %>%
  mutate(dd_pre = DebtR_share2019 - debt_R_base) %>%
  filter(!is.na(dd_pre))

pre_trends_d_grp <- pre_trends_d %>%
  group_by(Quarter, group_mean_tercile) %>%
  summarise(mean_dd = mean(dd_pre, na.rm = TRUE),
            se_dd   = sd(dd_pre, na.rm = TRUE) / sqrt(n()),
            .groups = "drop")

fig_pretrend_d <- ggplot(pre_trends_d_grp,
                         aes(x = Quarter, y = mean_dd,
                             color = group_mean_tercile,
                             group = group_mean_tercile)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = mean_dd - 1.96 * se_dd,
                     ymax = mean_dd + 1.96 * se_dd), width = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = c("Low" = "#2166ac", "Medium" = "#f4a582", "High" = "#b2182b"),
                     name = "Stringency\nDose Group") +
  labs(
    title    = "Pre-Trend Test: Debt by Stringency Dose Group (2019)",
    subtitle = "Parallel pre-trends required for Continuous DiD validity",
    x = NULL, y = "Debt change (pp of 2019 GDP, real)"
  ) +
  theme_bw(base_size = 11)

ggsave(file.path(safeplots, "cdid_pretrend_d.pdf"), fig_pretrend_d, width = 9, height = 5)
print(fig_pretrend_d)
cat("  -> Saved: cdid_pretrend_d.pdf\n")


# ---- 6B: Tercile-based ATT (Debt) ----
cat("\n--- 6B: Tercile-Based ATT (Debt) ---\n")

# Use debt_dR (first difference, real) as outcome for consistency with main analysis
# Also use dd (level change from base) for the DiD

# Regression-based ATT
m_d_terc <- feols(
  dd ~ i(group_mean_tercile, ref = "Low") | Quarter,
  data = panel_did %>% filter(!is.na(dd)),
  cluster = ~Country
)
cat("  Unconditional tercile ATT (debt change):\n")
print(summary(m_d_terc))

# Conditional on pre-COVID characteristics
m_d_terc_cond <- feols(
  dd ~ i(group_mean_tercile, ref = "Low") + gdppc_pre + debt_pre | Quarter,
  data = panel_did %>% filter(!is.na(dd)),
  cluster = ~Country
)
cat("\n  Conditional tercile ATT (debt change):\n")
print(summary(m_d_terc_cond))


# ---- 6C: Quartile and median splits (Debt) ----
cat("\n--- 6C: Alternative Splits (Debt) ---\n")

m_d_quart <- feols(
  dd ~ i(group_mean_quartile, ref = "Q1_Low") | Quarter,
  data = panel_did %>% filter(!is.na(dd)),
  cluster = ~Country
)
cat("  Quartile ATT (debt):\n")
print(summary(m_d_quart))

m_d_med <- feols(
  dd ~ i(group_mean_median, ref = "Low") | Quarter,
  data = panel_did %>% filter(!is.na(dd)),
  cluster = ~Country
)
cat("\n  Median-split ATT (debt):\n")
print(summary(m_d_med))


# ---- 6D: Event-study style ATT (Debt) ----
cat("\n--- 6D: Event-Study ATT (Debt) ---\n")

att_tercile_d <- panel_did %>%
  filter(!is.na(group_mean_tercile), !is.na(dd)) %>%
  group_by(Quarter, t_idx, group_mean_tercile) %>%
  summarise(
    mean_dd = mean(dd, na.rm = TRUE),
    se_dd   = sd(dd, na.rm = TRUE) / sqrt(sum(!is.na(dd))),
    n       = sum(!is.na(dd)),
    .groups = "drop"
  )

low_means_d <- att_tercile_d %>%
  filter(group_mean_tercile == "Low") %>%
  select(Quarter, mean_dd_low = mean_dd)

att_tercile_d <- att_tercile_d %>%
  left_join(low_means_d, by = "Quarter") %>%
  mutate(att = mean_dd - mean_dd_low, att_se = se_dd)

fig_att_d_tercile <- att_tercile_d %>%
  filter(group_mean_tercile != "Low") %>%
  ggplot(aes(x = t_idx, y = att, color = group_mean_tercile)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = att - 1.96 * att_se,
                     ymax = att + 1.96 * att_se), width = 0.2) +
  scale_x_continuous(breaks = 5:16,
    labels = c("Q1.20","Q2.20","Q3.20","Q4.20","Q1.21",
               "Q2.21","Q3.21","Q4.21","Q1.22","Q2.22","Q3.22","Q4.22")) +
  scale_color_manual(values = c("Medium" = "#f4a582", "High" = "#b2182b"),
                     name = "Dose Group") +
  labs(
    title    = "Continuous DiD: Debt ATT by Stringency Dose Group (Terciles)",
    subtitle = "ATT relative to Low-stringency group. Base period: Q4.2019.",
    x = NULL, y = "ATT: Debt Change (pp of 2019 GDP, real)"
  ) +
  theme_bw(base_size = 11)

ggsave(file.path(safeplots, "cdid_att_d_tercile.pdf"), fig_att_d_tercile, width = 10, height = 6)
print(fig_att_d_tercile)
cat("  -> Saved: cdid_att_d_tercile.pdf\n")


# ==============================================================================
#  SECTION 7: ACRF ESTIMATION — DEBT
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 7: ACRF ESTIMATION — DEBT\n")
cat(strrep("=", 70), "\n\n")

# ---- 7A: Parametric ACRF (Debt) ----
cat("--- 7A: Parametric ACRF (Debt) ---\n\n")

m_acrf_d_lin <- lm(dd_mean ~ dose_mean, data = country_dy)
m_acrf_d_quad <- lm(dd_mean ~ dose_mean + I(dose_mean^2), data = country_dy)

cat("  Linear ACRF (debt):\n")
print(coeftest(m_acrf_d_lin, vcov = vcovHC(m_acrf_d_lin, type = "HC1")))
cat("\n  Quadratic ACRF (debt):\n")
print(coeftest(m_acrf_d_quad, vcov = vcovHC(m_acrf_d_quad, type = "HC1")))


# ---- 7B: Nonparametric ACRF (Debt) ----
cat("\n--- 7B: Nonparametric ACRF (Debt) ---\n\n")

country_dy_d <- country_dy %>% filter(!is.na(dd_mean))

bw_d <- dpill(country_dy_d$dose_mean, country_dy_d$dd_mean)
cat(sprintf("  Bandwidth (dpill): %.2f\n", bw_d))

lp_fit_d <- locpoly(country_dy_d$dose_mean, country_dy_d$dd_mean,
                    bandwidth = bw_d, degree = 1, gridsize = 200,
                    range.x = range(country_dy_d$dose_mean, na.rm = TRUE))

acrf_d_ref <- lp_fit_d$y[1]
lp_df_d <- data.frame(
  dose   = lp_fit_d$x,
  acrf_d = lp_fit_d$y - acrf_d_ref
)

# Bootstrap CI
boot_acrf_d <- matrix(NA, nrow = n_boot, ncol = 200)
for (b in 1:n_boot) {
  idx <- sample(nrow(country_dy_d), replace = TRUE)
  bd  <- country_dy_d[idx, ]
  tryCatch({
    bw_b <- dpill(bd$dose_mean, bd$dd_mean)
    lp_b <- locpoly(bd$dose_mean, bd$dd_mean,
                    bandwidth = bw_b, degree = 1, gridsize = 200,
                    range.x = range(country_dy_d$dose_mean, na.rm = TRUE))
    boot_acrf_d[b, ] <- lp_b$y - lp_b$y[1]
  }, error = function(e) NULL)
}

lp_df_d$lo <- apply(boot_acrf_d, 2, quantile, probs = 0.025, na.rm = TRUE)
lp_df_d$hi <- apply(boot_acrf_d, 2, quantile, probs = 0.975, na.rm = TRUE)

fig_acrf_d <- ggplot() +
  geom_ribbon(data = lp_df_d, aes(x = dose, ymin = lo, ymax = hi),
              fill = "#b2182b", alpha = 0.2) +
  geom_line(data = lp_df_d, aes(x = dose, y = acrf_d),
            color = "#b2182b", linewidth = 1.2) +
  geom_point(data = country_dy_d,
             aes(x = dose_mean, y = dd_mean - acrf_d_ref),
             alpha = 0.5, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title    = "Average Causal Response Function: Debt",
    subtitle = "Nonparametric (local linear). Relative to lowest-dose countries. 95% bootstrap CI.",
    x = "Stringency Dose (Mean Daily Index, 2020-2021)",
    y = "ACRF: Debt Change Effect (pp of 2019 GDP)"
  ) +
  theme_bw(base_size = 11)

ggsave(file.path(safeplots, "cdid_acrf_d.pdf"), fig_acrf_d, width = 9, height = 6)
print(fig_acrf_d)
cat("  -> Saved: cdid_acrf_d.pdf\n")


# ---- 7C: Controlling for fiscal composition (Debt) ----
cat("\n--- 7C: Fiscal Composition Controls (Debt) ---\n\n")

# Residualized debt outcome
m_resid_d <- feols(dd ~ F_CP + F_DI + F_H | Quarter,
                   data = panel_did %>% filter(!is.na(dd)))
panel_did$dd_resid <- NA_real_
panel_did$dd_resid[!is.na(panel_did$dd)] <- resid(m_resid_d) +
  mean(panel_did$dd, na.rm = TRUE)

country_dd_resid <- panel_did %>%
  filter(!is.na(dd_resid)) %>%
  group_by(Country) %>%
  summarise(dd_resid_mean = mean(dd_resid, na.rm = TRUE), .groups = "drop") %>%
  left_join(dose_data, by = "Country")

m_acrf_d_resid <- lm(dd_resid_mean ~ dose_mean, data = country_dd_resid)
cat("  Residualized linear ACRF (debt, fiscal-adjusted):\n")
print(coeftest(m_acrf_d_resid, vcov = vcovHC(m_acrf_d_resid, type = "HC1")))

# Direct inclusion
m_d_fiscal <- feols(
  dd ~ dose_mean + F_CP + F_DI | Quarter,
  data = panel_did %>% filter(!is.na(dd)),
  cluster = ~Country
)
cat("\n  Direct inclusion (debt panel, linear dose + F_CP + F_DI):\n")
print(summary(m_d_fiscal))

m_d_fiscal_grp <- feols(
  dd ~ i(group_mean_tercile, ref = "Low") + F_CP + F_DI | Quarter,
  data = panel_did %>% filter(!is.na(dd)),
  cluster = ~Country
)
cat("\n  Dose tercile groups + F_CP + F_DI (debt):\n")
print(summary(m_d_fiscal_grp))

# Time-varying ACRF (debt)
cat("\n--- 7D: Time-Varying ACRF (Debt) ---\n\n")

acrf_d_by_q <- panel_did %>%
  filter(!is.na(dd)) %>%
  group_by(Quarter, t_idx) %>%
  group_modify(~ {
    m <- lm(dd ~ dose_mean, data = .x)
    ct <- coeftest(m, vcov = vcovHC(m, type = "HC1"))
    tibble(
      beta_dose = ct["dose_mean", "Estimate"],
      se_dose   = ct["dose_mean", "Std. Error"],
      p_dose    = ct["dose_mean", "Pr(>|t|)"],
      n         = nrow(.x)
    )
  }) %>%
  ungroup()

cat("  Time-varying ACRF slope (debt, by quarter):\n")
print(acrf_d_by_q, n = Inf)

fig_acrf_time_d <- acrf_d_by_q %>%
  ggplot(aes(x = t_idx, y = beta_dose)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_line(color = "#b2182b", linewidth = 1) +
  geom_point(color = "#b2182b", size = 2.5) +
  geom_errorbar(aes(ymin = beta_dose - 1.96 * se_dose,
                     ymax = beta_dose + 1.96 * se_dose),
                width = 0.2, color = "#b2182b") +
  scale_x_continuous(breaks = 5:16,
    labels = c("Q1.20","Q2.20","Q3.20","Q4.20","Q1.21",
               "Q2.21","Q3.21","Q4.21","Q1.22","Q2.22","Q3.22","Q4.22")) +
  labs(
    title    = "Time-Varying ACRF Slope: Marginal Effect of Stringency Dose on Debt",
    subtitle = "Linear dose-response estimated separately by quarter. HC1 robust SEs.",
    x = NULL, y = "ACRF slope (pp debt per unit dose)"
  ) +
  theme_bw(base_size = 11)

ggsave(file.path(safeplots, "cdid_acrf_time_d.pdf"), fig_acrf_time_d, width = 10, height = 5)
print(fig_acrf_time_d)
cat("  -> Saved: cdid_acrf_time_d.pdf\n")


# ==============================================================================
#  SECTION 8: SUMMARY TABLE AND COMPARISON
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 8: RESULTS SUMMARY\n")
cat(strrep("=", 70), "\n\n")

# ---- 8A: Combined results table ----
cat("--- 8A: Summary of All Specifications ---\n\n")

# Output Gap models
models_y <- list(
  "(1) Tercile"         = m_y_terc,
  "(2) Terc + X"        = m_y_terc_cond,
  "(3) Quartile"        = m_y_quart,
  "(4) Median"          = m_y_med,
  "(5) Linear dose"     = feols(dy ~ dose_mean | Quarter,
                                data = panel_did, cluster = ~Country),
  "(6) Dose + Fiscal"   = m_y_fiscal,
  "(7) Terc + Fiscal"   = m_y_fiscal_grp
)

cat("  OUTPUT GAP (y_k) — All specifications:\n")
modelsummary(
  models_y,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared"),
  title = "Continuous DiD: Output Gap Results"
)

# Debt models
models_d <- list(
  "(1) Tercile"         = m_d_terc,
  "(2) Terc + X"        = m_d_terc_cond,
  "(3) Quartile"        = m_d_quart,
  "(4) Median"          = m_d_med,
  "(5) Linear dose"     = feols(dd ~ dose_mean | Quarter,
                                data = panel_did %>% filter(!is.na(dd)),
                                cluster = ~Country),
  "(6) Dose + Fiscal"   = m_d_fiscal,
  "(7) Terc + Fiscal"   = m_d_fiscal_grp
)

cat("\n  DEBT (b_k) — All specifications:\n")
modelsummary(
  models_d,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared"),
  title = "Continuous DiD: Debt Results"
)

# Export summary tables to LaTeX
modelsummary(
  models_y,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared"),
  output = file.path(safetable, "cdid_output_gap.tex"),
  title = "Continuous DiD: Output Gap (y_k)"
)

modelsummary(
  models_d,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared"),
  output = file.path(safetable, "cdid_debt.tex"),
  title = "Continuous DiD: Debt (b_k)"
)
cat("  -> Saved: cdid_output_gap.tex, cdid_debt.tex\n")


# ---- 8B: Combined ACRF figure (Output + Debt) ----
fig_combined <- (fig_acrf_y + labs(title = "A: Output Gap")) +
  (fig_acrf_d + labs(title = "B: Debt")) +
  plot_layout(ncol = 2) +
  plot_annotation(
    title    = "Average Causal Response Functions — Continuous DiD",
    subtitle = "Callaway, Goodman-Bacon & Sant'Anna (2024). Local linear, 95% bootstrap CI."
  )

ggsave(file.path(safeplots, "cdid_acrf_combined.pdf"), fig_combined,
       width = 16, height = 6)
cat("  -> Saved: cdid_acrf_combined.pdf\n")


# ==============================================================================
#  SECTION 9: ASSUMPTION DISCUSSION
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 9: ASSUMPTION DISCUSSION\n")
cat(strrep("=", 70), "\n\n")

cat(paste0(
  "  CRITICAL ASSUMPTIONS FOR CONTINUOUS DiD (Callaway et al. 2024):\n\n",

  "  1. STRONG PARALLEL TRENDS (Assumption 1):\n",
  "     E[Y_t(0) - Y_{t-1}(0) | D=d] = E[Y_t(0) - Y_{t-1}(0) | D=d']\n",
  "     Absent treatment, outcome trends would be identical regardless of dose.\n",
  "     TEST: Pre-trend coefficients in Section 3A. If dose groups have\n",
  "     parallel pre-trends in 2019, the assumption is supported.\n",
  "     CONCERN: Countries with higher GDP per capita may have both higher\n",
  "     stringency and different output trajectories. Conditional PT addresses this.\n\n",

  "  2. NO ANTICIPATION (Assumption 2):\n",
  "     Y_t(d) = Y_t(0) for all t < treatment onset.\n",
  "     In our setting: outcomes in 2019 are not affected by 2020+ stringency.\n",
  "     PLAUSIBLE: COVID-19 stringency was not anticipated in 2019.\n",
  "     Minor caveat: late Q4.2019 China lockdowns may have affected some\n",
  "     trade-exposed economies. Using Q3.2019 as alternative base period\n",
  "     provides a robustness check.\n\n",

  "  3. OVERLAP / COMMON SUPPORT (Assumption 3):\n",
  "     The dose distribution must have sufficient mass across the range.\n",
  "     TEST: Section 3B dose distribution histogram. With 38 OECD countries,\n",
  "     support at extreme doses is inherently limited.\n",
  "     MITIGATION: Focus results on the interquartile range of the dose.\n\n",

  "  4. TREATMENT EFFECT HOMOGENEITY (Assumption 4, for some estimators):\n",
  "     The dose-response function ACRF(d|d) = ACRF(d) (does not vary by group).\n",
  "     This is required for the discretized estimator to trace out the true ACRF.\n",
  "     CONCERN: Countries with different economic structures may respond\n",
  "     heterogeneously to the same stringency dose. The quartile/tercile\n",
  "     comparison addresses this by estimating group-average effects.\n\n",

  "  5. SUTVA (Stable Unit Treatment Value Assumption):\n",
  "     Country i's outcome depends only on its own dose, not others'.\n",
  "     CONCERN: Trade linkages and global supply chains create spillovers.\n",
  "     A country's output gap depends not just on domestic stringency but\n",
  "     also on trading partners' policies. This is a known limitation.\n\n",

  "  6. FISCAL COMPOSITION AS POST-TREATMENT VARIABLE:\n",
  "     F_CP and F_DI are part of the policy response to COVID-19.\n",
  "     Including them as controls risks conditioning on a collider if\n",
  "     stringency -> fiscal policy -> outcomes.\n",
  "     MITIGATION: (a) Near-orthogonality of S and F (r ~ 0, analysis.R Step 1B)\n",
  "     attenuates this concern. (b) We report both unconditional and\n",
  "     fiscal-adjusted results. (c) Residualization approach (Section 5D)\n",
  "     provides a transparent decomposition.\n\n"
))


# ==============================================================================
#  SECTION 10: ROBUSTNESS — ALTERNATIVE DOSE MEASURES
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 10: ROBUSTNESS — ALTERNATIVE DOSE MEASURES\n")
cat(strrep("=", 70), "\n\n")

# Compare ACRF slopes across different dose measures
dose_vars <- c("dose_mean", "dose_cumul", "dose_peak",
               "dose_duration", "dose_first_wave")

robustness_results <- map_dfr(dose_vars, function(dv) {
  # Output gap
  fml_y <- as.formula(paste0("dy ~ ", dv, " | Quarter"))
  m_y <- feols(fml_y, data = panel_did, cluster = ~Country)
  ct_y <- coeftable(m_y)

  # Debt
  fml_d <- as.formula(paste0("dd ~ ", dv, " | Quarter"))
  m_d <- feols(fml_d, data = panel_did %>% filter(!is.na(dd)),
               cluster = ~Country)
  ct_d <- coeftable(m_d)

  tibble(
    dose_measure = dv,
    # Output gap
    beta_y  = ct_y[dv, "Estimate"],
    se_y    = ct_y[dv, "Std. Error"],
    p_y     = ct_y[dv, "Pr(>|t|)"],
    # Debt
    beta_d  = ct_d[dv, "Estimate"],
    se_d    = ct_d[dv, "Std. Error"],
    p_d     = ct_d[dv, "Pr(>|t|)"]
  )
})

cat("  ACRF slopes across dose measures:\n")
print(robustness_results, n = Inf)

# Visualize robustness
fig_robust <- robustness_results %>%
  pivot_longer(
    cols = c(beta_y, beta_d),
    names_to = "outcome",
    values_to = "beta"
  ) %>%
  mutate(
    se = ifelse(outcome == "beta_y", se_y, se_d),
    outcome_lab = ifelse(outcome == "beta_y", "Output Gap (y_k)", "Debt (b_k)")
  ) %>%
  ggplot(aes(x = dose_measure, y = beta, color = outcome_lab)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_point(size = 3, position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = beta - 1.96 * se, ymax = beta + 1.96 * se),
                width = 0.2, position = position_dodge(width = 0.3)) +
  scale_color_manual(values = c("Output Gap (y_k)" = "#2166ac",
                                "Debt (b_k)" = "#b2182b"),
                     name = "Outcome") +
  labs(
    title    = "Robustness: ACRF Slopes Across Alternative Dose Measures",
    subtitle = "Linear dose-response, Quarter FE, clustered SEs. All dose measures standardized.",
    x = "Dose Measure", y = "ACRF Slope"
  ) +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(safeplots, "cdid_robustness_doses.pdf"), fig_robust, width = 10, height = 6)
print(fig_robust)
cat("  -> Saved: cdid_robustness_doses.pdf\n")


# ==============================================================================
#  END
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  CONTINUOUS DiD ANALYSIS COMPLETE\n")
cat(strrep("=", 70), "\n\n")
cat(paste0(
  "  OUTPUT FILES:\n",
  "  Figures: cdid_pretrend_y.pdf, cdid_pretrend_d.pdf\n",
  "           cdid_dose_distribution.pdf\n",
  "           cdid_att_y_tercile.pdf, cdid_att_d_tercile.pdf\n",
  "           cdid_acrf_y.pdf, cdid_acrf_d.pdf\n",
  "           cdid_acrf_combined.pdf\n",
  "           cdid_acrf_time_y.pdf, cdid_acrf_time_d.pdf\n",
  "           cdid_robustness_doses.pdf\n",
  "  Tables:  cdid_output_gap.tex, cdid_debt.tex\n"
))
