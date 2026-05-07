# ============================================================================
# CONTINUOUS DiD — REDUCED-FORM BENCHMARK
# Callaway, Goodman-Bacon & Sant'Anna (2025) via contdid package
# ============================================================================
#
# PURPOSE:
# This section applies the state-of-the-art continuous difference-in-
# differences estimator as a reduced-form benchmark. The results motivate
# the structural TWFE specification in the main analysis by demonstrating
# three fundamental limitations of the reduced-form approach in the
# pandemic policy setting:
#
#   (1) TWO SIMULTANEOUS TREATMENTS: Stringency (S) and fiscal composition
#       (F_CP, F_DI) are deployed jointly. The continuous DiD estimates a
#       composite dose-response that cannot separate the S x F_CP cushioning
#       channel (eta) identified in the structural model.
#
#   (2) DYNAMIC, TIME-VARYING DOSE: Stringency oscillates with pandemic
#       waves across quarters. contdid requires a time-invariant dose,
#       collapsing the entire S trajectory into a single mean — destroying
#       the within-country dynamics that the structural model exploits via
#       the persistence channel (psi * S * y_{k-1}).
#
#   (3) NO CLEAN TREATMENT ONSET: There is no single adoption date G.
#       Countries locked down, reopened, re-locked in response to successive
#       waves. The staggered-adoption framework assumes a permanent switch
#       from untreated to treated — fundamentally at odds with the oscillating
#       policy paths observed during COVID-19.
#
# DESPITE THESE LIMITATIONS, the continuous DiD provides:
#   - A credible pre-trend validation (20 pre-treatment quarters, 2015-2019)
#   - A reduced-form dose-response that is consistent with the structural
#     estimates (negative impact, positive recovery)
#   - Evidence that fiscal composition is a confounder the reduced-form
#     cannot properly handle, motivating the structural approach
# ============================================================================

library(contdid)
library(dplyr)
library(tidyr)
library(ggplot2)
library(fixest)
library(readxl)
library(patchwork)
library(lmtest)
library(sandwich)
library(lubridate)

conflicted::conflict_prefer("select",   "dplyr")
conflicted::conflict_prefer("filter",   "dplyr")
conflicted::conflicts_prefer(dplyr::lag)
conflicted::conflicts_prefer(dplyr::first)
conflicted::conflicts_prefer(dplyr::last)
conflicted::conflicts_prefer(lubridate::year)
conflicted::conflicts_prefer(lubridate::quarter)
conflicted::conflicts_prefer(lubridate::month)
conflicted::conflicts_prefer(fixest::pvalue)

set.seed(1234)

base_dir  <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files"
safeplots <- file.path(base_dir, "output/figures")
safetable <- file.path(base_dir, "output/tables")

load(file.path(base_dir, "data/processed/dataforanalysis.RData"))
ox <- readr::read_csv(file.path(base_dir, "data/raw/oxford stringency/Oxcnat.csv"),
                       show_col_types = FALSE)
fm1 <- read_excel(file.path(base_dir, "data/raw/fiscal measures/fiscal_classified_v1_7.xlsx"))
fm1 <- fm1 %>%
  mutate(YQ = paste0("Q", Quarter, ".", Year),
         YQ_ord = factor(YQ, levels = sort(unique(paste0("Q", Quarter, ".", Year))),
                          ordered = TRUE)) %>%
  filter(!PolicyCode %in% c(5, 6, 11, 12, 15, 16))

oecd <- c("AUS","AUT","BEL","CAN","CHL","COL","CRI","CZE","DNK","EST",
  "FIN","FRA","DEU","GRC","HUN","ISL","IRL","ISR","ITA","JPN",
  "LVA","LTU","LUX","MEX","NLD","NZL","NOR","POL","PRT","SVK",
  "SVN","KOR","ESP","SWE","CHE","TUR","GBR","USA")

cat("\n", strrep("=", 70), "\n")
cat("  CONTINUOUS DiD — REDUCED-FORM BENCHMARK\n")
cat("  Callaway, Goodman-Bacon & Sant'Anna (2025)\n")
cat(strrep("=", 70), "\n\n")


# ==============================================================================
#  SECTION 1: DATA PREPARATION
# ==============================================================================

# Full quarter sequence Q1.2015 — Q4.2022 (20 pre + 12 post quarters)
all_quarters <- paste0("Q", rep(1:4, 8), ".", rep(2015:2022, each = 4))

# --- Dose: mean daily stringency 2020-2021 (time-invariant per country) ---
ox2 <- ox %>%
  filter(CountryCode %in% oecd, Jurisdiction == "NAT_TOTAL") %>%
  mutate(date = ymd(Date), Country = CountryCode) %>%
  filter(date >= "2020-01-01", date <= "2021-12-31",
         !is.na(StringencyIndex_Average))

dose <- ox2 %>%
  group_by(Country) %>%
  summarise(D = mean(StringencyIndex_Average, na.rm = TRUE), .groups = "drop")

# --- G: comparison group = bottom 20% of dose (8 countries) ---
low_threshold <- quantile(dose$D, 0.20)
g_df <- dose %>% mutate(G = ifelse(D <= low_threshold, 0L, 22L))
# G=22 = Q2.2020 (the 22nd quarter: Q1.2015=1, ..., Q2.2020=22)

cat(sprintf("  Dose range: %.1f to %.1f (mean=%.1f)\n",
            min(dose$D), max(dose$D), mean(dose$D)))
cat(sprintf("  Comparison group (G=0, dose<=%.1f): %d countries\n",
            low_threshold, sum(g_df$G == 0)))
cat("    ", paste(g_df$Country[g_df$G == 0], collapse = ", "), "\n")
cat(sprintf("  Treated (G=22, Q2.2020): %d countries\n\n", sum(g_df$G > 0)))

# --- Fiscal composition (cumulative 2020-2021) ---
fiscal_cum <- fm1 %>%
  filter(broad_fiscal == 1) %>%
  mutate(Quarter_fmt = paste0("Q", Quarter, ".", Year)) %>%
  filter(Quarter_fmt %in% all_quarters[21:28]) %>%  # Q1.2020-Q4.2021
  group_by(Country) %>%
  summarise(
    F_CP_cum = sum(broad_fiscal_gdp[transmission_channel == "CP"], na.rm = TRUE) * 100,
    F_DI_cum = sum(broad_fiscal_gdp[transmission_channel == "DI"], na.rm = TRUE) * 100,
    F_H_cum  = sum(broad_fiscal_gdp[transmission_channel == "H"],  na.rm = TRUE) * 100,
    F_tot_cum = sum(broad_fiscal_gdp, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(CP_share = ifelse(F_tot_cum > 0, F_CP_cum / F_tot_cum, NA_real_))

# --- Quarterly fiscal panel ---
df_fiscal <- fm1 %>%
  filter(broad_fiscal == 1) %>%
  mutate(Quarter = as.character(YQ_ord)) %>%
  filter(Quarter %in% all_quarters) %>%
  group_by(Country, Quarter) %>%
  summarise(
    F_CP = sum(broad_fiscal_gdp[transmission_channel == "CP"], na.rm = TRUE) * 100,
    F_DI = sum(broad_fiscal_gdp[transmission_channel == "DI"], na.rm = TRUE) * 100,
    F_H  = sum(broad_fiscal_gdp[transmission_channel == "H"],  na.rm = TRUE) * 100,
    .groups = "drop"
  )

# --- Build panel: Q1.2015 — Q4.2022 ---
panel <- qdata %>%
  filter(Quarter %in% all_quarters) %>%
  select(Country, Quarter, year_only, y_t_pct, DebtR_share2019,
         rGDP_pc_2019, debt_2019) %>%
  mutate(t_idx = match(Quarter, all_quarters)) %>%
  left_join(dose, by = "Country") %>%
  left_join(g_df %>% select(Country, G), by = "Country") %>%
  left_join(fiscal_cum %>% select(Country, CP_share, F_CP_cum, F_DI_cum),
            by = "Country") %>%
  left_join(df_fiscal, by = c("Country", "Quarter")) %>%
  mutate(F_CP = replace_na(F_CP, 0), F_DI = replace_na(F_DI, 0),
         F_H = replace_na(F_H, 0))

# Numeric country id
ids <- panel %>% distinct(Country) %>% arrange(Country) %>% mutate(id = row_number())
panel <- panel %>% left_join(ids, by = "Country")

# Balance check
obs_per <- table(panel$Country)
full_T <- max(obs_per)
panel_bal <- panel %>% filter(Country %in% names(obs_per[obs_per == full_T]))

cat(sprintf("  Panel: %d obs, %d countries, %d quarters (Q1.2015-Q4.2022)\n",
            nrow(panel_bal), n_distinct(panel_bal$Country),
            n_distinct(panel_bal$Quarter)))
cat(sprintf("  Pre-treatment periods: %d quarters (Q1.2015-Q4.2019)\n", 20))
cat(sprintf("  Post-treatment periods: %d quarters (Q1.2020-Q4.2022)\n\n", 12))


# ==============================================================================
#  SECTION 2: PRE-TREND VALIDATION (20 quarters, 2015-2019)
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("  SECTION 2: PRE-TREND VALIDATION (2015-2019)\n")
cat(strrep("=", 70), "\n\n")

# Base period: Q4.2019 (t_idx = 20)
base_y <- panel_bal %>%
  filter(t_idx == 20) %>%
  select(Country, y_base = y_t_pct, d_base = DebtR_share2019)

pre_data <- panel_bal %>%
  filter(t_idx <= 19) %>%  # Q1.2015 - Q3.2019
  left_join(base_y, by = "Country") %>%
  mutate(dy_pre = y_t_pct - y_base)

# Tercile groups for visual
dose_terciles <- dose %>%
  mutate(dose_group = cut(D,
    breaks = quantile(D, c(0, 1/3, 2/3, 1)),
    labels = c("Low", "Medium", "High"), include.lowest = TRUE)) %>%
  select(Country, dose_group)

# --- Formal pre-trend test ---
pt_test <- feols(dy_pre ~ D | t_idx, data = pre_data, cluster = ~Country)
cat("  Pre-trend test (dy ~ dose | quarter FE, 19 pre-periods):\n")
print(summary(pt_test))
cat(sprintf("\n  dose coefficient: %.4f (SE=%.4f, p=%.4f) — %s\n\n",
            coef(pt_test)["D"], se(pt_test)["D"], pvalue(pt_test)["D"],
            ifelse(pvalue(pt_test)["D"] > 0.10,
                   "PASS: no differential pre-trend",
                   "CAUTION: differential pre-trend")))

# --- Visual: pre-trends by dose tercile ---
pre_trends <- pre_data %>%
  left_join(dose_terciles, by = "Country") %>%
  group_by(t_idx, dose_group) %>%
  summarise(mean_y = mean(y_t_pct, na.rm = TRUE),
            se_y   = sd(y_t_pct, na.rm = TRUE) / sqrt(n()),
            .groups = "drop")

q_labels <- all_quarters[1:20]
fig_pretrend <- ggplot(pre_trends,
  aes(x = t_idx, y = mean_y, color = dose_group, group = dose_group)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  geom_ribbon(aes(ymin = mean_y - 1.96 * se_y, ymax = mean_y + 1.96 * se_y,
                  fill = dose_group), alpha = 0.12, color = NA) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = 20.5, linetype = "dotted", color = "red", linewidth = 0.8) +
  annotate("text", x = 20.5, y = max(pre_trends$mean_y) + 0.5,
           label = "COVID onset", color = "red", hjust = -0.1, size = 3) +
  scale_x_continuous(breaks = seq(1, 20, 4),
    labels = q_labels[seq(1, 20, 4)]) +
  scale_color_manual(values = c("Low" = "#2166ac", "Medium" = "#f4a582",
                                "High" = "#b2182b"), name = "Stringency\nDose Group") +
  scale_fill_manual(values = c("Low" = "#2166ac", "Medium" = "#f4a582",
                                "High" = "#b2182b"), guide = "none") +
  labs(title = "Pre-Trend Validation: Output Gap by Stringency Dose Group (2015-2019)",
       subtitle = "20 pre-treatment quarters. Parallel trends support the identifying assumption.",
       x = NULL, y = "Output gap (pp of potential GDP)") +
  theme_bw(base_size = 11)

ggsave(file.path(safeplots, "cdid_pretrend_y_full.pdf"), fig_pretrend, width = 11, height = 5.5)
print(fig_pretrend)
cat("  -> Saved: cdid_pretrend_y_full.pdf\n\n")


# ==============================================================================
#  SECTION 3: CONTINUOUS DiD ESTIMATION
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("  SECTION 3: CONTINUOUS DiD ESTIMATION (contdid)\n")
cat(strrep("=", 70), "\n\n")

# Prepare data for contdid
df_y <- panel_bal %>%
  filter(!is.na(y_t_pct)) %>%
  select(id, t_idx, Y = y_t_pct, D, G) %>%
  as.data.frame()
df_y$D[df_y$G == 0] <- 0

df_d <- panel_bal %>%
  filter(!is.na(DebtR_share2019)) %>%
  select(id, t_idx, Y = DebtR_share2019, D, G) %>%
  as.data.frame()
df_d$D[df_d$G == 0] <- 0

# --- 3A: Output Gap — ATT and ACRT by dose ---
cat("--- 3A: Output Gap — ATT & ACRT by dose ---\n")

cd_y_att <- tryCatch(
  cont_did(yname = "Y", tname = "t_idx", idname = "id", dname = "D", gname = "G",
    data = df_y, target_parameter = "level", aggregation = "dose",
    treatment_type = "continuous", control_group = "notyettreated",
    degree = 1, num_knots = 0, biters = 1000, cband = TRUE, print_details = TRUE),
  error = function(e) { cat(sprintf("  ERROR: %s\n", e$message)); NULL })

if (!is.null(cd_y_att)) {
  cat("\n  === ATT by Dose (Output Gap) ===\n")
  summary(cd_y_att)
}

cd_y_acrt <- tryCatch(
  cont_did(yname = "Y", tname = "t_idx", idname = "id", dname = "D", gname = "G",
    data = df_y, target_parameter = "slope", aggregation = "dose",
    treatment_type = "continuous", control_group = "notyettreated",
    degree = 1, num_knots = 0, biters = 1000, cband = TRUE, print_details = TRUE),
  error = function(e) { cat(sprintf("  ERROR: %s\n", e$message)); NULL })

if (!is.null(cd_y_acrt)) {
  cat("\n  === ACRT by Dose (Output Gap) ===\n")
  summary(cd_y_acrt)
}

# --- 3B: Output Gap — Event Study ACRT ---
cat("\n--- 3B: Output Gap — Event Study ACRT ---\n")

cd_y_es <- tryCatch(
  cont_did(yname = "Y", tname = "t_idx", idname = "id", dname = "D", gname = "G",
    data = df_y, target_parameter = "slope", aggregation = "eventstudy",
    treatment_type = "continuous", control_group = "notyettreated",
    degree = 1, num_knots = 0, biters = 1000, cband = TRUE, print_details = TRUE),
  error = function(e) { cat(sprintf("  ERROR: %s\n", e$message)); NULL })

if (!is.null(cd_y_es)) {
  cat("\n  === Event Study ACRT (Output Gap) ===\n")
  summary(cd_y_es)
}

# --- 3C: Debt — ATT, ACRT, Event Study ---
cat("\n--- 3C: Debt — ATT & ACRT by dose ---\n")

cd_d_att <- tryCatch(
  cont_did(yname = "Y", tname = "t_idx", idname = "id", dname = "D", gname = "G",
    data = df_d, target_parameter = "level", aggregation = "dose",
    treatment_type = "continuous", control_group = "notyettreated",
    degree = 1, num_knots = 0, biters = 1000, cband = TRUE, print_details = TRUE),
  error = function(e) { cat(sprintf("  ERROR: %s\n", e$message)); NULL })

if (!is.null(cd_d_att)) {
  cat("\n  === ATT by Dose (Debt) ===\n")
  summary(cd_d_att)
}

cd_d_acrt <- tryCatch(
  cont_did(yname = "Y", tname = "t_idx", idname = "id", dname = "D", gname = "G",
    data = df_d, target_parameter = "slope", aggregation = "dose",
    treatment_type = "continuous", control_group = "notyettreated",
    degree = 1, num_knots = 0, biters = 1000, cband = TRUE, print_details = TRUE),
  error = function(e) { cat(sprintf("  ERROR: %s\n", e$message)); NULL })

if (!is.null(cd_d_acrt)) {
  cat("\n  === ACRT by Dose (Debt) ===\n")
  summary(cd_d_acrt)
}

cat("\n--- 3D: Debt — Event Study ACRT ---\n")

cd_d_es <- tryCatch(
  cont_did(yname = "Y", tname = "t_idx", idname = "id", dname = "D", gname = "G",
    data = df_d, target_parameter = "slope", aggregation = "eventstudy",
    treatment_type = "continuous", control_group = "notyettreated",
    degree = 1, num_knots = 0, biters = 1000, cband = TRUE, print_details = TRUE),
  error = function(e) { cat(sprintf("  ERROR: %s\n", e$message)); NULL })

if (!is.null(cd_d_es)) {
  cat("\n  === Event Study ACRT (Debt) ===\n")
  summary(cd_d_es)
}


# ==============================================================================
#  SECTION 4: FISCAL COMPOSITION — RESIDUALIZED OUTCOMES
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 4: FISCAL COMPOSITION TEST\n")
cat(strrep("=", 70), "\n\n")

# Residualize Y w.r.t. fiscal policy (post-treatment only)
post_idx <- which(panel_bal$t_idx >= 21)  # Q1.2020+
m_res_y <- lm(y_t_pct ~ F_CP + F_DI + F_H + factor(t_idx),
              data = panel_bal[post_idx, ])
panel_bal$y_resid <- panel_bal$y_t_pct
panel_bal$y_resid[post_idx] <- residuals(m_res_y) +
  mean(panel_bal$y_t_pct[post_idx], na.rm = TRUE)

cat("  Fiscal regression (post-treatment output gap):\n")
print(coeftest(m_res_y, vcov = vcovHC(m_res_y, type = "HC1"))[1:3, ])

df_yr <- panel_bal %>%
  filter(!is.na(y_resid)) %>%
  select(id, t_idx, Y = y_resid, D, G) %>%
  as.data.frame()
df_yr$D[df_yr$G == 0] <- 0

cat("\n--- Residualized ACRT (Output Gap, fiscal-adjusted) ---\n")
cd_yr <- tryCatch(
  cont_did(yname = "Y", tname = "t_idx", idname = "id", dname = "D", gname = "G",
    data = df_yr, target_parameter = "slope", aggregation = "dose",
    treatment_type = "continuous", control_group = "notyettreated",
    degree = 1, num_knots = 0, biters = 1000, cband = TRUE, print_details = FALSE),
  error = function(e) { cat(sprintf("  ERROR: %s\n", e$message)); NULL })

if (!is.null(cd_yr)) {
  cat("\n  Residualized ACRT (Output Gap):\n")
  summary(cd_yr)
}

# Same for debt
valid_d <- which(panel_bal$t_idx >= 21 & !is.na(panel_bal$DebtR_share2019))
m_res_d <- lm(DebtR_share2019 ~ F_CP + F_DI + F_H + factor(t_idx),
              data = panel_bal[valid_d, ])
panel_bal$d_resid <- panel_bal$DebtR_share2019
panel_bal$d_resid[valid_d] <- residuals(m_res_d) +
  mean(panel_bal$DebtR_share2019[valid_d], na.rm = TRUE)

cat("\n  Fiscal regression (post-treatment debt):\n")
print(coeftest(m_res_d, vcov = vcovHC(m_res_d, type = "HC1"))[1:3, ])

df_dr <- panel_bal %>%
  filter(!is.na(d_resid)) %>%
  select(id, t_idx, Y = d_resid, D, G) %>%
  as.data.frame()
df_dr$D[df_dr$G == 0] <- 0

cat("\n--- Residualized ACRT (Debt, fiscal-adjusted) ---\n")
cd_dr <- tryCatch(
  cont_did(yname = "Y", tname = "t_idx", idname = "id", dname = "D", gname = "G",
    data = df_dr, target_parameter = "slope", aggregation = "dose",
    treatment_type = "continuous", control_group = "notyettreated",
    degree = 1, num_knots = 0, biters = 1000, cband = TRUE, print_details = FALSE),
  error = function(e) { cat(sprintf("  ERROR: %s\n", e$message)); NULL })

if (!is.null(cd_dr)) {
  cat("\n  Residualized ACRT (Debt):\n")
  summary(cd_dr)
}


# ==============================================================================
#  SECTION 5: SAVE FIGURES
# ==============================================================================

if (!is.null(cd_y_att)) {
  pdf(file.path(safeplots, "cdid_contdid_att_y.pdf"), width = 9, height = 6)
  print(ggcont_did(cd_y_att)); dev.off()
}
if (!is.null(cd_y_acrt)) {
  pdf(file.path(safeplots, "cdid_contdid_acrt_y.pdf"), width = 9, height = 6)
  print(ggcont_did(cd_y_acrt)); dev.off()
}
if (!is.null(cd_y_es)) {
  pdf(file.path(safeplots, "cdid_contdid_es_acrt_y.pdf"), width = 10, height = 6)
  print(ggcont_did(cd_y_es)); dev.off()
}
if (!is.null(cd_d_att)) {
  pdf(file.path(safeplots, "cdid_contdid_att_d.pdf"), width = 9, height = 6)
  print(ggcont_did(cd_d_att)); dev.off()
}
if (!is.null(cd_d_acrt)) {
  pdf(file.path(safeplots, "cdid_contdid_acrt_d.pdf"), width = 9, height = 6)
  print(ggcont_did(cd_d_acrt)); dev.off()
}
if (!is.null(cd_d_es)) {
  pdf(file.path(safeplots, "cdid_contdid_es_acrt_d.pdf"), width = 10, height = 6)
  print(ggcont_did(cd_d_es)); dev.off()
}


# ==============================================================================
#  SECTION 6: CONCLUSION — WHY CONTINUOUS DiD IS INSUFFICIENT HERE
# ==============================================================================
#
# The continuous DiD of Callaway, Goodman-Bacon & Sant'Anna (2025)
# provides a credible reduced-form benchmark for the pandemic trilemma.
# The pre-trend assumption is validated over 20 quarters (2015-2019), and
# the event-study ACRT reveals a significant negative impact of stringency
# on output at treatment onset, followed by a positive recovery — consistent
# with the structural estimates.
#
# However, three features of the pandemic policy setting fundamentally
# limit what this estimator can identify:
#
# -----------------------------------------------------------------------
# LIMITATION 1: DUAL TREATMENT — S AND F ARE DEPLOYED JOINTLY
# -----------------------------------------------------------------------
#
# The pandemic triggered two simultaneous policy responses: containment
# (stringency S) and fiscal support (F_CP, F_DI). The continuous DiD
# treats S as the single dose and estimates a composite ACRT that bundles:
#
#   ACRT_reduced_form = dY/dS_direct + dY/dF * dF/dS
#
# This composite cannot decompose the three CP channels identified in
# the structural model:
#   (a) alpha_CP:  direct level effect of CP on output
#   (b) eta:       S x F_CP cushioning (CP effectiveness depends on lockdown)
#   (c) eta_p:     F_CP x y_{k-1} persistence reduction
#
# EVIDENCE: The residualization test (Section 4) shows:
#   - Output ACRT is stable after fiscal adjustment (0.13 -> 0.11) because
#     S and F_CP are near-orthogonal (r = 0.065)
#   - Debt ACRT changes substantially (0.19 -> 0.65) because fiscal spending
#     IS a key channel through which stringency transmits to debt
#   This confirms that fiscal composition is a confounder that the reduced-
#   form approach cannot properly separate from the direct stringency effect.
#
# The structural TWFE specification solves this by including S, F_CP, F_DI,
# and the S x F_CP interaction as separate regressors, identifying each
# channel under the orthogonality condition validated in analysis.R Step 1B.
#
# -----------------------------------------------------------------------
# LIMITATION 2: THE DOSE IS DYNAMIC AND TIME-VARYING
# -----------------------------------------------------------------------
#
# contdid requires the dose D to be time-invariant (fixed per country).
# We use the 2020-2021 mean of daily StringencyIndex as D. But stringency
# oscillated dramatically across pandemic waves:
#
#   - Q2.2020: Most countries at peak lockdown (S > 70)
#   - Q3.2020: Reopening (S drops to 40-50)
#   - Q4.2020-Q1.2021: Second/third wave lockdowns (S rises again)
#   - Q2-Q3.2021: Vaccination-driven reopening (S declines)
#
# Two countries with identical mean dose (D = 55) may have had:
#   (a) Steady moderate restrictions throughout (low adjustment costs)
#   (b) Sharp oscillations between full lockdown and full reopening
#       (high uncertainty, firm-exit risk, hysteresis)
#
# The structural model captures this via the ψ * S_k * y_{k-1} persistence
# channel: the output cost depends not just on S_k but on the interaction
# of current stringency with the inherited output gap. The continuous DiD
# collapses this dynamic mechanism into a single static number.
#
# Similarly, fiscal composition evolved over time: CP concentrated in
# Q1-Q2.2020 (immediate crisis response), DI peaked 1-2 quarters later
# (disbursement lag). The structural model uses F_DI at lag 2 to capture
# this timing; the continuous DiD cannot distinguish contemporaneous from
# lagged effects.
#
# -----------------------------------------------------------------------
# LIMITATION 3: NO CLEAN TREATMENT ONSET (STAGGERED ADOPTION VIOLATED)
# -----------------------------------------------------------------------
#
# The staggered adoption framework assumes:
#   - Units are untreated until period G, then permanently treated
#   - D is the dose received upon treatment adoption
#
# In the pandemic:
#   - ALL countries received some stringency (no true "never-treated")
#   - Stringency was turned on AND off repeatedly (not permanent)
#   - The comparison group (8 low-dose countries) still experienced real
#     treatment — EST, FIN, ISL, JPN, LUX, LVA, NOR, NZL had stringency
#     indices of 37-46, not zero
#   - NZL pursued elimination (border closure) rather than low-stringency,
#     making it a different policy regime, not a lesser dose of the same one
#
# The G assignment is necessarily arbitrary: any threshold between 20 and 60
# produces a different comparison group and different results. The structural
# TWFE approach avoids this by using within-country time variation in S,
# with quarter FE absorbing common pandemic waves and country FE absorbing
# institutional heterogeneity.
#
# -----------------------------------------------------------------------
# WHAT THE CONTINUOUS DiD DOES CONTRIBUTE
# -----------------------------------------------------------------------
#
# Despite these limitations, the continuous DiD provides three useful inputs:
#
# 1. PRE-TREND VALIDATION: The 20-quarter pre-period confirms that dose
#    groups trended in parallel before COVID. This supports the exogeneity
#    of S (conditional on FE) assumed in the structural model.
#
# 2. REDUCED-FORM CONSISTENCY: The event-study ACRT shows:
#    - Negative impact at onset (ACRT = -0.28** at e=0)
#    - Positive recovery from e=6 onward (ACRT = +0.21 to +0.42*)
#    This matches the structural model's finding: S has a direct negative
#    effect (alpha_S < 0) but higher-S countries recover faster (ψ > 0
#    combined with F_CP cushioning → faster mean-reversion).
#
# 3. FISCAL CONFOUNDING DIAGNOSIS: The residualization test confirms that
#    fiscal composition must be modeled explicitly — motivating the
#    structural specification with separate F_CP, F_DI, and S x F_CP terms.
#
# -----------------------------------------------------------------------
# SUMMARY TABLE
# -----------------------------------------------------------------------
#
#  Feature              | contdid (reduced-form)  | Structural TWFE
#  ---------------------|-------------------------|-------------------------
#  Treatment            | Single dose D           | S, F_CP, F_DI, S*F_CP
#  Time variation       | Collapsed to mean       | Quarter-by-quarter
#  Comparison group     | Arbitrary threshold     | Within-country FE
#  Dynamic effects      | Static dose-response    | psi*S*y_{k-1} persistence
#  Fiscal channels      | Bundled or residualized | Separate coefficients
#  DI timing            | Contemporaneous         | Lag 2 (empirically selected)
#  Identification       | Cross-country dose      | Within-country S x F_CP
#  Pre-trends           | 20 quarters (strong)    | Hausman/Mundlak test
#
# The continuous DiD serves as a valid reduced-form benchmark that is
# CONSISTENT with but LESS INFORMATIVE than the structural TWFE results.
# The structural approach is required to identify the fiscal composition
# channels (alpha_CP, eta, eta_p) that are the core contribution of the
# pandemic trilemma model.
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  ANALYSIS COMPLETE\n")
cat(strrep("=", 70), "\n\n")
cat("  See Section 6 comments for full methodological conclusion.\n")
cat("  Figures saved to: ", safeplots, "\n\n")
