# ============================================================================
# DEBT EQUATION V15 - ROBUSTNESS BATTERY        (updated 2026-06-15)
# ----------------------------------------------------------------------------
# Main spec (V15, 15.06.2026):
#   debt_v15 <- feols(debt_dR_adj ~ y_lag1 + F_CP_above_3 + F_CP_loans_mid
#                     + F_CP_guar_lo + F_DI + F_H_lag1 | Country,
#                     data = pdataD, subset = ~ t_idx >= 4 & t_idx <= 16)
#   DV  = interest-adjusted real debt change (debt_dR - r_t * b_{i,k-1});
#   CP  = above-the-line flow + loans (60% take-up) + guarantees (25%);
#   DI  contemporaneous; health spending at lag 1; country FE; SE by country.
# Every check below perturbs ONE element of this spec.
# ============================================================================
library(fixest); library(dplyr); library(car)

# --- Canonical V15 spec reused by every check -------------------------------
rhs_v15       <- c("y_lag1", "F_CP_above_3", "F_CP_loans_mid",
                   "F_CP_guar_lo", "F_DI", "F_H_lag1")
main_sub_debt <- ~ t_idx >= 4 & t_idx <= 16
ssc_v15       <- ssc(K.adj = TRUE, G.adj = TRUE)
keep_v15      <- c("y_lag1", "F_CP_above_3", "F_CP_loans_mid", "F_CP_loans_mid_lag1",
                   "F_CP_guar_lo", "F_DI", "F_H", "F_H_lag1")

fdebt <- function(dv, rhs, fe = "Country")
  as.formula(paste(dv, "~", paste(rhs, collapse = " + "), "|", fe))
show  <- function(m, title) { cat("\n==========", title, "==========\n")
  print(summary(m, cluster = ~ Country, ssc = ssc_v15)) }

main_fml_debt <- fdebt("debt_dR_adj", rhs_v15)
debt_v15      <- feols(main_fml_debt, data = pdataD, subset = main_sub_debt)
show(debt_v15, "V15 MAIN (reference)")

# Construct lags needed by the checks below (loans/guarantees, y).
pdataD <- pdataD |>
  group_by(Country) |> arrange(t_idx) |>
  mutate(y_lag2              = lag(y_t_pct, 2),
         F_CP_loans_mid_lag1 = lag(F_CP_loans_mid, 1),
         F_CP_guar_lo_lag1   = lag(F_CP_guar_lo,   1)) |>
  ungroup()

# ---------------------------------------------------------------------------
# (1) OUTPUT-GAP TIMING  [bullet 1]
#     V15 uses predetermined y_lag1; test contemporaneous y_t_pct and y_lag2.
# ---------------------------------------------------------------------------
d_y0 <- feols(fdebt("debt_dR_adj", c("y_t_pct", rhs_v15[-1])), pdataD, subset = main_sub_debt)
d_y2 <- feols(fdebt("debt_dR_adj", c("y_lag2",  rhs_v15[-1])), pdataD, subset = main_sub_debt)
show(d_y0, "(1) OUTPUT TIMING: contemporaneous y_t_pct")
show(d_y2, "(1) OUTPUT TIMING: y_lag2")

# ---------------------------------------------------------------------------
# (2) UNADJUSTED DEBT CHANGE  [bullet 2]
#     Drop the mechanical debt-service netting: DV = debt_dR (not debt_dR_adj).
# ---------------------------------------------------------------------------
d_unadj <- feols(fdebt("debt_dR", rhs_v15), pdataD, subset = main_sub_debt)
show(d_unadj, "(2) UNADJUSTED DV: debt_dR")

# ---------------------------------------------------------------------------
# (3) TAKE-UP SENSITIVITY GRID  [bullet 3]
#     Loans 40/60/80 % x guarantees 25/35/50 %, V15 RHS otherwise unchanged.
# ---------------------------------------------------------------------------
takeup_grid <- expand.grid(loans = c(0.40, 0.60, 0.80), guar = c(0.25, 0.35, 0.50))
for (i in seq_len(nrow(takeup_grid))) {
  tl <- takeup_grid$loans[i]; tg <- takeup_grid$guar[i]
  df_tmp <- pdataD |> mutate(loans_tu = F_CP_loans * tl, guar_tu = F_CP_guar * tg)
  m <- feols(debt_dR_adj ~ y_lag1 + F_CP_above_3 + loans_tu + guar_tu + F_DI + F_H_lag1 | Country,
             data = df_tmp, subset = main_sub_debt)
  show(m, sprintf("(3) TAKE-UP: loans %.0f%% | guar %.0f%%", tl * 100, tg * 100))
}

# ---------------------------------------------------------------------------
# (4) LAGGED BELOW-THE-LINE COMMITMENTS  [bullet 4]
#     Replace loans/guarantees with their one-quarter lags.
# ---------------------------------------------------------------------------
d_belowlag <- feols(
  fdebt("debt_dR_adj", c("y_lag1", "F_CP_above_3",
                         "F_CP_loans_mid_lag1", "F_CP_guar_lo_lag1", "F_DI", "F_H_lag1")),
  pdataD, subset = main_sub_debt)
show(d_belowlag, "(4) LAGGED loans + guarantees")

# ---------------------------------------------------------------------------
# (5) HEALTH-SPENDING TIMING  [bullet 5]
#     V15 uses F_H_lag1; test contemporaneous F_H and both F_H + F_H_lag1.
# ---------------------------------------------------------------------------
d_h0   <- feols(fdebt("debt_dR_adj", c("y_lag1","F_CP_above_3","F_CP_loans_mid","F_CP_guar_lo","F_DI","F_H")),
                pdataD, subset = main_sub_debt)
d_hbth <- feols(fdebt("debt_dR_adj", c("y_lag1","F_CP_above_3","F_CP_loans_mid","F_CP_guar_lo","F_DI","F_H","F_H_lag1")),
                pdataD, subset = main_sub_debt)
show(d_h0,   "(5) HEALTH timing: contemporaneous F_H")
show(d_hbth, "(5) HEALTH timing: F_H + F_H_lag1")

# ---------------------------------------------------------------------------
# (6) ESTIMATION WINDOW  [bullet 6]
# ---------------------------------------------------------------------------
d_s5  <- feols(main_fml_debt, pdataD, subset = ~ t_idx >= 5 & t_idx <= 16)
d_e14 <- feols(main_fml_debt, pdataD, subset = ~ t_idx >= 4 & t_idx <= 14)
d_e15 <- feols(main_fml_debt, pdataD, subset = ~ t_idx >= 4 & t_idx <= 15)
show(d_s5,  "(6) WINDOW: t_idx >= 5")
show(d_e14, "(6) WINDOW: t_idx <= 14")
show(d_e15, "(6) WINDOW: t_idx <= 15")

# ---------------------------------------------------------------------------
# (7) ALTERNATIVE TIME CONTROLS  [bullet 7]
#     Linear quarter trend; Year-FE; Quarter-FE (absorbs the deployment cycle).
# ---------------------------------------------------------------------------
d_trend <- feols(fdebt("debt_dR_adj", c(rhs_v15, "as.numeric(t_idx)")), pdataD, subset = main_sub_debt)
d_yrfe  <- feols(fdebt("debt_dR_adj", rhs_v15, fe = "Country + year_only"), pdataD, subset = main_sub_debt)
d_qfe   <- feols(fdebt("debt_dR_adj", rhs_v15, fe = "Country + t_idx"),     pdataD, subset = main_sub_debt)
show(d_trend, "(7) TIME: linear quarter trend")
show(d_yrfe,  "(7) TIME: Year-FE")
show(d_qfe,   "(7) TIME: Quarter-FE (absorbs deployment cycle)")

# ---------------------------------------------------------------------------
# (8) WILD-CLUSTER BOOTSTRAP  [bullet 8]   (requires fwildclusterboot)
# ---------------------------------------------------------------------------
if (requireNamespace("fwildclusterboot", quietly = TRUE)) {
  library(fwildclusterboot)
  d_boot <- feols(fdebt("debt_dR_adj", rhs_v15), data = pdataD |> filter(t_idx >= 4, t_idx <= 16) |>
                    mutate(Country = as.factor(Country)), vcov = ~ Country)
  set.seed(16031995); dqrng::dqset.seed(16031995)
  for (p in rhs_v15) {
    cat("\n========== (8) Wild-Cluster Bootstrap:", p, "==========\n")
    print(summary(boottest(d_boot, param = p, clustid = c("Country"),
                           B = 99999, type = "rademacher", impose_null = TRUE,
                           p_val_type = "two-tailed")))
  }
} else {
  cat("\n[(8) wild-cluster bootstrap skipped: fwildclusterboot not installed]\n")
}

# ---------------------------------------------------------------------------
# (9) LEAVE-ONE-COUNTRY-OUT JACKKNIFE  [bullet 9]
# ---------------------------------------------------------------------------
ctys <- unique(as.character(pdataD$Country[pdataD$t_idx >= 4 & pdataD$t_idx <= 16]))
loo  <- t(sapply(ctys, function(g) {
  m <- feols(main_fml_debt, data = subset(pdataD, as.character(Country) != g), subset = main_sub_debt)
  coef(m)[c("F_CP_loans_mid", "F_CP_above_3", "y_lag1")]
}))
b0 <- coef(debt_v15)[c("F_CP_loans_mid", "F_CP_above_3", "y_lag1")]
cat("\n========== (9) LEAVE-ONE-COUNTRY-OUT JACKKNIFE ==========\n")
cat("full-sample coefs:\n"); print(round(b0, 4))
cat("jackknife ranges:\n")
for (k in colnames(loo)) cat(sprintf("  %-16s [%.4f, %.4f]\n", k, min(loo[, k]), max(loo[, k])))
cat("most influential (|delta| on F_CP_loans_mid):\n")
print(round(sort(abs(loo[, "F_CP_loans_mid"] - b0["F_CP_loans_mid"]), decreasing = TRUE)[1:3], 4))

# ---------------------------------------------------------------------------
# (10) OUTLIER EXCLUSION  [bullet 10]
#      TUR (inflation), IRL (MNC-driven GDP); option: drop extreme debt jumps.
# ---------------------------------------------------------------------------
d_noout <- feols(main_fml_debt, pdataD,
                 subset = ~ t_idx >= 4 & t_idx <= 16 & !Country %in% c("TUR", "IRL"))
show(d_noout, "(10) EXCLUDING TUR + IRL")

# ---------------------------------------------------------------------------
# (+) AGGREGATION: pooled below-the-line (loans + guarantees as one stock)
# ---------------------------------------------------------------------------
df_pool <- pdataD |> mutate(F_CP_below_mid = F_CP_loans_mid + F_CP_guar_lo)
d_pool  <- feols(fdebt("debt_dR_adj", c("y_lag1","F_CP_above_3","F_CP_below_mid","F_DI","F_H_lag1")),
                 df_pool, subset = main_sub_debt)
show(d_pool, "(+) POOLED below-the-line")

# ---------------------------------------------------------------------------
# (+) ALTERNATIVE OUTCOME: nominal debt change (V15 RHS)
# ---------------------------------------------------------------------------
if (!"debt_dN" %in% names(pdataD))
  pdataD <- pdataD |> group_by(Country) |> arrange(t_idx) |>
    mutate(debt_dN = DebtN_share2019 - lag(DebtN_share2019)) |> ungroup()
d_nom <- feols(fdebt("debt_dN", rhs_v15), pdataD, subset = main_sub_debt)
show(d_nom, "(+) NOMINAL outcome: debt_dN")

# ---------------------------------------------------------------------------
# (+) VIF & PAIRWISE CORRELATIONS on the V15 regressors
# ---------------------------------------------------------------------------
df_v <- pdataD |> filter(t_idx >= 4, t_idx <= 16)
cat("\n========== (+) VIF (V15) ==========\n")
print(vif(lm(as.formula(paste("debt_dR_adj ~", paste(rhs_v15, collapse = " + "))), data = df_v)))
cat("\n========== (+) Pairwise correlations (V15 regressors) ==========\n")
print(round(cor(df_v[, rhs_v15], use = "complete.obs"), 3))
