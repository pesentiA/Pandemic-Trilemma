# =============================================================================
#  PANDEMIC TRILEMMA — SUPERSEDED / EXPLORATORY SPECIFICATIONS
# =============================================================================
#  Moved out of analysis.R on 2026-07-12 while preparing the replication
#  package. These blocks document the specification search that led to the
#  V14 output equation and the V15 debt equation, but they do NOT produce
#  any table or figure used in the paper. They are kept for transparency
#  and for the dissertation's version history.
#
#  HOW TO RUN:
#    This file is NOT self-contained. Blocks 1-5 require analysis.R to have
#    been executed at least through "STEP 3 -- THEORETICAL DISAGGREGATION
#    (V14 final)" so that `df_bin` (with F_CP_above_flow_lag2,
#    F_CP_belowstock, F_CP_loans_lo/mid, F_CP_guar_lo, F_DI_lag1, t_idx, ...)
#    exists in the workspace. Block 6 additionally requires the debt-equation
#    stage of analysis.R to have been run (pdataD with debt_dR_adj).
#
#  NOTE: several blocks below overwrite the object `v14` / `debt_v15`.
#  analysis.R re-estimates the canonical baselines after these blocks were
#  removed, so running THIS file leaves the main-script results untouched
#  only if you re-run the corresponding baseline afterwards.
#
#  CONTENTS:
#    1. Above x Below complementarity (interaction spec with Year control)
#    2. Depreciating capacity stock (Kcap): raw, mean-centered, bounded-share
#       interactions -- empirical support for the planner's chi_cap_liq
#       parameter (calibrated, not directly estimated; see paper App.)
#    3. Flow x Flow interaction variant of the CP channels
#    4. Above-the-line entered as a cumulative stock (discarded: loses
#       observations through the extra lag and is conceptually less clean)
#    5. plm cross-check attempts for V14 (exploratory; NOTE: the first plm
#       call references a column `F_CP_above_stock` that is never created --
#       in a clean session this block STOPS with an error. Kept verbatim to
#       document the state it was in; the feols-vs-plm equivalence claim was
#       never established by this code.)
#    6. Debt equation: earlier main-spec candidates (22.05.2026 version with
#       contemporaneous F_DI; test version with linear year trend and
#       mid take-up loans)
# =============================================================================


# =============================================================================
#  BLOCK 1 — ABOVE x BELOW COMPLEMENTARITY (exploratory)
#  Adds the F_CP_above_flow_lag2 x F_CP_belowstock interaction plus a year
#  control to the V14 baseline. Motivated the complementarity discussion:
#  neither CP instrument works fully in isolation.
#  (Overwrites `v14`; analysis.R re-estimates the baseline afterwards.)
# =============================================================================

v14 <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw
  + F_CP_above_flow_lag2
  + F_CP_belowstock + F_CP_above_flow_lag2*F_CP_belowstock
  + F_DI_lag1 * S_mean_tw +year_only  | Country ,
  data = df_bin,  subset = ~ t_idx >= 4 & t_idx <= 14
)

summary(v14, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE))


# =============================================================================
#  BLOCK 2 — DEPRECIATING CAPACITY STOCK (Kcap)
#  Builds a geometrically depreciating stock of above-the-line CP flows with
#  a 6-quarter half-life (consistent with the planner calibration) and tests
#  its interaction with the below-the-line stock in three variants:
#    (a) raw interaction with year control,
#    (b) mean-centered interaction (preferred reading),
#    (c) bounded share interaction (closest to the theoretical model).
# =============================================================================

# Depreciation rate delta implied by a 6-quarter half-life
target_half_life_cap_q <- 6
delta_cap <- 1 - 0.5^(1 / target_half_life_cap_q)

df_bin <- df_bin %>%
  arrange(Country, t_idx) %>%
  group_by(Country) %>%
  mutate(
    F_CP_above_flow = if_else(is.na(F_CP_above_3), 0, F_CP_above_3),

    Kcap = accumulate(
      F_CP_above_flow,
      ~ (1 - delta_cap) * .x + .y,
      .init = 0
    )[-1]
  ) %>%
  ungroup()

# (a) Raw Kcap x Below-Stock interaction with year control
v14_stock <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw
  + Kcap
  + F_CP_belowstock
  + Kcap:F_CP_belowstock
  + F_DI_lag1 * S_mean_tw
  + year_only
  | Country,
  data = df_bin,
  subset = ~ t_idx >= 4 & t_idx <= 14
)

summary(
  v14_stock,
  cluster = ~ Country,
  ssc = ssc(K.adj = TRUE, G.adj = TRUE)
)


# (b) Mean-centered version -- the preferred variant of this exploration
df_bin <- df_bin %>%
  mutate(
    Kcap_c = Kcap - mean(Kcap, na.rm = TRUE),
    F_CP_belowstock_c = F_CP_belowstock - mean(F_CP_belowstock, na.rm = TRUE)
  )

v14_stock_centered <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw
  + Kcap_c
  + F_CP_belowstock_c
  + Kcap_c:F_CP_belowstock_c
  + F_DI_lag1 * S_mean_tw
  | Country,
  data = df_bin,
  subset = ~ t_idx >= 4 & t_idx <= 14
)

summary(
  v14_stock_centered,
  cluster = ~ Country,
  ssc = ssc(K.adj = TRUE, G.adj = TRUE)
)

# (c) Bounded-share interaction -- closest to the theoretical model

cap_scale <- max(quantile(df_bin$F_CP_above_3[df_bin$F_CP_above_3 > 0], 0.99, na.rm = TRUE), 1)

df_bin <- df_bin %>%
  mutate(
    capshare = Kcap / (Kcap + cap_scale),
    Kbelow_capshare = F_CP_belowstock * capshare
  )

v14_bounded <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw
  + Kcap
  + F_CP_belowstock
  + Kbelow_capshare
  + F_DI_lag1 * S_mean_tw
  | Country,
  data = df_bin,
  subset = ~ t_idx >= 4 & t_idx <= 14
)

summary(
  v14_bounded,
  cluster = ~ Country,
  ssc = ssc(K.adj = TRUE, G.adj = TRUE)
)


# =============================================================================
#  BLOCK 3 — FLOW x FLOW VARIANT OF THE CP CHANNELS
#  Replaces the Below-STOCK with a below-the-line FLOW (loans mid take-up +
#  guarantees low take-up) and interacts the two CP flows.
# =============================================================================

df_bin <- df_bin %>%
  mutate(
    F_CP_above_flow = coalesce(F_CP_above_3, 0),
    F_CP_below_flow = coalesce(F_CP_loans_mid, 0) + coalesce(F_CP_guar_lo, 0)
  )

df_bin <- df_bin %>%
  arrange(Country, t_idx) %>%
  group_by(Country) %>%
  mutate(
    F_CP_above_flow_lag1 = lag(F_CP_above_flow, 1),
    F_CP_above_flow_lag2 = lag(F_CP_above_flow, 2),
    F_CP_below_flow_lag1 = lag(F_CP_below_flow, 1),
    F_CP_below_flow_lag2 = lag(F_CP_below_flow, 2)
  ) %>%
  ungroup()

v14 <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw
  + F_CP_above_flow_lag2*F_CP_below_flow
  + F_DI_lag1 * S_mean_tw  | Country ,
  data = df_bin,  subset = ~ t_idx >= 4 & t_idx <= 14
)

summary(v14, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE))


# Interpretation:
# The baseline model estimates separable effects of above-line CP and below-line liquidity.
# The mechanism checks suggest that this separability is restrictive:
# preserved capacity (Kcap) and liquidity support are complementary.
# In particular, the centered stock-stock and bounded interaction specifications show
# positive and significant interaction effects, implying that below-line liquidity is
# most effective when productive capacity has been preserved.
# I therefore use the interaction results as empirical support for the planner extension,
# but keep chi_cap_liq as a calibrated bounded parameter rather than a directly estimated coefficient.


# =============================================================================
#  BLOCK 4 — ABOVE-THE-LINE AS A CUMULATIVE STOCK (discarded)
#  Enters the above-the-line channel as a lagged cumulative stock instead of
#  a flow. Discarded: not as clean, and observations are lost to the extra lag.
# =============================================================================

df_bin <- df_bin %>%
  arrange(Country, t_idx) %>%
  group_by(Country) %>%
  mutate(
    F_CP_above_3_stock_lag1 = lag(F_CP_above_3_stock, 1),
    F_CP_above_3_stock_lag2 = lag(F_CP_above_3_stock, 2),
  ) %>%
  ungroup()

v14 <- feols(
  y_t_pct ~ y_lag1 + S_mean_tw
  + F_CP_above_3_stock_lag1*F_CP_belowstock
  + F_DI_lag1 * S_mean_tw  | Country ,
  data = df_bin,  subset = ~ t_idx >= 4 & t_idx <= 14
)

summary(v14, cluster = ~ Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE))

## Not as clean: we lose observations through the additional lag.


# =============================================================================
#  BLOCK 5 — plm CROSS-CHECK ATTEMPTS FOR V14 (exploratory, BROKEN)
#  Intended to reproduce the V14 point estimates with plm. NOTE:
#  the first plm call below references `F_CP_above_stock`, a column that is
#  never created anywhere (only `F_CP_above_3_stock` exists), so in a clean
#  session this block stops with an error. In addition, neither plm formula
#  matches the V14 baseline regressors, so the printed feols-vs-plm
#  comparison never compared like with like. Kept verbatim as documentation.
# =============================================================================
library(plm)

# Local `pdata_v14` avoids shadowing the top-level `pdata` defined in
# Stage 2b (it covers the full 2019Q1-2022Q4 window and is needed by
# the later debt-equation stage).
pdata_v14 <- pdata.frame(
  df_bin |> filter(t_idx >= 4 & t_idx <= 14),
  index = c("Country", "Quarter")
)


# pdata_v14 is a pdata.frame -> index already set and sorted.
# Build the stock per country via ave() (base R, avoids dplyr/plm conflicts):

pdata_v14$F_CP_below_flow_adj <- pdata_v14$F_CP_guar_lo + pdata_v14$F_CP_loans_lo

pdata_v14$F_CP_belowstock_adj <- ave(
  pdata_v14$F_CP_below_flow_adj,
  index(pdata_v14)[[1]],          # first index = Country (grouping variable)
  FUN = cumsum
)

library(dplyr)
pdata_v14 <- pdata_v14 %>%
  arrange(Country, Quarter) %>%
  group_by(Country) %>%
  mutate(F_CP_below_flow_adj_lag2 = lag(F_CP_below_flow_adj, 2)) %>%
  ungroup()

## With adjusted below-stock
## (BROKEN: `F_CP_above_stock` does not exist -- errors in a clean session)
v14_plm <- plm(
  y_t_pct ~ y_lag1 + S_mean_tw
  + F_CP_above_stock+ (F_CP_below_flow_adj_lag2^2) + F_CP_below_flow_adj_lag2
  + F_DI_lag1 + F_DI_lag1:S_mean_tw,
  data   = pdata_v14,
  model  = "within",
  effect = "individual"
)


# Makes no real difference; loans mid or lo give the same coefficient.

# Cluster-robust SE (CRV1, country-level)
coeftest(v14_plm, vcov = vcovHC(v14_plm, cluster = "group", type = "HC1"))

coefs_feols <- coef(v14)
coefs_plm   <- coef(v14_plm)

print(coefs_feols)
print(coefs_plm)


# Second attempt (different regressor set; also does not match V14):
library(plm)

pdata_v14$F_CP_below<-I(pdata_v14$F_CP_guar+pdata_v14$F_CP_loans)

pdata_v14 <- pdata_v14%>%
  arrange(Country, Quarter) %>%
  group_by(Country) %>%
  mutate(F_CP_below_lag1 = lag(F_CP_below, 2)) %>%
  ungroup()

v14_plm <- plm(
  y_t_pct ~ y_lag1_recession + S_mean_tw
  + F_CP_above_flow_lag2+
  + F_DI_lag1 + F_DI_lag1:S_mean_tw,
  data   = pdata_v14,
  model  = "within",
  effect = "individual"
)

# Cluster-robust SE (CRV1, country-level)
coeftest(v14_plm, vcov = vcovHC(v14_plm, cluster = "group", type = "HC1"))


# =============================================================================
#  BLOCK 6 — DEBT EQUATION: EARLIER MAIN-SPEC CANDIDATES
#  Requires the debt stage of analysis.R (pdataD with debt_dR_adj, F_H_lag1).
#  (Overwrites `debt_v15`; analysis.R re-estimates the canonical V15
#  afterwards in the robustness battery.)
# =============================================================================

# (a) Main candidate of 22.05.2026: contemporaneous F_DI, no health control
debt_v15 <- feols(
  debt_dR_adj ~ y_t_pct + F_CP_above_3 + F_CP_loans_lo +
    F_CP_guar_lo + F_DI  | Country,
  data = pdataD, subset = ~ t_idx >= 4 & t_idx <= 16)
summary(debt_v15, cluster = ~Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE))

# (b) Test variant: mid take-up loans plus a linear year trend
debt_v15 <- feols(
  debt_dR_adj ~ y_lag1 + F_CP_above_3 + F_CP_loans_mid +
    F_CP_guar_lo + F_DI_lag1 + F_H_lag1 + as.numeric(year_only)| Country,
  data = pdataD, subset = ~ t_idx >= 4 & t_idx <= 16)
summary(debt_v15, cluster = ~Country, ssc = ssc(K.adj = TRUE, G.adj = TRUE))

fe_b <- fixef(debt_v15)$Country

print(fe_b)
