# ============================================================
# V14 Robustness Interpretation
# ------------------------------------------------------------
# VERIFIED by full re-run on 2026-06-15 (R 4.6.0, N = 418,
# 38 countries x 11 quarters, Q4.2019-Q2.2022). All coefficients,
# signs, significance levels and Ns below reproduce exactly.
# ============================================================
#
# (1) ASYMMETRY: tightening vs loosening
#   Delta S+ = -0.125*** (p < 0.001)      [S_tightening]
#   Delta S- = -0.005    (p = 0.79)       [S_loosening]
#   Tightening contracts output ~26x more than loosening
#   expands it. Direct evidence for hysteresis: productive
#   capacity destroyed during tightening does not return
#   mechanically during loosening. The asymmetric ratchet
#   provides structural justification for the persistence
#   channel and the AR(1) recovery dynamics in V14. Above-Flow
#   (0.49**) and DI:S (-0.030*) remain significant in this
#   richer specification. (Below-Stock turns -0.24* here,
#   consistent with its trend-/identification-fragility.)
#
# (2) SAMPLE SPLITS: heterogeneity is economically interpretable
#
#   Above-Flow (alpha_above):
#     High-S          0.95**    Low-S          0.15 n.s.
#     High-income     0.50      Low-income     0.63***
#     High pre-debt   1.07***   Low pre-debt   0.32
#     High soc.net    0.78**    Low soc.net    0.40
#   -> Above is identified where there is shock to absorb
#      (High-S) and where fiscal space allows deployment
#      (High pre-debt countries deployed CP more aggressively
#      and credibly).
#
#   DI push-on-string (alpha_S,DI):
#     High-S         -0.071**   Low-S         -0.063**
#     High-income    -0.087*    Low-income    -0.029.
#     High soc.net   -0.122*    Low soc.net   -0.029.
#   -> Mechanism robust across all splits. Stronger in
#      High-income / High soc.net groups where formal consumption
#      channels are more developed and binding constraints
#      under containment are sharper.
#
#   Below-Stock (alpha_below):
#     Significant only in High-S (0.51.) and Low soc.net (1.09*)
#     -> Loans/guarantees matter most where shock is severe
#        (High-S) and automatic stabilizers are weak (Low soc.net):
#        liquidity protection substitutes for missing welfare
#        state buffers.
#
# (3) SAMPLE-WINDOW: Above-Flow and DI:S stable
#
#   Above-Flow:
#     Baseline Q4.19-Q2.22   0.544**
#     Narrow   Q1.20-Q1.22   0.368*
#     Wide     Q4.19-Q4.22   0.723**
#     Only 2020              0.729**
#   -> Above-Flow channel is operative from 2020 onwards
#      (Only-2020 N=190 delivers the largest coefficient),
#      ruling out the late-recovery-phase as the source of
#      identification.
#
#   DI push-on-string:
#     Baseline               -0.041**
#     Wide                   -0.052**
#     Narrow / Only 2020     n.s. (power loss at small N)
#   -> Mechanism stable in full samples; insignificance in
#      narrow windows reflects loss of S-variation rather than
#      mechanism failure.
#
#   Below-Stock:
#     Significant only in Baseline (0.26.); insignificant in
#     Narrow, Wide, Only-2020. Confirms identification
#     fragility documented in Year-FE and decay analyses:
#     Below-Stock is empirically weaker than Above-Flow.
#
# OVERALL ROBUSTNESS:
#   Above-Flow + DI push-on-string + S level effect form the
#   empirically robust core of V14. Below-Stock is theoretically
#   motivated and survives the main spec but is sensitive to
#   sample window and time-fixed-effects specifications.
#   Tightening-loosening asymmetry provides independent
#   structural support for the AR(1) persistence framing.
#
# Cross-check (full battery, same re-run): DCDH negative-weight
# shares Above 8.7% / DI 2.7% / Below 26.8%; take-up grid leaves
# Above (0.541-0.546) and DI:S (-0.040 to -0.041) invariant;
# sample restriction t_idx>=6 inflates Below to 5.87 (loss of the
# zero-stock anchor); lag selection confirms Above lag-2 and DI
# lag-1 as the correct, sign-consistent choices.
