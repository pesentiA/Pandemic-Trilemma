# V14 Robustness Re-Run — Verification Report
**Date:** 2026-06-15 · **Script:** `analysis.R` (frozen snapshot `analysis_snapshot.R`, 5,347 lines)
**Engine:** R 4.6.0 · **Sample:** 38 OECD countries, t_idx ∈ [4,14] = Q4.2019–Q2.2022, N = 418

## What was run
The frozen snapshot was executed from line 1 through the end of the V14 robustness battery
(just before `STAGE 4 - DEBT EQUATION`), evaluated expression-by-expression in an isolated
environment so missing-package blocks skip gracefully. **531 top-level expressions; 4 errored**,
all expected skips (see below). Output tables/figures were redirected to `out_tables/` and
`out_figures/` so the canonical outputs were **not** overwritten. Full console log: `run_log.txt`.

### Skipped (uninstalled packages — NOT part of the 3501–3576 interpretation)
| Block | Package | Status |
|---|---|---|
| Wild-cluster bootstrap (`boottest`) | `fwildclusterboot` | skipped |
| Cluster-jackknife (`summclust`) | `summclust` | skipped (+ 2 dependent `sc` lines) |

The `polars` import (line ~103) is a **leftover** — never used in the pipeline — and was neutralised.
No descriptive/map block failed (no `sf`/`gt`/`corrplot` function is actually called in this range).

---

## VERDICT: the V14 Robustness Interpretation (≈ lines 3501–3576) reproduces in full.
Every coefficient, sign, significance star and N matches. Only **two trivial rounding nits** found
(both outside the 3501–3576 block, in the `FAZIT`/diagnostics comments).

### V14 main specification
| Parameter | Documented | Reproduced | ✓ |
|---|---|---|---|
| ρ_y (`y_lag1`) | 0.231*** | 0.23108*** | ✓ |
| α_S (`S_mean_tw`) | −0.095*** | −0.09515*** | ✓ |
| α_above (`F_CP_above_flow_lag2`) | 0.544** | 0.54416** | ✓ |
| α_below (`F_CP_belowstock`) | 0.261 (p≈.08) | 0.26117 (p=.080) | ✓ |
| α_DI (`F_DI_lag1`) | 1.470* | 1.47049* | ✓ |
| α_S,DI (`S_mean_tw:F_DI_lag1`) | −0.041** | −0.04058** | ✓ |
| Within R² | 0.436 | 0.43582 | ✓ |
| RMSE | 2.83 | 2.833 | ✓ |
| N | 418 | 418 | ✓ |
| DI break-even S* = α_DI/\|α_S,DI\| | 36 | 36.24 | ✓ |
| VIF Above / Below | ~1.2 / ~1.1 | 1.235 / 1.086 | ✓ |
| **Cor(Above, Below)** | **0.10** | **0.117** | ⚠ rounds to 0.12 |

### (1) Asymmetry — tightening vs. loosening (`m_asym`)
| | Documented | Reproduced | ✓ |
|---|---|---|---|
| ΔS⁺ (`S_tightening`) | −0.125*** (p<.001) | −0.12462*** (p=8.7e-9) | ✓ |
| ΔS⁻ (`S_loosening`) | −0.005 (p=.79) | −0.00473 (p=.787) | ✓ |
| **ratio ΔS⁺/ΔS⁻** | **~25×** | **26.4×** | ⚠ ≈26× |
| Above-Flow still sig | yes | 0.486** | ✓ |
| DI:S still sig | yes | −0.030* | ✓ |

*Note (not currently documented):* in this richer spec `F_CP_belowstock` turns **−0.241\*** (significantly
negative) — worth a one-line mention, as it reinforces the "Below-Stock is identification-fragile" theme.

### (2) Sample splits — Above-Flow / DI:S / Below-Stock
| Group | Above doc→repro | DI:S doc→repro | Below doc→repro |
|---|---|---|---|
| High-S | 0.95**→0.951** | −0.071**→−0.071** | 0.51.→0.505. |
| Low-S | 0.15→0.148 | −0.063**→−0.063** | —→0.053 |
| High-income | 0.50→0.501 | −0.087*→−0.087* | —→0.100 |
| Low-income | 0.63***→0.634*** | −0.029.→−0.029. | —→0.269 |
| High pre-debt | 1.07***→1.072*** | (−0.026)→−0.026 | —→0.248 |
| Low pre-debt | 0.32→0.320 | −0.048***→−0.048*** | —→0.362 |
| High soc.net | 0.78**→0.781** | −0.122*→−0.122* | —→0.097 |
| Low soc.net | 0.40→0.399 | −0.029.→−0.029. | 1.09*→1.093* |

All match. Below-Stock significant only in High-S and Low soc.net — exactly as documented.

### (3) Sample-window robustness
| Window | Above doc→repro | DI:S doc→repro | Below doc→repro | N |
|---|---|---|---|---|
| Baseline Q4.19–Q2.22 | 0.544**→0.544** | −0.041**→−0.041** | 0.26.→0.261. | 418 |
| Narrow Q1.20–Q1.22 | 0.368*→0.368* | n.s.→−0.025 n.s. | n.s.→0.370 | 342 |
| Wide Q4.19–Q4.22 | 0.723**→0.723** | −0.052**→−0.052** | n.s.→0.084 | 494 |
| Only 2020 | 0.729**→0.729** | n.s.→−0.009 n.s. | n.s.→−0.153 | 190 |

All match.

---

## Adjacent battery checks (own inline interpretations) — also reproduce
| Check | Documented | Reproduced |
|---|---|---|
| Sample restriction `t_idx≥6` inflates Below | 5.87 | 5.873*** ✓ |
| Sample restriction ranges: Above / DI:S | 0.36–0.54 / −0.031…−0.041 | 0.356–0.544 / −0.031…−0.041 ✓ |
| Take-up grid: Above / DI:S invariant | 0.541–0.547 / −0.040…−0.041 | 0.541–0.546 / −0.040…−0.041 ✓ |
| Take-up grid: Below range | 0.13–0.26 (baseline largest) | 0.131–0.261 ✓ |
| DCDH neg-weight share: Above / DI / Below | 8.7% / 2.7% / 26.8% | 8.65% / 2.69% / 26.83% ✓ |
| Lag selection Above: 0,1 negative; 2 positive | — | −0.532**, −1.204***, +0.544** ✓ |
| Lag selection DI: lag-2 flips interaction sign | — | +0.038** ✓ |
| Outlier excl. TUR+IRL: core stable | — | Above 0.450*, DI:S −0.035*, N=396 ✓ |
| Mundlak: within = V14; FE joint test | — | within identical; χ²=25.68, p=3.7e-5 ✓ |

## Two rounding nits to fix in the comments (cosmetic, not in 3501–3576)
1. `Cor(Above_lag2, Belowstock)` is **0.117** (reads "0.10" in the Step-4 / FAZIT comments → use 0.12).
2. Asymmetry ratio is **≈26×** (reads "~25×" → use ~26×).

## Not numerically verified this pass (would need 2 CRAN installs)
Wild-cluster bootstrap p-values (claim p≈.058/.059 for Below/DI:S) and the summclust jackknife.
Say the word to install `fwildclusterboot` + `summclust` and I'll confirm those too.
