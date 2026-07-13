# The Pandemic Trilemma

Replication package for **"The Pandemic Trilemma"** (Pesenti, University of Basel).

A deadly virus confronts governments with a trilemma between excess mortality,
output stabilization, and fiscal sustainability. This paper constructs a
database of 2,301 fiscal measures across 38 OECD economies, classified by
transmission mechanism, estimates a transition system linking instruments,
containment, output, infections, and debt, and evaluates observed policies
against a weighted planner benchmark and a weight-free constrained policy
frontier.

The replication consists of two independent parts:

1. **Empirical replication (R)** — descriptives, estimation of the transition
   system, robustness battery, and construction of the solver inputs.
2. **Calibration and model solving (MATLAB)** — validation of the calibrated
   system, the representative-economy planner and frontier, and the
   cross-country frontier analysis.

Part 2 can be run without Part 1: the MATLAB input files are shipped in the
repository. <!-- CONFIRM: sind die drei CSVs im Repo enthalten? -->

---

## Repository structure

```
Pandemic-Trilemma/
├── Files/
│   ├── code/
│   │   ├── R/
│   │   │   ├── datapreparation.R      <!-- CONFIRM: Dateiname des qcode/Prep-Skripts -->
│   │   │   ├── descriptives.R         # daily descriptives, theta construction,
│   │   │   │                          # aggregation to quarterly level
│   │   │   └── Analysis.R             # estimation, robustness battery, tables
│   │   └── matlab/
│   │       ├── calibration_v22.m      # forward-roll validation (38 countries + mean)
│   │       ├── v23_average_results.m  # representative-economy planner + frontier
│   │       └── v23_cross_country.m    # country-level frontier analysis
│   ├── data/                          <!-- CONFIRM: Pfade -->
│   │   ├── country_data_for_matlab.csv
│   │   ├── weekly_mortality_matlab.csv
│   │   └── theta_quarterly_CRI_JPN_TUR_frommonthly.csv
│   ├── output/
│   │   ├── figures/
│   │   └── tables/
│   └── text/                          # LaTeX sources of the paper
└── README.md
```

---

## Requirements

**R** (>= 4.2) with packages:
`dplyr`, `tidyr`, `ggplot2`, `lubridate`, `fixest`, `modelsummary`,
`kableExtra`, `knitr`
<!-- CONFIRM: fehlende Packages ergaenzen (readxl? haven? sandwich? boot fuer wild bootstrap?) -->

**MATLAB** (R2021a or later) with the **Optimization Toolbox** (`fmincon` with
the SQP algorithm is required for the frontier programs). The Parallel
Computing Toolbox is optional: the country loop in `v23_cross_country.m` is
embarrassingly parallel and can be switched from `for` to `parfor`.

No further toolboxes are needed; the iLQR solver is self-contained.

---

## Data availability

The measure-level fiscal database (2,301 classified measures, 38 OECD
economies, Q4.2019–Q4.2022) was hand-collected and classified by the author;
the accompanying codebook documents every variable and the classification
rules. <!-- CONFIRM: liegt die Datenbank im Repo, oder auf Anfrage? Quelle
der Rohdaten (IMF Fiscal Monitor / nationale Quellen) hier nennen. -->

Public inputs: containment stringency from the Oxford COVID-19 Government
Response Tracker; quarterly national accounts and public debt from OECD
sources; weekly excess mortality from the World Mortality Dataset.
<!-- CONFIRM: exakte Quellenliste gegen Data Appendix pruefen. -->

Costa Rica, Japan, and Türkiye lack weekly excess-mortality coverage; their
infection states are constructed from monthly data
(`theta_quarterly_CRI_JPN_TUR_frommonthly.csv`).

---

## Part 1 — Empirical replication (R)

Run the scripts in this order. Set the path variables at the top of each
script (`safedata`, `safeplots`, `safetable`) to your local directories.

**Step 0 — `datapreparation.R`.** Builds the analysis dataset from the raw
sources and saves `datafordescriptives.RData`.
<!-- CONFIRM: Skriptname + ob Rohdaten dafuer im Repo liegen. -->

**Step 1 — `descriptives.R`.** Daily descriptives on the two policy
instruments (containment and fiscal support) and the health outcome,
including the construction of the imputed prevalence state θ̂ by inverting
wave-specific effective IFRs. Ends by aggregating to the quarterly panel,
which enables the quarterly descriptives on output and debt. Exports the
descriptive figures and tables of Section 3 and the Data Appendix.

**Step 2 — `Analysis.R`.** Estimates the two outcome equations of the
transition system (output and debt) by TWFE via `fixest::feols`, runs the
full robustness battery (alternative dependent variables, sample splits,
leave-one-out, wild cluster bootstrap, dCDH heterogeneity diagnostics,
Mundlak decomposition), and exports the estimation and robustness tables.
Also runs the weekly health-side regressions of the Health Appendix.

**Step 3 — solver inputs.** Exports `country_data_for_matlab.csv` and
`weekly_mortality_matlab.csv` for Part 2.
<!-- CONFIRM: welches Skript schreibt die MATLAB-CSVs — Analysis.R oder ein separates Export-Skript? -->

Every robustness check reported in the paper is contained, described, and
commented in `Analysis.R`.

---

## Part 2 — Calibration and model solving (MATLAB)

All three scripts are self-contained: set the data path at the top, then run.
They share the same transition system, parameters, and policy menu; the
scripts differ only in which object they solve.

**Step 1 — `calibration_v22.m` (validation).** Rolls the estimated and
calibrated transition system forward under observed policies for all 38
countries and the OECD mean, and produces the fit diagnostics of the
Calibration Appendix (endpoint tables, RMSE moments, debt-change validation,
fiscal channel decomposition).

**Step 2 — `v23_average_results.m` (representative economy).** Solves the
weighted planner benchmark (iLQR, four multistarts) and the three
constrained frontier programs (SQP via `fmincon`, five multistarts each) for
the representative OECD economy. Key run flags at the top of the script:

| Flag | Default | Purpose |
|---|---|---|
| `RUN.weight_sweep` | `true` | weighted-planner sweep over (τ_b, λ_d) |
| `RUN.frontier` | `true` | the three frontier programs (main result) |
| `RUN.chi_sweep` | `false` | complementarity sweep χ ∈ {0, 0.25, 0.5} — set to `true` for the paper's robustness table |
| `RUN.no_anticipation` | `false` | foresight-value variant (Q1.2020 controls fixed at observed) |

Outputs: `trilemma_v22_results` tables and the planner/frontier figures of
Section 7.1–7.2. Runtime: minutes for the planner, roughly [X] minutes for
the full frontier set on a standard desktop. <!-- CONFIRM: Laufzeit -->

**Step 3 — `v23_cross_country.m` (country analysis).** Repeats the frontier
analysis for each of the 38 economies under the common policy menu, with
country-specific initial conditions, fixed effects, onset shocks, and
epidemiological innovation paths. Produces the country ranking, the
diagnostics/counterfactual tables, and the cross-country figures of Section
7.3, including the Italy relaxed-ramping variant
(`RUN.ita_relaxed_ramp = true`) and the IFR-invariance check.

Runtime warning: 38 countries × 3 scenarios × 5 starts ≈ 570 `fmincon` runs.
For a quick test, set `RUN.countries_subset = {'JPN','DEU','USA'}`. The
country loop parallelizes with `parfor`.

---

## Mapping: paper objects → scripts

| Paper object | Script |
|---|---|
| Tables 1, descriptive figures (Section 3) | `descriptives.R` |
| Table 2 (transition parameters), robustness appendix | `Analysis.R` |
| Health-side estimates (Health Appendix) | `Analysis.R` |
| Calibration fit (Calibration Appendix) | `calibration_v22.m` |
| Planner benchmark, frontier, χ-sweep (Section 7.1–7.2) | `v23_average_results.m` |
| Country ranking, fiscal exchange rate (Section 7.3) | `v23_cross_country.m` |

---

## Contact

Aulis Pesenti — University of Basel, Faculty of Business and Economics.
<!-- CONFIRM: E-Mail-Adresse und ggf. Lizenz (empfohlen: MIT fuer Code,
CC-BY-4.0 fuer die Datenbank/das Codebook). -->
