# =============================================================================
#  REDUCED-FORM EXTENSIONS — Jordà LP + Economist DV robustness
#  -----------------------------------------------------------------------------
#  (a) Local projections: d_{k+h} = delta_theta^(h) * theta_k + ...   for h = 0..4
#  (b) Robustness with the Economist-modeled DV (also weekly; replaces p_proj).
#
#  Sample restriction:  date <= 2021-12-31  (excludes Omicron-dominated 2022).
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(lubridate)
  library(fixest); library(ggplot2); library(modelsummary)
})

base <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma"
log_path <- file.path(base, "Files/output/reduced_form_d_theta_LP.log")
sink(log_path, split = TRUE)

cat(strrep("=", 78), "\n  REDUCED-FORM: Jordà LP + Economist DV — ", format(Sys.time()), "\n",
    strrep("=", 78), "\n", sep = "")

# -----------------------------------------------------------------------------
# (0) Inputs: pre-built weekly panel (from reduced_form_d_theta.R) + raw RData
# -----------------------------------------------------------------------------
wkly <- read.csv(file.path(base, "Files/data/processed/weekly_reduced_form_panel.csv"))
wkly$date <- as.Date(wkly$date)
cat(sprintf("\nLoaded weekly panel:  %d rows, %d countries, date %s -> %s\n",
            nrow(wkly), n_distinct(wkly$Country),
            min(wkly$date), max(wkly$date)))

load(file.path(base, "Files/data/processed/dataforanalysis.RData"))   # panel_w

# -----------------------------------------------------------------------------
# (1) Apply sample restriction: through 31.12.2021
# -----------------------------------------------------------------------------
SAMPLE_END <- as.Date("2021-12-31")
wkly <- wkly %>% filter(date <= SAMPLE_END)
cat(sprintf("Restricted sample: <=%s, kept %d rows, %d countries, %d weeks\n",
            SAMPLE_END, nrow(wkly), n_distinct(wkly$Country),
            n_distinct(wkly$date)))

# -----------------------------------------------------------------------------
# (2) Build all leads h = 0..4 within country  (Jordà 2005 LP)
#     d_{i, k+h} for h in {0,1,2,3,4}
# -----------------------------------------------------------------------------
wkly <- wkly %>%
  arrange(Country, date) %>%
  group_by(Country) %>%
  mutate(
    d_h0 = p_proj,
    d_h1 = lead(p_proj, 1),
    d_h2 = lead(p_proj, 2),
    d_h3 = lead(p_proj, 3),
    d_h4 = lead(p_proj, 4)
  ) %>%
  ungroup()

# -----------------------------------------------------------------------------
# (3) Local projections — three specifications per horizon
# -----------------------------------------------------------------------------
horizons   <- 0:4
spec_names <- c("CFE", "CFE+S", "CFE+wave")

run_lp <- function(h, spec) {
  dv <- paste0("d_h", h)
  base_rhs <- "theta_hat"
  if (spec == "CFE+S")    base_rhs <- "theta_hat + S_daily_mean"
  if (spec == "CFE+wave") fe <- "| Country + wave_coarse" else fe <- "| Country"
  f <- as.formula(paste(dv, "~", base_rhs, fe))
  feols(f, data = wkly, vcov = ~ Country)
}

lp_results <- expand.grid(h = horizons, spec = spec_names,
                          stringsAsFactors = FALSE) %>%
  rowwise() %>%
  mutate(
    fit       = list(run_lp(h, spec)),
    coef      = coef(fit)["theta_hat"],
    se        = sqrt(diag(vcov(fit)))["theta_hat"],
    n         = nobs(fit),
    r2_within = fitstat(fit, "wr2", verbose = FALSE)$wr2
  ) %>%
  ungroup() %>%
  mutate(ci_lo = coef - 1.96 * se,
         ci_hi = coef + 1.96 * se)

cat("\n--- Local-projection estimates ---\n")
print(lp_results %>% select(spec, h, coef, se, ci_lo, ci_hi, n, r2_within))

# -----------------------------------------------------------------------------
# (4) IRF plot — δ_θ^(h) across horizons, one panel per spec
# -----------------------------------------------------------------------------
lp_plot <- lp_results %>%
  mutate(spec = factor(spec, levels = spec_names))

p_lp <- ggplot(lp_plot, aes(x = h, y = coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), fill = "steelblue", alpha = 0.20) +
  geom_line(linewidth = 0.7, color = "steelblue") +
  geom_point(size = 2.2, color = "steelblue") +
  facet_wrap(~ spec, ncol = 3) +
  labs(
    title    = expression(paste("Local projection: ", d[k+h], " = ", delta[theta]^(h), " ", theta[k])),
    subtitle = "Weekly panel, 35 countries, sample <= 2021-12-31",
    x        = "Horizon h (weeks)",
    y        = expression(delta[theta]^(h))
  ) +
  theme_bw(base_size = 11)

ggsave(file.path(base, "Files/output/figures/fig_lp_d_theta.pdf"), p_lp,
       width = 10, height = 4)
cat("\n  Saved: Files/output/figures/fig_lp_d_theta.pdf\n")

# -----------------------------------------------------------------------------
# (5) ECONOMIST DV ROBUSTNESS
#     - Read Economist excess-mortality file (weekly, 237 countries)
#     - Restrict to OECD-38 via country crosswalk
#     - Merge by (Country, isoyr, isowk) with theta_hat from panel_w
#     - Re-run h = 0..4 LPs with the alternative DV
# -----------------------------------------------------------------------------
cat("\n", strrep("=", 78), "\n  ECONOMIST DV ROBUSTNESS\n", strrep("=", 78), "\n", sep = "")

econ <- read.csv(file.path(base,
  "Files/data/raw/outcomes and controls/Excess Mortality/excess_mortality_economist.csv"),
  stringsAsFactors = FALSE)
econ$date <- as.Date(econ$date)

cat(sprintf("Economist file: %d rows, %d countries, date %s -> %s\n",
            nrow(econ), n_distinct(econ$country),
            min(econ$date), max(econ$date)))

# OECD-38 crosswalk (same mapping used in analysis.R BLOCK 6)
crosswalk <- tibble(
  country_name = c("Australia","Austria","Belgium","Canada","Chile",
                   "Colombia","Costa Rica","Czechia","Denmark","Estonia",
                   "Finland","France","Germany","Greece","Hungary",
                   "Iceland","Ireland","Israel","Italy","Japan",
                   "Latvia","Lithuania","Luxembourg","Mexico",
                   "Netherlands","New Zealand","Norway","Poland",
                   "Portugal","Slovakia","Slovenia","South Korea",
                   "Spain","Sweden","Switzerland","Turkey",
                   "United Kingdom","United States"),
  Country = c("AUS","AUT","BEL","CAN","CHL","COL","CRI","CZE","DNK","EST",
              "FIN","FRA","DEU","GRC","HUN","ISL","IRL","ISR","ITA","JPN",
              "LVA","LTU","LUX","MEX","NLD","NZL","NOR","POL","PRT","SVK",
              "SVN","KOR","ESP","SWE","CHE","TUR","GBR","USA")
)

econ_oecd <- econ %>%
  inner_join(crosswalk, by = c("country" = "country_name")) %>%
  filter(date <= SAMPLE_END) %>%
  mutate(
    isoyr  = isoyear(date),
    isowk  = isoweek(date),
    # Economist gives weekly excess deaths/100k; rescale to /million for parity
    excess_econ_pm = estimated_daily_excess_deaths_per_100k * 10
  ) %>%
  select(Country, isoyr, isowk, date_econ = date, excess_econ_pm)

cat(sprintf("Economist OECD-38: %d rows, %d countries, <= %s\n",
            nrow(econ_oecd), n_distinct(econ_oecd$Country), SAMPLE_END))

# Merge Economist DV onto the weekly panel by (Country, isoyr, isowk).
# The two date-conventions can differ by a day; isoweek matching aligns them.
wkly_econ <- wkly %>%
  left_join(econ_oecd, by = c("Country", "isoyr", "isowk")) %>%
  arrange(Country, date) %>%
  group_by(Country) %>%
  mutate(
    de_h0 = excess_econ_pm,
    de_h1 = lead(excess_econ_pm, 1),
    de_h2 = lead(excess_econ_pm, 2),
    de_h3 = lead(excess_econ_pm, 3),
    de_h4 = lead(excess_econ_pm, 4)
  ) %>%
  ungroup()

cat(sprintf("\nMerged panel after Economist join: %d rows  (NA on excess_econ_pm: %d)\n",
            nrow(wkly_econ), sum(is.na(wkly_econ$excess_econ_pm))))

run_lp_econ <- function(h) {
  dv <- paste0("de_h", h)
  f  <- as.formula(sprintf("%s ~ theta_hat | Country", dv))
  feols(f, data = wkly_econ, vcov = ~ Country)
}

lp_econ <- tibble(h = horizons) %>%
  rowwise() %>%
  mutate(
    fit       = list(run_lp_econ(h)),
    coef      = coef(fit)["theta_hat"],
    se        = sqrt(diag(vcov(fit)))["theta_hat"],
    n         = nobs(fit),
    r2_within = fitstat(fit, "wr2", verbose = FALSE)$wr2
  ) %>%
  ungroup() %>%
  mutate(ci_lo = coef - 1.96 * se,
         ci_hi = coef + 1.96 * se)

cat("\n--- LP estimates with Economist DV (excess deaths per million) ---\n")
print(lp_econ %>% select(h, coef, se, ci_lo, ci_hi, n, r2_within))

# Side-by-side plot: panel_w (p-score) vs Economist (excess per million).
# Two different scales, so panel_w is shown on left axis and Economist on right via separate panels.
lp_plot_long <- bind_rows(
  lp_results %>% filter(spec == "CFE") %>%
    transmute(h, coef, ci_lo, ci_hi, source = "panel_w (p_proj, p-score)"),
  lp_econ %>%
    transmute(h, coef, ci_lo, ci_hi, source = "Economist (per million)")
) %>%
  mutate(source = factor(source, levels = c("panel_w (p_proj, p-score)",
                                            "Economist (per million)")))

p_lp_compare <- ggplot(lp_plot_long, aes(x = h, y = coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), fill = "steelblue", alpha = 0.18) +
  geom_line(linewidth = 0.7, color = "steelblue") +
  geom_point(size = 2.2, color = "steelblue") +
  facet_wrap(~ source, scales = "free_y") +
  labs(
    title    = "LP: weekly excess mortality on contemporaneous theta",
    subtitle = "Both DV sources, Country-FE, sample <= 2021-12-31",
    x = "Horizon h (weeks)",
    y = expression(delta[theta]^(h))
  ) +
  theme_bw(base_size = 11)

ggsave(file.path(base, "Files/output/figures/fig_lp_d_theta_econ.pdf"),
       p_lp_compare, width = 10, height = 4)
cat("\n  Saved: Files/output/figures/fig_lp_d_theta_econ.pdf\n")

# -----------------------------------------------------------------------------
# (6) Compact LaTeX comparison table (panel_w CFE specs across horizons)
# -----------------------------------------------------------------------------
ms_lp <- list()
for (h in horizons) {
  ms_lp[[paste0("h=", h)]] <- run_lp(h, "CFE")
}
out_tex <- file.path(base, "Files/output/tables/tab_lp_d_theta.tex")
modelsummary(
  ms_lp,
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared", "r.squared.within"),
  coef_map = c("theta_hat" = "$\\theta_k$"),
  output = out_tex,
  title  = paste0("LP estimates: $d_{k+h} = \\delta_\\theta^{(h)} \\theta_k$ ",
                  "(weekly panel, sample $\\le$ 2021-12-31, Country FE, CRV1 SEs)")
)
cat(sprintf("\n  Saved LaTeX table: %s\n", out_tex))

cat("\n", strrep("=", 78), "\n  END\n", strrep("=", 78), "\n", sep = "")
sink()
cat("Log:", log_path, "\n")
