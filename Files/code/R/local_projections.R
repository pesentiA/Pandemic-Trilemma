# =============================================================================
#  LOCAL PROJECTIONS (Jordà 2005) — Fiscal Policy Transmission Dynamics
#  Panel LP: y_{i,t+h} - y_{i,t-1} = alpha_ih + beta_h * shock_it + X_it + FE
#
#  Design:
#  - Shock period: Q1.2020 – Q4.2021 (active fiscal deployment)
#  - Pre-pandemic (2019): baseline with F = 0, anchors the counterfactual
#  - Post-pandemic (2022): provides response horizons for late shocks
#  - Horizons: h = 0, ..., 5 (0 to 5 quarters ahead)
#  - SEs: Country-clustered (robust to serial correlation from overlapping)
# =============================================================================

rm(list = ls())

packages_vector <- c("dplyr", "plm", "lmtest", "sandwich", "fixest",
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
conflicted::conflicts_prefer(lubridate::quarter)
conflicted::conflicts_prefer(lubridate::year)
conflicted::conflicts_prefer(dplyr::between)

safedata <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/processed"
load(file.path(safedata, "dataforanalysis.RData"))

safeplots <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/output/figures"

set.seed(1234)

fm_path <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/raw/fiscal measures/fiscal_classified_v1_7.xlsx"

# =============================================================================
#  PANEL CONSTRUCTION — Use widest possible time range
#  qdata: Q1.2015 – Q4.2024
#  fiscal: Q1.2019 – Q4.2022 (zero outside this range)
#  theta:  Q1.2020 – Q4.2022 (NA before pandemic)
# =============================================================================

# All quarters we want in the panel (wide range for LP leads)
all_qs <- paste0("Q", rep(1:4, each = 1, times = 10), ".",
                 rep(2015:2024, each = 4))
all_qs <- unique(all_qs)
# Sort properly
all_qs <- all_qs[order(as.numeric(gsub(".*\\.", "", all_qs)),
                       as.numeric(gsub("Q([0-9]).*", "\\1", all_qs)))]

# Block 1: qdata
df_qdata <- qdata %>%
  filter(Quarter %in% all_qs) %>%
  select(Country, Quarter, year_only, quarter_only,
         y_t_pct, DebtN_share2019, DebtR_share2019,
         Qpopulation_th, inflation_index, vax_rate,
         rGDP_pc_2019, debt_2019, d_t_pct)

pop_2019 <- df_qdata[df_qdata$Quarter == "Q4.2019", c("Country", "Qpopulation_th")]
names(pop_2019)[2] <- "pop_2019"
df_qdata <- merge(df_qdata, pop_2019, by = "Country")

# Block 2: theta (only pandemic period)
df_theta <- theta_quarterly_full %>%
  mutate(Quarter = as.character(YQ)) %>%
  filter(Quarter %in% all_qs) %>%
  select(Country, Quarter, theta_mean)

# Block 3: fiscal measures
fm1 <- readxl::read_excel(fm_path)
fm1 <- fm1 %>%
  mutate(
    YQ     = paste0("Q", Quarter, ".", Year),
    YQ_ord = factor(YQ, levels = sort(unique(paste0("Q", Quarter, ".", Year))), ordered = TRUE)
  ) %>%
  filter(!PolicyCode %in% c(5, 6, 11, 12, 15, 16))

fiscal_qs <- paste0("Q", rep(1:4, times = 4), ".", rep(2019:2022, each = 4))

df_fiscal <- fm1 %>%
  filter(broad_fiscal == 1) %>%
  mutate(Quarter = as.character(YQ_ord)) %>%
  filter(Quarter %in% fiscal_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(
    F_CP = sum(broad_fiscal_gdp[transmission_channel == "CP"], na.rm = TRUE),
    F_DI = sum(broad_fiscal_gdp[transmission_channel == "DI"], na.rm = TRUE),
    F_H  = sum(broad_fiscal_gdp[transmission_channel == "H"],  na.rm = TRUE),
    .groups = "drop"
  )

# CP sub-components (loans vs guarantees)
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
  filter(Quarter %in% fiscal_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(
    F_CP_above = sum(CP_above, na.rm = TRUE),
    F_CP_loans = sum(CP_loans, na.rm = TRUE),
    F_CP_guar  = sum(CP_guar,  na.rm = TRUE),
    .groups = "drop"
  )

# DI sub-components
df_fiscal_di <- fm1 %>%
  filter(broad_fiscal == 1, transmission_channel == "DI") %>%
  mutate(
    Quarter = as.character(YQ_ord),
    DI_sub = case_when(
      PolicyCode %in% c("35","36","37","38") ~ "transfers",
      PolicyCode %in% c("27","28","29")      ~ "demand",
      TRUE ~ "tax"
    )
  ) %>%
  filter(Quarter %in% fiscal_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(
    F_DI_transfers = sum(broad_fiscal_gdp[DI_sub == "transfers"], na.rm = TRUE),
    F_DI_demand    = sum(broad_fiscal_gdp[DI_sub == "demand"],    na.rm = TRUE),
    .groups = "drop"
  )

# Stringency
df_stringency <- panel_w %>%
  filter(!is.na(S_mean)) %>%
  mutate(year = year(date), quarter = quarter(date),
         Quarter = paste0("Q", quarter, ".", year)) %>%
  filter(Quarter %in% all_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(S_mean_pw = mean(S_mean, na.rm = TRUE), .groups = "drop")

# Merge
df <- df_qdata %>%
  left_join(df_theta,      by = c("Country", "Quarter")) %>%
  left_join(df_fiscal,     by = c("Country", "Quarter")) %>%
  left_join(df_fiscal_cp,  by = c("Country", "Quarter")) %>%
  left_join(df_fiscal_di,  by = c("Country", "Quarter")) %>%
  left_join(df_stringency, by = c("Country", "Quarter")) %>%
  mutate(
    across(starts_with("F_"), ~replace_na(., 0)),
    S_mean_pw = replace_na(S_mean_pw, 0)
  ) %>%
  arrange(Country, Quarter)

# Convert to pp GDP (percentage points)
df <- df %>%
  mutate(
    across(starts_with("F_"), ~. * 100),
    S_mean_tw    = S_mean_pw * 100,
    theta_pct    = theta_mean * 100,
    vax_rate_pct = vax_rate * 100
  )

# Create numeric time index for proper ordering
df <- df %>%
  mutate(
    qnum = as.numeric(gsub("Q([0-9]).*", "\\1", Quarter)),
    ynum = as.numeric(gsub(".*\\.", "", Quarter)),
    t_index = (ynum - 2015) * 4 + qnum
  )

cat(sprintf("Panel: %d obs, %d countries, quarters %s to %s\n",
            nrow(df), n_distinct(df$Country),
            min(df$Quarter), max(df$Quarter)))

# =============================================================================
#  LP DATA PREPARATION — Create leads of dependent variables
#  y_{t+h} and debt_{t+h} for h = 0,...,H_max
# =============================================================================

H_max <- 5  # Maximum horizon

df <- df %>%
  arrange(Country, t_index) %>%
  group_by(Country) %>%
  mutate(
    y_lag1 = dplyr::lag(y_t_pct, 1),
    # Cumulative output change: y_{t+h} - y_{t-1}
    dy_h0 = y_t_pct - dplyr::lag(y_t_pct, 1),
    dy_h1 = dplyr::lead(y_t_pct, 1) - dplyr::lag(y_t_pct, 1),
    dy_h2 = dplyr::lead(y_t_pct, 2) - dplyr::lag(y_t_pct, 1),
    dy_h3 = dplyr::lead(y_t_pct, 3) - dplyr::lag(y_t_pct, 1),
    dy_h4 = dplyr::lead(y_t_pct, 4) - dplyr::lag(y_t_pct, 1),
    dy_h5 = dplyr::lead(y_t_pct, 5) - dplyr::lag(y_t_pct, 1),
    # Debt: cumulative change in real debt/2019GDP
    debt_dR = DebtR_share2019 - dplyr::lag(DebtR_share2019, 1),
    dd_h0 = DebtR_share2019 - dplyr::lag(DebtR_share2019, 1),
    dd_h1 = dplyr::lead(DebtR_share2019, 1) - dplyr::lag(DebtR_share2019, 1),
    dd_h2 = dplyr::lead(DebtR_share2019, 2) - dplyr::lag(DebtR_share2019, 1),
    dd_h3 = dplyr::lead(DebtR_share2019, 3) - dplyr::lag(DebtR_share2019, 1),
    dd_h4 = dplyr::lead(DebtR_share2019, 4) - dplyr::lag(DebtR_share2019, 1),
    dd_h5 = dplyr::lead(DebtR_share2019, 5) - dplyr::lag(DebtR_share2019, 1)
  ) %>%
  ungroup()

# Estimation sample: shock period Q1.2020 – Q4.2021
# (fiscal deployment period; pre-2020 has F=0, post-2021 minimal)
# Include 2019 as baseline and 2022Q1 as buffer
lp_sample <- df %>%
  filter(t_index >= (2019 - 2015) * 4 + 1,   # Q1.2019
         t_index <= (2022 - 2015) * 4 + 4)     # Q4.2022

cat(sprintf("LP sample: %d obs, %d countries, %d quarters\n",
            nrow(lp_sample), n_distinct(lp_sample$Country),
            n_distinct(lp_sample$Quarter)))

# =============================================================================
#  LP ESTIMATION FUNCTION
#  Uses fixest::feols for speed and cluster-robust SEs
# =============================================================================

run_lp <- function(data, dep_vars, shock_var, controls, fe = "Country + Quarter",
                   cluster = "Country", H = H_max) {
  results <- tibble()
  for (h in 0:H) {
    dv <- dep_vars[h + 1]
    if (!dv %in% names(data)) next

    fml_str <- paste0(dv, " ~ ", shock_var)
    if (length(controls) > 0 && controls[1] != "") {
      fml_str <- paste0(fml_str, " + ", paste(controls, collapse = " + "))
    }
    fml_str <- paste0(fml_str, " | ", fe)
    fml <- as.formula(fml_str)

    m <- tryCatch(
      feols(fml, data = data, cluster = cluster),
      error = function(e) NULL
    )

    if (is.null(m)) next

    ct <- summary(m)$coeftable
    shock_names <- strsplit(shock_var, " \\+ ")[[1]]

    for (sn in shock_names) {
      sn_clean <- trimws(sn)
      if (sn_clean %in% rownames(ct)) {
        results <- bind_rows(results, tibble(
          h        = h,
          variable = sn_clean,
          coef     = ct[sn_clean, "Estimate"],
          se       = ct[sn_clean, "Std. Error"],
          tval     = ct[sn_clean, "t value"],
          pval     = ct[sn_clean, "Pr(>|t|)"],
          ci90_lo  = ct[sn_clean, "Estimate"] - 1.645 * ct[sn_clean, "Std. Error"],
          ci90_hi  = ct[sn_clean, "Estimate"] + 1.645 * ct[sn_clean, "Std. Error"],
          ci95_lo  = ct[sn_clean, "Estimate"] - 1.96  * ct[sn_clean, "Std. Error"],
          ci95_hi  = ct[sn_clean, "Estimate"] + 1.96  * ct[sn_clean, "Std. Error"],
          nobs     = m$nobs
        ))
      }
    }
  }
  return(results)
}

# Helper: print LP results
print_lp <- function(res, title) {
  cat(sprintf("\n%s\n%s\n", title, strrep("=", nchar(title))))
  for (v in unique(res$variable)) {
    cat(sprintf("\n  %s:\n", v))
    sub <- res %>% filter(variable == v)
    cat(sprintf("  %3s  %9s  %8s  %7s  %8s  %5s\n",
                "h", "coef", "SE", "t-val", "p-val", "N"))
    for (i in 1:nrow(sub)) {
      stars <- ifelse(sub$pval[i]<0.001,"***",ifelse(sub$pval[i]<0.01,"** ",
               ifelse(sub$pval[i]<0.05,"*  ",ifelse(sub$pval[i]<0.1,".  ","   "))))
      cat(sprintf("  %3d  %8.4f%s %8.4f  %7.3f  %8.4f  %5d\n",
                  sub$h[i], sub$coef[i], stars, sub$se[i], sub$tval[i],
                  sub$pval[i], sub$nobs[i]))
    }
  }
}

# Helper: plot IRFs
plot_irf <- function(res, varname, title, ylab, color = "steelblue") {
  sub <- res %>% filter(variable == varname)
  if (nrow(sub) == 0) return(ggplot() + ggtitle(paste("No data for", varname)))

  # Add h = -1 as anchor (zero by construction)
  anchor <- tibble(h = -1, coef = 0, ci90_lo = 0, ci90_hi = 0,
                   ci95_lo = 0, ci95_hi = 0)
  sub <- bind_rows(anchor, sub)

  ggplot(sub, aes(x = h, y = coef)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_ribbon(aes(ymin = ci95_lo, ymax = ci95_hi), alpha = 0.15, fill = color) +
    geom_ribbon(aes(ymin = ci90_lo, ymax = ci90_hi), alpha = 0.25, fill = color) +
    geom_line(linewidth = 1.1, color = color) +
    geom_point(size = 2.5, color = color) +
    scale_x_continuous(breaks = -1:H_max) +
    labs(title = title, x = "Quarters after shock", y = ylab) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 11))
}


# =============================================================================
#  1. OUTPUT GAP IRFs: CP vs DI (pooled)
# =============================================================================
cat("\n\n")
cat(strrep("#", 70), "\n")
cat("  1. OUTPUT GAP: LP IRFs for CP and DI (pooled)\n")
cat(strrep("#", 70), "\n")

dy_vars <- paste0("dy_h", 0:H_max)

res_y_cp <- run_lp(lp_sample, dy_vars, "F_CP",
                   controls = c("F_DI", "S_mean_tw", "y_lag1", "theta_pct"),
                   fe = "Country + Quarter")

res_y_di <- run_lp(lp_sample, dy_vars, "F_DI",
                   controls = c("F_CP", "S_mean_tw", "y_lag1", "theta_pct"),
                   fe = "Country + Quarter")

print_lp(res_y_cp, "OUTPUT: IRF of F_CP -> cumulative output change")
print_lp(res_y_di, "OUTPUT: IRF of F_DI -> cumulative output change")

# Joint estimation (both in same equation)
res_y_joint <- run_lp(lp_sample, dy_vars, "F_CP + F_DI",
                      controls = c("S_mean_tw", "y_lag1", "theta_pct"),
                      fe = "Country + Quarter")
print_lp(res_y_joint, "OUTPUT: Joint LP (CP + DI)")


# =============================================================================
#  2. OUTPUT GAP IRFs: CP SUB-COMPONENTS (loans vs guarantees)
# =============================================================================
cat("\n\n")
cat(strrep("#", 70), "\n")
cat("  2. OUTPUT GAP: CP Sub-Components (Above / Loans / Guarantees)\n")
cat(strrep("#", 70), "\n")

# Guarantee-adjusted variable
lp_sample$F_CP_guar_adj <- lp_sample$F_CP_guar * 0.35

res_y_cp_sub <- run_lp(lp_sample, dy_vars,
                       "F_CP_above + F_CP_loans + F_CP_guar_adj",
                       controls = c("F_DI", "S_mean_tw", "y_lag1", "theta_pct"),
                       fe = "Country + Quarter")
print_lp(res_y_cp_sub, "OUTPUT: CP Sub-Components IRFs")


# =============================================================================
#  3. OUTPUT GAP IRFs: DI SUB-COMPONENTS (transfers vs demand)
# =============================================================================
cat("\n\n")
cat(strrep("#", 70), "\n")
cat("  3. OUTPUT GAP: DI Sub-Components (Transfers vs Demand)\n")
cat(strrep("#", 70), "\n")

res_y_di_sub <- run_lp(lp_sample, dy_vars,
                       "F_DI_transfers + F_DI_demand",
                       controls = c("F_CP", "S_mean_tw", "y_lag1", "theta_pct"),
                       fe = "Country + Quarter")
print_lp(res_y_di_sub, "OUTPUT: DI Sub-Components IRFs")


# =============================================================================
#  4. OUTPUT GAP IRFs: WITH STRINGENCY INTERACTION (state-dependent LP)
#     LP with S*F_CP to capture the capacity-preservation channel
# =============================================================================
cat("\n\n")
cat(strrep("#", 70), "\n")
cat("  4. OUTPUT GAP: State-Dependent LP (S * F_CP interaction)\n")
cat(strrep("#", 70), "\n")

lp_sample$S_x_FCP <- lp_sample$S_mean_tw * lp_sample$F_CP

res_y_state <- run_lp(lp_sample, dy_vars,
                      "F_CP + S_x_FCP + F_DI",
                      controls = c("S_mean_tw", "y_lag1", "theta_pct"),
                      fe = "Country + Quarter")
print_lp(res_y_state, "OUTPUT: State-Dependent LP (F_CP + S*F_CP + F_DI)")


# =============================================================================
#  5. DEBT IRFs: CP vs DI
# =============================================================================
cat("\n\n")
cat(strrep("#", 70), "\n")
cat("  5. DEBT: LP IRFs for CP and DI\n")
cat(strrep("#", 70), "\n")

dd_vars <- paste0("dd_h", 0:H_max)

res_d_joint <- run_lp(lp_sample, dd_vars, "F_CP + F_DI",
                      controls = c("y_t_pct", "S_mean_tw"),
                      fe = "Country")
print_lp(res_d_joint, "DEBT: Joint LP (CP + DI)")


# =============================================================================
#  6. DEBT IRFs: CP SUB-COMPONENTS
# =============================================================================
cat("\n\n")
cat(strrep("#", 70), "\n")
cat("  6. DEBT: CP Sub-Components (Above / Loans / Guarantees)\n")
cat(strrep("#", 70), "\n")

res_d_cp_sub <- run_lp(lp_sample, dd_vars,
                       "F_CP_above + F_CP_loans + F_CP_guar_adj",
                       controls = c("F_DI", "y_t_pct", "S_mean_tw"),
                       fe = "Country")
print_lp(res_d_cp_sub, "DEBT: CP Sub-Components IRFs")


# =============================================================================
#  7. DEBT IRFs: DI SUB-COMPONENTS
# =============================================================================
cat("\n\n")
cat(strrep("#", 70), "\n")
cat("  7. DEBT: DI Sub-Components (Transfers vs Demand)\n")
cat(strrep("#", 70), "\n")

res_d_di_sub <- run_lp(lp_sample, dd_vars,
                       "F_DI_transfers + F_DI_demand",
                       controls = c("F_CP", "y_t_pct", "S_mean_tw"),
                       fe = "Country")
print_lp(res_d_di_sub, "DEBT: DI Sub-Components IRFs")


# =============================================================================
#  8. CUMULATIVE FISCAL MULTIPLIERS
#     Multiplier_h = sum(dy_0...dy_h) / sum(F_0...F_h)
#     In LP framework: the coefficient at horizon h is already the cumulative
#     output response per unit shock. So the multiplier is simply beta_h.
#     (since dy_h = y_{t+h} - y_{t-1} is cumulative by construction)
# =============================================================================
cat("\n\n")
cat(strrep("#", 70), "\n")
cat("  8. CUMULATIVE FISCAL MULTIPLIERS\n")
cat(strrep("#", 70), "\n\n")

cat("  Note: In this LP specification, the dependent variable is\n")
cat("  y_{t+h} - y_{t-1}, which is the cumulative output change.\n")
cat("  The coefficient beta_h directly gives the cumulative multiplier\n")
cat("  (pp output per pp GDP of fiscal shock).\n\n")

cat("  --- Output Multipliers (from joint LP) ---\n")
cat(sprintf("  %3s  %12s  %12s\n", "h", "CP multiplier", "DI multiplier"))
cat("  ", strrep("-", 40), "\n")

for (h in 0:H_max) {
  cp_row <- res_y_joint %>% filter(variable == "F_CP", h == !!h)
  di_row <- res_y_joint %>% filter(variable == "F_DI", h == !!h)
  cp_str <- if(nrow(cp_row) > 0) {
    stars <- ifelse(cp_row$pval<0.05,"*",ifelse(cp_row$pval<0.1,"."," "))
    sprintf("%8.4f%s", cp_row$coef, stars)
  } else "       ---"
  di_str <- if(nrow(di_row) > 0) {
    stars <- ifelse(di_row$pval<0.05,"*",ifelse(di_row$pval<0.1,"."," "))
    sprintf("%8.4f%s", di_row$coef, stars)
  } else "       ---"
  cat(sprintf("  %3d  %12s  %12s\n", h, cp_str, di_str))
}

cat("\n  --- Debt Multipliers (from joint LP) ---\n")
cat(sprintf("  %3s  %12s  %12s\n", "h", "CP -> debt", "DI -> debt"))
cat("  ", strrep("-", 40), "\n")

for (h in 0:H_max) {
  cp_row <- res_d_joint %>% filter(variable == "F_CP", h == !!h)
  di_row <- res_d_joint %>% filter(variable == "F_DI", h == !!h)
  cp_str <- if(nrow(cp_row) > 0) {
    stars <- ifelse(cp_row$pval<0.05,"*",ifelse(cp_row$pval<0.1,"."," "))
    sprintf("%8.4f%s", cp_row$coef, stars)
  } else "       ---"
  di_str <- if(nrow(di_row) > 0) {
    stars <- ifelse(di_row$pval<0.05,"*",ifelse(di_row$pval<0.1,"."," "))
    sprintf("%8.4f%s", di_row$coef, stars)
  } else "       ---"
  cat(sprintf("  %3d  %12s  %12s\n", h, cp_str, di_str))
}

# Efficiency ratio: output gain per unit debt created
cat("\n  --- CP Efficiency: Output Gain per Unit Debt (h-by-h) ---\n")
for (h in 0:H_max) {
  y_cp <- res_y_joint %>% filter(variable == "F_CP", h == !!h)
  d_cp <- res_d_joint %>% filter(variable == "F_CP", h == !!h)
  if (nrow(y_cp) > 0 && nrow(d_cp) > 0 && d_cp$coef != 0) {
    ratio <- y_cp$coef / d_cp$coef
    cat(sprintf("    h=%d: %.3f pp output / %.3f pp debt = %.2f efficiency ratio\n",
                h, y_cp$coef, d_cp$coef, ratio))
  }
}

cat("\n  --- DI Efficiency: Output Gain per Unit Debt (h-by-h) ---\n")
for (h in 0:H_max) {
  y_di <- res_y_joint %>% filter(variable == "F_DI", h == !!h)
  d_di <- res_d_joint %>% filter(variable == "F_DI", h == !!h)
  if (nrow(y_di) > 0 && nrow(d_di) > 0 && d_di$coef != 0) {
    ratio <- y_di$coef / d_di$coef
    cat(sprintf("    h=%d: %.3f pp output / %.3f pp debt = %.2f efficiency ratio\n",
                h, y_di$coef, d_di$coef, ratio))
  }
}


# =============================================================================
#  9. IRF PLOTS
# =============================================================================
cat("\n\n")
cat(strrep("#", 70), "\n")
cat("  9. GENERATING IRF PLOTS\n")
cat(strrep("#", 70), "\n")

# --- Output IRFs: CP vs DI ---
p_y_cp <- plot_irf(res_y_joint, "F_CP",
                   "CP -> Output Gap (cumulative)", "pp output", "darkgreen")
p_y_di <- plot_irf(res_y_joint, "F_DI",
                   "DI -> Output Gap (cumulative)", "pp output", "firebrick")

p_output <- p_y_cp | p_y_di
p_output <- p_output + plot_annotation(
  title = "Local Projections: Fiscal Policy -> Output Gap",
  subtitle = "Cumulative response (y_{t+h} - y_{t-1}) per 1 pp GDP fiscal shock"
)

ggsave(file.path(safeplots, "lp_output_cp_di.pdf"), p_output,
       width = 12, height = 5)
cat("  Saved: lp_output_cp_di.pdf\n")

# --- Output IRFs: CP Sub-Components ---
p_y_above <- plot_irf(res_y_cp_sub, "F_CP_above",
                      "Above-the-line", "pp output", "darkgreen")
p_y_loans <- plot_irf(res_y_cp_sub, "F_CP_loans",
                      "Loans (Code 40+41)", "pp output", "steelblue")
p_y_guar  <- plot_irf(res_y_cp_sub, "F_CP_guar_adj",
                      "Guarantees (adj 0.35)", "pp output", "darkorange")

p_cp_sub <- p_y_above | p_y_loans | p_y_guar
p_cp_sub <- p_cp_sub + plot_annotation(
  title = "Local Projections: CP Sub-Components -> Output Gap"
)

ggsave(file.path(safeplots, "lp_output_cp_subcomponents.pdf"), p_cp_sub,
       width = 15, height = 5)
cat("  Saved: lp_output_cp_subcomponents.pdf\n")

# --- Output IRFs: DI Sub-Components ---
p_y_transfers <- plot_irf(res_y_di_sub, "F_DI_transfers",
                          "Direct Transfers", "pp output", "firebrick")
p_y_demand    <- plot_irf(res_y_di_sub, "F_DI_demand",
                          "Demand Stimulus", "pp output", "purple")

p_di_sub <- p_y_transfers | p_y_demand
p_di_sub <- p_di_sub + plot_annotation(
  title = "Local Projections: DI Sub-Components -> Output Gap"
)

ggsave(file.path(safeplots, "lp_output_di_subcomponents.pdf"), p_di_sub,
       width = 12, height = 5)
cat("  Saved: lp_output_di_subcomponents.pdf\n")

# --- State-dependent: F_CP and S*F_CP ---
p_y_fcp  <- plot_irf(res_y_state, "F_CP",
                     "F_CP (level)", "pp output", "darkgreen")
p_y_sfcp <- plot_irf(res_y_state, "S_x_FCP",
                     "S * F_CP (interaction)", "pp output", "darkorange")
p_y_fdi  <- plot_irf(res_y_state, "F_DI",
                     "F_DI", "pp output", "firebrick")

p_state <- p_y_fcp | p_y_sfcp | p_y_fdi
p_state <- p_state + plot_annotation(
  title = "State-Dependent LP: CP effectiveness conditional on Stringency"
)

ggsave(file.path(safeplots, "lp_output_state_dependent.pdf"), p_state,
       width = 15, height = 5)
cat("  Saved: lp_output_state_dependent.pdf\n")

# --- Debt IRFs: CP vs DI ---
p_d_cp <- plot_irf(res_d_joint, "F_CP",
                   "CP -> Debt (cumulative)", "pp debt/2019GDP", "darkgreen")
p_d_di <- plot_irf(res_d_joint, "F_DI",
                   "DI -> Debt (cumulative)", "pp debt/2019GDP", "firebrick")

p_debt <- p_d_cp | p_d_di
p_debt <- p_debt + plot_annotation(
  title = "Local Projections: Fiscal Policy -> Government Debt",
  subtitle = "Cumulative response (debt_{t+h} - debt_{t-1}) per 1 pp GDP fiscal shock"
)

ggsave(file.path(safeplots, "lp_debt_cp_di.pdf"), p_debt,
       width = 12, height = 5)
cat("  Saved: lp_debt_cp_di.pdf\n")

# --- Debt IRFs: CP Sub-Components ---
p_d_above <- plot_irf(res_d_cp_sub, "F_CP_above",
                      "Above-the-line", "pp debt/2019GDP", "darkgreen")
p_d_loans <- plot_irf(res_d_cp_sub, "F_CP_loans",
                      "Loans", "pp debt/2019GDP", "steelblue")
p_d_guar  <- plot_irf(res_d_cp_sub, "F_CP_guar_adj",
                      "Guarantees (adj)", "pp debt/2019GDP", "darkorange")

p_debt_sub <- p_d_above | p_d_loans | p_d_guar
p_debt_sub <- p_debt_sub + plot_annotation(
  title = "Local Projections: CP Sub-Components -> Government Debt"
)

ggsave(file.path(safeplots, "lp_debt_cp_subcomponents.pdf"), p_debt_sub,
       width = 15, height = 5)
cat("  Saved: lp_debt_cp_subcomponents.pdf\n")

# --- Combined 2x2: Main result ---
p_main <- (p_y_cp | p_y_di) / (p_d_cp | p_d_di)
p_main <- p_main + plot_annotation(
  title = "Local Projections: Fiscal Transmission Channels",
  subtitle = "Top: Output Gap response | Bottom: Debt response | Per 1 pp GDP fiscal shock"
)

ggsave(file.path(safeplots, "lp_main_2x2.pdf"), p_main,
       width = 13, height = 10)
cat("  Saved: lp_main_2x2.pdf\n")

cat("\n\n========== LOCAL PROJECTIONS COMPLETE ==========\n")
