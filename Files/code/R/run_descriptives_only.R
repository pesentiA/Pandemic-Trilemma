# Test the new descriptives section from analysis.R
# Replicates data prep (lines 1-658) then runs the descriptives block

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(lubridate)
  library(readxl); library(ggplot2); library(modelsummary)
  library(data.table)
})

conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflicts_prefer(dplyr::lag)
conflicted::conflicts_prefer(dplyr::first)
conflicted::conflicts_prefer(dplyr::coalesce)
conflicted::conflicts_prefer(dplyr::recode)
conflicted::conflicts_prefer(lubridate::intersect)
conflicted::conflicts_prefer(lubridate::quarter)
conflicted::conflicts_prefer(lubridate::year)
conflicted::conflicts_prefer(lubridate::wday)
conflicted::conflicts_prefer(dplyr::last)
conflicted::conflicts_prefer(data.table::isoweek)
conflicted::conflicts_prefer(data.table::month)
conflicted::conflicts_prefer(dplyr::lead)
conflicted::conflicts_prefer(modelsummary::SD)
conflicted::conflicts_prefer(lubridate::union)

set.seed(1234)
options(max.print = 99, scipen = 999, na.print = "")

# --- Load data (same as analysis.R) ---
safedata <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/processed"
load(file.path(safedata, "dataforanalysis.RData"))

safeplots <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/output/figures"
safetable <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/output/tables"

fm1 <- read_excel("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/raw/fiscal measures/fiscal_classified_v1_7.xlsx")
fm1 <- fm1 %>%
  mutate(YQ = paste0("Q", Quarter, ".", Year),
         YQ_ord = factor(YQ, levels = sort(unique(paste0("Q", Quarter, ".", Year))), ordered = TRUE)) %>%
  filter(!PolicyCode %in% c(5, 6, 11, 12, 15, 16))

pandemic_qs <- c("Q1.2019","Q2.2019","Q3.2019","Q4.2019",
                 "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                 "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
                 "Q1.2022","Q2.2022","Q3.2022","Q4.2022")

# Blocks 1-4 (minimal merge, same as analysis.R)
df_qdata <- qdata %>% filter(Quarter %in% pandemic_qs) %>%
  select(Country, Quarter, year_only, quarter_only, y_t_pct, d_t_pct,
         vax_rate, p_proj_all_ages, p_avg_all_ages, StringencyIndex_PopWeighted, Qpopulation_th)
pop_2019 <- df_qdata[df_qdata$Quarter=="Q4.2019", c("Country","Qpopulation_th")]
names(pop_2019)[2] <- "pop_2019"
df_qdata <- merge(df_qdata, pop_2019, by="Country")

df_theta <- theta_quarterly_full %>%
  mutate(Quarter = as.character(YQ)) %>%
  filter(Quarter %in% pandemic_qs) %>%
  select(Country, Quarter, theta_mean, S_mean, S_max)

df_fiscal <- fm1 %>% filter(broad_fiscal == 1) %>%
  mutate(Quarter = as.character(YQ_ord)) %>%
  filter(Quarter %in% pandemic_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(F_CP = sum(broad_fiscal_gdp[transmission_channel == "CP"], na.rm=TRUE),
            F_DI = sum(broad_fiscal_gdp[transmission_channel == "DI"], na.rm=TRUE),
            F_H  = sum(broad_fiscal_gdp[transmission_channel == "H"],  na.rm=TRUE),
            F_total = sum(broad_fiscal_gdp, na.rm=TRUE), .groups="drop")

df_stringency <- panel_w %>% filter(!is.na(S_mean)) %>%
  mutate(year = year(date), quarter = quarter(date),
         Quarter = paste0("Q", quarter, ".", year)) %>%
  filter(Quarter %in% pandemic_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(S_mean_pw = mean(S_mean, na.rm=TRUE),
            S_max_pw  = max(S_max, na.rm=TRUE),
            S_sd      = sd(S_mean, na.rm=TRUE), .groups="drop")

df <- df_qdata %>%
  left_join(df_theta, by=c("Country","Quarter")) %>%
  left_join(df_fiscal, by=c("Country","Quarter")) %>%
  left_join(df_stringency, by=c("Country","Quarter")) %>%
  mutate(quarter_num = as.integer(str_sub(Quarter, 2, 2)),
         Quarter = factor(Quarter, levels = pandemic_qs, ordered = TRUE),
         F_CP = replace_na(F_CP, 0), F_DI = replace_na(F_DI, 0),
         F_H = replace_na(F_H, 0), F_total = replace_na(F_total, 0)) %>%
  arrange(Country, Quarter)

# Scale variables (same as analysis.R lines 400-417)
pdata <- df %>%
  filter(Quarter %in% c("Q4.2019","Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                         "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
                         "Q1.2022","Q2.2022","Q3.2022","Q4.2022")) %>%
  mutate(S_mean_tw = S_mean_pw * 100,
         S_max_tw  = S_max_pw * 100,
         F_CP = F_CP * 100, F_DI = F_DI * 100, F_H = F_H * 100,
         vax_rate = vax_rate * 100,
         theta_pct = theta_mean * 100)

# Lags
pdata <- pdata %>%
  group_by(Country) %>%
  arrange(Quarter) %>%
  mutate(y_lag1 = lag(y_t_pct, 1),
         F_DI_lag2 = lag(F_DI, 2)) %>%
  ungroup()

# pdataY + main_sample
quarter_order <- c("Q1.2019","Q2.2019","Q3.2019","Q4.2019",
                   "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                   "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
                   "Q1.2022","Q2.2022","Q3.2022","Q4.2022")
pdataY <- pdata
pdataY$t_idx <- match(as.character(pdataY$Quarter), quarter_order)

main_sample <- pdataY %>% filter(t_idx >= 5 & t_idx <= 13)
cat(sprintf("main_sample: %d obs, %d countries\n\n", nrow(main_sample), n_distinct(main_sample$Country)))

# ========== RUN THE DESCRIPTIVES BLOCK ==========

cat("\n", strrep("=", 70), "\n")
cat("  DESCRIPTIVES FOR PAPER\n")
cat(strrep("=", 70), "\n\n")

# --- D1: Within vs. Between Variation Decomposition ---
cat("--- D1: Within vs. Between Variation Decomposition ---\n\n")

decomp_vars <- c("y_t_pct", "S_mean_tw", "F_CP", "F_DI", "F_H",
                  "p_proj_all_ages", "theta_pct", "vax_rate")
decomp_labels <- c("Output gap (pp)", "Stringency (0-100)",
                    "CP (pp GDP)", "DI (pp GDP)", "Health (pp GDP)",
                    "Excess mortality (P-score)", "Infection prev. (%)",
                    "Vaccination (%)")

cat(sprintf("  %-28s %8s %8s %8s %8s %8s\n",
            "Variable", "Overall", "Between", "Within", "W/O (%)", "B/O (%)"))
cat("  ", strrep("-", 78), "\n")

for (i in seq_along(decomp_vars)) {
  v <- decomp_vars[i]
  if (!v %in% colnames(main_sample)) { cat(sprintf("  SKIP: %s not found\n", v)); next }
  d <- main_sample %>%
    filter(!is.na(.data[[v]])) %>%
    group_by(Country) %>%
    mutate(x_bar_i = mean(.data[[v]], na.rm=TRUE),
           x_within = .data[[v]] - x_bar_i) %>%
    ungroup()
  sd_o <- sd(d[[v]], na.rm=TRUE)
  sd_b <- sd(d$x_bar_i, na.rm=TRUE)
  sd_w <- sd(d$x_within, na.rm=TRUE)
  cat(sprintf("  %-28s %8.3f %8.3f %8.3f %7.1f%% %7.1f%%\n",
              decomp_labels[i], sd_o, sd_b, sd_w,
              (sd_w/sd_o)*100, (sd_b/sd_o)*100))
}

# --- D2: Correlation Matrix ---
cat("\n--- D2: Pairwise Correlation Matrix ---\n\n")
cor_vars <- c("y_t_pct", "S_mean_tw", "F_CP", "F_DI",
              "p_proj_all_ages", "theta_pct", "vax_rate")
cor_data <- main_sample %>% select(all_of(cor_vars)) %>% filter(complete.cases(.))
cor_mat <- cor(cor_data)
print(round(cor_mat, 3))

cat(sprintf("\n  r(S, F_CP)    = %+.3f\n", cor(cor_data$S_mean_tw, cor_data$F_CP)))
cat(sprintf("  r(S, F_DI)    = %+.3f\n", cor(cor_data$S_mean_tw, cor_data$F_DI)))
cat(sprintf("  r(S, theta)   = %+.3f\n", cor(cor_data$S_mean_tw, cor_data$theta_pct)))
cat(sprintf("  r(F_CP, F_DI) = %+.3f\n", cor(cor_data$F_CP, cor_data$F_DI)))

# --- D3: Pre-pandemic validation ---
cat("\n--- D3: Pre-pandemic Validation ---\n")
y_2019 <- pdataY$y_t_pct[pdataY$t_idx <= 4 & !is.na(pdataY$y_t_pct)]
cat(sprintf("  2019 output gap: mean=%.4f, SD=%.4f, N=%d\n", mean(y_2019), sd(y_2019), length(y_2019)))
tt <- t.test(y_2019, mu = 0)
cat(sprintf("  t-test H0: mean=0: t=%.3f, p=%.4f\n", tt$statistic, tt$p.value))

# --- D4: Fiscal distribution ---
cat("\n--- D4: Fiscal Instrument Distributions ---\n")
for (fv in c("F_CP", "F_DI", "F_H")) {
  x <- main_sample[[fv]][!is.na(main_sample[[fv]])]
  cat(sprintf("  %-6s: N=%d, zeros=%d (%.1f%%), skew=%.2f, P50=%.3f, P90=%.3f\n",
              fv, length(x), sum(x==0), mean(x==0)*100,
              mean(((x-mean(x))/sd(x))^3), median(x), quantile(x, 0.90)))
}

cat("\nDone.\n")
