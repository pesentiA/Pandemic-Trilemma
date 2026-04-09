suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(lubridate)
  library(readxl); library(data.table)
})

conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflicts_prefer(dplyr::lag)
conflicted::conflicts_prefer(dplyr::first)
conflicted::conflicts_prefer(dplyr::last)
conflicted::conflicts_prefer(dplyr::coalesce)
conflicted::conflicts_prefer(lubridate::quarter)
conflicted::conflicts_prefer(lubridate::year)
conflicted::conflicts_prefer(data.table::isoweek)
conflicted::conflicts_prefer(data.table::month)
conflicted::conflicts_prefer(dplyr::lead)
conflicted::conflicts_prefer(lubridate::union)

safedata <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/processed"
load(file.path(safedata, "dataforanalysis.RData"))

fm1 <- read_excel("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/raw/fiscal measures/fiscal_classified_v1_7.xlsx")
fm1 <- fm1 %>%
  mutate(YQ = paste0("Q", Quarter, ".", Year),
         YQ_ord = factor(YQ, levels = sort(unique(paste0("Q", Quarter, ".", Year))), ordered = TRUE)) %>%
  filter(!PolicyCode %in% c(5, 6, 11, 12, 15, 16))

pandemic_qs <- c("Q1.2019","Q2.2019","Q3.2019","Q4.2019",
                 "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                 "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
                 "Q1.2022","Q2.2022","Q3.2022","Q4.2022")

df_qdata <- qdata %>% filter(Quarter %in% pandemic_qs) %>%
  select(Country, Quarter, y_t_pct, p_proj_all_ages, vax_rate, Qpopulation_th)

df_theta <- theta_quarterly_full %>% mutate(Quarter = as.character(YQ)) %>%
  filter(Quarter %in% pandemic_qs) %>% select(Country, Quarter, theta_mean, S_mean)

df_fiscal <- fm1 %>% filter(broad_fiscal == 1) %>%
  mutate(Quarter = as.character(YQ_ord)) %>% filter(Quarter %in% pandemic_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(F_CP = sum(broad_fiscal_gdp[transmission_channel == "CP"], na.rm=TRUE),
            F_DI = sum(broad_fiscal_gdp[transmission_channel == "DI"], na.rm=TRUE),
            F_H  = sum(broad_fiscal_gdp[transmission_channel == "H"],  na.rm=TRUE),
            .groups = "drop")

df_stringency <- panel_w %>% filter(!is.na(S_mean)) %>%
  mutate(year = year(date), quarter = quarter(date),
         Quarter = paste0("Q", quarter, ".", year)) %>%
  filter(Quarter %in% pandemic_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(S_mean_pw = mean(S_mean, na.rm=TRUE), .groups = "drop")

df <- df_qdata %>%
  left_join(df_theta, by=c("Country","Quarter")) %>%
  left_join(df_fiscal, by=c("Country","Quarter")) %>%
  left_join(df_stringency, by=c("Country","Quarter")) %>%
  mutate(F_CP = replace_na(F_CP, 0), F_DI = replace_na(F_DI, 0),
         F_H = replace_na(F_H, 0), S_mean_pw = replace_na(S_mean_pw, 0),
         theta_mean = replace_na(theta_mean, 0),
         p_proj_all_ages = replace_na(p_proj_all_ages, 0),
         vax_rate = replace_na(vax_rate, 0))

df <- df %>%
  mutate(S_mean_tw = S_mean_pw * 100, F_CP = F_CP * 100, F_DI = F_DI * 100,
         F_H = F_H * 100, theta_pct = theta_mean * 100, vax_rate = vax_rate * 100)

# t_idx
df$t_idx <- match(as.character(df$Quarter), pandemic_qs)

# Main sample: t_idx 5-14 (Q1.2020-Q2.2022, 380 obs)
main_sample <- df %>% filter(t_idx >= 5 & t_idx <= 14)
cat(sprintf("Sample: %d obs, %d countries, %d quarters\n\n",
            nrow(main_sample), n_distinct(main_sample$Country),
            n_distinct(main_sample$Quarter)))

# =============================================================================
# D1: Within vs Between Decomposition
# =============================================================================
cat("=== D1: Within vs Between Variation Decomposition ===\n\n")

decomp_vars <- c("y_t_pct", "S_mean_tw", "F_CP", "F_DI", "F_H",
                  "p_proj_all_ages", "theta_pct", "vax_rate")
decomp_labels <- c("y_ik", "S_ik", "F_CP_ik", "F_DI_ik", "F_H_ik",
                    "d_ik", "theta_ik", "Vax_ik")

cat(sprintf("%-12s %8s %8s %8s %8s %8s\n",
            "Variable", "Overall", "Between", "Within", "W/O(%)", "B/O(%)"))
cat(strrep("-", 60), "\n")

for (i in seq_along(decomp_vars)) {
  v <- decomp_vars[i]
  d <- main_sample %>%
    filter(!is.na(.data[[v]])) %>%
    group_by(Country) %>%
    mutate(x_bar_i = mean(.data[[v]], na.rm=TRUE),
           x_within = .data[[v]] - x_bar_i) %>%
    ungroup()
  sd_o <- sd(d[[v]], na.rm=TRUE)
  sd_b <- sd(d$x_bar_i, na.rm=TRUE)
  sd_w <- sd(d$x_within, na.rm=TRUE)
  cat(sprintf("%-12s %8.3f %8.3f %8.3f %7.1f %7.1f\n",
              decomp_labels[i], sd_o, sd_b, sd_w,
              (sd_w/sd_o)*100, (sd_b/sd_o)*100))
}

# =============================================================================
# D2: Correlation Matrix
# =============================================================================
cat("\n\n=== D2: Pairwise Correlation Matrix ===\n\n")

cor_vars <- c("y_t_pct", "S_mean_tw", "F_CP", "F_DI",
              "p_proj_all_ages", "theta_pct", "vax_rate")

cor_data <- main_sample %>% select(all_of(cor_vars)) %>% filter(complete.cases(.))
cor_mat <- cor(cor_data)

cat(sprintf("N complete cases: %d\n\n", nrow(cor_data)))

# Print lower triangle
cor_labels <- c("y", "S", "F_CP", "F_DI", "d", "theta", "Vax")
for (j in 1:length(cor_vars)) {
  row_str <- sprintf("%-8s", cor_labels[j])
  for (k in 1:length(cor_vars)) {
    if (k > j) {
      row_str <- paste0(row_str, "        ")
    } else if (k == j) {
      row_str <- paste0(row_str, "    1   ")
    } else {
      row_str <- paste0(row_str, sprintf(" %6.2f ", cor_mat[j, k]))
    }
  }
  cat(row_str, "\n")
}

cat(sprintf("\nKey correlations:\n"))
cat(sprintf("  r(S, F_CP)    = %+.2f\n", cor_mat["S_mean_tw","F_CP"]))
cat(sprintf("  r(S, F_DI)    = %+.2f\n", cor_mat["S_mean_tw","F_DI"]))
cat(sprintf("  r(S, theta)   = %+.2f\n", cor_mat["S_mean_tw","theta_pct"]))
cat(sprintf("  r(F_CP, F_DI) = %+.2f\n", cor_mat["F_CP","F_DI"]))
cat(sprintf("  r(S, d)       = %+.2f\n", cor_mat["S_mean_tw","p_proj_all_ages"]))
