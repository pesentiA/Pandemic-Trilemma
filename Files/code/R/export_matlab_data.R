suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(lubridate)
  library(readxl); library(data.table)
})

conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflicts_prefer(dplyr::lag)
conflicted::conflicts_prefer(dplyr::first)
conflicted::conflicts_prefer(dplyr::coalesce)
conflicted::conflicts_prefer(dplyr::recode)
conflicted::conflicts_prefer(lubridate::quarter)
conflicted::conflicts_prefer(lubridate::year)
conflicted::conflicts_prefer(dplyr::last)
conflicted::conflicts_prefer(data.table::isoweek)
conflicted::conflicts_prefer(data.table::month)
conflicted::conflicts_prefer(dplyr::lead)
conflicted::conflicts_prefer(lubridate::union)

set.seed(1234)
options(scipen = 999)

# --- Paths ---
safedata <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/processed"
outdir   <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/code/R"

load(file.path(safedata, "dataforanalysis.RData"))

fm1 <- read_excel("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/raw/fiscal measures/fiscal_classified_v1_7.xlsx")
fm1 <- fm1 %>%
  mutate(YQ = paste0("Q", Quarter, ".", Year),
         YQ_ord = factor(YQ, levels = sort(unique(paste0("Q", Quarter, ".", Year))), ordered = TRUE)) %>%
  filter(!PolicyCode %in% c(5, 6, 11, 12, 15, 16))

# All quarters needed (Q3.2019 for debt lag, Q4.2019-Q4.2022 for export)
all_qs <- c("Q3.2019","Q4.2019",
            "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
            "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
            "Q1.2022","Q2.2022","Q3.2022","Q4.2022")

export_qs <- c("Q4.2019",
               "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
               "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
               "Q1.2022","Q2.2022","Q3.2022","Q4.2022")

# --- Block 1: qdata ---
df_qdata <- qdata %>%
  filter(Quarter %in% all_qs) %>%
  select(Country, Quarter, y_t_pct, DebtR_share2019,
         p_proj_all_ages, vax_rate)

# --- Block 2: theta + S ---
df_theta <- theta_quarterly_full %>%
  mutate(Quarter = as.character(YQ)) %>%
  filter(Quarter %in% all_qs) %>%
  select(Country, Quarter, theta_mean, S_mean)

# --- Block 3: fiscal ---
df_fiscal <- fm1 %>%
  filter(broad_fiscal == 1) %>%
  mutate(Quarter = as.character(YQ_ord)) %>%
  filter(Quarter %in% all_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(
    F_CP = sum(broad_fiscal_gdp[transmission_channel == "CP"], na.rm = TRUE),
    F_DI = sum(broad_fiscal_gdp[transmission_channel == "DI"], na.rm = TRUE),
    F_H  = sum(broad_fiscal_gdp[transmission_channel == "H"],  na.rm = TRUE),
    .groups = "drop"
  )

# --- Block 3b: CP sub-components ---
df_cp_sub <- fm1 %>%
  filter(broad_fiscal == 1) %>%
  mutate(
    Quarter = as.character(YQ_ord),
    CP_above = ifelse(transmission_channel == "CP" & category == 1, broad_fiscal_gdp, 0),
    CP_below = ifelse(transmission_channel == "CP" & category == 2, broad_fiscal_gdp, 0),
    CP_guar  = ifelse(transmission_channel == "CP" & category == 2 &
                        PolicyCode == "43", broad_fiscal_gdp, 0)
  ) %>%
  filter(Quarter %in% all_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(
    F_CP_above = sum(CP_above, na.rm = TRUE),
    F_CP_below_raw = sum(CP_below, na.rm = TRUE),
    F_CP_guar  = sum(CP_guar, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    F_CP_below_adj_mid = F_CP_below_raw - F_CP_guar + F_CP_guar * 0.35
  )

# --- Block 4: stringency ---
df_stringency <- panel_w %>%
  filter(!is.na(S_mean)) %>%
  mutate(year = year(date), quarter = quarter(date),
         Quarter = paste0("Q", quarter, ".", year)) %>%
  filter(Quarter %in% all_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(S_mean_pw = mean(S_mean, na.rm = TRUE), .groups = "drop")

# --- Merge ---
df <- df_qdata %>%
  left_join(df_theta, by = c("Country","Quarter")) %>%
  left_join(df_fiscal, by = c("Country","Quarter")) %>%
  left_join(df_cp_sub, by = c("Country","Quarter")) %>%
  left_join(df_stringency, by = c("Country","Quarter")) %>%
  mutate(
    F_CP = replace_na(F_CP, 0),
    F_DI = replace_na(F_DI, 0),
    F_H  = replace_na(F_H, 0),
    F_CP_above = replace_na(F_CP_above, 0),
    F_CP_below_adj_mid = replace_na(F_CP_below_adj_mid, 0),
    S_mean_pw = replace_na(S_mean_pw, 0),
    theta_mean = replace_na(theta_mean, 0),
    p_proj_all_ages = replace_na(p_proj_all_ages, 0),
    vax_rate = replace_na(vax_rate, 0)
  )

# --- Scale (×100, same as analysis.R) ---
df <- df %>%
  mutate(
    S_mean_tw  = S_mean_pw * 100,
    F_CP       = F_CP * 100,
    F_DI       = F_DI * 100,
    F_H        = F_H * 100,
    F_CP_above = F_CP_above * 100,
    F_CP_below_adj_mid = F_CP_below_adj_mid * 100,
    theta_pct  = theta_mean * 100,
    vax_rate   = vax_rate * 100
  )

# --- Debt first difference (chronological sorting) ---
df <- df %>%
  mutate(
    q_num = as.integer(substr(as.character(Quarter), 2, 2)),
    yr    = as.integer(substr(as.character(Quarter), 4, 7)),
    date_sort = as.Date(paste0(yr, "-", (q_num - 1) * 3 + 1, "-01"))
  ) %>%
  arrange(Country, date_sort) %>%
  group_by(Country) %>%
  mutate(debt_dR = DebtR_share2019 - lag(DebtR_share2019, 1)) %>%
  ungroup()

# --- Export: Q4.2019 - Q4.2022 ---
export_data <- df %>%
  filter(Quarter %in% export_qs) %>%
  select(
    Country, Quarter,
    S_mean_tw, theta_pct,
    F_CP, F_CP_above, F_CP_below_adj_mid, F_DI, F_H,
    y_t_pct, debt_dR,
    excess_mortality = p_proj_all_ages,
    vax_rate
  ) %>%
  as.data.frame()

# --- Diagnostics ---
cat("=== Export Summary ===\n")
cat(sprintf("  Countries: %d\n", length(unique(export_data$Country))))
cat(sprintf("  Quarters:  %d (%s to %s)\n",
            length(unique(export_data$Quarter)),
            min(as.character(export_data$Quarter)),
            max(as.character(export_data$Quarter))))
cat(sprintf("  Rows:      %d\n", nrow(export_data)))

cat("\n=== Missing Values ===\n")
print(colSums(is.na(export_data)))

cat("\n=== Variable Ranges ===\n")
for (v in names(export_data)[-(1:2)]) {
  cat(sprintf("  %-25s  min=%8.3f  max=%8.3f  mean=%8.3f\n",
              v, min(export_data[[v]], na.rm=TRUE),
              max(export_data[[v]], na.rm=TRUE),
              mean(export_data[[v]], na.rm=TRUE)))
}

outfile <- file.path(outdir, "country_data_for_matlab.csv")
write.csv(export_data, outfile, row.names = FALSE)
cat(sprintf("\n  Exported to %s\n", outfile))
cat(sprintf("  File size: %.1f KB\n", file.size(outfile) / 1024))
