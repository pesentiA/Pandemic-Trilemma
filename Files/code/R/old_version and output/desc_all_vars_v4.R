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

# Need Q4.2019 for debt FD lag source, Q3.2019 for debt lag of Q4.2019
all_qs <- c("Q3.2019","Q4.2019",
            "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
            "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
            "Q1.2022","Q2.2022")

est_qs <- c("Q1.2020","Q2.2020","Q3.2020","Q4.2020",
            "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
            "Q1.2022","Q2.2022")

# --- Build blocks ---
df_qdata <- qdata %>%
  filter(Quarter %in% all_qs) %>%
  select(Country, Quarter, y_t_pct, DebtR_share2019,
         p_proj_all_ages, vax_rate)

df_theta <- theta_quarterly_full %>%
  mutate(Quarter = as.character(YQ)) %>%
  filter(Quarter %in% all_qs) %>%
  select(Country, Quarter, theta_mean)

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

df_stringency <- panel_w %>%
  filter(!is.na(S_mean)) %>%
  mutate(year = year(date), quarter = quarter(date),
         Quarter = paste0("Q", quarter, ".", year)) %>%
  filter(Quarter %in% all_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(S_mean_pw = mean(S_mean, na.rm = TRUE), .groups = "drop")

df <- df_qdata %>%
  left_join(df_theta, by = c("Country","Quarter")) %>%
  left_join(df_fiscal, by = c("Country","Quarter")) %>%
  left_join(df_stringency, by = c("Country","Quarter")) %>%
  mutate(
    F_CP = replace_na(F_CP, 0), F_DI = replace_na(F_DI, 0),
    F_H  = replace_na(F_H, 0), S_mean_pw = replace_na(S_mean_pw, 0),
    theta_mean = replace_na(theta_mean, 0),
    p_proj_all_ages = replace_na(p_proj_all_ages, 0),
    vax_rate = replace_na(vax_rate, 0)
  )

# Scale x100
df <- df %>%
  mutate(S_mean_tw = S_mean_pw * 100, F_CP = F_CP * 100,
         F_DI = F_DI * 100, F_H = F_H * 100,
         theta_pct = theta_mean * 100, vax_rate = vax_rate * 100)

# Debt FD (chronological)
df <- df %>%
  mutate(
    q_num = as.integer(substr(as.character(Quarter), 2, 2)),
    yr    = as.integer(substr(as.character(Quarter), 4, 7)),
    date_sort = as.Date(paste0(yr, "-", (q_num - 1) * 3 + 1, "-01"))
  ) %>%
  arrange(Country, date_sort) %>%
  group_by(Country) %>%
  mutate(delta_b = DebtR_share2019 - lag(DebtR_share2019, 1)) %>%
  ungroup()

# --- Estimation sample: Q1.2020 - Q2.2022 (10 quarters, 380 obs) ---
est <- df %>% filter(Quarter %in% est_qs)

cat(sprintf("Estimation sample: %d obs, %d countries, %d quarters\n\n",
            nrow(est), n_distinct(est$Country), n_distinct(est$Quarter)))

# --- Descriptives ---
vars <- list(
  list(name = "y_t_pct",         col = "y_t_pct"),
  list(name = "delta_b",         col = "delta_b"),
  list(name = "S_mean_tw",       col = "S_mean_tw"),
  list(name = "F_CP",            col = "F_CP"),
  list(name = "F_DI",            col = "F_DI"),
  list(name = "F_H",             col = "F_H"),
  list(name = "p_proj_all_ages", col = "p_proj_all_ages"),
  list(name = "theta_pct",       col = "theta_pct"),
  list(name = "vax_rate",        col = "vax_rate")
)

cat(sprintf("%-20s %5s %10s %10s %10s %10s %10s %10s\n",
            "Variable", "N", "Mean", "SD", "Min", "P25", "P75", "Max"))
cat(strrep("-", 95), "\n")

for (v in vars) {
  x <- est[[v$col]][!is.na(est[[v$col]])]
  cat(sprintf("%-20s %5d %10.4f %10.4f %10.4f %10.4f %10.4f %10.4f\n",
              v$name, length(x), mean(x), sd(x), min(x),
              quantile(x, 0.25), quantile(x, 0.75), max(x)))
}
