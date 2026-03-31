suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(readxl)
})

conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflicts_prefer(dplyr::lag)
conflicted::conflicts_prefer(dplyr::first)
conflicted::conflicts_prefer(dplyr::last)
conflicted::conflicts_prefer(dplyr::coalesce)
conflicted::conflicts_prefer(lubridate::quarter)
conflicted::conflicts_prefer(lubridate::year)

safedata <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/processed"
load(file.path(safedata, "dataforanalysis.RData"))

fm1 <- read_excel("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/raw/fiscal measures/fiscal_classified_v1_7.xlsx")
fm1 <- fm1 %>%
  mutate(
    YQ     = paste0("Q", Quarter, ".", Year),
    YQ_ord = factor(YQ, levels = sort(unique(paste0("Q", Quarter, ".", Year))), ordered = TRUE)
  ) %>%
  filter(!PolicyCode %in% c(5, 6, 11, 12, 15, 16))

# Include Q4.2019 for lagging debt
all_qs <- c("Q4.2019",
            "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
            "Q1.2021","Q2.2021","Q3.2021","Q4.2021")

est_qs <- c("Q1.2020","Q2.2020","Q3.2020","Q4.2020",
            "Q1.2021","Q2.2021","Q3.2021","Q4.2021")

# --- qdata with delta_b ---
df_qdata <- qdata %>%
  filter(Quarter %in% all_qs) %>%
  select(Country, Quarter, y_t_pct, DebtR_share2019, vax_rate,
         p_proj_all_ages, StringencyIndex_PopWeighted) %>%
  arrange(Country, Quarter) %>%
  group_by(Country) %>%
  mutate(delta_b = DebtR_share2019 - lag(DebtR_share2019, 1)) %>%
  ungroup() %>%
  filter(Quarter %in% est_qs)

# --- theta ---
df_theta <- theta_quarterly_full %>%
  mutate(Quarter = as.character(YQ)) %>%
  filter(Quarter %in% est_qs) %>%
  select(Country, Quarter, theta_mean)

# --- fiscal ---
df_fiscal <- fm1 %>%
  filter(broad_fiscal == 1) %>%
  mutate(Quarter = as.character(YQ_ord)) %>%
  filter(Quarter %in% est_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(
    F_CP = sum(broad_fiscal_gdp[transmission_channel == "CP"], na.rm = TRUE),
    F_DI = sum(broad_fiscal_gdp[transmission_channel == "DI"], na.rm = TRUE),
    F_H  = sum(broad_fiscal_gdp[transmission_channel == "H"],  na.rm = TRUE),
    .groups = "drop"
  )

# --- stringency ---
df_stringency <- panel_w %>%
  filter(!is.na(S_mean)) %>%
  mutate(
    year    = year(date),
    quarter = quarter(date),
    Quarter = paste0("Q", quarter, ".", year)
  ) %>%
  filter(Quarter %in% est_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(S_mean_pw = mean(S_mean, na.rm = TRUE), .groups = "drop")

# --- Merge ---
est <- df_qdata %>%
  left_join(df_theta,      by = c("Country", "Quarter")) %>%
  left_join(df_fiscal,     by = c("Country", "Quarter")) %>%
  left_join(df_stringency, by = c("Country", "Quarter")) %>%
  mutate(
    F_CP = replace_na(F_CP, 0),
    F_DI = replace_na(F_DI, 0),
    F_H  = replace_na(F_H,  0)
  )

cat(sprintf("Estimation sample: %d obs, %d countries\n\n",
            nrow(est), n_distinct(est$Country)))

# --- Print descriptives ---
vars <- c("y_t_pct", "delta_b", "S_mean_pw", "F_CP", "F_DI", "F_H",
          "p_proj_all_ages", "theta_mean", "vax_rate")

cat(sprintf("%-20s %5s %10s %10s %10s %10s %10s %10s\n",
            "Variable", "N", "Mean", "SD", "Min", "P25", "P75", "Max"))
cat(strrep("-", 95), "\n")

for (v in vars) {
  x <- est[[v]][!is.na(est[[v]])]
  cat(sprintf("%-20s %5d %10.4f %10.4f %10.4f %10.4f %10.4f %10.4f\n",
              v, length(x), mean(x), sd(x), min(x),
              quantile(x, 0.25), quantile(x, 0.75), max(x)))
}
