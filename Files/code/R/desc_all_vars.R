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

# Load fiscal measures
fm1 <- read_excel("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/raw/fiscal measures/fiscal_classified_v1_7.xlsx")
fm1 <- fm1 %>%
  mutate(
    YQ     = paste0("Q", Quarter, ".", Year),
    YQ_ord = factor(YQ, levels = sort(unique(paste0("Q", Quarter, ".", Year))), ordered = TRUE)
  ) %>%
  filter(!PolicyCode %in% c(5, 6, 11, 12, 15, 16))

pandemic_qs <- c(
  "Q1.2019","Q2.2019","Q3.2019","Q4.2019",
  "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
  "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
  "Q1.2022","Q2.2022","Q3.2022","Q4.2022")

# --- Block 1: qdata ---
df_qdata <- qdata %>%
  filter(Quarter %in% pandemic_qs) %>%
  select(Country, Quarter, year_only, quarter_only,
         y_t_pct, d_t_pct, vax_rate, p_proj_all_ages,
         StringencyIndex_PopWeighted, Qpopulation_th)

# --- Block 2: theta ---
df_theta <- theta_quarterly_full %>%
  mutate(Quarter = as.character(YQ)) %>%
  filter(Quarter %in% pandemic_qs) %>%
  select(Country, Quarter, theta_mean, S_mean)

# --- Block 3: fiscal ---
df_fiscal <- fm1 %>%
  filter(broad_fiscal == 1) %>%
  mutate(Quarter = as.character(YQ_ord)) %>%
  filter(Quarter %in% pandemic_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(
    F_CP    = sum(broad_fiscal_gdp[transmission_channel == "CP"], na.rm = TRUE),
    F_DI    = sum(broad_fiscal_gdp[transmission_channel == "DI"], na.rm = TRUE),
    F_H     = sum(broad_fiscal_gdp[transmission_channel == "H"],  na.rm = TRUE),
    .groups = "drop"
  )

# --- Block 4: stringency from panel_w ---
df_stringency <- panel_w %>%
  filter(!is.na(S_mean)) %>%
  mutate(
    year    = year(date),
    quarter = quarter(date),
    Quarter = paste0("Q", quarter, ".", year)
  ) %>%
  filter(Quarter %in% pandemic_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(
    S_mean_pw = mean(S_mean, na.rm = TRUE),
    .groups   = "drop"
  )

# --- Merge ---
df <- df_qdata %>%
  left_join(df_theta,      by = c("Country", "Quarter")) %>%
  left_join(df_fiscal,     by = c("Country", "Quarter")) %>%
  left_join(df_stringency, by = c("Country", "Quarter")) %>%
  mutate(
    F_CP = replace_na(F_CP, 0),
    F_DI = replace_na(F_DI, 0),
    F_H  = replace_na(F_H,  0)
  )

# --- Restrict to estimation sample: Q1.2020 - Q4.2021 ---
est <- df %>%
  filter(Quarter %in% c("Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                         "Q1.2021","Q2.2021","Q3.2021","Q4.2021"))

cat(sprintf("Estimation sample: %d obs, %d countries, %d quarters\n",
            nrow(est), n_distinct(est$Country), n_distinct(est$Quarter)))

# --- Compute descriptives ---
vars <- list(
  "y_t_pct"    = "y_t_pct",
  "d_t_pct"    = "d_t_pct",
  "S_mean_pw"  = "S_mean_pw",
  "F_CP"       = "F_CP",
  "F_DI"       = "F_DI",
  "F_H"        = "F_H",
  "p_proj_all_ages" = "p_proj_all_ages",
  "theta_mean" = "theta_mean",
  "vax_rate"   = "vax_rate"
)

cat("\n=== Summary Statistics (Estimation Sample: Q1.2020 - Q4.2021) ===\n\n")
cat(sprintf("%-20s %6s %10s %10s %10s %10s %10s %10s\n",
            "Variable", "N", "Mean", "SD", "Min", "P25", "P75", "Max"))
cat(strrep("-", 96), "\n")

for (nm in names(vars)) {
  v <- est[[vars[[nm]]]]
  v <- v[!is.na(v)]
  cat(sprintf("%-20s %6d %10.4f %10.4f %10.4f %10.4f %10.4f %10.4f\n",
              nm, length(v), mean(v), sd(v), min(v),
              quantile(v, 0.25), quantile(v, 0.75), max(v)))
}

# Also compute for the full sample Q1.2019-Q4.2022
cat("\n\n=== Summary Statistics (Full Sample: Q1.2019 - Q4.2022) ===\n\n")
cat(sprintf("%-20s %6s %10s %10s %10s %10s %10s %10s\n",
            "Variable", "N", "Mean", "SD", "Min", "P25", "P75", "Max"))
cat(strrep("-", 96), "\n")

for (nm in names(vars)) {
  v <- df[[vars[[nm]]]]
  v <- v[!is.na(v)]
  cat(sprintf("%-20s %6d %10.4f %10.4f %10.4f %10.4f %10.4f %10.4f\n",
              nm, length(v), mean(v), sd(v), min(v),
              quantile(v, 0.25), quantile(v, 0.75), max(v)))
}
