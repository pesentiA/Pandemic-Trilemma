suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readxl); library(lubridate); library(data.table)
})
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflicts_prefer(dplyr::lag)
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

all_qs <- c("Q3.2019","Q4.2019",
            "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
            "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
            "Q1.2022","Q2.2022")
est_qs <- c("Q1.2020","Q2.2020","Q3.2020","Q4.2020",
            "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
            "Q1.2022","Q2.2022")

df_qdata <- qdata %>% filter(Quarter %in% all_qs) %>%
  select(Country, Quarter, y_t_pct, DebtR_share2019, p_proj_all_ages, vax_rate)
df_theta <- theta_quarterly_full %>% mutate(Quarter = as.character(YQ)) %>%
  filter(Quarter %in% all_qs) %>% select(Country, Quarter, theta_mean)
df_fiscal <- fm1 %>% filter(broad_fiscal == 1) %>%
  mutate(Quarter = as.character(YQ_ord)) %>% filter(Quarter %in% all_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(F_CP = sum(broad_fiscal_gdp[transmission_channel == "CP"], na.rm=TRUE),
            F_DI = sum(broad_fiscal_gdp[transmission_channel == "DI"], na.rm=TRUE),
            F_H  = sum(broad_fiscal_gdp[transmission_channel == "H"],  na.rm=TRUE),
            .groups = "drop")
df_stringency <- panel_w %>% filter(!is.na(S_mean)) %>%
  mutate(year = year(date), quarter = quarter(date),
         Quarter = paste0("Q", quarter, ".", year)) %>%
  filter(Quarter %in% all_qs) %>%
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

df <- df %>%
  mutate(q_num = as.integer(substr(as.character(Quarter), 2, 2)),
         yr = as.integer(substr(as.character(Quarter), 4, 7)),
         date_sort = as.Date(paste0(yr, "-", (q_num - 1) * 3 + 1, "-01"))) %>%
  arrange(Country, date_sort) %>%
  group_by(Country) %>%
  mutate(delta_b = DebtR_share2019 - lag(DebtR_share2019, 1)) %>%
  ungroup()

est <- df %>% filter(Quarter %in% est_qs)

# --- Cumulative/average per country across the sample ---
country_totals <- est %>%
  group_by(Country) %>%
  summarise(
    # Output: average output gap (mean recession depth)
    y_mean        = mean(y_t_pct, na.rm = TRUE),
    # Debt: cumulative change (total debt accumulated)
    cum_delta_b   = sum(delta_b, na.rm = TRUE),
    # Stringency: average containment intensity
    S_mean        = mean(S_mean_tw, na.rm = TRUE),
    # Fiscal: cumulative deployment over the period
    cum_CP        = sum(F_CP, na.rm = TRUE),
    cum_DI        = sum(F_DI, na.rm = TRUE),
    cum_H         = sum(F_H, na.rm = TRUE),
    # Mortality: cumulative excess mortality (sum of P-scores)
    cum_excess    = sum(p_proj_all_ages, na.rm = TRUE),
    # Infections: cumulative prevalence
    cum_theta     = sum(theta_pct, na.rm = TRUE),
    .groups = "drop"
  )

cat(sprintf("Countries: %d\n\n", nrow(country_totals)))

# --- Cross-country distribution of totals ---
cat("=== Cross-Country Distribution of Cumulative/Average Values ===\n")
cat("    (Q1.2020-Q2.2022, 10 quarters per country)\n\n")

vars <- list(
  list(name = "Avg. output gap (pp)",      col = "y_mean"),
  list(name = "Cum. debt change (pp GDP)",  col = "cum_delta_b"),
  list(name = "Avg. stringency (0-100)",    col = "S_mean"),
  list(name = "Cum. CP (% of 2019 GDP)",    col = "cum_CP"),
  list(name = "Cum. DI (% of 2019 GDP)",    col = "cum_DI"),
  list(name = "Cum. Health (% of 2019 GDP)",col = "cum_H"),
  list(name = "Cum. excess mort. (P-score)",col = "cum_excess"),
  list(name = "Cum. infection prev. (%)",   col = "cum_theta")
)

cat(sprintf("%-34s %6s %8s %8s %8s %8s %8s %8s\n",
            "Variable", "N", "Mean", "Median", "SD", "Min", "P25", "Max"))
cat(strrep("-", 100), "\n")

for (v in vars) {
  x <- country_totals[[v$col]]
  cat(sprintf("%-34s %6d %8.2f %8.2f %8.2f %8.2f %8.2f %8.2f\n",
              v$name, length(x), mean(x), median(x), sd(x),
              min(x), quantile(x, 0.25), max(x)))
}

# --- Full country table ---
cat("\n\n=== Full Country Table (sorted by cumulative debt) ===\n\n")
cat(sprintf("%-5s %8s %8s %8s %8s %8s %8s %8s %8s\n",
            "Cty", "Avg.y", "Cum.Db", "Avg.S", "Cum.CP", "Cum.DI",
            "Cum.H", "Cum.ExM", "Cum.Th"))
cat(strrep("-", 80), "\n")

ct_sorted <- country_totals %>% arrange(desc(cum_delta_b))
for (i in 1:nrow(ct_sorted)) {
  r <- ct_sorted[i, ]
  cat(sprintf("%-5s %8.2f %8.2f %8.1f %8.2f %8.2f %8.2f %8.1f %8.2f\n",
              r$Country, r$y_mean, r$cum_delta_b, r$S_mean,
              r$cum_CP, r$cum_DI, r$cum_H, r$cum_excess, r$cum_theta))
}
