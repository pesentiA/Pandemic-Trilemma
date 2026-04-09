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

est_qs <- c("Q1.2020","Q2.2020","Q3.2020","Q4.2020",
            "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
            "Q1.2022","Q2.2022")

df_fiscal <- fm1 %>%
  filter(broad_fiscal == 1) %>%
  mutate(Quarter = as.character(YQ_ord)) %>%
  filter(Quarter %in% est_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(F_CP = sum(broad_fiscal_gdp[transmission_channel == "CP"], na.rm = TRUE),
            .groups = "drop")

df_stringency <- panel_w %>%
  filter(!is.na(S_mean)) %>%
  mutate(year = year(date), quarter = quarter(date),
         Quarter = paste0("Q", quarter, ".", year)) %>%
  filter(Quarter %in% est_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(S_mean_pw = mean(S_mean, na.rm = TRUE), .groups = "drop")

# Merge
df <- expand.grid(Country = unique(df_stringency$Country), Quarter = est_qs, stringsAsFactors = FALSE) %>%
  left_join(df_fiscal, by = c("Country","Quarter")) %>%
  left_join(df_stringency, by = c("Country","Quarter")) %>%
  mutate(F_CP = replace_na(F_CP, 0) * 100,
         S_mean_pw = replace_na(S_mean_pw, 0) * 100)

# Country-quarters with S == 0
cat("=== Country-quarters with S = 0 ===\n")
s_zero <- df %>% filter(S_mean_pw == 0)
if (nrow(s_zero) > 0) {
  print(s_zero %>% arrange(Country, Quarter))
} else {
  cat("  None.\n")
}

# Country-quarters with F_CP == 0
cat("\n=== Country-quarters with F_CP = 0 ===\n")
cp_zero <- df %>% filter(F_CP == 0)
cat(sprintf("  %d country-quarters (%.1f%%)\n", nrow(cp_zero), nrow(cp_zero)/nrow(df)*100))

# Countries with F_CP == 0 in ALL quarters
cat("\n=== Countries with F_CP = 0 in ALL sample quarters ===\n")
cp_always_zero <- df %>%
  group_by(Country) %>%
  summarise(all_zero_CP = all(F_CP == 0), n_zero = sum(F_CP == 0), .groups = "drop") %>%
  filter(all_zero_CP)
if (nrow(cp_always_zero) > 0) {
  print(cp_always_zero)
} else {
  cat("  None — all countries deployed some CP.\n")
}

# Countries with BOTH S == 0 AND F_CP == 0 in any quarter
cat("\n=== Country-quarters with BOTH S = 0 AND F_CP = 0 ===\n")
both_zero <- df %>% filter(S_mean_pw == 0 & F_CP == 0)
if (nrow(both_zero) > 0) {
  print(both_zero %>% arrange(Country, Quarter))
} else {
  cat("  None.\n")
}

# By country: number of quarters with zero CP
cat("\n=== Quarters with F_CP = 0, by country ===\n")
df %>%
  group_by(Country) %>%
  summarise(n_zero_CP = sum(F_CP == 0), n_zero_S = sum(S_mean_pw == 0),
            mean_CP = round(mean(F_CP), 3), mean_S = round(mean(S_mean_pw), 1),
            .groups = "drop") %>%
  filter(n_zero_CP > 0) %>%
  arrange(desc(n_zero_CP)) %>%
  print(n = 50)
