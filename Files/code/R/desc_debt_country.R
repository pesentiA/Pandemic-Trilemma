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

all_qs <- c("Q3.2019","Q4.2019",
            "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
            "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
            "Q1.2022","Q2.2022")
est_qs <- c("Q1.2020","Q2.2020","Q3.2020","Q4.2020",
            "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
            "Q1.2022","Q2.2022")

df <- qdata %>%
  filter(Quarter %in% all_qs) %>%
  select(Country, Quarter, DebtR_share2019) %>%
  mutate(
    q_num = as.integer(substr(as.character(Quarter), 2, 2)),
    yr    = as.integer(substr(as.character(Quarter), 4, 7)),
    date_sort = as.Date(paste0(yr, "-", (q_num - 1) * 3 + 1, "-01"))
  ) %>%
  arrange(Country, date_sort) %>%
  group_by(Country) %>%
  mutate(delta_b = DebtR_share2019 - lag(DebtR_share2019, 1)) %>%
  ungroup() %>%
  filter(Quarter %in% est_qs)

options(width = 160)

# --- By country: level and first difference ---
cat("=== DebtR_share2019 LEVEL by country (Q1.2020-Q2.2022) ===\n\n")
level_by_cty <- df %>%
  group_by(Country) %>%
  summarise(
    n = n(),
    mean_level = round(mean(DebtR_share2019, na.rm = TRUE), 2),
    min_level  = round(min(DebtR_share2019, na.rm = TRUE), 2),
    max_level  = round(max(DebtR_share2019, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_level))
print(level_by_cty, n = 40)

cat("\n\n=== delta_b (FIRST DIFFERENCE) by country (Q1.2020-Q2.2022) ===\n\n")
fd_by_cty <- df %>%
  group_by(Country) %>%
  summarise(
    n      = sum(!is.na(delta_b)),
    mean   = round(mean(delta_b, na.rm = TRUE), 4),
    median = round(median(delta_b, na.rm = TRUE), 4),
    sd     = round(sd(delta_b, na.rm = TRUE), 4),
    min    = round(min(delta_b, na.rm = TRUE), 4),
    max    = round(max(delta_b, na.rm = TRUE), 4),
    cum    = round(sum(delta_b, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(cum))

cat(sprintf("%-6s %3s %8s %8s %8s %8s %8s %8s\n",
            "Cty", "N", "Mean", "Median", "SD", "Min", "Max", "Cum."))
cat(strrep("-", 62), "\n")
for (i in 1:nrow(fd_by_cty)) {
  r <- fd_by_cty[i, ]
  cat(sprintf("%-6s %3d %8.3f %8.3f %8.3f %8.3f %8.3f %8.2f\n",
              r$Country, r$n, r$mean, r$median, r$sd, r$min, r$max, r$cum))
}

cat("\n\n=== delta_b time series for selected countries ===\n\n")
for (cty in c("USA", "GBR", "DEU", "JPN", "ITA", "FRA", "ESP", "GRC", "CHL", "EST")) {
  cat(sprintf("--- %s ---\n", cty))
  df %>%
    filter(Country == cty) %>%
    select(Quarter, DebtR_share2019, delta_b) %>%
    mutate(across(c(DebtR_share2019, delta_b), ~ round(.x, 3))) %>%
    print()
  cat("\n")
}
