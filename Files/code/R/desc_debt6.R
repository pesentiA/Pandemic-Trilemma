suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
})
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflicts_prefer(dplyr::lag)

safedata <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/processed"
load(file.path(safedata, "dataforanalysis.RData"))

# Include Q4.2019 as lag source only
lag_and_est <- c("Q4.2019",
                 "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                 "Q1.2021","Q2.2021","Q3.2021","Q4.2021")

est_only <- c("Q1.2020","Q2.2020","Q3.2020","Q4.2020",
              "Q1.2021","Q2.2021","Q3.2021","Q4.2021")

df <- qdata %>%
  filter(Quarter %in% lag_and_est) %>%
  select(Country, Quarter, DebtR_share2019) %>%
  mutate(
    q_num = as.integer(substr(Quarter, 2, 2)),
    yr    = as.integer(substr(Quarter, 4, 7)),
    date  = as.Date(paste0(yr, "-", (q_num - 1) * 3 + 1, "-01"))
  ) %>%
  arrange(Country, date) %>%
  group_by(Country) %>%
  mutate(delta_b = DebtR_share2019 - lag(DebtR_share2019, 1)) %>%
  ungroup() %>%
  filter(Quarter %in% est_only)  # drop Q4.2019 (lag source only)

cat(sprintf("N: %d, missing delta_b: %d\n\n", nrow(df), sum(is.na(df$delta_b))))

x <- df$delta_b[!is.na(df$delta_b)]
cat(sprintf("  N:      %d\n", length(x)))
cat(sprintf("  Mean:   %.4f\n", mean(x)))
cat(sprintf("  SD:     %.4f\n", sd(x)))
cat(sprintf("  Min:    %.4f\n", min(x)))
cat(sprintf("  P25:    %.4f\n", quantile(x, 0.25)))
cat(sprintf("  Median: %.4f\n", median(x)))
cat(sprintf("  P75:    %.4f\n", quantile(x, 0.75)))
cat(sprintf("  Max:    %.4f\n", max(x)))

cat("\n=== By Quarter ===\n")
df %>%
  filter(!is.na(delta_b)) %>%
  group_by(Quarter) %>%
  summarise(n = n(), mean = round(mean(delta_b), 3),
            sd = round(sd(delta_b), 3), .groups = "drop") %>%
  print(n = Inf)
