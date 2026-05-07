suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
})
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflicts_prefer(dplyr::lag)
conflicted::conflicts_prefer(lubridate::quarter)
conflicted::conflicts_prefer(lubridate::year)

safedata <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/processed"
load(file.path(safedata, "dataforanalysis.RData"))

all_qs <- c("Q3.2019","Q4.2019",
            "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
            "Q1.2021","Q2.2021","Q3.2021","Q4.2021")

est_qs <- c("Q4.2019",
            "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
            "Q1.2021","Q2.2021","Q3.2021","Q4.2021")

# Create proper date for sorting
df <- qdata %>%
  filter(Quarter %in% all_qs) %>%
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
  filter(Quarter %in% est_qs)

cat(sprintf("N total: %d\n", nrow(df)))
cat(sprintf("N with delta_b: %d\n", sum(!is.na(df$delta_b))))
cat(sprintf("N missing: %d\n\n", sum(is.na(df$delta_b))))

# Check USA
cat("=== USA ===\n")
df %>% filter(Country == "USA") %>%
  select(Country, Quarter, date, DebtR_share2019, delta_b) %>%
  print()

# Missing by quarter
cat("\n=== Missing by Quarter ===\n")
df %>% group_by(Quarter) %>%
  summarise(n = n(), n_na = sum(is.na(delta_b)), .groups = "drop") %>%
  print()

# Descriptives
cat("\n=== Descriptives delta_b (Q4.2019 - Q4.2021) ===\n")
x <- df$delta_b[!is.na(df$delta_b)]
cat(sprintf("  N:      %d\n", length(x)))
cat(sprintf("  Mean:   %.4f\n", mean(x)))
cat(sprintf("  SD:     %.4f\n", sd(x)))
cat(sprintf("  Min:    %.4f\n", min(x)))
cat(sprintf("  P25:    %.4f\n", quantile(x, 0.25)))
cat(sprintf("  Median: %.4f\n", median(x)))
cat(sprintf("  P75:    %.4f\n", quantile(x, 0.75)))
cat(sprintf("  Max:    %.4f\n", max(x)))

cat("\n=== Top 5 ===\n")
df %>% filter(!is.na(delta_b)) %>% arrange(desc(delta_b)) %>% head(5) %>%
  select(Country, Quarter, delta_b) %>% print()

cat("\n=== Bottom 5 ===\n")
df %>% filter(!is.na(delta_b)) %>% arrange(delta_b) %>% head(5) %>%
  select(Country, Quarter, delta_b) %>% print()
