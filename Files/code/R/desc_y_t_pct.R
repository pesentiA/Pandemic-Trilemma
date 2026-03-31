suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")

safedata <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/processed"
load(file.path(safedata, "dataforanalysis.RData"))

pandemic_qs <- c(
  "Q1.2019","Q2.2019","Q3.2019","Q4.2019",
  "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
  "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
  "Q1.2022","Q2.2022","Q3.2022","Q4.2022")

df <- qdata %>%
  filter(Quarter %in% pandemic_qs) %>%
  select(Country, Quarter, y_t_pct)

cat("=== y_t_pct: Output Gap (HP-filtered, % of potential) ===\n\n")

cat("--- Overall ---\n")
y <- df$y_t_pct[!is.na(df$y_t_pct)]
cat(sprintf("  N obs:     %d\n", length(y)))
cat(sprintf("  N missing: %d\n", sum(is.na(df$y_t_pct))))
cat(sprintf("  Countries: %d\n", n_distinct(df$Country[!is.na(df$y_t_pct)])))
cat(sprintf("  Mean:      %.4f\n", mean(y)))
cat(sprintf("  Median:    %.4f\n", median(y)))
cat(sprintf("  SD:        %.4f\n", sd(y)))
cat(sprintf("  Min:       %.4f\n", min(y)))
cat(sprintf("  Max:       %.4f\n", max(y)))
cat(sprintf("  P5:        %.4f\n", quantile(y, 0.05)))
cat(sprintf("  P25:       %.4f\n", quantile(y, 0.25)))
cat(sprintf("  P75:       %.4f\n", quantile(y, 0.75)))
cat(sprintf("  P95:       %.4f\n", quantile(y, 0.95)))

cat("\n--- By Quarter ---\n")
by_q <- df %>%
  filter(!is.na(y_t_pct)) %>%
  group_by(Quarter) %>%
  summarise(
    n      = n(),
    mean   = round(mean(y_t_pct), 4),
    sd     = round(sd(y_t_pct), 4),
    min    = round(min(y_t_pct), 4),
    p25    = round(quantile(y_t_pct, 0.25), 4),
    median = round(median(y_t_pct), 4),
    p75    = round(quantile(y_t_pct, 0.75), 4),
    max    = round(max(y_t_pct), 4),
    .groups = "drop"
  ) %>%
  arrange(Quarter)

options(width = 200)
print(by_q, n = Inf)

cat("\n--- 5 Worst Country-Quarters ---\n")
print(df %>% filter(!is.na(y_t_pct)) %>% arrange(y_t_pct) %>% head(5))

cat("\n--- 5 Best Country-Quarters ---\n")
print(df %>% filter(!is.na(y_t_pct)) %>% arrange(desc(y_t_pct)) %>% head(5))
