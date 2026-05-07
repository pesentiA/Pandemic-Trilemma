suppressPackageStartupMessages(library(dplyr))
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflicts_prefer(dplyr::lag)

safedata <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/processed"
load(file.path(safedata, "dataforanalysis.RData"))

all_qs <- c("Q4.2019",
            "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
            "Q1.2021","Q2.2021","Q3.2021","Q4.2021")

df <- qdata %>%
  filter(Quarter %in% all_qs) %>%
  select(Country, Quarter, DebtR_share2019) %>%
  arrange(Country, Quarter) %>%
  group_by(Country) %>%
  mutate(
    delta_b = DebtR_share2019 - lag(DebtR_share2019, 1)
  ) %>%
  ungroup() %>%
  filter(Quarter != "Q4.2019")  # drop the lag-source quarter

cat("=== delta_b = first difference of DebtR_share2019 ===\n")
cat("   (quarterly change in real debt / 2019 GDP, pp)\n\n")

x <- df$delta_b[!is.na(df$delta_b)]
cat(sprintf("  N:      %d\n", length(x)))
cat(sprintf("  Mean:   %.4f\n", mean(x)))
cat(sprintf("  SD:     %.4f\n", sd(x)))
cat(sprintf("  Min:    %.4f\n", min(x)))
cat(sprintf("  P25:    %.4f\n", quantile(x, 0.25)))
cat(sprintf("  Median: %.4f\n", median(x)))
cat(sprintf("  P75:    %.4f\n", quantile(x, 0.75)))
cat(sprintf("  Max:    %.4f\n", max(x)))

cat("\n=== Top 10 ===\n")
options(width = 120)
print(df %>% filter(!is.na(delta_b)) %>% arrange(desc(delta_b)) %>% head(10))

cat("\n=== Bottom 5 ===\n")
print(df %>% filter(!is.na(delta_b)) %>% arrange(delta_b) %>% head(5))

cat("\n=== By Quarter ===\n")
by_q <- df %>%
  filter(!is.na(delta_b)) %>%
  group_by(Quarter) %>%
  summarise(
    n    = n(),
    mean = round(mean(delta_b), 4),
    sd   = round(sd(delta_b), 4),
    min  = round(min(delta_b), 4),
    max  = round(max(delta_b), 4),
    .groups = "drop"
  )
print(by_q, n = Inf)

cat("\n=== DebtR_share2019 level (for context) ===\n")
lev <- qdata %>%
  filter(Quarter %in% c("Q4.2019","Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                         "Q1.2021","Q2.2021","Q3.2021","Q4.2021")) %>%
  select(Country, Quarter, DebtR_share2019)
for (v in c("DebtR_share2019")) {
  y <- lev[[v]][!is.na(lev[[v]])]
  cat(sprintf("  %-20s N=%d  Mean=%.2f  SD=%.2f  Min=%.2f  Max=%.2f\n",
              v, length(y), mean(y), sd(y), min(y), max(y)))
}
