suppressPackageStartupMessages(library(dplyr))
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")

safedata <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/processed"
load(file.path(safedata, "dataforanalysis.RData"))

pandemic_qs <- c("Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                 "Q1.2021","Q2.2021","Q3.2021","Q4.2021")

df <- qdata %>%
  filter(Quarter %in% pandemic_qs) %>%
  select(Country, Quarter, d_t_pct, d_t_pct_r)

cat("=== Top 10 d_t_pct (NOMINAL debt gap, pp of 2019 GDP) ===\n")
options(width = 120)
print(df %>% arrange(desc(d_t_pct)) %>% head(10))

cat("\n=== Top 10 d_t_pct_r (REAL debt gap, pp of 2019 GDP) ===\n")
print(df %>% arrange(desc(d_t_pct_r)) %>% head(10))

cat("\n=== Comparison: nominal vs real ===\n")
for (v in c("d_t_pct", "d_t_pct_r")) {
  x <- df[[v]][!is.na(df[[v]])]
  cat(sprintf("  %-12s N=%d  Mean=%.2f  SD=%.2f  Min=%.2f  P25=%.2f  P75=%.2f  Max=%.2f\n",
              v, length(x), mean(x), sd(x), min(x),
              quantile(x, 0.25), quantile(x, 0.75), max(x)))
}

cat("\n=== Country with max d_t_pct ===\n")
print(df %>% arrange(desc(d_t_pct)) %>% head(1))

cat("\n=== JPN ===\n")
print(df %>% filter(Country == "JPN") %>% select(Country, Quarter, d_t_pct, d_t_pct_r))

cat("\n=== USA ===\n")
print(df %>% filter(Country == "USA") %>% select(Country, Quarter, d_t_pct, d_t_pct_r))

cat("\n=== GBR ===\n")
print(df %>% filter(Country == "GBR") %>% select(Country, Quarter, d_t_pct, d_t_pct_r))

cat("\n=== Bottom 5 d_t_pct (negative = debt below trend) ===\n")
print(df %>% arrange(d_t_pct) %>% head(5))
