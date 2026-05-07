suppressPackageStartupMessages({library(dplyr); library(data.table)})
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")

safedata <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/processed"
load(file.path(safedata, "dataforanalysis.RData"))

est_qs <- c("Q1.2020","Q2.2020","Q3.2020","Q4.2020",
            "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
            "Q1.2022","Q2.2022")

est <- qdata %>%
  filter(Quarter %in% est_qs) %>%
  select(Country, Quarter, vax_rate) %>%
  mutate(vax_pct = vax_rate * 100)

cat("=== Panel A: Quarterly observations (N=380) ===\n\n")
x <- est$vax_pct
cat(sprintf("  N:      %d\n", length(x)))
cat(sprintf("  Mean:   %.2f\n", mean(x, na.rm=TRUE)))
cat(sprintf("  Median: %.2f\n", median(x, na.rm=TRUE)))
cat(sprintf("  SD:     %.2f\n", sd(x, na.rm=TRUE)))
cat(sprintf("  Min:    %.2f\n", min(x, na.rm=TRUE)))
cat(sprintf("  P25:    %.2f\n", quantile(x, 0.25, na.rm=TRUE)))
cat(sprintf("  P75:    %.2f\n", quantile(x, 0.75, na.rm=TRUE)))
cat(sprintf("  Max:    %.2f\n", max(x, na.rm=TRUE)))

cat("\n=== Panel B: End-of-sample vax rate per country (N=38) ===\n\n")
end_vax <- est %>%
  filter(Quarter == "Q2.2022") %>%
  select(Country, vax_pct) %>%
  arrange(vax_pct)

x2 <- end_vax$vax_pct
cat(sprintf("  N:      %d\n", length(x2)))
cat(sprintf("  Mean:   %.2f\n", mean(x2)))
cat(sprintf("  Median: %.2f\n", median(x2)))
cat(sprintf("  SD:     %.2f\n", sd(x2)))
cat(sprintf("  Min:    %.2f\n", min(x2)))
cat(sprintf("  P25:    %.2f\n", quantile(x2, 0.25)))
cat(sprintf("  P75:    %.2f\n", quantile(x2, 0.75)))
cat(sprintf("  Max:    %.2f\n", max(x2)))

cat("\n=== All countries end-of-sample vax rate (Q2.2022) ===\n")
print(end_vax, n = 40)
