suppressPackageStartupMessages(library(dplyr))
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflicts_prefer(dplyr::lag)

safedata <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/processed"
load(file.path(safedata, "dataforanalysis.RData"))

# Include Q3.2019 as lag source for Q4.2019
all_qs <- c("Q3.2019","Q4.2019",
            "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
            "Q1.2021","Q2.2021","Q3.2021","Q4.2021")

est_qs <- c("Q4.2019",
            "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
            "Q1.2021","Q2.2021","Q3.2021","Q4.2021")

df <- qdata %>%
  filter(Quarter %in% all_qs) %>%
  select(Country, Quarter, DebtR_share2019) %>%
  arrange(Country, Quarter) %>%
  group_by(Country) %>%
  mutate(delta_b = DebtR_share2019 - lag(DebtR_share2019, 1)) %>%
  ungroup() %>%
  filter(Quarter %in% est_qs)

cat(sprintf("N total: %d\n", nrow(df)))
cat(sprintf("N with delta_b: %d\n", sum(!is.na(df$delta_b))))
cat(sprintf("N missing delta_b: %d\n", sum(is.na(df$delta_b))))

cat("\n=== Missing delta_b by Quarter ===\n")
df %>%
  group_by(Quarter) %>%
  summarise(n = n(), n_na = sum(is.na(delta_b)), .groups = "drop") %>%
  print()

cat("\n=== Countries missing Q3.2019 DebtR_share2019 ===\n")
q3 <- qdata %>% filter(Quarter == "Q3.2019") %>% select(Country, DebtR_share2019)
cat(sprintf("Countries with Q3.2019 data: %d\n", sum(!is.na(q3$DebtR_share2019))))
missing <- q3 %>% filter(is.na(DebtR_share2019))
if (nrow(missing) > 0) print(missing)

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
