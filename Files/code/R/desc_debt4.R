suppressPackageStartupMessages(library(dplyr))
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflicts_prefer(dplyr::lag)

safedata <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/processed"
load(file.path(safedata, "dataforanalysis.RData"))

# Check USA raw data
cat("=== USA: DebtR_share2019 from Q2.2019 to Q2.2020 ===\n")
qdata %>%
  filter(Country == "USA",
         Quarter %in% c("Q2.2019","Q3.2019","Q4.2019","Q1.2020","Q2.2020")) %>%
  select(Country, Quarter, DebtR_share2019) %>%
  print()

# Now compute delta_b properly — include everything from Q3.2019
all_qs <- c("Q3.2019","Q4.2019",
            "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
            "Q1.2021","Q2.2021","Q3.2021","Q4.2021")

est_qs <- c("Q4.2019",
            "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
            "Q1.2021","Q2.2021","Q3.2021","Q4.2021")

df <- qdata %>%
  filter(Quarter %in% all_qs) %>%
  select(Country, Quarter, DebtR_share2019) %>%
  arrange(Country, Quarter)

# Check for USA: is Q4.2019 right before Q1.2020?
cat("\n=== USA rows in df ===\n")
df %>% filter(Country == "USA") %>% print()

# Compute lag
df <- df %>%
  group_by(Country) %>%
  mutate(
    prev_q = lag(Quarter, 1),
    prev_debt = lag(DebtR_share2019, 1),
    delta_b = DebtR_share2019 - prev_debt
  ) %>%
  ungroup()

cat("\n=== USA with lags ===\n")
df %>% filter(Country == "USA") %>% print(width = 150)

# Now filter to estimation sample
est <- df %>% filter(Quarter %in% est_qs)

cat(sprintf("\nEst sample: %d obs, %d with delta_b, %d missing\n",
            nrow(est), sum(!is.na(est$delta_b)), sum(is.na(est$delta_b))))

# Which quarters are missing?
cat("\n=== Missing by Quarter in est ===\n")
est %>%
  group_by(Quarter) %>%
  summarise(n = n(), n_na = sum(is.na(delta_b)), .groups = "drop") %>%
  print()
