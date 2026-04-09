suppressPackageStartupMessages({library(dplyr); library(data.table)})
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")

safedata <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/processed"
load(file.path(safedata, "dataforanalysis.RData"))

est_qs <- c("Q1.2020","Q2.2020","Q3.2020","Q4.2020",
            "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
            "Q1.2022","Q2.2022")

vax <- qdata %>%
  filter(Quarter %in% est_qs) %>%
  select(Country, Quarter, vax_rate)

# Countries with 0 or NA vax in Q2.2022
cat("=== Vax rate in Q2.2022 ===\n")
vax %>% filter(Quarter == "Q2.2022") %>%
  arrange(vax_rate) %>%
  head(10) %>%
  print()

# MEX specifically
cat("\n=== MEX vax_rate across all quarters ===\n")
vax %>% filter(Country == "MEX") %>% print()

# Countries with vax_rate == 0 or NA
cat("\n=== Countries with any vax_rate = 0 or NA ===\n")
vax %>%
  group_by(Country) %>%
  summarise(n_zero = sum(vax_rate == 0, na.rm=TRUE),
            n_na = sum(is.na(vax_rate)),
            last_vax = dplyr::last(vax_rate),
            .groups = "drop") %>%
  filter(n_zero > 0 | n_na > 0) %>%
  arrange(desc(n_zero)) %>%
  print(n = 40)
