suppressPackageStartupMessages({library(dplyr); library(data.table)})
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")

safedata <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/processed"
load(file.path(safedata, "dataforanalysis.RData"))

# Check what vax columns exist in oxd_d
cat("=== Columns in oxd_d containing 'vac' or 'Vac' ===\n")
vax_cols <- grep("vac|Vac|VAC|PopulationVaccinated", names(oxd_d), value = TRUE, ignore.case = TRUE)
print(vax_cols)

# Check CHE specifically
cat("\n=== CHE vaccination data in oxd_d ===\n")
che_vax <- oxd_d %>%
  filter(Country == "CHE") %>%
  select(Country, Date, any_of(vax_cols))

cat(sprintf("  N rows: %d\n", nrow(che_vax)))
cat(sprintf("  Date range: %s to %s\n", min(che_vax$Date), max(che_vax$Date)))

# Show non-zero/non-NA values
for (vc in vax_cols) {
  x <- che_vax[[vc]]
  cat(sprintf("  %s: %d non-NA, %d non-zero, max=%.4f\n",
              vc, sum(!is.na(x)), sum(x > 0, na.rm = TRUE), max(x, na.rm = TRUE)))
}

# Show last few rows for CHE
cat("\n=== CHE last 10 rows ===\n")
che_vax %>% tail(10) %>% print()

# Compare: what does qdata have for CHE?
cat("\n=== CHE in qdata (vax columns) ===\n")
qdata_vax_cols <- grep("vac|Vac|VAC|Population", names(qdata), value = TRUE, ignore.case = TRUE)
print(qdata_vax_cols)

qdata %>%
  filter(Country == "CHE") %>%
  select(Country, Quarter, any_of(c("vax_rate", qdata_vax_cols))) %>%
  print(n = 20)

# Also check oxd_spatial_d for CHE
cat("\n=== Columns in oxd_spatial_d containing 'vac' ===\n")
vax_cols_sp <- grep("vac|Vac|VAC", names(oxd_spatial_d), value = TRUE, ignore.case = TRUE)
print(vax_cols_sp)

if (length(vax_cols_sp) > 0) {
  cat("\n=== CHE in oxd_spatial_d ===\n")
  oxd_spatial_d %>%
    filter(CountryCode == "CHE") %>%
    select(CountryCode, Date, any_of(vax_cols_sp)) %>%
    filter(!is.na(.[[vax_cols_sp[1]]])) %>%
    tail(10) %>%
    print()
}
