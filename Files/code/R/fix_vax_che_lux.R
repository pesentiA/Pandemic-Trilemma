suppressPackageStartupMessages({library(dplyr); library(data.table)})
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")

safedata <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/processed"
load(file.path(safedata, "dataforanalysis.RData"))

# Get population from qdata (Q1.2020, in thousands)
pop_che <- qdata %>% filter(Country == "CHE", Quarter == "Q1.2020") %>% pull(Qpopulation_th) * 1000
pop_lux <- qdata %>% filter(Country == "LUX", Quarter == "Q1.2020") %>% pull(Qpopulation_th) * 1000
cat(sprintf("CHE pop: %.0f\nLUX pop: %.0f\n\n", pop_che, pop_lux))

# OWID total vaccinations (people_vaccinated = at least 1 dose)
# Source: https://github.com/owid/covid-19-data/blob/master/public/data/vaccinations/country_data/
# Use people_vaccinated (at least 1 dose) as the metric, consistent with vax_rate in qdata

# Switzerland: people_vaccinated from OWID
che_vax <- data.frame(
  Quarter = c("Q1.2020","Q2.2020","Q3.2020","Q4.2020",
              "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
              "Q1.2022","Q2.2022"),
  people_vax = c(0, 0, 0, 6280,
                 971531, 4375747, 5554313, 5966497,
                 6078521, 6087495)
) %>%
  mutate(vax_rate_new = people_vax / pop_che)

# Luxembourg: people_vaccinated from OWID
lux_vax <- data.frame(
  Quarter = c("Q1.2020","Q2.2020","Q3.2020","Q4.2020",
              "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
              "Q1.2022","Q2.2022"),
  people_vax = c(0, 0, 0, 1076,
                 73151, 341061, 418605, 464399,
                 480329, 481312)
) %>%
  mutate(vax_rate_new = people_vax / pop_lux)

cat("=== CHE vaccination rates (people_vaccinated / pop) ===\n")
print(che_vax)
cat("\n=== LUX vaccination rates ===\n")
print(lux_vax)

# Current values in qdata
cat("\n=== Current qdata vax_rate for CHE ===\n")
qdata %>% filter(Country == "CHE") %>%
  select(Country, Quarter, vax_rate) %>%
  filter(Quarter %in% che_vax$Quarter) %>%
  print()

cat("\n=== Current qdata vax_rate for LUX ===\n")
qdata %>% filter(Country == "LUX") %>%
  select(Country, Quarter, vax_rate) %>%
  filter(Quarter %in% lux_vax$Quarter) %>%
  print()

# Patch: update vax_rate in qdata for CHE and LUX
for (i in 1:nrow(che_vax)) {
  idx <- which(qdata$Country == "CHE" & qdata$Quarter == che_vax$Quarter[i])
  if (length(idx) == 1) qdata$vax_rate[idx] <- che_vax$vax_rate_new[i]
}
for (i in 1:nrow(lux_vax)) {
  idx <- which(qdata$Country == "LUX" & qdata$Quarter == lux_vax$Quarter[i])
  if (length(idx) == 1) qdata$vax_rate[idx] <- lux_vax$vax_rate_new[i]
}

# Verify
cat("\n=== PATCHED qdata vax_rate for CHE ===\n")
qdata %>% filter(Country == "CHE") %>%
  select(Country, Quarter, vax_rate) %>%
  filter(Quarter %in% che_vax$Quarter) %>%
  print()

cat("\n=== PATCHED qdata vax_rate for LUX ===\n")
qdata %>% filter(Country == "LUX") %>%
  select(Country, Quarter, vax_rate) %>%
  filter(Quarter %in% lux_vax$Quarter) %>%
  print()

# Save updated RData
save(list = ls(envir = .GlobalEnv)[ls(envir = .GlobalEnv) != "safedata"],
     file = file.path(safedata, "dataforanalysis.RData"))
cat(sprintf("\nSaved updated dataforanalysis.RData to %s\n", safedata))
