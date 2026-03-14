####################################################################
#   PHASE 1: LOADING DATA PACKAGES AND CLEANING
####################################################################

##Beschreibung hinzufügen
##Install Packages and load Data ----
#.rs.restartR()

rm(list=ls())

packages_vector <- c( "did2s","haven", "dplyr",  "sandwich",  "jtools", "data.table",
                      "fBasics","gtools","rnaturalearth", "rnaturalearthdata", "foreign","gt", "Synth","gridExtra", "fixest","huxtable", 
                      "xtable", "foreign", "stargazer", "AER", "causalweight", "tidyr","expss","stringr","pscore","AER","ggplot2","haven","lubridate" ,"knitr",
                      "kableExtra", "psych", "pastecs","purrr","magrittr","did","remote", "did2s", 
                      "patchwork", "readxl", "did2s", "plm", "scales", "mFilter", "countrycode", "tidyverse", "corrplot")


#install.packages(packages_vector)
lapply(packages_vector, require, character.only = TRUE) 


# List loaded packages 
(.packages())

# Set options
options(max.print = 9999, scipen = 999, na.print = "")

dir <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working"
setwd(dir)

set.seed(1234)

# Set preferences:
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflicts_prefer(dplyr::lag)
conflicted::conflicts_prefer(dplyr::first)
conflicted::conflicts_prefer(dplyr::coalesce)
conflicted::conflicts_prefer(dplyr::recode)
conflicted::conflicts_prefer(lubridate::intersect)
conflicted::conflicts_prefer(lubridate::quarter)
conflicted::conflicts_prefer(lubridate::year)
conflicted::conflicts_prefer(lubridate::wday)
conflicted::conflicts_prefer(dplyr::last)
conflicted::conflicts_prefer(data.table::isoweek)
conflicted::conflicts_prefer(data.table::month)

qdata<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/qdata.csv", header=TRUE, sep=",")

# Define units of interest
oecd_countries <- c("AUS", "AUT", "BEL", "CAN", "CHL","CRI", "COL", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN", "KOR", "LVA", "LTU", "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA")

# Create time index
extract_time_index <- function(q) {
  quarter <- as.integer(sub("Q([1-4])\\..*", "\\1", q))
  year    <- as.integer(sub(".*\\.(\\d{4})", "\\1", q))
  return((year - 2015) * 4 + quarter)
}

qdata <- qdata %>%
  mutate(TimeIndex = extract_time_index(Quarter))

#-------------------------------------------------------------------------------
## Cleaning out GFCF mith Modifiied GCFC for Irleand

mdd <- read_excel("1001.xlsx") %>%
  select(Country, Quarter, mdd_index)

mdd_irl <- subset(mdd, Country == "IRL")


ix <- qdata$Country == "IRL"
qdata$Gross.fc[ix] <- as.numeric(mdd_irl$mdd_index)[
  match(qdata$Quarter[ix], mdd_irl$Quarter)
]

sum(qdata$Country == "IRL" & !is.na(qdata$Gross.fc))


# Merge with Main Dataset
look <- setNames(as.numeric(mdd_irl$mdd_index), mdd_irl$Quarter)

ix <- qdata$Country == "IRL"
qdata$Gross.fc[ix] <- look[ qdata$Quarter[ix] ]   # only IRL


irl_fc <- qdata %>%
  filter(Country == "IRL") %>%
  select(Quarter, Gross.fc)


# 1) Quarter vereinheitlichen (Qn.YYYY) & Typen sauber setzen
norm_quarter <- function(x){
  x <- gsub("\\s+", "", as.character(x))
  sub("^(\\d{4})-?Q([1-4])$", "Q\\2.\\1", x)  # 2015Q1/2015-Q1 -> Q1.2015
}

qdata <- qdata %>%
  mutate(
    Country = toupper(trimws(Country)),
    Quarter = norm_quarter(Quarter)
  )

mdd <- mdd %>%
  mutate(
    Country   = toupper(trimws(Country)),
    Quarter   = norm_quarter(Quarter),
    mdd_index = suppressWarnings(as.numeric(mdd_index))
  )

# 2) Diagnose: deckt mdd alle IRL-Quartale ab?
setdiff(qdata$Quarter[qdata$Country=="IRL"], mdd$Quarter[mdd$Country=="IRL"])
# -> sollte ideally character(0) liefern; sonst siehst du fehlende Quartale.

# 3) Ersetzen NUR fuer IRL (Join nur auf Quarter reicht, wenn Country==IRL gefiltert)
qdata <- qdata %>%
  left_join(
    mdd %>% filter(Country=="IRL") %>% select(Quarter, mdd_index),
    by = "Quarter"
  ) %>%
  mutate(
    Gross.fc = if_else(Country=="IRL" & !is.na(mdd_index), mdd_index, Gross.fc)
  ) %>%
  select(-mdd_index)

# 4) Kontrolle
qdata %>% filter(Country=="IRL") %>% select(Quarter, Gross.fc) %>% arrange(Quarter)

#-------------------------------------------------------------------------------
## TEST GDP WERTE MIT MFDD FÜR IRL ERSETZEN

mdd <- read_excel("irl.xlsx") %>%
  transmute(
    Country = toupper(trimws(Country)),
    Quarter = str_replace_all(as.character(Quarter), "\\s+", "") %>%
      str_replace("^(\\d{4})-?Q([1-4])$", "Q\\2\\.\\1"),
    mfdd_index = as.numeric(`Modified Final Domestic Demand (Index, Q1.2020=100)`)
  )


qdata <- qdata %>%
  left_join(mdd %>% filter(Country == "IRL") %>% select(Country, Quarter, mfdd_index),
            by = c("Country","Quarter")) %>%
  mutate(
    GDP = if_else(Country == "IRL" & !is.na(mfdd_index),
                  as.numeric(mfdd_index),
                  suppressWarnings(as.numeric(GDP)))
  ) %>%
  select(-mfdd_index)


#-----------------------------------------------------------------------------
## Adding Population


# --- 1) Population laden und ln-Aenderungen bauen ---
pop <- read_excel("population3.xlsx")

pop <- pop %>%
  dplyr::filter(!(Country == "COL" & Quarter == "Q4.2024")) %>%
  tibble::add_row(Country = "COL", Quarter = "Q4.2024", Qpopulation_th = 52696.0)


kor_add <- tribble(
  ~Country, ~Quarter,  ~Qpopulation_th,
  "KOR",    "Q2.2024", 51271.5,
  "KOR",    "Q3.2024", 51248.2,
  "KOR",    "Q4.2024", 51217.2
) %>%
  mutate(
    year = as.integer(str_sub(Quarter, -4L)),
    q    = as.integer(str_match(Quarter, "^Q([1-4])\\.")[,2])
  )


pop <- pop %>%
  filter(!(Country == "KOR" & Quarter %in% kor_add$Quarter)) %>%
  bind_rows(kor_add) %>%
  arrange(Country, year, q) %>%
  select(-year, -q)


# --- 2) Mit deinen GDP/Komponenten mergen ---
qdata <- qdata %>%
  left_join(pop, by = c("Country","Quarter"))


#-------------------------------------------------------------------------------
##load new real gdp growth data
gdp2<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/404.csv", header=TRUE, sep=",")

gdp2 <- gdp2 %>%
  mutate(
    Quarter = as.character(Quarter),
    Quarter = str_trim(Quarter),
    Quarter = str_replace(Quarter, "^(\\d{4})-Q([1-4])$", "Q\\2.\\1")
  )

qdata <- left_join(qdata, gdp2, by = c("Country", "Quarter"))


qdata <- qdata %>%
  dplyr::mutate(
    !!sym("QReal.GDP.Growth")     := .data[["Gross.domestic.product"]],
    !!sym("QX")                   := .data[["Exports.of.goods.and.services"]],
    !!sym("QM")                   := .data[["Imports.of.goods.and.services"]],
    !!sym("QPrivate.Consumption") := .data[["Final.consumption.expenditure"]],
    !!sym("QGov.Cons")            := .data[["Final.Government.consumption.expenditure"]],
    !!sym("QGross.Cap.form")      := .data[["Gross.fixed.capital.formation"]]
  )


#---------------------------------------------------------------------------------
## Loading oxford tracker data
#old version
#oxd<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/qox2.csv", header=TRUE, sep=",")
#not the raw data
#oxd<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/OxCGRT_nat_latest.csv", header=TRUE, sep=",")
#raw data
oxd0<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/OxCGRT_nat_differentiated_withnotes_2020.csv", header=TRUE, sep=",")
oxd1<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/OxCGRT_nat_differentiated_withnotes_2021.csv", header=TRUE, sep=",")
oxd2<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/OxCGRT_nat_differentiated_withnotes_2022.csv", header=TRUE, sep=",")

oxd <- dplyr::bind_rows(oxd0, oxd1, oxd2)

#Create dataset with more than the oecd countries for spatial tests
oxd_spatial<-oxd

#alles ausser OECD entfernen
oxd <- oxd |> 
  dplyr::filter(CountryCode %in% oecd_countries)


#Spalten entfernen die nur NA haben-> vlt zuerst zusammenführen
oxd <- oxd |>
  dplyr::select(where(~ !all(is.na(.x))))

oxd <- oxd |> 
  dplyr::select(-Jurisdiction, -CountryName)


#move the indexes in front
oxd <- oxd |>
  dplyr::relocate(
    # Stringency
    StringencyIndex_NonVaccinated,
    StringencyIndex_NonVaccinated_ForDisplay,
    StringencyIndex_Vaccinated,
    StringencyIndex_Vaccinated_ForDisplay,
    StringencyIndex_SimpleAverage,
    StringencyIndex_SimpleAverage_ForDisplay,
    StringencyIndex_WeightedAverage,
    StringencyIndex_WeightedAverage_ForDisplay,
    # Government response
    GovernmentResponseIndex_NonVaccinated,
    GovernmentResponseIndex_NonVaccinated_ForDisplay,
    GovernmentResponseIndex_Vaccinated,
    GovernmentResponseIndex_Vaccinated_ForDisplay,
    GovernmentResponseIndex_SimpleAverage,
    GovernmentResponseIndex_SimpleAverage_ForDisplay,
    GovernmentResponseIndex_WeightedAverage,
    GovernmentResponseIndex_WeightedAverage_ForDisplay,
    # Containment & health
    ContainmentHealthIndex_NonVaccinated,
    ContainmentHealthIndex_NonVaccinated_ForDisplay,
    ContainmentHealthIndex_Vaccinated,
    ContainmentHealthIndex_Vaccinated_ForDisplay,
    ContainmentHealthIndex_SimpleAverage,
    ContainmentHealthIndex_SimpleAverage_ForDisplay,
    ContainmentHealthIndex_WeightedAverage,
    ContainmentHealthIndex_WeightedAverage_ForDisplay,
    # Panemic stats
    ConfirmedCases,
    ConfirmedDeaths,
    PopulationVaccinated,
    .after = Date
  )


oxd <- oxd |>
  mutate(
    Date = as.Date(as.character(Date), format = "%Y%m%d")
  )


#overall descriptives
colnames(oxd)

vars <- c(
  "StringencyIndex_NonVaccinated_ForDisplay",
  "StringencyIndex_Vaccinated_ForDisplay",
  "StringencyIndex_SimpleAverage_ForDisplay",
  "StringencyIndex_WeightedAverage_ForDisplay",
  "GovernmentResponseIndex_NonVaccinated_ForDisplay",
  "GovernmentResponseIndex_Vaccinated_ForDisplay",
  "GovernmentResponseIndex_SimpleAverage_ForDisplay",
  "GovernmentResponseIndex_WeightedAverage_ForDisplay",
  "ContainmentHealthIndex_NonVaccinated_ForDisplay",
  "ContainmentHealthIndex_Vaccinated_ForDisplay",
  "ContainmentHealthIndex_SimpleAverage_ForDisplay",
  "ContainmentHealthIndex_WeightedAverage_ForDisplay"
)


oxd |> 
  dplyr::select(all_of(vars)) |>
  summary()



##plot over time
## 1) Verlauf für ein Land (Beispiel: CHE) – tägliche Werte
oxd_long_che <- oxd |>
  filter(CountryCode == "CHE") |>
  select(CountryCode, Date, all_of(vars)) |>
  pivot_longer(
    cols = all_of(vars),
    names_to = "Index",
    values_to = "Value"
  )

ggplot(oxd_long_che, aes(x = Date, y = Value, colour = Index)) +
  geom_line(linewidth = 0.4) +
  labs(
    x = "Date",
    y = "Index value (0–100)",
    colour = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal"
  )

## 2) Optional: täglicher Durchschnitt über alle Länder
oxd_long_mean <- oxd |>
  select(Date, all_of(vars)) |>
  group_by(Date) |>
  summarise(across(all_of(vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop") |>
  pivot_longer(
    cols = all_of(vars),
    names_to = "Index",
    values_to = "Value"
  )

ggplot(oxd_long_mean, aes(x = Date, y = Value, colour = Index)) +
  geom_line(linewidth = 0.4) +
  labs(
    x = "Date",
    y = "Average index value (0–100)",
    colour = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal"
  )


##only the weighted average indices

# 2) Indizes definieren
vars <- c(
  "StringencyIndex_WeightedAverage_ForDisplay",
  "GovernmentResponseIndex_WeightedAverage_ForDisplay",
  "ContainmentHealthIndex_WeightedAverage_ForDisplay"
)

# 3) Durchschnitt über alle Länder, 01.01.2020–31.12.2021
oxd_long_mean_2020_21 <- oxd |>
  filter(Date >= as.Date("2020-01-01"),
         Date <= as.Date("2021-12-31")) |>
  select(Date, all_of(vars)) |>
  group_by(Date) |>
  summarise(
    across(all_of(vars), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = all_of(vars),
    names_to = "Index",
    values_to = "Value"
  )

# 4) Plot
ggplot(oxd_long_mean_2020_21,
       aes(x = Date, y = Value, colour = Index)) +
  geom_line(linewidth = 0.4) +
  labs(
    x = "Date",
    y = "Average index value (0–100)",
    colour = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal"
  )



p <- oxd |>
  filter(Date >= as.Date("2020-01-01"),
         Date <= as.Date("2021-12-31")) |>
  select(CountryCode, Date, ContainmentHealthIndex_WeightedAverage_ForDisplay) |>
  ggplot(aes(Date,
             ContainmentHealthIndex_WeightedAverage_ForDisplay,
             colour = CountryCode)) +
  geom_line(linewidth = 0.3) +
  labs(
    x = "Date",
    y = "ContainmentHealthIndex (weighted, display)",
    colour = "Country"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box      = "horizontal"
  )

p



# Summary Statistics pro Land für 2020–2021 für Containment and Health Index
summ_country <- oxd |>
  filter(Date >= as.Date("2020-01-01"),
         Date <= as.Date("2021-12-31")) |>
  group_by(CountryCode) |>
  summarise(
    n_obs   = sum(!is.na(ContainmentHealthIndex_WeightedAverage_ForDisplay)),
    mean    = mean(ContainmentHealthIndex_WeightedAverage_ForDisplay, na.rm = TRUE),
    sd      = sd(ContainmentHealthIndex_WeightedAverage_ForDisplay,   na.rm = TRUE),
    min     = min(ContainmentHealthIndex_WeightedAverage_ForDisplay,  na.rm = TRUE),
    q25     = quantile(ContainmentHealthIndex_WeightedAverage_ForDisplay, 0.25, na.rm = TRUE),
    median  = median(ContainmentHealthIndex_WeightedAverage_ForDisplay,   na.rm = TRUE),
    q75     = quantile(ContainmentHealthIndex_WeightedAverage_ForDisplay, 0.75, na.rm = TRUE),
    max     = max(ContainmentHealthIndex_WeightedAverage_ForDisplay,  na.rm = TRUE)
  )

print(summ_country,n=38)


# Summary Statistics pro Land für den StringencyIndex_WeightedAverage_ForDisplay, 2020–2021
summ_stringency_country <- oxd |>
  filter(Date >= as.Date("2020-01-01"),
         Date <= as.Date("2021-12-31")) |>
  group_by(CountryCode) |>
  summarise(
    n_obs  = sum(!is.na(StringencyIndex_WeightedAverage_ForDisplay)),
    mean   = mean(StringencyIndex_WeightedAverage_ForDisplay, na.rm = TRUE),
    sd     = sd(StringencyIndex_WeightedAverage_ForDisplay,   na.rm = TRUE),
    min    = min(StringencyIndex_WeightedAverage_ForDisplay,  na.rm = TRUE),
    q25    = quantile(StringencyIndex_WeightedAverage_ForDisplay, 0.25, na.rm = TRUE),
    median = median(StringencyIndex_WeightedAverage_ForDisplay,   na.rm = TRUE),
    q75    = quantile(StringencyIndex_WeightedAverage_ForDisplay, 0.75, na.rm = TRUE),
    max    = max(StringencyIndex_WeightedAverage_ForDisplay,  na.rm = TRUE)
  )

print(summ_stringency_country, n=38)


cor(oxd$StringencyIndex_WeightedAverage_ForDisplay, oxd$ContainmentHealthIndex_WeightedAverage_ForDisplay)


cor(
  oxd$StringencyIndex_WeightedAverage_ForDisplay,
  oxd$ContainmentHealthIndex_WeightedAverage_ForDisplay,
  use   = "complete.obs",
  method = "spearman"
)



oxd <- oxd |>
  dplyr::rename(
    Country = CountryCode
  )


# Basic structure
cat("=== BASIC DATA STRUCTURE ===\n")
cat("Dimensions:", nrow(oxd), "rows ×", ncol(oxd), "columns\n")
cat("Date range:", min(oxd$Date, na.rm=TRUE), "to", max(oxd$Date, na.rm=TRUE), "\n")
cat("Number of countries:", n_distinct(oxd$Country), "\n\n")

# Check country codes
cat("=== COUNTRIES IN DATASET ===\n")
countries <- oxd %>%
  distinct(Country) %>%
  arrange(Country)
print(countries)

# Verify 38 OECD countries
oecd_38 <- c("AUS", "AUT", "BEL", "CAN", "CHL", "COL", "CRI", "CZE", "DNK", 
             "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", 
             "ITA", "JPN", "KOR", "LVA", "LTU", "LUX", "MEX", "NLD", "NZL", 
             "NOR", "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", 
             "GBR", "USA")

missing_countries <- setdiff(oecd_38, oxd$Country)
extra_countries <- setdiff(oxd$Country, oecd_38)

cat("\nMissing OECD countries:", 
    ifelse(length(missing_countries) > 0, paste(missing_countries, collapse=", "), "None"), "\n")
cat("Extra countries:", 
    ifelse(length(extra_countries) > 0, paste(extra_countries, collapse=", "), "None"), "\n\n")

# ==============================================================================
# 2. IDENTIFY AVAILABLE INDICES
# ==============================================================================

cat("=== AVAILABLE CONTAINMENT & HEALTH INDICES ===\n")

# Check for all possible variants
index_variants <- c(
  # Without vaccine differentiation (M-version)
  "ContainmentHealthIndex_WeightedAverage",
  "ContainmentHealthIndex_SimpleAverage",
  "ContainmentHealthIndex_WeightedAverage_ForDisplay",
  "ContainmentHealthIndex_SimpleAverage_ForDisplay",
  
  # With vaccine differentiation
  "ContainmentHealthIndex_NonVaccinated_WeightedAverage",
  "ContainmentHealthIndex_Vaccinated_WeightedAverage",
  "ContainmentHealthIndex_NonVaccinated_SimpleAverage",
  "ContainmentHealthIndex_Vaccinated_SimpleAverage",
  "ContainmentHealthIndex_NonVaccinated_WeightedAverage_ForDisplay",
  "ContainmentHealthIndex_Vaccinated_WeightedAverage_ForDisplay",
  
  # Stringency alternatives
  "StringencyIndex_WeightedAverage",
  "StringencyIndex_SimpleAverage",
  "StringencyIndex_WeightedAverage_ForDisplay"
)

available_indices <- index_variants[index_variants %in% names(oxd)]
cat("Found", length(available_indices), "index variants:\n")
print(available_indices)

# Identify primary variable
if ("ContainmentHealthIndex_WeightedAverage" %in% names(oxd)) {
  primary_var <- "ContainmentHealthIndex_WeightedAverage"
  cat("\n✅ PRIMARY VARIABLE IDENTIFIED:", primary_var, "\n")
} else if ("ContainmentHealthIndex_NonVaccinated_WeightedAverage" %in% names(oxd)) {
  primary_var <- "ContainmentHealthIndex_NonVaccinated_WeightedAverage"
  cat("\n⚠️ Only differentiated version available, using:", primary_var, "\n")
} else {
  stop("ERROR: No suitable Containment & Health Index found!")
}

# ==============================================================================
# 3. MISSING VALUES ANALYSIS
# ==============================================================================

cat("\n=== MISSING VALUES ANALYSIS ===\n")
##NO MISSING VALUES

# COMPARE WEIGHTED VS SIMPLE AVERAGE VALUES
# ==============================================================================

cat("\n=== COMPARISON: WEIGHTED VS SIMPLE AVERAGE ===\n")

if (all(c("ContainmentHealthIndex_WeightedAverage", 
          "ContainmentHealthIndex_SimpleAverage") %in% names(oxd))) {
  
  # Calculate gap
  oxd <- oxd %>%
    mutate(
      Gap_Weighted_Simple = ContainmentHealthIndex_SimpleAverage - 
        ContainmentHealthIndex_WeightedAverage
    )
  
  # Gap statistics
  gap_stats <- oxd %>%
    summarize(
      Mean_Gap = mean(Gap_Weighted_Simple, na.rm = TRUE),
      SD_Gap = sd(Gap_Weighted_Simple, na.rm = TRUE),
      Max_Gap = max(Gap_Weighted_Simple, na.rm = TRUE),
      Correlation = cor(ContainmentHealthIndex_WeightedAverage,
                        ContainmentHealthIndex_SimpleAverage,
                        use = "complete.obs")
    )
  
  cat("\n📊 Weighted vs Simple Average:\n")
  print(gap_stats)
  
  # Gap by country
  gap_by_country <- oxd %>%
    group_by(Country) %>%
    summarize(
      Mean_Gap = mean(Gap_Weighted_Simple, na.rm = TRUE),
      Max_Gap = max(Gap_Weighted_Simple, na.rm = TRUE),
      Median_Gap = median(Gap_Weighted_Simple, na.rm = TRUE)
    ) %>%
    arrange(desc(Mean_Gap))
  
  cat("\n📊 Countries with Largest Weighted-Simple Gaps:\n")
  cat("(Large gaps indicate frequent targeted/regional policies)\n\n")
  print(kable(head(gap_by_country, 10), digits = 2))
  
  # Plot comparison for selected countries
  selected_countries <- c("USA", "DEU", "ITA", "SWE", "FRA", "GBR")
  
  p_comparison <- oxd %>%
    filter(Country %in% selected_countries) %>%
    select(Country, Date, 
           Weighted = ContainmentHealthIndex_WeightedAverage,
           Simple = ContainmentHealthIndex_SimpleAverage) %>%
    pivot_longer(cols = c(Weighted, Simple),
                 names_to = "Version",
                 values_to = "Value") %>%
    ggplot(aes(x = Date, y = Value, color = Version)) +
    geom_line(linewidth = 0.8) +
    facet_wrap(~Country, ncol = 2) +
    theme_minimal() +
    labs(
      title = "Weighted vs Simple Average by Country",
      subtitle = "Gap shows when policies were regionally targeted",
      x = "Date",
      y = "Containment & Health Index"
    ) +
    theme(legend.position = "bottom")
  
  print(p_comparison)
  
} else {
  cat("⚠️ Both Weighted and Simple versions not available for comparison\n")
}



# ALLE Länder in einem großen facet_wrap (wird mehrere Seiten scrollen)
p_all <- oxd %>%
  select(Country, Date, 
         Weighted = ContainmentHealthIndex_WeightedAverage,
         Simple = ContainmentHealthIndex_SimpleAverage) %>%
  pivot_longer(cols = c(Weighted, Simple),
               names_to = "Version",
               values_to = "Value") %>%
  ggplot(aes(x = Date, y = Value, color = Version)) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~Country, ncol = 4, scales = "free_y") +  # scales="free_y" optional
  theme_minimal() +
  labs(
    title = "Weighted vs Simple Average - All OECD Countries",
    subtitle = "Gap shows when policies were regionally targeted",
    x = "Date",
    y = "Containment & Health Index"
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 8),
    axis.text = element_text(size = 6)
  )

print(p_all)

##ERST AB Q2.2021 HAT ES EINE VARIATION IN DNE ZWEI MESSUNGEN->VARIATION AUCH SEHR KLEIN
##Problem Index unterscheidet nicht nach Target Vax and non Vax

##For vax and non-vax

p_all <- oxd %>%
  select(Country, Date, 
         Weighted = ContainmentHealthIndex_NonVaccinated,
         Simple = ContainmentHealthIndex_Vaccinated) %>%
  pivot_longer(cols = c(Weighted, Simple),
               names_to = "Version",
               values_to = "Value") %>%
  ggplot(aes(x = Date, y = Value, color = Version)) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~Country, ncol = 4, scales = "free_y") +  # scales="free_y" optional
  theme_minimal() +
  labs(
    title = "Non-Vax vs Vax - All OECD Countries",
    subtitle = "Gap shows when policies were targeted based on vax status",
    x = "Date",
    y = "Containment & Health Index"
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 8),
    axis.text = element_text(size = 6)
  )

print(p_all)

# Speichere als große PDF
# ggsave("comparison_all_countries.pdf", p_all, 
#        width = 16, height = 20)

#Deutlich höherer differenz, problem, eigenen Index bauen der das berücksichtigt-> Wie stark wird die Bevölkerung getroffen?

# ==============================================================================
# 1. CREATE POPULATION-WEIGHTED CONTAINMENT INDEX
# ==============================================================================


cat("\n", rep("=", 80), "\n", sep = "")
cat("CREATING POPULATION-WEIGHTED CONTAINMENT INDEX\n")
cat(rep("=", 80), "\n\n", sep = "")

# 1. Create the population-weighted index
oxd <- oxd %>%
  mutate(
    # Normalize vaccination rate to 0-1 (from percentage)
    vax_rate = PopulationVaccinated / 100,
    
    # Create population-weighted index
    ContainmentHealthIndex_PopWeighted = 
      (1 - vax_rate) * ContainmentHealthIndex_NonVaccinated + 
      vax_rate * ContainmentHealthIndex_Vaccinated,
    
    # Additional useful metrics
    Vax_Discount = ContainmentHealthIndex_NonVaccinated - 
      ContainmentHealthIndex_Vaccinated,
    Effective_Discount = vax_rate * Vax_Discount
  )

# 2. Move new variable to front of dataset (after key identifiers)
oxd <- oxd %>%
  select(
    # Key identifiers first
    Country, Date,
    
    # Our new PRIMARY variable
    ContainmentHealthIndex_PopWeighted,
    
    # Then the component variables
    ContainmentHealthIndex_NonVaccinated,
    ContainmentHealthIndex_Vaccinated,
    PopulationVaccinated,
    vax_rate,
    Vax_Discount,
    Effective_Discount,
    
    # Everything else
    everything()
  )



cat("✅ Population-Weighted Index created and moved to front\n")
cat("   Variable name:", primary_var, "\n")
cat("   Based on: ContainmentHealthIndex_NonVaccinated & _Vaccinated\n")
cat("   Weighted by: PopulationVaccinated (%)\n\n")


# DESCRIPTIVE STATISTICS

cat("=== DESCRIPTIVE STATISTICS ===\n\n")

desc_stats <- oxd %>%
  summarize(
    Mean_PopWeighted = mean(ContainmentHealthIndex_PopWeighted, na.rm = TRUE),
    Mean_NonVax = mean(ContainmentHealthIndex_NonVaccinated, na.rm = TRUE),
    Mean_Vax = mean(ContainmentHealthIndex_Vaccinated, na.rm = TRUE),
    SD_PopWeighted = sd(ContainmentHealthIndex_PopWeighted, na.rm = TRUE),
    SD_NonVax = sd(ContainmentHealthIndex_NonVaccinated, na.rm = TRUE),
    SD_Vax = sd(ContainmentHealthIndex_Vaccinated, na.rm = TRUE),
    Mean_VaxRate = mean(PopulationVaccinated, na.rm = TRUE),
    Mean_Discount = mean(Vax_Discount, na.rm = TRUE)
  )

print(kable(desc_stats, digits = 2, 
            caption = "Summary Statistics: Population-Weighted vs Component Indices"))

# Check for missing values
missing_check <- oxd %>%
  summarize(
    N_total = n(),
    N_PopWeighted = sum(!is.na(ContainmentHealthIndex_PopWeighted)),
    N_NonVax = sum(!is.na(ContainmentHealthIndex_NonVaccinated)),
    N_Vax = sum(!is.na(ContainmentHealthIndex_Vaccinated)),
    N_VaxRate = sum(!is.na(PopulationVaccinated)),
    Pct_Complete = round(N_PopWeighted / N_total * 100, 2)
  )

cat("\n=== DATA COMPLETENESS ===\n")
print(kable(missing_check))

# GRAPHICAL COMPARISON 1: TIME SERIES (Average across all countries)


cat("\n=== CREATING COMPARISON PLOTS ===\n\n")

# Overall time series
p_timeseries <- oxd %>%
  group_by(Date) %>%
  summarize(
    `Population-Weighted` = mean(ContainmentHealthIndex_PopWeighted, na.rm = TRUE),
    `Non-Vaccinated` = mean(ContainmentHealthIndex_NonVaccinated, na.rm = TRUE),
    `Vaccinated` = mean(ContainmentHealthIndex_Vaccinated, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(`Population-Weighted`, `Non-Vaccinated`, `Vaccinated`),
               names_to = "Index",
               values_to = "Value") %>%
  ggplot(aes(x = Date, y = Value, color = Index, linewidth = Index)) +
  geom_line() +
  scale_color_manual(
    values = c("Population-Weighted" = "black",
               "Non-Vaccinated" = "#E74C3C",
               "Vaccinated" = "#2ECC71")
  ) +
  scale_linewidth_manual(
    values = c("Population-Weighted" = 1.5,
               "Non-Vaccinated" = 1,
               "Vaccinated" = 1)
  ) +
  theme_minimal() +
  labs(
    title = "Comparison of Containment Indices Over Time",
    subtitle = "Average across all OECD countries",
    x = "Date",
    y = "Index Value (0-100)",
    color = "Index Type",
    linewidth = "Index Type",
    caption = "Population-Weighted accounts for share of population under each policy regime"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray40")
  )

print(p_timeseries)
##Top Plot, zeigt wie sich die Indexe anfangen zu unterscheiden und wie sich die weighted kurve nach unten angleicht


# GRAPHICAL COMPARISON 2: WITH VACCINATION RATE OVERLAY

# Create dual-axis plot with vaccination rate
p_with_vaxrate <- oxd %>%
  group_by(Date) %>%
  summarize(
    `Population-Weighted` = mean(ContainmentHealthIndex_PopWeighted, na.rm = TRUE),
    `Non-Vaccinated` = mean(ContainmentHealthIndex_NonVaccinated, na.rm = TRUE),
    `Vaccinated` = mean(ContainmentHealthIndex_Vaccinated, na.rm = TRUE),
    VaxRate = mean(PopulationVaccinated, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(`Population-Weighted`, `Non-Vaccinated`, `Vaccinated`),
               names_to = "Index",
               values_to = "Value") %>%
  ggplot(aes(x = Date)) +
  # Vaccination rate as area
  geom_area(aes(y = VaxRate), fill = "lightblue", alpha = 0.3) +
  # Index lines
  geom_line(aes(y = Value, color = Index, linewidth = Index)) +
  scale_color_manual(
    values = c("Population-Weighted" = "black",
               "Non-Vaccinated" = "#E74C3C",
               "Vaccinated" = "#2ECC71")
  ) +
  scale_linewidth_manual(
    values = c("Population-Weighted" = 1.5,
               "Non-Vaccinated" = 1,
               "Vaccinated" = 1)
  ) +
  theme_minimal() +
  labs(
    title = "Policy Indices with Vaccination Rate Context",
    subtitle = "Blue area shows vaccination rate (%). As vax rate rises, PopWeighted diverges from NonVax",
    x = "Date",
    y = "Index Value (0-100) / Vaccination Rate (%)",
    color = "Index Type",
    linewidth = "Index Type"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold")
  )

print(p_with_vaxrate)

##with vaccination rate

# GRAPHICAL COMPARISON 3: SELECTED COUNTRIES (Faceted)


# Select diverse countries
selected_countries <- c("USA", "GBR", "DEU", "FRA", "ITA", "ESP", 
                        "SWE", "AUS", "JPN", "KOR", "CAN", "CHE")

p_countries <- oxd %>%
  filter(Country %in% selected_countries) %>%
  select(Country, Date,
         `Population-Weighted` = ContainmentHealthIndex_PopWeighted,
         `Non-Vaccinated` = ContainmentHealthIndex_NonVaccinated,
         `Vaccinated` = ContainmentHealthIndex_Vaccinated) %>%
  pivot_longer(cols = c(`Population-Weighted`, `Non-Vaccinated`, `Vaccinated`),
               names_to = "Index",
               values_to = "Value") %>%
  ggplot(aes(x = Date, y = Value, color = Index, linewidth = Index)) +
  geom_line() +
  facet_wrap(~Country, ncol = 3, scales = "free_y") +
  scale_color_manual(
    values = c("Population-Weighted" = "black",
               "Non-Vaccinated" = "#E74C3C",
               "Vaccinated" = "#2ECC71")
  ) +
  scale_linewidth_manual(
    values = c("Population-Weighted" = 1.2,
               "Non-Vaccinated" = 0.8,
               "Vaccinated" = 0.8)
  ) +
  theme_minimal() +
  labs(
    title = "Index Comparison by Country",
    subtitle = "PopWeighted (black) falls between NonVax (red) and Vax (green) based on population shares",
    x = "Date",
    y = "Index Value",
    color = "Index Type",
    linewidth = "Index Type"
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
  )

print(p_countries)


# GRAPHICAL COMPARISON 4: DIVERGENCE ANALYSIS

# Show when and where indices diverge
p_divergence <- oxd %>%
  group_by(Date) %>%
  summarize(
    Gap_NV_Pop = mean(ContainmentHealthIndex_NonVaccinated - 
                        ContainmentHealthIndex_PopWeighted, na.rm = TRUE),
    Gap_Pop_V = mean(ContainmentHealthIndex_PopWeighted - 
                       ContainmentHealthIndex_Vaccinated, na.rm = TRUE),
    Gap_NV_V = mean(ContainmentHealthIndex_NonVaccinated - 
                      ContainmentHealthIndex_Vaccinated, na.rm = TRUE),
    VaxRate = mean(PopulationVaccinated, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(Gap_NV_Pop, Gap_Pop_V, Gap_NV_V),
               names_to = "Gap_Type",
               values_to = "Gap_Value") %>%
  mutate(
    Gap_Type = case_when(
      Gap_Type == "Gap_NV_Pop" ~ "NonVax - PopWeighted",
      Gap_Type == "Gap_Pop_V" ~ "PopWeighted - Vax",
      Gap_Type == "Gap_NV_V" ~ "NonVax - Vax"
    )
  ) %>%
  ggplot(aes(x = Date, y = Gap_Value, color = Gap_Type)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(
    values = c("NonVax - PopWeighted" = "#E74C3C",
               "PopWeighted - Vax" = "#2ECC71",
               "NonVax - Vax" = "#F39C12")
  ) +
  theme_minimal() +
  labs(
    title = "Index Divergence Over Time",
    subtitle = "Gap between different index specifications (average across countries)",
    x = "Date",
    y = "Gap (Index Points)",
    color = "Comparison",
    caption = "Rising gaps indicate increasing vaccine differentiation"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold")
  )

print(p_divergence)


# GRAPHICAL COMPARISON 5: SCATTER PLOT (PopWeighted vs NonVax)


p_scatter <- oxd %>%
  filter(!is.na(ContainmentHealthIndex_PopWeighted),
         !is.na(ContainmentHealthIndex_NonVaccinated)) %>%
  ggplot(aes(x = ContainmentHealthIndex_NonVaccinated, 
             y = ContainmentHealthIndex_PopWeighted,
             color = PopulationVaccinated)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  scale_color_viridis_c(name = "Vaccination\nRate (%)") +
  theme_minimal() +
  labs(
    title = "PopWeighted vs NonVax Index",
    subtitle = "Points below red line: PopWeighted < NonVax (due to high vaccination)",
    x = "Non-Vaccinated Index",
    y = "Population-Weighted Index",
    caption = "Color shows vaccination rate"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold")
  )

print(p_scatter)

# QUANTITATIVE COMPARISON


cat("\n", rep("=", 80), "\n", sep = "")
cat("QUANTITATIVE COMPARISON\n")
cat(rep("=", 80), "\n\n", sep = "")

# Correlation matrix
cat("=== CORRELATION MATRIX ===\n\n")

cor_matrix <- oxd %>%
  select(ContainmentHealthIndex_PopWeighted,
         ContainmentHealthIndex_NonVaccinated,
         ContainmentHealthIndex_Vaccinated) %>%
  cor(use = "complete.obs")

print(kable(round(cor_matrix, 4), 
            caption = "Correlations between Index Specifications"))

# When do indices diverge?
cat("\n=== DIVERGENCE TIMELINE ===\n\n")

divergence_check <- oxd %>%
  mutate(
    Divergence = abs(ContainmentHealthIndex_NonVaccinated - 
                       ContainmentHealthIndex_PopWeighted)
  ) %>%
  group_by(Date) %>%
  summarize(
    Mean_Divergence = mean(Divergence, na.rm = TRUE),
    Pct_Large_Divergence = mean(Divergence > 5, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  filter(Pct_Large_Divergence > 10)  # More than 10% of countries

if (nrow(divergence_check) > 0) {
  first_divergence <- min(divergence_check$Date)
  cat("First significant divergence (>10% countries with gap >5):", 
      as.character(first_divergence), "\n")
  cat("→ Before this date: Indices are essentially identical\n")
  cat("→ After this date: Population-weighting becomes important\n\n")
} else {
  cat("No significant divergence detected in sample period\n\n")
}

# Measurement bias by vaccination rate
cat("=== MEASUREMENT BIAS BY VACCINATION QUARTILE ===\n\n")


bias_by_vaxrate <- oxd %>%
  filter(
    !is.na(PopulationVaccinated),
    !is.na(ContainmentHealthIndex_PopWeighted)
  ) %>%
  mutate(
    # Quartile direkt aus Rängen
    Vax_Quartile = ntile(PopulationVaccinated, 4),
    Vax_Quartile = factor(
      Vax_Quartile,
      levels = 1:4,
      labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)")
    ),
    Bias = ContainmentHealthIndex_NonVaccinated -
      ContainmentHealthIndex_PopWeighted
  ) %>%
  group_by(Vax_Quartile) %>%
  summarize(
    Mean_VaxRate    = mean(PopulationVaccinated, na.rm = TRUE),
    Mean_NonVax     = mean(ContainmentHealthIndex_NonVaccinated, na.rm = TRUE),
    Mean_PopWeighted = mean(ContainmentHealthIndex_PopWeighted, na.rm = TRUE),
    Mean_Bias       = mean(Bias, na.rm = TRUE),
    N_obs           = n(),
    .groups = "drop"
  )


print(kable(bias_by_vaxrate, digits = 2,
            caption = "How NonVax Index Overestimates by Vaccination Level"))

cat("\n📊 Interpretation:\n")
cat("   Bias = NonVax - PopWeighted\n")
cat("   Higher vaccination → Larger bias from using NonVax alone\n")
cat("   Q4 shows the measurement problem: NonVax overstates stringency\n")

cat("\n", rep("=", 80), "\n", sep = "")
cat("✅ POPULATION-WEIGHTED INDEX CREATED AND VALIDATED\n")
cat(rep("=", 80), "\n\n", sep = "")
#===============================================================================
##Problem, containment index erfast nicht nur Beschränkungen sondern auch H Werte
#Lösung: Stringency PopWeighted machen plus noch einen eigenen für Pandemic Control (alle H) dann noch zusätzlich den Containment gemeinsam nehmen

# ==============================================================================
# 2. CREATE STRINGENCY INDEX - EXCLUDING H1 AS IT IS NOT MANDATORY AND HANDLING PRE AND POST VACCINE DIFFERENTIATION (Pop Weighted)
# ==============================================================================


cat("\n", rep("=", 80), "\n", sep = "")
cat("CREATING STRINGENCY INDEX (Handles pre/post vaccine differentiation)\n")
cat(rep("=", 80), "\n\n", sep = "")


# STEP 1: Create vax_rate and define maxima

oxd <- oxd %>%
  mutate(
    vax_rate = if_else(is.na(PopulationVaccinated) | PopulationVaccinated == 0, 
                       0, PopulationVaccinated / 100)
  )

max_values <- list(
  C1 = 3, C2 = 3, C3 = 2, C4 = 4, C5 = 2, C6 = 3, C7 = 2, C8 = 4,
  H1 = 2, H2 = 3, H3 = 2, H6 = 4, H7 = 5, H8 = 3
)

# STEP 2: Create unified variables (M before differentiation, NV/V after)

cat("=== Creating unified C-policy variables ===\n")

oxd <- oxd %>%
  mutate(
    # For each C-policy, use M (Majority) if NV is missing, otherwise use NV
    # This handles the transition from pre-differentiation to post-differentiation
    
    # C1 - Schools
    C1_unified_NV = case_when(
      !is.na(C1NV_School.closing) ~ C1NV_School.closing,
      !is.na(C1M_School.closing) ~ C1M_School.closing,
      !is.na(C1E_School.closing) ~ C1E_School.closing,
      TRUE ~ NA_real_
    ),
    C1_unified_V = case_when(
      !is.na(C1V_School.closing) ~ C1V_School.closing,
      !is.na(C1M_School.closing) ~ C1M_School.closing,
      !is.na(C1E_School.closing) ~ C1E_School.closing,
      TRUE ~ NA_real_
    ),
    C1_unified_Flag = case_when(
      !is.na(C1NV_Flag) ~ C1NV_Flag,
      !is.na(C1M_Flag) ~ C1M_Flag,
      !is.na(C1E_Flag) ~ C1E_Flag,
      TRUE ~ 1  # Default to national
    ),
    
    # C2 - Workplace
    C2_unified_NV = case_when(
      !is.na(C2NV_Workplace.closing) ~ C2NV_Workplace.closing,
      !is.na(C2M_Workplace.closing) ~ C2M_Workplace.closing,
      !is.na(C2E_Workplace.closing) ~ C2E_Workplace.closing,
      TRUE ~ NA_real_
    ),
    C2_unified_V = case_when(
      !is.na(C2V_Workplace.closing) ~ C2V_Workplace.closing,
      !is.na(C2M_Workplace.closing) ~ C2M_Workplace.closing,
      !is.na(C2E_Workplace.closing) ~ C2E_Workplace.closing,
      TRUE ~ NA_real_
    ),
    C2_unified_Flag = case_when(
      !is.na(C2NV_Flag) ~ C2NV_Flag,
      !is.na(C2M_Flag) ~ C2M_Flag,
      !is.na(C2E_Flag) ~ C2E_Flag,
      TRUE ~ 1
    ),
    
    # C3 - Events
    C3_unified_NV = case_when(
      !is.na(C3NV_Cancel.public.events) ~ C3NV_Cancel.public.events,
      !is.na(C3M_Cancel.public.events) ~ C3M_Cancel.public.events,
      !is.na(C3E_Cancel.public.events) ~ C3E_Cancel.public.events,
      TRUE ~ NA_real_
    ),
    C3_unified_V = case_when(
      !is.na(C3V_Cancel.public.events) ~ C3V_Cancel.public.events,
      !is.na(C3M_Cancel.public.events) ~ C3M_Cancel.public.events,
      !is.na(C3E_Cancel.public.events) ~ C3E_Cancel.public.events,
      TRUE ~ NA_real_
    ),
    C3_unified_Flag = case_when(
      !is.na(C3NV_Flag) ~ C3NV_Flag,
      !is.na(C3M_Flag) ~ C3M_Flag,
      !is.na(C3E_Flag) ~ C3E_Flag,
      TRUE ~ 1
    ),
    
    # C4 - Gatherings
    C4_unified_NV = case_when(
      !is.na(C4NV_Restrictions.on.gatherings) ~ C4NV_Restrictions.on.gatherings,
      !is.na(C4M_Restrictions.on.gatherings) ~ C4M_Restrictions.on.gatherings,
      !is.na(C4E_Restrictions.on.gatherings) ~ C4E_Restrictions.on.gatherings,
      TRUE ~ NA_real_
    ),
    C4_unified_V = case_when(
      !is.na(C4V_Restrictions.on.gatherings) ~ C4V_Restrictions.on.gatherings,
      !is.na(C4M_Restrictions.on.gatherings) ~ C4M_Restrictions.on.gatherings,
      !is.na(C4E_Restrictions.on.gatherings) ~ C4E_Restrictions.on.gatherings,
      TRUE ~ NA_real_
    ),
    C4_unified_Flag = case_when(
      !is.na(C4NV_Flag) ~ C4NV_Flag,
      !is.na(C4M_Flag) ~ C4M_Flag,
      !is.na(C4E_Flag) ~ C4E_Flag,
      TRUE ~ 1
    ),
    
    # C5 - Transport
    C5_unified_NV = case_when(
      !is.na(C5NV_Close.public.transport) ~ C5NV_Close.public.transport,
      !is.na(C5M_Close.public.transport) ~ C5M_Close.public.transport,
      !is.na(C5E_Close.public.transport) ~ C5E_Close.public.transport,
      TRUE ~ NA_real_
    ),
    C5_unified_V = case_when(
      !is.na(C5V_Close.public.transport) ~ C5V_Close.public.transport,
      !is.na(C5M_Close.public.transport) ~ C5M_Close.public.transport,
      !is.na(C5E_Close.public.transport) ~ C5E_Close.public.transport,
      TRUE ~ NA_real_
    ),
    C5_unified_Flag = case_when(
      !is.na(C5NV_Flag) ~ C5NV_Flag,
      !is.na(C5M_Flag) ~ C5M_Flag,
      !is.na(C5E_Flag) ~ C5E_Flag,
      TRUE ~ 1
    ),
    
    # C6 - Stay at home
    C6_unified_NV = case_when(
      !is.na(C6NV_Stay.at.home.requirements) ~ C6NV_Stay.at.home.requirements,
      !is.na(C6M_Stay.at.home.requirements) ~ C6M_Stay.at.home.requirements,
      !is.na(C6E_Stay.at.home.requirements) ~ C6E_Stay.at.home.requirements,
      TRUE ~ NA_real_
    ),
    C6_unified_V = case_when(
      !is.na(C6V_Stay.at.home.requirements) ~ C6V_Stay.at.home.requirements,
      !is.na(C6M_Stay.at.home.requirements) ~ C6M_Stay.at.home.requirements,
      !is.na(C6E_Stay.at.home.requirements) ~ C6E_Stay.at.home.requirements,
      TRUE ~ NA_real_
    ),
    C6_unified_Flag = case_when(
      !is.na(C6NV_Flag) ~ C6NV_Flag,
      !is.na(C6M_Flag) ~ C6M_Flag,
      !is.na(C6E_Flag) ~ C6E_Flag,
      TRUE ~ 1
    ),
    
    # C7 - Internal movement
    C7_unified_NV = case_when(
      !is.na(C7NV_Restrictions.on.internal.movement) ~ C7NV_Restrictions.on.internal.movement,
      !is.na(C7M_Restrictions.on.internal.movement) ~ C7M_Restrictions.on.internal.movement,
      !is.na(C7E_Restrictions.on.internal.movement) ~ C7E_Restrictions.on.internal.movement,
      TRUE ~ NA_real_
    ),
    C7_unified_V = case_when(
      !is.na(C7V_Restrictions.on.internal.movement) ~ C7V_Restrictions.on.internal.movement,
      !is.na(C7M_Restrictions.on.internal.movement) ~ C7M_Restrictions.on.internal.movement,
      !is.na(C7E_Restrictions.on.internal.movement) ~ C7E_Restrictions.on.internal.movement,
      TRUE ~ NA_real_
    ),
    C7_unified_Flag = case_when(
      !is.na(C7NV_Flag) ~ C7NV_Flag,
      !is.na(C7M_Flag) ~ C7M_Flag,
      !is.na(C7E_Flag) ~ C7E_Flag,
      TRUE ~ 1
    ),
    
    # C8 - International travel (no FLAG)
    C8_unified_NV = case_when(
      !is.na(C8NV_International.travel.controls) ~ C8NV_International.travel.controls,
      !is.na(C8EV_International.travel.controls) ~ C8EV_International.travel.controls,
      !is.na(C8E_International.travel.controls) ~ C8E_International.travel.controls,
      TRUE ~ NA_real_
    ),
    C8_unified_V = case_when(
      !is.na(C8V_International.travel.controls) ~ C8V_International.travel.controls,
      !is.na(C8EV_International.travel.controls) ~ C8EV_International.travel.controls,
      !is.na(C8E_International.travel.controls) ~ C8E_International.travel.controls,
      TRUE ~ NA_real_
    )
  )

cat("✅ Unified C-policy variables created\n\n")

# STEP 3: Normalize and adjust with FLAGS

cat("=== Normalizing and adjusting C-policies ===\n")

oxd <- oxd %>%
  mutate(
    # Normalize and FLAG-adjust for NV
    C1_NV_adj = (C1_unified_NV / max_values$C1) * 100 * (1 - 0.5 * (1 - C1_unified_Flag)),
    C2_NV_adj = (C2_unified_NV / max_values$C2) * 100 * (1 - 0.5 * (1 - C2_unified_Flag)),
    C3_NV_adj = (C3_unified_NV / max_values$C3) * 100 * (1 - 0.5 * (1 - C3_unified_Flag)),
    C4_NV_adj = (C4_unified_NV / max_values$C4) * 100 * (1 - 0.5 * (1 - C4_unified_Flag)),
    C5_NV_adj = (C5_unified_NV / max_values$C5) * 100 * (1 - 0.5 * (1 - C5_unified_Flag)),
    C6_NV_adj = (C6_unified_NV / max_values$C6) * 100 * (1 - 0.5 * (1 - C6_unified_Flag)),
    C7_NV_adj = (C7_unified_NV / max_values$C7) * 100 * (1 - 0.5 * (1 - C7_unified_Flag)),
    C8_NV_adj = (C8_unified_NV / max_values$C8) * 100,
    
    # Normalize and FLAG-adjust for V (same as NV before differentiation)
    C1_V_adj = (C1_unified_V / max_values$C1) * 100 * (1 - 0.5 * (1 - C1_unified_Flag)),
    C2_V_adj = (C2_unified_V / max_values$C2) * 100 * (1 - 0.5 * (1 - C2_unified_Flag)),
    C3_V_adj = (C3_unified_V / max_values$C3) * 100 * (1 - 0.5 * (1 - C3_unified_Flag)),
    C4_V_adj = (C4_unified_V / max_values$C4) * 100 * (1 - 0.5 * (1 - C4_unified_Flag)),
    C5_V_adj = (C5_unified_V / max_values$C5) * 100 * (1 - 0.5 * (1 - C5_unified_Flag)),
    C6_V_adj = (C6_unified_V / max_values$C6) * 100 * (1 - 0.5 * (1 - C6_unified_Flag)),
    C7_V_adj = (C7_unified_V / max_values$C7) * 100 * (1 - 0.5 * (1 - C7_unified_Flag)),
    C8_V_adj = (C8_unified_V / max_values$C8) * 100,
    
    # Replace NA with 0
    across(c(C1_NV_adj:C8_V_adj), ~if_else(is.na(.), 0, .))
  )

cat("✅ Normalized and FLAG-adjusted\n\n")

# STEP 4: Calculate Stringency Index

cat("=== Calculating Stringency Index ===\n")

oxd <- oxd %>%
  mutate(
    # Average of C-policies
    StringencyIndex_NV = (C1_NV_adj + C2_NV_adj + C3_NV_adj + C4_NV_adj +
                            C5_NV_adj + C6_NV_adj + C7_NV_adj + C8_NV_adj) / 8,
    
    StringencyIndex_V = (C1_V_adj + C2_V_adj + C3_V_adj + C4_V_adj +
                           C5_V_adj + C6_V_adj + C7_V_adj + C8_V_adj) / 8,
    
    # Population-weighted
    StringencyIndex_PopWeighted = (1 - vax_rate) * StringencyIndex_NV + 
      vax_rate * StringencyIndex_V
  )

cat("✅ Stringency Index created\n\n")

# Check
summary_stats <- oxd %>%
  summarize(
    Mean = mean(StringencyIndex_PopWeighted, na.rm = TRUE),
    SD = sd(StringencyIndex_PopWeighted, na.rm = TRUE),
    Min = min(StringencyIndex_PopWeighted, na.rm = TRUE),
    Max = max(StringencyIndex_PopWeighted, na.rm = TRUE),
    N_nonzero = sum(StringencyIndex_PopWeighted > 0, na.rm = TRUE)
  )

print(kable(summary_stats, digits = 2))

# Check by year
yearly_check <- oxd %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarize(
    Mean = mean(StringencyIndex_PopWeighted, na.rm = TRUE),
    N_nonzero = sum(StringencyIndex_PopWeighted > 0, na.rm = TRUE),
    N_total = n()
  )

cat("\nBy year:\n")
print(kable(yearly_check, digits = 2))

# Correlation with Oxford
cor_oxford <- cor(oxd$StringencyIndex_PopWeighted,
                  oxd$StringencyIndex_WeightedAverage,
                  use = "complete.obs")
cat("\nCorrelation with Oxford Stringency:", round(cor_oxford, 4), "\n")

cat("\n✅ DONE! StringencyIndex_PopWeighted should now have values from 2020 onwards.\n")
#===============================================================================
##Bereinigter STRINGENCY INDEX ERSTELLT-> Stimmt nicht ganz mit Oxford überein->ÜBERPRÜFEN INDEM DER OXFORD INDEX (MIT H1) REPLIZIERT WIRD

# ==============================================================================
# 3. CREATING STRINGENCY INDEX WITH H1 (C1-C8 + H1)
# ==============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("CREATING STRINGENCY INDEX WITH H1 (Matches Oxford definition)\n")
cat(rep("=", 80), "\n\n", sep = "")

# STEP 1: Create vax_rate and define maxima (already done, but repeated for clarity)

oxd <- oxd %>%
  mutate(
    vax_rate = if_else(is.na(PopulationVaccinated) | PopulationVaccinated == 0, 
                       0, PopulationVaccinated / 100)
  )

max_values <- list(
  C1 = 3, C2 = 3, C3 = 2, C4 = 4, C5 = 2, C6 = 3, C7 = 2, C8 = 4,
  H1 = 2, H2 = 3, H3 = 2, H6 = 4, H7 = 5, H8 = 3
)

# STEP 2: Create unified C-policy variables (same as before)

cat("=== Creating unified C-policy variables ===\n")

oxd <- oxd %>%
  mutate(
    # C1 - Schools
    C1_unified_NV = case_when(
      !is.na(C1NV_School.closing) ~ C1NV_School.closing,
      !is.na(C1M_School.closing) ~ C1M_School.closing,
      !is.na(C1E_School.closing) ~ C1E_School.closing,
      TRUE ~ NA_real_
    ),
    C1_unified_V = case_when(
      !is.na(C1V_School.closing) ~ C1V_School.closing,
      !is.na(C1M_School.closing) ~ C1M_School.closing,
      !is.na(C1E_School.closing) ~ C1E_School.closing,
      TRUE ~ NA_real_
    ),
    C1_unified_Flag = case_when(
      !is.na(C1NV_Flag) ~ C1NV_Flag,
      !is.na(C1M_Flag) ~ C1M_Flag,
      !is.na(C1E_Flag) ~ C1E_Flag,
      TRUE ~ 1
    ),
    
    # C2 - Workplace
    C2_unified_NV = case_when(
      !is.na(C2NV_Workplace.closing) ~ C2NV_Workplace.closing,
      !is.na(C2M_Workplace.closing) ~ C2M_Workplace.closing,
      !is.na(C2E_Workplace.closing) ~ C2E_Workplace.closing,
      TRUE ~ NA_real_
    ),
    C2_unified_V = case_when(
      !is.na(C2V_Workplace.closing) ~ C2V_Workplace.closing,
      !is.na(C2M_Workplace.closing) ~ C2M_Workplace.closing,
      !is.na(C2E_Workplace.closing) ~ C2E_Workplace.closing,
      TRUE ~ NA_real_
    ),
    C2_unified_Flag = case_when(
      !is.na(C2NV_Flag) ~ C2NV_Flag,
      !is.na(C2M_Flag) ~ C2M_Flag,
      !is.na(C2E_Flag) ~ C2E_Flag,
      TRUE ~ 1
    ),
    
    # C3 - Events
    C3_unified_NV = case_when(
      !is.na(C3NV_Cancel.public.events) ~ C3NV_Cancel.public.events,
      !is.na(C3M_Cancel.public.events) ~ C3M_Cancel.public.events,
      !is.na(C3E_Cancel.public.events) ~ C3E_Cancel.public.events,
      TRUE ~ NA_real_
    ),
    C3_unified_V = case_when(
      !is.na(C3V_Cancel.public.events) ~ C3V_Cancel.public.events,
      !is.na(C3M_Cancel.public.events) ~ C3M_Cancel.public.events,
      !is.na(C3E_Cancel.public.events) ~ C3E_Cancel.public.events,
      TRUE ~ NA_real_
    ),
    C3_unified_Flag = case_when(
      !is.na(C3NV_Flag) ~ C3NV_Flag,
      !is.na(C3M_Flag) ~ C3M_Flag,
      !is.na(C3E_Flag) ~ C3E_Flag,
      TRUE ~ 1
    ),
    
    # C4 - Gatherings
    C4_unified_NV = case_when(
      !is.na(C4NV_Restrictions.on.gatherings) ~ C4NV_Restrictions.on.gatherings,
      !is.na(C4M_Restrictions.on.gatherings) ~ C4M_Restrictions.on.gatherings,
      !is.na(C4E_Restrictions.on.gatherings) ~ C4E_Restrictions.on.gatherings,
      TRUE ~ NA_real_
    ),
    C4_unified_V = case_when(
      !is.na(C4V_Restrictions.on.gatherings) ~ C4V_Restrictions.on.gatherings,
      !is.na(C4M_Restrictions.on.gatherings) ~ C4M_Restrictions.on.gatherings,
      !is.na(C4E_Restrictions.on.gatherings) ~ C4E_Restrictions.on.gatherings,
      TRUE ~ NA_real_
    ),
    C4_unified_Flag = case_when(
      !is.na(C4NV_Flag) ~ C4NV_Flag,
      !is.na(C4M_Flag) ~ C4M_Flag,
      !is.na(C4E_Flag) ~ C4E_Flag,
      TRUE ~ 1
    ),
    
    # C5 - Transport
    C5_unified_NV = case_when(
      !is.na(C5NV_Close.public.transport) ~ C5NV_Close.public.transport,
      !is.na(C5M_Close.public.transport) ~ C5M_Close.public.transport,
      !is.na(C5E_Close.public.transport) ~ C5E_Close.public.transport,
      TRUE ~ NA_real_
    ),
    C5_unified_V = case_when(
      !is.na(C5V_Close.public.transport) ~ C5V_Close.public.transport,
      !is.na(C5M_Close.public.transport) ~ C5M_Close.public.transport,
      !is.na(C5E_Close.public.transport) ~ C5E_Close.public.transport,
      TRUE ~ NA_real_
    ),
    C5_unified_Flag = case_when(
      !is.na(C5NV_Flag) ~ C5NV_Flag,
      !is.na(C5M_Flag) ~ C5M_Flag,
      !is.na(C5E_Flag) ~ C5E_Flag,
      TRUE ~ 1
    ),
    
    # C6 - Stay at home
    C6_unified_NV = case_when(
      !is.na(C6NV_Stay.at.home.requirements) ~ C6NV_Stay.at.home.requirements,
      !is.na(C6M_Stay.at.home.requirements) ~ C6M_Stay.at.home.requirements,
      !is.na(C6E_Stay.at.home.requirements) ~ C6E_Stay.at.home.requirements,
      TRUE ~ NA_real_
    ),
    C6_unified_V = case_when(
      !is.na(C6V_Stay.at.home.requirements) ~ C6V_Stay.at.home.requirements,
      !is.na(C6M_Stay.at.home.requirements) ~ C6M_Stay.at.home.requirements,
      !is.na(C6E_Stay.at.home.requirements) ~ C6E_Stay.at.home.requirements,
      TRUE ~ NA_real_
    ),
    C6_unified_Flag = case_when(
      !is.na(C6NV_Flag) ~ C6NV_Flag,
      !is.na(C6M_Flag) ~ C6M_Flag,
      !is.na(C6E_Flag) ~ C6E_Flag,
      TRUE ~ 1
    ),
    
    # C7 - Internal movement
    C7_unified_NV = case_when(
      !is.na(C7NV_Restrictions.on.internal.movement) ~ C7NV_Restrictions.on.internal.movement,
      !is.na(C7M_Restrictions.on.internal.movement) ~ C7M_Restrictions.on.internal.movement,
      !is.na(C7E_Restrictions.on.internal.movement) ~ C7E_Restrictions.on.internal.movement,
      TRUE ~ NA_real_
    ),
    C7_unified_V = case_when(
      !is.na(C7V_Restrictions.on.internal.movement) ~ C7V_Restrictions.on.internal.movement,
      !is.na(C7M_Restrictions.on.internal.movement) ~ C7M_Restrictions.on.internal.movement,
      !is.na(C7E_Restrictions.on.internal.movement) ~ C7E_Restrictions.on.internal.movement,
      TRUE ~ NA_real_
    ),
    C7_unified_Flag = case_when(
      !is.na(C7NV_Flag) ~ C7NV_Flag,
      !is.na(C7M_Flag) ~ C7M_Flag,
      !is.na(C7E_Flag) ~ C7E_Flag,
      TRUE ~ 1
    ),
    
    # C8 - International travel (no FLAG)
    C8_unified_NV = case_when(
      !is.na(C8NV_International.travel.controls) ~ C8NV_International.travel.controls,
      !is.na(C8EV_International.travel.controls) ~ C8EV_International.travel.controls,
      !is.na(C8E_International.travel.controls) ~ C8E_International.travel.controls,
      TRUE ~ NA_real_
    ),
    C8_unified_V = case_when(
      !is.na(C8V_International.travel.controls) ~ C8V_International.travel.controls,
      !is.na(C8EV_International.travel.controls) ~ C8EV_International.travel.controls,
      !is.na(C8E_International.travel.controls) ~ C8E_International.travel.controls,
      TRUE ~ NA_real_
    ),
    
    # NEW: H1 - Public Information Campaigns
    # H1 has no NV/V differentiation, only E (Everyone)
    H1_unified = case_when(
      !is.na(H1E_Public.information.campaigns) ~ H1E_Public.information.campaigns,
      TRUE ~ NA_real_
    ),
    H1_unified_Flag = case_when(
      !is.na(H1E_Flag) ~ H1E_Flag,
      TRUE ~ 1  # Default to national
    )
  )

cat("✅ Unified C-policy and H1 variables created\n\n")

# STEP 3: Normalize and adjust with FLAGS

cat("=== Normalizing and adjusting C-policies and H1 ===\n")

oxd <- oxd %>%
  mutate(
    # Normalize and FLAG-adjust C-policies for NV
    C1_NV_adj = (C1_unified_NV / max_values$C1) * 100 * (1 - 0.5 * (1 - C1_unified_Flag)),
    C2_NV_adj = (C2_unified_NV / max_values$C2) * 100 * (1 - 0.5 * (1 - C2_unified_Flag)),
    C3_NV_adj = (C3_unified_NV / max_values$C3) * 100 * (1 - 0.5 * (1 - C3_unified_Flag)),
    C4_NV_adj = (C4_unified_NV / max_values$C4) * 100 * (1 - 0.5 * (1 - C4_unified_Flag)),
    C5_NV_adj = (C5_unified_NV / max_values$C5) * 100 * (1 - 0.5 * (1 - C5_unified_Flag)),
    C6_NV_adj = (C6_unified_NV / max_values$C6) * 100 * (1 - 0.5 * (1 - C6_unified_Flag)),
    C7_NV_adj = (C7_unified_NV / max_values$C7) * 100 * (1 - 0.5 * (1 - C7_unified_Flag)),
    C8_NV_adj = (C8_unified_NV / max_values$C8) * 100,
    
    # Normalize and FLAG-adjust C-policies for V
    C1_V_adj = (C1_unified_V / max_values$C1) * 100 * (1 - 0.5 * (1 - C1_unified_Flag)),
    C2_V_adj = (C2_unified_V / max_values$C2) * 100 * (1 - 0.5 * (1 - C2_unified_Flag)),
    C3_V_adj = (C3_unified_V / max_values$C3) * 100 * (1 - 0.5 * (1 - C3_unified_Flag)),
    C4_V_adj = (C4_unified_V / max_values$C4) * 100 * (1 - 0.5 * (1 - C4_unified_Flag)),
    C5_V_adj = (C5_unified_V / max_values$C5) * 100 * (1 - 0.5 * (1 - C5_unified_Flag)),
    C6_V_adj = (C6_unified_V / max_values$C6) * 100 * (1 - 0.5 * (1 - C6_unified_Flag)),
    C7_V_adj = (C7_unified_V / max_values$C7) * 100 * (1 - 0.5 * (1 - C7_unified_Flag)),
    C8_V_adj = (C8_unified_V / max_values$C8) * 100,
    
    # Normalize and FLAG-adjust H1 (no NV/V differentiation)
    H1_norm = (H1_unified / max_values$H1) * 100,
    H1_adj = H1_norm * (1 - 0.5 * (1 - H1_unified_Flag)),
    
    # Replace NA with 0
    across(c(C1_NV_adj:C8_V_adj, H1_norm, H1_adj), ~if_else(is.na(.), 0, .))
  )

cat("✅ Normalized and FLAG-adjusted (including H1)\n\n")

# STEP 4: Calculate BOTH Stringency Indices

cat("=== Calculating Stringency Indices (with and without H1) ===\n")

oxd <- oxd %>%
  mutate(
    
    # Index 1: C1-C8 ONLY (our preferred measure)
    StringencyIndex_NV = (C1_NV_adj + C2_NV_adj + C3_NV_adj + C4_NV_adj +
                            C5_NV_adj + C6_NV_adj + C7_NV_adj + C8_NV_adj) / 8,
    
    StringencyIndex_V = (C1_V_adj + C2_V_adj + C3_V_adj + C4_V_adj +
                           C5_V_adj + C6_V_adj + C7_V_adj + C8_V_adj) / 8,
    
    StringencyIndex_PopWeighted = (1 - vax_rate) * StringencyIndex_NV + 
      vax_rate * StringencyIndex_V,
    
    # Index 2: C1-C8 + H1 (Oxford definition)
    StringencyH1_NV = (C1_NV_adj + C2_NV_adj + C3_NV_adj + C4_NV_adj +
                         C5_NV_adj + C6_NV_adj + C7_NV_adj + C8_NV_adj +
                         H1_adj) / 9,
    
    StringencyH1_V = (C1_V_adj + C2_V_adj + C3_V_adj + C4_V_adj +
                        C5_V_adj + C6_V_adj + C7_V_adj + C8_V_adj +
                        H1_adj) / 9,
    
    StringencyH1_PopWeighted = (1 - vax_rate) * StringencyH1_NV + 
      vax_rate * StringencyH1_V
  )

cat("✅ Both Stringency Indices created:\n")
cat("   - StringencyIndex_PopWeighted (C1-C8 only)\n")
cat("   - StringencyH1_PopWeighted (C1-C8 + H1, matches Oxford)\n\n")

# STEP 5: Validation and Comparison

cat("=== VALIDATION: Which index matches Oxford? ===\n\n")

# Summary statistics
summary_comparison <- oxd %>%
  summarize(
    `C1-C8 only` = mean(StringencyIndex_PopWeighted, na.rm = TRUE),
    `C1-C8+H1` = mean(StringencyH1_PopWeighted, na.rm = TRUE),
    `Oxford` = mean(StringencyIndex_WeightedAverage, na.rm = TRUE),
    `Diff (C1-C8)` = mean(StringencyIndex_PopWeighted - StringencyIndex_WeightedAverage, na.rm = TRUE),
    `Diff (C1-C8+H1)` = mean(StringencyH1_PopWeighted - StringencyIndex_WeightedAverage, na.rm = TRUE)
  )

print(kable(summary_comparison, digits = 2,
            caption = "Mean Values: Which matches Oxford?"))

# Correlations
cor_without_h1 <- cor(oxd$StringencyIndex_PopWeighted,
                      oxd$StringencyIndex_WeightedAverage,
                      use = "complete.obs")

cor_with_h1 <- cor(oxd$StringencyH1_PopWeighted,
                   oxd$StringencyIndex_WeightedAverage,
                   use = "complete.obs")

cat("\nCorrelations:\n")
cat("C1-C8 only vs Oxford:", round(cor_without_h1, 4), "\n")
cat("C1-C8+H1 vs Oxford:  ", round(cor_with_h1, 4), "\n\n")

# RMSE
rmse_without_h1 <- sqrt(mean((oxd$StringencyIndex_PopWeighted - 
                                oxd$StringencyIndex_WeightedAverage)^2, 
                             na.rm = TRUE))

rmse_with_h1 <- sqrt(mean((oxd$StringencyH1_PopWeighted - 
                             oxd$StringencyIndex_WeightedAverage)^2, 
                          na.rm = TRUE))

cat("RMSE (lower is better):\n")
cat("C1-C8 only vs Oxford:", round(rmse_without_h1, 2), "\n")
cat("C1-C8+H1 vs Oxford:  ", round(rmse_with_h1, 2), "\n\n")

# By year
yearly_validation <- oxd %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarize(
    `C1-C8` = mean(StringencyIndex_PopWeighted, na.rm = TRUE),
    `C1-C8+H1` = mean(StringencyH1_PopWeighted, na.rm = TRUE),
    `Oxford` = mean(StringencyIndex_WeightedAverage, na.rm = TRUE),
    `Diff_C1C8` = `C1-C8` - Oxford,
    `Diff_C1C8H1` = `C1-C8+H1` - Oxford,
    .groups = "drop"
  )

cat("By Year:\n")
print(kable(yearly_validation, digits = 2))

# Test on specific observation
cat("\n=== TEST CASE: Germany, April 1, 2020 ===\n\n")

test_obs <- oxd %>%
  filter(Country == "DEU", Date == as.Date("2020-04-01")) %>%
  select(
    Country, Date,
    H1_adj,
    StringencyIndex_PopWeighted,
    StringencyH1_PopWeighted,
    StringencyIndex_WeightedAverage
  )

if (nrow(test_obs) > 0) {
  print(kable(t(test_obs), col.names = "Value", digits = 2))
  
  diff_without <- test_obs$StringencyIndex_PopWeighted - 
    test_obs$StringencyIndex_WeightedAverage
  diff_with <- test_obs$StringencyH1_PopWeighted - 
    test_obs$StringencyIndex_WeightedAverage
  
  cat("\nDifference from Oxford:\n")
  cat("Without H1:", round(diff_without, 2), "points\n")
  cat("With H1:   ", round(diff_with, 2), "points\n\n")
}
#===============================================================================
##Fertig erstellt, beide PopWeighted Stringency Index, ohne und mit H1
##Problem: Neben Beschränkungen gab es auch Bemühung die Pandemie anders zu kontrollieren-> Tests usw.-> Deshalb eigenen Control Index bauen

# ==============================================================================
# 4. CREATE POPULATION-WEIGHTED CONTROL INDEX
# ==============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("CREATING CONTROL INDEX (Population-Weighted)\n")
cat("Components: C1-C8 + H1, H2, H3, H6, H7, H8\n")
cat(rep("=", 80), "\n\n", sep = "")

# PREREQUISITE: Ensure C-policies and vax_rate already exist
# (Assumes you already ran the stringency index code which created C1-C8_adj)

# STEP 1: Create unified H-policy variables

cat("=== Creating unified H-policy variables ===\n")

oxd <- oxd %>%
  mutate(
    # H1 - Public Information Campaigns (E only, has FLAG)
    H1_unified = case_when(
      !is.na(H1E_Public.information.campaigns) ~ H1E_Public.information.campaigns,
      TRUE ~ NA_real_
    ),
    H1_unified_Flag = case_when(
      !is.na(H1E_Flag) ~ H1E_Flag,
      TRUE ~ 1  # Default to national
    ),
    
    # H2 - Testing Policy (E only, NO FLAG)
    H2_unified = case_when(
      !is.na(H2E_Testing.policy) ~ H2E_Testing.policy,
      TRUE ~ NA_real_
    ),
    
    # H3 - Contact Tracing (E only, NO FLAG)
    H3_unified = case_when(
      !is.na(H3E_Contact.tracing) ~ H3E_Contact.tracing,
      TRUE ~ NA_real_
    ),
    
    # H6 - Facial Coverings (HAS NV/V differentiation, has FLAG)
    H6_unified_NV = case_when(
      !is.na(H6NV_Facial.Coverings) ~ H6NV_Facial.Coverings,
      !is.na(H6M_Facial.Coverings) ~ H6M_Facial.Coverings,
      !is.na(H6E_Facial.Coverings) ~ H6E_Facial.Coverings,
      TRUE ~ NA_real_
    ),
    H6_unified_V = case_when(
      !is.na(H6V_Facial.Coverings) ~ H6V_Facial.Coverings,
      !is.na(H6M_Facial.Coverings) ~ H6M_Facial.Coverings,
      !is.na(H6E_Facial.Coverings) ~ H6E_Facial.Coverings,
      TRUE ~ NA_real_
    ),
    H6_unified_Flag = case_when(
      !is.na(H6NV_Flag) ~ H6NV_Flag,
      !is.na(H6M_Flag) ~ H6M_Flag,
      !is.na(H6E_Flag) ~ H6E_Flag,
      TRUE ~ 1
    ),
    
    # H7 - Vaccination Policy (E only, has FLAG)
    H7_unified = case_when(
      !is.na(H7E_Vaccination.policy) ~ H7E_Vaccination.policy,
      TRUE ~ NA_real_
    ),
    H7_unified_Flag = case_when(
      !is.na(H7E_Flag) ~ H7E_Flag,
      TRUE ~ 1
    ),
    
    # H8 - Protection of Elderly (HAS NV/V differentiation, has FLAG)
    H8_unified_NV = case_when(
      !is.na(H8NV_Protection.of.elderly.people) ~ H8NV_Protection.of.elderly.people,
      !is.na(H8M_Protection.of.elderly.people) ~ H8M_Protection.of.elderly.people,
      !is.na(H8E_Protection.of.elderly.people) ~ H8E_Protection.of.elderly.people,
      TRUE ~ NA_real_
    ),
    H8_unified_V = case_when(
      !is.na(H8V_Protection.of.elderly.people) ~ H8V_Protection.of.elderly.people,
      !is.na(H8M_Protection.of.elderly.people) ~ H8M_Protection.of.elderly.people,
      !is.na(H8E_Protection.of.elderly.people) ~ H8E_Protection.of.elderly.people,
      TRUE ~ NA_real_
    ),
    H8_unified_Flag = case_when(
      !is.na(H8NV_Flag) ~ H8NV_Flag,
      !is.na(H8M_Flag) ~ H8M_Flag,
      !is.na(H8E_Flag) ~ H8E_Flag,
      TRUE ~ 1
    )
  )

cat("✅ Unified H-policy variables created\n\n")

# STEP 2: Normalize and FLAG-adjust H-policies

cat("=== Normalizing and adjusting H-policies ===\n")

oxd <- oxd %>%
  mutate(
    # H1 - Public Info (normalize + FLAG adjust)
    H1_norm = (H1_unified / max_values$H1) * 100,
    H1_adj = H1_norm * (1 - 0.5 * (1 - H1_unified_Flag)),
    # H2 - Testing (normalize, NO FLAG)
    H2_norm = (H2_unified / max_values$H2) * 100,
    H2_adj = H2_norm,  # No FLAG adjustment
    # H3 - Contact Tracing (normalize, NO FLAG)
    H3_norm = (H3_unified / max_values$H3) * 100,
    H3_adj = H3_norm,  # No FLAG adjustment
    # H6 - Masks (normalize + FLAG adjust, NV/V differentiation)
    H6_NV_norm = (H6_unified_NV / max_values$H6) * 100,
    H6_V_norm = (H6_unified_V / max_values$H6) * 100,
    H6_NV_adj = H6_NV_norm * (1 - 0.5 * (1 - H6_unified_Flag)),
    H6_V_adj = H6_V_norm * (1 - 0.5 * (1 - H6_unified_Flag)),
    # Population-weighted H6
    H6_PopWeighted = (1 - vax_rate) * H6_NV_adj + vax_rate * H6_V_adj,
    # H7 - Vaccination (normalize + FLAG adjust)
    H7_norm = (H7_unified / max_values$H7) * 100,
    H7_adj = H7_norm * (1 - 0.5 * (1 - H7_unified_Flag)),
    # H8 - Elderly Protection (normalize + FLAG adjust, NV/V differentiation)
    H8_NV_norm = (H8_unified_NV / max_values$H8) * 100,
    H8_V_norm = (H8_unified_V / max_values$H8) * 100,
    H8_NV_adj = H8_NV_norm * (1 - 0.5 * (1 - H8_unified_Flag)),
    H8_V_adj = H8_V_norm * (1 - 0.5 * (1 - H8_unified_Flag)),
    # Population-weighted H8
    H8_PopWeighted = (1 - vax_rate) * H8_NV_adj + vax_rate * H8_V_adj
  ) %>%
  # Replace NA with 0 ONLY for numeric H-policy columns (FIXED!)
  mutate(
    across(c(H1_norm, H1_adj, 
             H2_norm, H2_adj, 
             H3_norm, H3_adj,
             H6_NV_norm, H6_V_norm, H6_NV_adj, H6_V_adj, H6_PopWeighted,
             H7_norm, H7_adj,
             H8_NV_norm, H8_V_norm, H8_NV_adj, H8_V_adj, H8_PopWeighted), 
           ~if_else(is.na(.), 0, .))
  )

cat("✅ H-policies normalized and adjusted\n\n")

# STEP 3: Calculate Control Index

cat("=== Calculating Control Index ===\n")

oxd <- oxd %>%
  mutate(
    # For NonVaccinated: C1-C8 (NV) + H1, H2, H3 + H6 (NV) + H7 + H8 (NV)
    ControlIndex_NV = (
      C1_NV_adj + C2_NV_adj + C3_NV_adj + C4_NV_adj +
        C5_NV_adj + C6_NV_adj + C7_NV_adj + C8_NV_adj +
        H1_adj + H2_adj + H3_adj + H6_NV_adj + H7_adj + H8_NV_adj
    ) / 14,
    
    # For Vaccinated: C1-C8 (V) + H1, H2, H3 + H6 (V) + H7 + H8 (V)
    ControlIndex_V = (
      C1_V_adj + C2_V_adj + C3_V_adj + C4_V_adj +
        C5_V_adj + C6_V_adj + C7_V_adj + C8_V_adj +
        H1_adj + H2_adj + H3_adj + H6_V_adj + H7_adj + H8_V_adj
    ) / 14,
    
    # Population-weighted Control Index
    ControlIndex_PopWeighted = (1 - vax_rate) * ControlIndex_NV + 
      vax_rate * ControlIndex_V
  )

cat("✅ Control Index created\n\n")

# STEP 4: Validation

cat("=== VALIDATION ===\n\n")

# Summary statistics
ch_summary <- oxd %>%
  summarize(
    Mean = mean(ControlIndex_PopWeighted, na.rm = TRUE),
    SD = sd(ControlIndex_PopWeighted, na.rm = TRUE),
    Min = min(ControlIndex_PopWeighted, na.rm = TRUE),
    Max = max(ControlIndex_PopWeighted, na.rm = TRUE),
    N_complete = sum(!is.na(ControlIndex_PopWeighted))
  )

cat("Our Control Index:\n")
print(kable(ch_summary, digits = 2))

# Compare with Oxford's version
comparison <- oxd %>%
  summarize(
    Our_Control = mean(ControlIndex_PopWeighted, na.rm = TRUE),
    Oxford_CH = mean(ContainmentHealthIndex_WeightedAverage, na.rm = TRUE),
    Difference = Our_Control - Oxford_CH,
    Correlation = cor(ControlIndex_PopWeighted,
                      ContainmentHealthIndex_WeightedAverage,
                      use = "complete.obs"),
    RMSE = sqrt(mean((ControlIndex_PopWeighted - 
                        ContainmentHealthIndex_WeightedAverage)^2,
                     na.rm = TRUE))
  )

cat("\n=== COMPARISON WITH OXFORD ===\n\n")
print(kable(comparison, digits = 3))

if (comparison$Correlation > 0.98) {
  cat("\n✅ EXCELLENT: High correlation with Oxford's index!\n")
} else if (comparison$Correlation > 0.95) {
  cat("\n✅ GOOD: Strong correlation with Oxford's index\n")
} else {
  cat("\n⚠️  Moderate correlation - may need investigation\n")
}

# Component contribution analysis
cat("\n=== COMPONENT CONTRIBUTION ===\n\n")

component_means <- oxd %>%
  summarize(
    C_policies = mean((C1_NV_adj + C2_NV_adj + C3_NV_adj + C4_NV_adj +
                         C5_NV_adj + C6_NV_adj + C7_NV_adj + C8_NV_adj) / 8, 
                      na.rm = TRUE),
    H_policies = mean((H1_adj + H2_adj + H3_adj + H6_PopWeighted + 
                         H7_adj + H8_PopWeighted) / 6, 
                      na.rm = TRUE),
    Full_Index = mean(ControlIndex_PopWeighted, na.rm = TRUE)
  )

cat("Average contribution:\n")
print(kable(component_means, digits = 2))

# By year
yearly_ch <- oxd %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarize(
    Our_Control = mean(ControlIndex_PopWeighted, na.rm = TRUE),
    Oxford_CH = mean(ContainmentHealthIndex_WeightedAverage, na.rm = TRUE),
    Difference = Our_Control - Oxford_CH,
    .groups = "drop"
  )

cat("\n=== BY YEAR ===\n\n")
print(kable(yearly_ch, digits = 2))

# STEP 5: Visualization

cat("\n=== Creating Comparison Plot ===\n\n")

p_ch_comparison <- oxd %>%
  group_by(Date) %>%
  summarize(
    `Our Control` = mean(ControlIndex_PopWeighted, na.rm = TRUE),
    `Oxford C&H` = mean(ContainmentHealthIndex_WeightedAverage, na.rm = TRUE),
    `Our Stringency` = mean(StringencyIndex_PopWeighted, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = -Date, names_to = "Index", values_to = "Value") %>%
  ggplot(aes(x = Date, y = Value, color = Index, linetype = Index, linewidth = Index)) +
  geom_line() +
  scale_color_manual(
    values = c("Our Control" = "#3498DB",
               "Oxford C&H" = "black",
               "Our Stringency" = "#E74C3C")
  ) +
  scale_linetype_manual(
    values = c("Our Control" = "solid",
               "Oxford C&H" = "dashed",
               "Our Stringency" = "solid")
  ) +
  scale_linewidth_manual(
    values = c("Our Control" = 1.2,
               "Oxford C&H" = 1.5,
               "Our Stringency" = 1)
  ) +
  theme_minimal() +
  labs(
    title = "Control Index vs Stringency Index",
    subtitle = "Blue: Our Control (C1-C8 + H1-H8) | Black dashed: Oxford C&H | Red: Stringency (C1-C8)",
    x = "Date",
    y = "Index Value (0-100)",
    color = "Index",
    linetype = "Index",
    linewidth = "Index"
  ) +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold"))

print(p_ch_comparison)



# 1. Den Ordnerpfad EINMAL definieren
safeplots <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/output r/Plots"


# 2. Dateinamen definieren
name <- "comparison_index.png"

# 3. Plot speichern (file.path verbindet beides)
ggsave(
  filename = file.path(safeplots, name), 
  plot = p_ch_comparison, 
  width = 8, 
  height = 6
)


# Scatter plot
p_scatter_ch <- oxd %>%
  filter(!is.na(ControlIndex_PopWeighted),
         !is.na(ContainmentHealthIndex_WeightedAverage)) %>%
  ggplot(aes(x = ContainmentHealthIndex_WeightedAverage, 
             y = ControlIndex_PopWeighted)) +
  geom_point(alpha = 0.3, size = 1, color = "#3498DB") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
  coord_fixed() +
  theme_minimal() +
  labs(
    title = "Our Control Index vs Oxford's C&H Index",
    subtitle = paste("Correlation:", round(comparison$Correlation, 3)),
    x = "Oxford's Containment & Health Index",
    y = "Our Control Index (PopWeighted)"
  )

print(p_scatter_ch)

# STEP 6: Final Summary

cat("\n", rep("=", 80), "\n", sep = "")
cat("SUMMARY: CONTROL INDEX\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("✅ ControlIndex_PopWeighted created\n\n")

cat("Components (14 total):\n")
cat("  C-policies (8): C1-C8 (containment measures)\n")
cat("  H-policies (6): H1, H2, H3, H6, H7, H8 (health responses)\n\n")

cat("Population-weighted:\n")
cat("  • H6 (Masks): Weighted by vaccination status\n")
cat("  • H8 (Elderly): Weighted by vaccination status\n")
cat("  • H1, H2, H3, H7: Apply to everyone (no differentiation)\n\n")

cat("Use cases:\n")
cat("  • Comprehensive measure of government response\n")
cat("  • Includes both restrictive and health policies\n")
cat("  • Good for: Overall policy intensity\n")
cat("  • Less good for: Isolating economic costs (mixes types)\n\n")

cat("Correlation with Oxford:", round(comparison$Correlation, 3), "\n")
cat("Mean difference:", round(comparison$Difference, 2), "points\n\n")

cat(rep("=", 80), "\n\n", sep = "")

cat("Dataset now contains:\n")
cat("  • StringencyIndex_PopWeighted (C1-C8)\n")
cat("  • StringencyH1_PopWeighted (C1-C8 + H1)\n")
cat("  • ControlIndex_PopWeighted (C1-C8 + H1-H8)\n\n")

cat("All indices are population-weighted by vaccination status! ✅\n\n")

#===============================================================================
##All Variables erstellt

# ==============================================================================
# REORGANIZE DATASET: Move key indices to front
# ==============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("REORGANIZING DATASET: Moving indices to front\n")
cat(rep("=", 80), "\n\n", sep = "")

oxd <- oxd %>%
  select(
    # KEY IDENTIFIERS
    Country, Date,
    
    # OUR CUSTOM POPULATION-WEIGHTED INDICES (Main variables)
    StringencyIndex_PopWeighted,          # C1-C8 only (main measure)
    StringencyH1_PopWeighted,             # C1-C8 + H1 (Oxford definition but weighted with population)
    ControlIndex_PopWeighted,             # H1-H8 (comprehensive)
    ContainmentHealthIndex_PopWeighted,   # C1-C8+H1-H8
    
    
    # Component indices for reference
    StringencyIndex_NV,                   # C1-C8 NonVaccinated
    StringencyIndex_V,                    # C1-C8 Vaccinated
    StringencyH1_NV,                      # C1-C8+H1 NonVaccinated
    StringencyH1_V,                       # C1-C8+H1 Vaccinated
    ControlIndex_NV,                      # C1-C8+H1-H8 NonVaccinated
    ControlIndex_V,                       # C1-C8+H1-H8 Vaccinated
    
    # OXFORD'S ORIGINAL STRINGENCY INDICES (for comparison)
    StringencyIndex_NonVaccinated,
    StringencyIndex_Vaccinated,
    StringencyIndex_SimpleAverage,
    StringencyIndex_WeightedAverage,
    
    # OXFORD'S OTHER INDICES
    ContainmentHealthIndex_NonVaccinated,
    ContainmentHealthIndex_Vaccinated,
    ContainmentHealthIndex_SimpleAverage,
    ContainmentHealthIndex_WeightedAverage,
    
    GovernmentResponseIndex_NonVaccinated,
    GovernmentResponseIndex_Vaccinated,
    GovernmentResponseIndex_SimpleAverage,
    GovernmentResponseIndex_WeightedAverage,
    
    # VACCINATION DATA
    PopulationVaccinated,
    vax_rate,
    
    # KEY OUTCOME VARIABLES
    ConfirmedCases,
    ConfirmedDeaths,
    
    # H-POLICIES for mechanism analysis
    H1_adj,                               # Our calculated H1
    H2_adj,                               # Our calculated H2
    H3_adj,                               # Our calculated H3
    H6_PopWeighted,                       # Our calculated H6 (pop-weighted)
    H7_adj,                               # Our calculated H7
    H8_PopWeighted,                       # Our calculated H8 (pop-weighted)
    H1E_Public.information.campaigns,     # Raw H1 from Oxford
    
    # EVERYTHING ELSE
    everything()
  )

colnames(oxd)

cat("✅ Dataset reorganized\n\n")


# SHOW NEW STRUCTURE

cat("=== NEW DATASET STRUCTURE (First 30 columns) ===\n\n")
cat(paste(names(oxd)[1:30], collapse = "\n"), "\n\n")

cat("First 3 observations of key variables:\n\n")
print(head(select(oxd, 
                  Country, Date,
                  StringencyIndex_PopWeighted,
                  StringencyH1_PopWeighted,
                  ControlIndex_PopWeighted,
                  ContainmentHealthIndex_PopWeighted,
                  StringencyIndex_WeightedAverage,
                  PopulationVaccinated), 3))

# SUMMARY OF AVAILABLE INDICES

cat("\n", rep("=", 80), "\n", sep = "")
cat("SUMMARY: AVAILABLE INDICES FOR ANALYSIS\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("📊 PRIMARY INDICES (Use these!):\n\n")

cat("1️⃣  StringencyIndex_PopWeighted\n")
cat("   • Components: C1-C8 (containment only)\n")
cat("   • Population-weighted by vaccination status\n")
cat("   • Mean:", round(mean(oxd$StringencyIndex_PopWeighted, na.rm=TRUE), 2), "\n")
cat("   • Use for: MAIN ANALYSIS\n")
cat("   • Interpretation: Restrictive policies only\n\n")

cat("2️⃣  StringencyH1_PopWeighted\n")
cat("   • Components: C1-C8 + H1 (containment + public info)\n")
cat("   • Population-weighted by vaccination status\n")
cat("   • Mean:", round(mean(oxd$StringencyH1_PopWeighted, na.rm=TRUE), 2), "\n")
cat("   • Use for: ROBUSTNESS CHECK\n")
cat("   • Interpretation: Matches Oxford's definition\n\n")

cat("3️⃣  ControlIndex_PopWeighted\n")
cat("   • Components: H1, H2, H3, H6, H7, H8 (comprehensive)\n")
cat("   • Population-weighted by vaccination status\n")
cat("   • Mean:", round(mean(oxd$ControlIndex_PopWeighted, na.rm=TRUE), 2), "\n")
cat("   • Use for: ROBUSTNESS CHECK / ALTERNATIVE SPECIFICATION\n")
cat("   • Interpretation: All health policies\n\n")

cat("3️⃣ ContainmentHealthIndex_PopWeighted\n")
cat("   • Components: C1-C8 + H1, H2, H3, H6, H7, H8 (comprehensive)\n")
cat("   • Population-weighted by vaccination status\n")
cat("   • Mean:", round(mean(oxd$ContainmentHealthIndex_PopWeighted, na.rm=TRUE), 2), "\n")
cat("   • Use for: ROBUSTNESS CHECK / ALTERNATIVE SPECIFICATION\n")
cat("   • Interpretation: All containment + health policies\n\n")

cat("📋 OXFORD'S ORIGINAL INDICES (For comparison):\n\n")

cat("4️⃣  StringencyIndex_WeightedAverage\n")
cat("   • Oxford's official Stringency Index\n")
cat("   • Includes FLAG weighting\n")
cat("   • Mean:", round(mean(oxd$StringencyIndex_WeightedAverage, na.rm=TRUE), 2), "\n")
cat("   • Use for: Validation/comparison\n\n")

cat("5️⃣  ContainmentHealthIndex_WeightedAverage\n")
cat("   • Oxford's Containment & Health Index\n")
cat("   • Components: C1-C8 + H1, H2, H3, H6, H7, H8\n")
cat("   • Mean:", round(mean(oxd$ContainmentHealthIndex_WeightedAverage, na.rm=TRUE), 2), "\n")
cat("   • Use for: Alternative measure (includes health policies)\n\n")

# ==============================================================================
# CORRELATION MATRIX
# ==============================================================================

cat("=== CORRELATION MATRIX: Key Indices ===\n\n")

cor_matrix <- oxd %>%
  select(
    `Stringency (C1-C8) (Ours)` = StringencyIndex_PopWeighted,
    `C1-C8+H1 (Ours)` = StringencyH1_PopWeighted,
    `Control (Ours)` = ControlIndex_PopWeighted,
    `Containment (Ours)` = ContainmentHealthIndex_PopWeighted,
    `Oxford Stringency` = StringencyIndex_WeightedAverage,
    `Oxford C&H` = ContainmentHealthIndex_WeightedAverage
  ) %>%
  cor(use = "complete.obs")

print(kable(round(cor_matrix, 3), 
            caption = "Correlations between different index specifications"))


# 1. Pfad und Dateiname definieren (wie beim Plot!)
safetable <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/output r/Table Descriptives"
tex_datei <- file.path(safetable, "korrelationsmatrix_indexe.tex")

# 2. Den LaTeX-Code generieren
# Durch format = "latex" gibt kable sauberen LaTeX-Code aus
latex_output <- kable(
  round(cor_matrix, 3), 
  format = "latex", 
  caption = "Correlations between different index specifications"
)

# 3. Den Output in die Datei schreiben
cat(latex_output, file = tex_datei)


#===============================================================================
##ALLE INDEXE BERECHNET AUF TAGESDATEN, oxd bleibt bestehen, jetzt aber in gleiche Form wie die anderen Variablen bringen
#AGGREGATE INDICES UP TO QUARTERLY DATA

# ==============================================================================
# AGGREGATE INDICES TO QUARTERLY DATA (WITH C1-C8 POLICIES)
# ==============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("AGGREGATING INDICES TO QUARTERLY DATA\n")
cat("Including C1-C8 policies for mechanism analysis\n")
cat(rep("=", 80), "\n\n", sep = "")

# ==============================================================================
# STEP 1: Select ONLY index variables + identifiers + C1-C8 policies
# ==============================================================================

cat("=== Selecting index variables and C1-C8 policies ===\n\n")

oxd_indices <- oxd %>%
  select(
    # Identifiers
    Country, Date,
    
    # OUR MAIN INDICES (Population-weighted)
    StringencyIndex_PopWeighted,          # C1-C8
    StringencyH1_PopWeighted,             # C1-C8 + H1
    ControlIndex_PopWeighted,             # C1-C8 + H1-H8
    ContainmentHealthIndex_PopWeighted,   # Also C1-C8 + H1-H8
    
    # Component indices (NV/V)
    StringencyIndex_NV,
    StringencyIndex_V,
    StringencyH1_NV,
    StringencyH1_V,
    ControlIndex_NV,
    ControlIndex_V,
    
    # OXFORD'S STRINGENCY INDICES
    StringencyIndex_NonVaccinated,
    StringencyIndex_Vaccinated,
    StringencyIndex_SimpleAverage,
    StringencyIndex_WeightedAverage,
    
    # OXFORD'S CONTAINMENT & HEALTH INDICES
    ContainmentHealthIndex_NonVaccinated,
    ContainmentHealthIndex_Vaccinated,
    ContainmentHealthIndex_SimpleAverage,
    ContainmentHealthIndex_WeightedAverage,
    
    # OXFORD'S GOVERNMENT RESPONSE INDICES
    GovernmentResponseIndex_NonVaccinated,
    GovernmentResponseIndex_Vaccinated,
    GovernmentResponseIndex_SimpleAverage,
    GovernmentResponseIndex_WeightedAverage,
    
    # VACCINATION DATA
    PopulationVaccinated,
    vax_rate,
    
    # C1-C8 POLICIES (for mechanism analysis)
    C1_NV_adj,  # School closing
    C2_NV_adj,  # Workplace closing
    C3_NV_adj,  # Cancel public events
    C4_NV_adj,  # Restrictions on gatherings
    C5_NV_adj,  # Close public transport
    C6_NV_adj,  # Stay at home requirements
    C7_NV_adj,  # Restrictions on internal movement
    C8_NV_adj,  # International travel controls
    
    C1_V_adj,   # School closing (vaccinated)
    C2_V_adj,   # Workplace closing (vaccinated)
    C3_V_adj,   # Cancel public events (vaccinated)
    C4_V_adj,   # Restrictions on gatherings (vaccinated)
    C5_V_adj,   # Close public transport (vaccinated)
    C6_V_adj,   # Stay at home requirements (vaccinated)
    C7_V_adj,   # Restrictions on internal movement (vaccinated)
    C8_V_adj,   # International travel controls (vaccinated)
    
    # H-POLICIES (for mechanism analysis)
    H1_adj,
    H2_adj,
    H3_adj,
    H6_PopWeighted,
    H7_adj,
    H8_PopWeighted
  )

cat("✅ Selected", ncol(oxd_indices) - 2, "variables (", nrow(oxd_indices), "observations)\n\n")

# ==============================================================================
# STEP 2: Create Quarter variable (Format: Q1.2020)
# ==============================================================================

cat("=== Creating Quarter variable ===\n\n")

oxd_indices <- oxd_indices %>%
  mutate(
    Year = year(Date),
    Quarter_Num = quarter(Date),
    Quarter = paste0("Q", Quarter_Num, ".", Year)
  )

# Show unique quarters
unique_quarters <- sort(unique(oxd_indices$Quarter))
cat("Available quarters:\n")
cat(paste(head(unique_quarters, 12), collapse = ", "), "\n")
cat("...\n")
cat(paste(tail(unique_quarters, 12), collapse = ", "), "\n")
cat("Total quarters:", length(unique_quarters), "\n\n")

# ==============================================================================
# STEP 3: Aggregate to quarterly level
# ==============================================================================

cat("=== Aggregating to quarterly level ===\n\n")

oxd_quarterly <- oxd_indices %>%
  group_by(Country, Quarter, Year, Quarter_Num) %>%
  summarize(
    # OUR MAIN INDICES (average over quarter)
    StringencyIndex_PopWeighted = mean(StringencyIndex_PopWeighted, na.rm = TRUE),
    StringencyH1_PopWeighted = mean(StringencyH1_PopWeighted, na.rm = TRUE),
    ControlIndex_PopWeighted = mean(ControlIndex_PopWeighted, na.rm = TRUE),
    ContainmentHealthIndex_PopWeighted = mean(ContainmentHealthIndex_PopWeighted, na.rm = TRUE),
    
    # Component indices
    StringencyIndex_NV = mean(StringencyIndex_NV, na.rm = TRUE),
    StringencyIndex_V = mean(StringencyIndex_V, na.rm = TRUE),
    StringencyH1_NV = mean(StringencyH1_NV, na.rm = TRUE),
    StringencyH1_V = mean(StringencyH1_V, na.rm = TRUE),
    ControlIndex_NV = mean(ControlIndex_NV, na.rm = TRUE),
    ControlIndex_V = mean(ControlIndex_V, na.rm = TRUE),
    
    # OXFORD'S STRINGENCY INDICES
    StringencyIndex_NonVaccinated = mean(StringencyIndex_NonVaccinated, na.rm = TRUE),
    StringencyIndex_Vaccinated = mean(StringencyIndex_Vaccinated, na.rm = TRUE),
    StringencyIndex_SimpleAverage = mean(StringencyIndex_SimpleAverage, na.rm = TRUE),
    StringencyIndex_WeightedAverage = mean(StringencyIndex_WeightedAverage, na.rm = TRUE),
    
    # OXFORD'S CONTAINMENT & HEALTH INDICES
    ContainmentHealthIndex_NonVaccinated = mean(ContainmentHealthIndex_NonVaccinated, na.rm = TRUE),
    ContainmentHealthIndex_Vaccinated = mean(ContainmentHealthIndex_Vaccinated, na.rm = TRUE),
    ContainmentHealthIndex_SimpleAverage = mean(ContainmentHealthIndex_SimpleAverage, na.rm = TRUE),
    ContainmentHealthIndex_WeightedAverage = mean(ContainmentHealthIndex_WeightedAverage, na.rm = TRUE),
    
    # OXFORD'S GOVERNMENT RESPONSE INDICES
    GovernmentResponseIndex_NonVaccinated = mean(GovernmentResponseIndex_NonVaccinated, na.rm = TRUE),
    GovernmentResponseIndex_Vaccinated = mean(GovernmentResponseIndex_Vaccinated, na.rm = TRUE),
    GovernmentResponseIndex_SimpleAverage = mean(GovernmentResponseIndex_SimpleAverage, na.rm = TRUE),
    GovernmentResponseIndex_WeightedAverage = mean(GovernmentResponseIndex_WeightedAverage, na.rm = TRUE),
    
    # VACCINATION (end-of-quarter value)
    PopulationVaccinated = last(PopulationVaccinated, na_rm = TRUE),
    vax_rate = last(vax_rate, na_rm = TRUE),
    
    # C1-C8 POLICIES (average over quarter)
    C1_NV_adj = mean(C1_NV_adj, na.rm = TRUE),
    C2_NV_adj = mean(C2_NV_adj, na.rm = TRUE),
    C3_NV_adj = mean(C3_NV_adj, na.rm = TRUE),
    C4_NV_adj = mean(C4_NV_adj, na.rm = TRUE),
    C5_NV_adj = mean(C5_NV_adj, na.rm = TRUE),
    C6_NV_adj = mean(C6_NV_adj, na.rm = TRUE),
    C7_NV_adj = mean(C7_NV_adj, na.rm = TRUE),
    C8_NV_adj = mean(C8_NV_adj, na.rm = TRUE),
    
    C1_V_adj = mean(C1_V_adj, na.rm = TRUE),
    C2_V_adj = mean(C2_V_adj, na.rm = TRUE),
    C3_V_adj = mean(C3_V_adj, na.rm = TRUE),
    C4_V_adj = mean(C4_V_adj, na.rm = TRUE),
    C5_V_adj = mean(C5_V_adj, na.rm = TRUE),
    C6_V_adj = mean(C6_V_adj, na.rm = TRUE),
    C7_V_adj = mean(C7_V_adj, na.rm = TRUE),
    C8_V_adj = mean(C8_V_adj, na.rm = TRUE),
    
    # Population-weighted C-policies
    C1_PopWeighted = (1 - last(vax_rate, na_rm = TRUE)) * mean(C1_NV_adj, na.rm = TRUE) + 
      last(vax_rate, na_rm = TRUE) * mean(C1_V_adj, na.rm = TRUE),
    C2_PopWeighted = (1 - last(vax_rate, na_rm = TRUE)) * mean(C2_NV_adj, na.rm = TRUE) + 
      last(vax_rate, na_rm = TRUE) * mean(C2_V_adj, na.rm = TRUE),
    C3_PopWeighted = (1 - last(vax_rate, na_rm = TRUE)) * mean(C3_NV_adj, na.rm = TRUE) + 
      last(vax_rate, na_rm = TRUE) * mean(C3_V_adj, na.rm = TRUE),
    C4_PopWeighted = (1 - last(vax_rate, na_rm = TRUE)) * mean(C4_NV_adj, na.rm = TRUE) + 
      last(vax_rate, na_rm = TRUE) * mean(C4_V_adj, na.rm = TRUE),
    C5_PopWeighted = (1 - last(vax_rate, na_rm = TRUE)) * mean(C5_NV_adj, na.rm = TRUE) + 
      last(vax_rate, na_rm = TRUE) * mean(C5_V_adj, na.rm = TRUE),
    C6_PopWeighted = (1 - last(vax_rate, na_rm = TRUE)) * mean(C6_NV_adj, na.rm = TRUE) + 
      last(vax_rate, na_rm = TRUE) * mean(C6_V_adj, na.rm = TRUE),
    C7_PopWeighted = (1 - last(vax_rate, na_rm = TRUE)) * mean(C7_NV_adj, na.rm = TRUE) + 
      last(vax_rate, na_rm = TRUE) * mean(C7_V_adj, na.rm = TRUE),
    C8_PopWeighted = (1 - last(vax_rate, na_rm = TRUE)) * mean(C8_NV_adj, na.rm = TRUE) + 
      last(vax_rate, na_rm = TRUE) * mean(C8_V_adj, na.rm = TRUE),
    
    # H-POLICIES (average over quarter)
    H1_adj = mean(H1_adj, na.rm = TRUE),
    H2_adj = mean(H2_adj, na.rm = TRUE),
    H3_adj = mean(H3_adj, na.rm = TRUE),
    H6_PopWeighted = mean(H6_PopWeighted, na.rm = TRUE),
    H7_adj = mean(H7_adj, na.rm = TRUE),
    H8_PopWeighted = mean(H8_PopWeighted, na.rm = TRUE),
    
    # Metadata
    N_days = n(),  # Number of days in quarter with data
    
    .groups = "drop"
  ) %>%
  # Sort by Country and Quarter
  arrange(Country, Year, Quarter_Num) %>%
  # Reorganize: Put main indices first, then C-policies, then H-policies
  select(
    # Identifiers
    Country, Quarter, Year, Quarter_Num,
    
    # Our 4 main indices
    StringencyIndex_PopWeighted,
    StringencyH1_PopWeighted,
    ControlIndex_PopWeighted,
    ContainmentHealthIndex_PopWeighted,
    
    # C1-C8 policies (population-weighted - for mechanism analysis)
    C1_PopWeighted,  # School closing
    C2_PopWeighted,  # Workplace closing
    C3_PopWeighted,  # Cancel public events
    C4_PopWeighted,  # Restrictions on gatherings
    C5_PopWeighted,  # Close public transport
    C6_PopWeighted,  # Stay at home
    C7_PopWeighted,  # Internal movement
    C8_PopWeighted,  # International travel
    
    # H-policies (for mechanism analysis)
    H1_adj,
    H2_adj,
    H3_adj,
    H6_PopWeighted,
    H7_adj,
    H8_PopWeighted,
    
    # Vaccination
    PopulationVaccinated,
    vax_rate,
    
    # Everything else
    everything()
  )

cat("✅ Aggregated to", nrow(oxd_quarterly), "country-quarter observations\n\n")

# ==============================================================================
# STEP 4: Summary and validation
# ==============================================================================

cat("=== SUMMARY ===\n\n")

summary_quarterly <- oxd_quarterly %>%
  summarize(
    N_countries = n_distinct(Country),
    N_quarters = n_distinct(Quarter),
    N_observations = n(),
    First_quarter = min(Quarter),
    Last_quarter = max(Quarter),
    Mean_days_per_quarter = mean(N_days, na.rm = TRUE)
  )

print(kable(summary_quarterly, 
            caption = "Quarterly Dataset Summary"))

# Summary of C-policies
cat("\n\n=== C-POLICIES: Summary Statistics ===\n\n")

c_policies_summary <- oxd_quarterly %>%
  summarize(
    across(c(C1_PopWeighted, C2_PopWeighted, C3_PopWeighted, C4_PopWeighted,
             C5_PopWeighted, C6_PopWeighted, C7_PopWeighted, C8_PopWeighted),
           list(Mean = ~mean(., na.rm = TRUE),
                SD = ~sd(., na.rm = TRUE),
                Min = ~min(., na.rm = TRUE),
                Max = ~max(., na.rm = TRUE)),
           .names = "{.col}_{.fn}")
  ) %>%
  pivot_longer(everything(),
               names_to = c("Policy", ".value"),
               names_pattern = "(.+)_(Mean|SD|Min|Max)")

print(kable(c_policies_summary, digits = 2))

# Show first observations with C-policies
cat("\n\n=== First 5 observations (with C-policies) ===\n\n")
print(kable(head(select(oxd_quarterly, 
                        Country, Quarter,
                        StringencyIndex_PopWeighted,
                        C1_PopWeighted, C2_PopWeighted, C6_PopWeighted,
                        H1_adj, H6_PopWeighted), 5), 
            digits = 2))

# Check Germany
cat("\n\n=== Germany: Quarterly data with C-policies ===\n\n")
germany_quarterly <- oxd_quarterly %>%
  filter(Country == "DEU") %>%
  select(Quarter, 
         StringencyIndex_PopWeighted,
         C1_PopWeighted, C2_PopWeighted, C3_PopWeighted,
         C4_PopWeighted, C5_PopWeighted, C6_PopWeighted,
         C7_PopWeighted, C8_PopWeighted,
         PopulationVaccinated)

print(kable(head(germany_quarterly, 8), digits = 2))

# ==============================================================================
# STEP 5: Which C-policies varied most?
# ==============================================================================

cat("\n\n=== WHICH C-POLICIES VARIED MOST? ===\n\n")

c_variation <- oxd_quarterly %>%
  summarize(
    across(c(C1_PopWeighted:C8_PopWeighted),
           list(SD = ~sd(., na.rm = TRUE),
                CV = ~sd(., na.rm = TRUE) / mean(., na.rm = TRUE)),
           .names = "{.col}_{.fn}")
  ) %>%
  pivot_longer(everything(),
               names_to = c("Policy", "Metric"),
               names_pattern = "(.+)_(SD|CV)",
               values_to = "Value") %>%
  pivot_wider(names_from = Metric, values_from = Value) %>%
  arrange(desc(CV))

policy_labels <- c(
  "C1_PopWeighted" = "C1: School closing",
  "C2_PopWeighted" = "C2: Workplace closing",
  "C3_PopWeighted" = "C3: Cancel events",
  "C4_PopWeighted" = "C4: Gatherings",
  "C5_PopWeighted" = "C5: Public transport",
  "C6_PopWeighted" = "C6: Stay at home",
  "C7_PopWeighted" = "C7: Internal movement",
  "C8_PopWeighted" = "C8: International travel"
)

c_variation <- c_variation %>%
  mutate(Policy_Label = policy_labels[Policy])

print(kable(c_variation, digits = 3,
            caption = "C-policies ranked by variation (CV = Coefficient of Variation)"))

cat("\nInterpretation:\n")
cat("  Higher CV = More variation across countries/time\n")
cat("  → These policies are most useful for identification\n\n")

# ==============================================================================
# STEP 6: Visualization - C-policies over time
# ==============================================================================

cat("=== Creating C-policies visualization ===\n\n")

# Average C-policies over time
p_c_policies <- oxd_quarterly %>%
  group_by(Quarter, Year, Quarter_Num) %>%
  summarize(
    across(C1_PopWeighted:C8_PopWeighted,
           ~mean(., na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  arrange(Year, Quarter_Num) %>%
  mutate(Quarter_Seq = row_number()) %>%
  pivot_longer(cols = C1_PopWeighted:C8_PopWeighted,
               names_to = "Policy",
               values_to = "Value") %>%
  mutate(Policy_Label = policy_labels[Policy]) %>%
  ggplot(aes(x = Quarter_Seq, y = Value, color = Policy_Label)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  labs(
    title = "C-Policies Over Time (Quarterly Average)",
    subtitle = "Which containment policies were used when?",
    x = "Quarter",
    y = "Policy Stringency (0-100)",
    color = "Policy"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

print(p_c_policies)



oxd_q <- oxd_quarterly
rm(oxd_quarterly, oxd0, oxd1, oxd2, oxd_long_che, oxd_long_mean, oxd_long_mean_2020_21, germany_quarterly, c_policies_summary, 
   c_variation, ch_summary, comparison, component_means, bias_by_vaxrate, control_comparison, desc_stats, divergence_check, gap_by_country, gap_stats, 
   irl_fc, kor_add, max_values, mdd, mdd_irl, missing_check, summ_country, summ_stringency_country, summary_comparison, summary_quarterly, summary_stats,
   test_obs, validation_summary, yearly_ch, yearly_check, yearly_validation, pop, gdp2, cor_matrix, countries)

##GIBT UNS DEN oxd_q mit allen relevanten Indexen plus C1-C8 und H1-H8 auch adjusted)
#Zwei Main Variablen: Stringency_PopWeighted und ControlIndex_PopWeighted

#-------------------------------------------------------------------------------
## Load nominal rGDP data Chain Linked

nGDP<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/nGDP2.csv", header=TRUE, sep=",")

nGDP <- nGDP %>%
  filter(Adjustment == "Calendar and seasonally adjusted")

nGDP <- nGDP %>%
  rename(
    Country = REF_AREA,
    nGDP = OBS_VALUE,
    Quarter = TIME_PERIOD
  ) %>%
  mutate(
    Quarter = sub("(\\d{4})-(Q\\d)", "\\2.\\1", Quarter)
  )

qdata <- qdata %>%
  left_join(nGDP %>% select(Country, Quarter, nGDP), 
            by = c("Country", "Quarter"))

#-------------------------------------------------------------------------------
## Chain linked rGDP in USD data (ANNUAL LEVEL-> ONLY FOR GROWTH RATE)
##including data back to Q1.2010 for the HP Filter

rrgdp <- read_excel("rrgdp2.xlsx")

rrgdp <- rrgdp %>%
  rename(
    Country = Country,
    rGDP = rGDP,
    Quarter = Quarter
  ) %>%
  mutate(
    Quarter = sub("(\\d{4})-(Q\\d)", "\\2.\\1", Quarter)
)

qdata <- qdata %>%
  left_join(rrgdp %>% select(Country, Quarter, rGDP), 
            by = c("Country", "Quarter"))




#-------------------------------------------------------------------------------
## Chain linked rGDP in LC data (Quarterly)

rGDP_LC <- read_excel("rGDP_lc2.xlsx")

head(rGDP_LC)

rGDP_LC <- rGDP_LC %>%
  mutate(
    Quarter = sub("(\\d{4})-(Q\\d)", "\\2.\\1", Quarter)
  )

qdata <- qdata %>%
  left_join(rGDP_LC %>% select(Country, Quarter, rGDP_LC), 
            by = c("Country", "Quarter"))

#------------------------------------------------------------------------------
## Add nominal GDP in LC NGDP_2019
fm <- read_excel("fiscal_classified_v1.3.xlsx")
gdp_lookup <- fm %>%
  filter(!is.na(NGDP_2019)) %>%
  group_by(Country) %>%              # ggf. + ifs, falls in qdata vorhanden
  summarise(NGDP_2019 = first(NGDP_2019), .groups = "drop")

# Optional: prüfen, ob mehrere unterschiedliche Werte je Land existieren
conflicts <- fm %>%
  filter(!is.na(NGDP_2019)) %>%
  group_by(Country) %>%
  summarise(n_vals = n_distinct(NGDP_2019), .groups = "drop") %>%
  filter(n_vals > 1)
if (nrow(conflicts) > 0) {
  message("Achtung: Mehrere NGDP_2019-Werte pro Land gefunden. Es wird der erste verwendet.")
  print(conflicts)
}

# 2) In qdata mergen
qdata <- qdata %>%
  left_join(gdp_lookup, by = "Country")




# --- 1. Load Data -------------------------------------------------------------


library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(stringr)


df <- read_excel("fiscal_classified_v1.3.xlsx")
#df <- read_excel("fiscal_classified_v1_3.xlsx", sheet = "Fiscal Policy")

cat("Total observations loaded:", nrow(df), "\n")
cat("broad_fiscal distribution:\n")
print(table(df$broad_fiscal, useNA = "ifany"))


# --- 2. Define Code-Level Classification Mapping -----------------------------
#
#  This mapping implements the protocol from Section 3 of the classification
#  document. Each PolicyCode is assigned to exactly one transmission channel
#  based on its primary transmission mechanism as established in the theoretical
#  framework (Section 2 of the paper).
#
#  CP: Capacity Preservation (preserves productive structures, reduces rho_eff)
#  DI: Demand Injection (increases disposable income, operates through alpha_F^DI)
#  H:  Health expenditure (endogenous cost c_H * theta_k, not a policy instrument)

classification_map <- tribble(
  ~PolicyCode, ~transmission_channel, ~rationale,
  
  # --- Category 1: Revenue Measures to Protect Businesses (all CP) ---
  "1",  "CP", "Accelerated depreciation: reduces firm tax burden, preserves cash flow",
  "2",  "CP", "Loss carry-forward: allows offset of losses, preserves solvency",
  "3",  "CP", "Broaden deductibility: reduces effective firm tax rate",
  "4",  "CP", "Tax credits (business): reduces firm tax liability",
  "5",  "CP", "Deferral of tax filing (business): liquidity preservation",
  "6",  "CP", "Deferral of tax payments (business): liquidity preservation",
  "7",  "CP", "Tax rate reduction (business): reduces structural cost burden",
  "8",  "CP", "Tax amnesty (business): removes penalty-driven insolvency risk",
  "9",  "CP", "Accelerating refunds (business): injects owed liquidity to firms",
  "10", "CP", "Lower advance payment: reduces prepayment burden",
  "11", "CP", "Suspend debt collection: prevents enforcement-driven failure",
  "12", "CP", "Suspend audit activities: administrative relief during crisis",
  "13", "CP", "Tax exemption (business): eliminates specific tax obligations",
  "14", "CP", "Tax waiver/suspension (business): includes bankruptcy moratoriums",
  
  # --- Category 2: Revenue Measures to Protect Individuals ---
  # Deferrals -> CP (Decision Rule 3); all others -> DI (income channel)
  "15", "CP", "Deferral of tax filing (individual): liquidity preservation",
  "16", "CP", "Deferral of tax payments (individual): liquidity preservation",
  "17", "DI", "Tax rate reduction (individual): permanent disposable income increase",
  "18", "DI", "Tax amnesty (individual): increases household disposable income",
  "19", "DI", "Broaden deductibility (individual): lower effective tax rate on income",
  "20", "DI", "Tax credits (individual): direct increase in disposable income",
  "21", "DI", "Tax exemption (individual): eliminates tax obligation, raises income",
  "22", "DI", "Tax waiver/suspension (individual): reduces household tax burden",
  
  # --- Category 3: Revenue Measures to Boost Consumption/Demand (all DI) ---
  "25", "DI", "Consumption tax exemption: reduces consumer prices, stimulates spending",
  "26", "DI", "Lower VAT rates: reduces consumer prices, multiplier channel",
  
  # --- Category 4: Expenditure to Boost Consumption/Demand (all DI) ---
  "27", "DI", "Infrastructure investment: government spending multiplier",
  "28", "DI", "Green investment: government spending multiplier",
  "29", "DI", "Reactivate local tourism: consumption stimulus",
  
  # --- Category 5/5.5: Health Expenditure (all H) ---
  "30",      "H", "Lower tax on medical items: health system cost, calibrates c_H",
  "31",      "H", "Supply of low-cost medical items: health system cost",
  "32",      "H", "Supply of high-cost medical items: health system cost",
  "33",      "H", "Targeted health infrastructure: health system cost",
  "34",      "H", "Expansion of health human resources: health system cost",
  "general", "H", "General health related spending: health system cost",
  
  # --- Codes 23, 24: Health-adjacent (H) ---
  "23", "H", "Tax-free alcohol for disinfectant production: health supply facilitation",
  "24", "H", "Customs duty relief on medical goods: health supply facilitation",
  
  # --- Category 6: Expenditure Measures to Individuals ---
  # Codes 35-38: income to households -> DI; Code 39: wage subsidies -> CP
  "35", "DI", "Direct cash transfers to individuals: canonical demand injection",
  "36", "DI", "Expansion of unemployment benefits: income flow to unemployed",
  "37", "DI", "Temporary expansion of existing benefits: augments transfer programs",
  "38", "DI", "Supplementary ad hoc programs: pandemic income support to individuals",
  "39", "CP", "Wage compensation/short-time work: preserves employer-employee matches",
  
  # --- Category 7: Credit and Equity Measures ---
  # Firm-directed -> CP; Household-directed -> DI
  "40", "CP", "Preferential loans to firms: preserves firm solvency",
  "41", "CP", "Equity/quasi-equity investments: preserves firm capital structure",
  "42", "DI", "Preferential loans to households: liquidity to households, spending channel",
  "43", "CP", "Loan guarantees for business: preserves firm credit access",
  
  # --- Category 8: Expenditure Measures for Businesses (all CP) ---
  "45", "CP", "Income support general (business): preserves firm operations",
  "46", "CP", "Income support non-profit: preserves organizational capacity",
  "47", "CP", "Income support specific (sectoral): preserves sector capacity",
  "48", "CP", "Income support education sector: preserves institutional capacity",
  
  # --- Category 9: Transfers to Local Governments (CP) ---
  "49", "CP", "Income support for local governments: preserves public service capacity"
)


# --- 3. Classify Code "other" Case-by-Case -----------------------------------
#
#  All 41 "other" entries are broad_fiscal=2 (extensions) with fiscal weight = 0.
#  Classification is based on measure description content.
#  Since fiscal weight is zero, these do not affect any quantitative result.

classify_other <- function(description, country) {
  desc <- tolower(description)
  
  # CP indicators: short-time work, wage subsidy, ERTE, CIG, CEWS, PPP,
  #   firm support, business support, employer, tax deferral (business)
  cp_patterns <- c(
    "cews", "kurzarbeit", "short.time", "short-time", "erte",
    "cig ", "wage.support", "wage subsidy", "job.retention",
    "payroll tax", "ppp", "paycheck protection",
    "apoiar", "retoma", "social cooperative",
    "stamp.duty", "electronic invoice",
    "public contract", "loss continuity", "filing date",
    "interest remission", "administrative flexibility",
    "irc", "lodging tax", "vehicle ownership tax.*extended",
    "customs duty", "accommodation.*vat", "tax.*amendment",
    "refrendo"
  )
  
  # DI indicators: pension, household, individual, rent, social tariff,
  #   unemployment exclusion, consumer price
  di_patterns <- c(
    "pensi[oó]n", "pension", "bono social", "social tariff",
    "rent assistance", "alquiler", "unemployment.*exclusion",
    "lpg price", "property tax.*condonation", "predial",
    "sick.leave benefit", "incapacidad temporal",
    "surplus.*social service"
  )
  
  # Check CP patterns
  
  for (p in cp_patterns) {
    if (grepl(p, desc, perl = TRUE)) return("CP")
  }
  
  # Check DI patterns
  for (p in di_patterns) {
    if (grepl(p, desc, perl = TRUE)) return("DI")
  }
  
  # Default for unmatched "other": CP (most are business-related extensions)
  return("CP")
}


# --- 4. Apply Classification --------------------------------------------------

# Convert PolicyCode to character for consistent matching
df <- df %>%
  mutate(PolicyCode_str = as.character(PolicyCode))

# Step 4a: Map all coded measures via the classification map
df <- df %>%
  left_join(
    classification_map %>% select(PolicyCode, transmission_channel),
    by = c("PolicyCode_str" = "PolicyCode")
  )

# Step 4b: Classify code "other" case-by-case
mask_other <- !is.na(df$PolicyCode_str) & df$PolicyCode_str == "other" &
  df$broad_fiscal %in% c(1, 2, 3)
if (any(mask_other)) {
  df$transmission_channel[mask_other] <- mapply(
    classify_other,
    df$description[mask_other],
    df$Country[mask_other]
  )
}

# Step 4c: Handle measures with missing PolicyCode (NA)
#  - 7 EIB Pan-European Guarantee Fund entries (broad_fiscal=3):
#    EU-level loan guarantees -> CP
#  - ISL extensions (broad_fiscal=2) without PolicyCode:
#    classified based on description content
mask_na_fiscal <- is.na(df$PolicyCode) & df$broad_fiscal %in% c(1, 2, 3)
if (any(mask_na_fiscal)) {
  for (i in which(mask_na_fiscal)) {
    desc <- tolower(df$description[i])
    if (grepl("eib|european investment bank|guarantee fund", desc)) {
      df$transmission_channel[i] <- "CP"
    } else if (grepl("pension savings|pension withdrawal", desc)) {
      df$transmission_channel[i] <- "DI"
    } else if (grepl("arts|culture|media", desc)) {
      df$transmission_channel[i] <- "CP"
    } else if (grepl("grants to local|local authorit", desc)) {
      df$transmission_channel[i] <- "CP"
    } else {
      df$transmission_channel[i] <- "CP"  # Conservative default
    }
  }
}

# Step 4d: Set monetary policy (broad_fiscal=0) to NA
df$transmission_channel[df$broad_fiscal == 0] <- NA_character_

# Verify: any fiscal measures without classification?
fiscal <- df %>% filter(broad_fiscal %in% c(1, 2, 3))
n_unclassified <- sum(is.na(fiscal$transmission_channel))
cat("\nFiscal measures without classification:", n_unclassified, "\n")

if (n_unclassified > 0) {
  cat("Unclassified codes:\n")
  print(
    fiscal %>%
      filter(is.na(transmission_channel)) %>%
      count(PolicyCode_str, PolicyCategory) %>%
      arrange(desc(n))
  )
}


# --- 5. Add Robustness Flags -------------------------------------------------

df <- df %>%
  mutate(
    is_extension = as.integer(broad_fiscal == 2),
    is_eu        = as.integer(broad_fiscal == 3),
    is_deferral  = as.integer(category == 3),
    is_blw       = as.integer(category == 2)
  ) %>%
  # Replace NAs in flags with 0 (for non-fiscal measures)
  mutate(across(c(is_extension, is_eu, is_deferral, is_blw),
                ~replace_na(., 0L)))


# --- 6. Validation Checks ----------------------------------------------------

cat("\n============================================================\n")
cat("  VALIDATION REPORT\n")
cat("============================================================\n\n")

# 6a. Distribution of transmission channels
cat("--- Transmission Channel Distribution (broad_fiscal = 1) ---\n")
core <- df %>% filter(broad_fiscal == 1)
channel_dist <- core %>%
  group_by(transmission_channel) %>%
  summarise(
    n_measures  = n(),
    total_gdp   = sum(broad_fiscal_gdp, na.rm = TRUE),
    mean_gdp    = mean(broad_fiscal_gdp, na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  mutate(
    pct_measures = round(n_measures / sum(n_measures) * 100, 1),
    pct_gdp      = round(total_gdp / sum(total_gdp) * 100, 1)
  )
print(as.data.frame(channel_dist))

# 6b. Cross-validation: IMF category vs transmission channel
cat("\n--- Cross-Tabulation: IMF Category x Transmission Channel ---\n")
cat("    (broad_fiscal = 1 only)\n")
ct <- table(
  IMF_Category = core$category,
  Channel      = core$transmission_channel,
  useNA        = "ifany"
)
print(ct)

cat("\nExpected pattern:\n")
cat("  Above-the-line (cat=1): mix of DI and CP\n")
cat("  Below-the-line (cat=2): exclusively CP (loans & guarantees)\n")
cat("  Tax deferrals  (cat=3): exclusively CP (per protocol)\n")

# Verify below-the-line is all CP (except Code 42: household loans -> DI by design)
blw_non_cp <- core %>%
  filter(category == 2, transmission_channel != "CP")
blw_unexpected <- blw_non_cp %>%
  filter(PolicyCode_str != "42")
if (nrow(blw_unexpected) == 0) {
  cat("  CHECK PASSED: All BLW measures are CP (except Code 42: household loans -> DI).\n")
  if (nrow(blw_non_cp) > 0) {
    cat(sprintf("    Code 42 (household loans): %d measures classified as DI (expected).\n",
                nrow(blw_non_cp)))
  }
} else {
  cat("  WARNING: Unexpected BLW measures not classified as CP:\n")
  print(blw_unexpected %>% select(ID, Country, PolicyCode_str, transmission_channel))
}

# Verify deferrals are all CP
def_non_cp <- core %>%
  filter(category == 3, transmission_channel != "CP")
if (nrow(def_non_cp) == 0) {
  cat("  CHECK PASSED: All tax deferrals classified as CP.\n")
} else {
  cat("  WARNING: Tax deferrals not classified as CP:\n")
  print(def_non_cp %>% select(ID, Country, PolicyCode_str, transmission_channel))
}

# 6c. Fiscal volume by channel and country (top 10 by total)
cat("\n--- Fiscal Volume by Country and Channel (top 10, broad_fiscal=1) ---\n")
country_channel <- core %>%
  group_by(Country, transmission_channel) %>%
  summarise(gdp_pct = sum(broad_fiscal_gdp, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from  = transmission_channel,
    values_from = gdp_pct,
    values_fill = 0
  ) %>%
  mutate(Total = CP + DI + H) %>%
  arrange(desc(Total))
print(as.data.frame(head(country_channel, 15)))

# 6d. Code 39 volume check
cat("\n--- Code 39 (Wage Compensation) Volume ---\n")
c39 <- core %>% filter(PolicyCode_str == "39")
cat("  N measures:", nrow(c39), "\n")
cat("  Total % GDP:", round(sum(c39$broad_fiscal_gdp, na.rm = TRUE), 4), "\n")
cat("  Classified as:", unique(c39$transmission_channel), "\n")

# 6e. Deferral volume
cat("\n--- Tax Deferrals (IMF cat=3) Volume ---\n")
deferrals <- core %>% filter(category == 3)
cat("  N measures:", nrow(deferrals), "\n")
cat("  Total % GDP:", round(sum(deferrals$broad_fiscal_gdp, na.rm = TRUE), 4), "\n")
cat("  Classified as:", unique(deferrals$transmission_channel), "\n")


# --- 7. Export ----------------------------------------------------------------

# Drop helper column
df <- df %>% select(-PolicyCode_str)

# Write to xlsx
output_path <- "fiscal_classified_v1_4.xlsx"
write_xlsx(df, output_path)
cat("\n============================================================\n")
cat("  Output written to:", output_path, "\n")
cat("============================================================\n")

# Summary
cat("\nNew columns added:\n")
cat("  transmission_channel : DI / CP / H / NA (for monetary policy)\n")
cat("  is_extension         : 1 if broad_fiscal = 2\n")
cat("  is_eu                : 1 if broad_fiscal = 3\n")
cat("  is_deferral          : 1 if IMF category = 3\n")
cat("  is_blw               : 1 if IMF category = 2\n")


fm<-df
fm1<-fm
rm(df)


##drop dataframes that are no longer used
ls()

rm(
  list = c(
    "fisc_2020_21","fm_cat_long","fm_cat_summary",
    "fm_cat_wide","fm_er_country","fm_er_debug","fm_er_exp_cat1",
    "fm_er_long","fm_er_summary","fm_er_wide","fm_fiscal_2020_21",
    "fm_plot","fm_plot_long","fm_ta_er_country","fm_ta_er_country_cat1",
    "fm_ta_er_country_cat1_wide","fm_ta_er_long","fm_ta_er_long_cat1","fm_ta_er_summary",
    "fm_ta_er_summary_cat1","fm_ta_er_wide","fm_ta_exp_cat1_country","fm_ta_exp_cat1_country_wide",
    "fm_ta_exp_cat1_long","fm_ta_exp_cat1_summary","fm_ta_long","fm_ta_wide",
    "fm_totals","fm1_hh_guar",
    "fm1_with_na", "gdp_lookup", "codes_by_cat", "conflicts", "fm1_q_long", "fm1_w_long",
    "na_per_col", "rows_with_na_count", "total_na", "core", "blw_non_cp", "blw_unexpected", "c39",
    "channel_dist", "classification_map", "country_channel", "def_non_cp", "deferrals", "fiscal", "fm1_w"
  )
)



#-------------------------------------------------------------------------------
##setting qtime index

##expanding qtime
event_time_index <- 20
qdata <- qdata %>%
  mutate(qtime = TimeIndex - event_time_index)

##moving time variables in front
qdata <- qdata %>%
  relocate(Country, Quarter, qtime, TimeIndex)

# Replace NAs in the entire dataframe with 0
qdata[is.na(qdata)] <- 0


##Some checks on the data
ggplot(qdata, aes(x = Quarter, y = GDP_ln_yoy)) +
  geom_line() +  # Linienplot
  labs(title = "Real.GDP.Growth over Time",
       x = "Datum",
       y = "Wert") +
  theme_minimal() 


# Quartale als Faktor definieren, in der Reihenfolge der Zeit
qdata$Quarter <- factor(qdata$Quarter, levels = unique(qdata$Quarter))

# Eine neue Variable 'TimeIndex' erstellen, die eine laufende Nummer enth?lt
qdata <- qdata %>%
  mutate(TimeIndex = as.numeric(as.factor(Quarter)))

#-------------------------------------------------------------------------------
##get the excess mortality data


ed<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/world_mortality.csv", header=TRUE, sep=",")

ed2 <- ed %>% filter(iso3c %in% oecd_countries)

# Alternative mit korrekter lubridate Syntax
iso_week_monday <- function(y, w) {
  y <- as.integer(y); w <- as.integer(w)
  jan4 <- ymd(paste0(y, "-01-04"))
  # Korrekte Verwendung von week_start als named Parameter
  week1_mon <- jan4 - days(wday(jan4, week_start = 1) - 1L)
  week1_mon + days((w - 1L) * 7L)
}

# Gleiche Korrektur für n_iso_weeks
n_iso_weeks <- function(y) {
  y <- as.integer(y)
  jan4 <- ymd(paste0(y, "-01-04"))
  week1_mon <- jan4 - days(wday(jan4, week_start = 1) - 1L)
  dec28 <- ymd(paste0(y, "-12-28"))
  as.integer((dec28 - week1_mon) / ddays(7)) + 1L
}
# Donnerstag der ISO-Woche (garantiert im ISO-Jahr y)
iso_week_thursday <- function(y, w) {
  iso_week_monday(y, w) + days(3L)
}

# Anzahl ISO-Wochen im ISO-Jahr y (52/53)
n_iso_weeks <- function(y) {
  y <- as.integer(y)
  jan4 <- ymd(paste0(y, "-01-04"))
  week1_mon <- jan4 - days(wday(jan4, week_start = 1) - 1L)
  dec28 <- ymd(paste0(y, "-12-28"))
  as.integer((dec28 - week1_mon) / ddays(7)) + 1L
}

# 1) Weekly-Daten bereinigen; Quartal über ISO-Donnerstag bestimmen
wk <- ed2 %>%
  filter(time_unit == "weekly") %>%
  mutate(across(c(year, time), as.integer)) %>%
  filter(!is.na(year), !is.na(time), time >= 1L, time <= 53L) %>%
  mutate(
    week_start = iso_week_monday(year, time),
    week_thu   = iso_week_thursday(year, time),            # NEU: ISO-Donnerstag
    qtr        = as.yearqtr(week_thu),                     # Quartal via Donnerstag
    Quarter    = format(qtr, "Q%q.%Y")
  )

# 2) Quartalssummen mit korrekt zugewiesenem ISO-Jahr/Quartal
ed_q_weekly <- wk %>%
  group_by(iso3c, country_name, Quarter) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE), .groups = "drop") %>%
  arrange(iso3c, Quarter)

# 3) Lückenprüfung ISO-Wochen (52/53 dynamisch)
missing_weeks <- wk %>%
  distinct(iso3c, country_name, year) %>%
  rowwise() %>%
  mutate(nw = n_iso_weeks(year), exp = list(seq.int(1L, nw))) %>%
  ungroup() %>%
  unnest(exp, names_repair = "minimal") %>%
  rename(week = exp) %>%
  anti_join(wk %>% distinct(iso3c, year, time) %>% rename(week = time),
            by = c("iso3c","year","week")) %>%
  arrange(iso3c, year, week)

# 4) Vollständige Quartalsachse je Land (NA, wo Summe fehlt)
q_axis <- wk %>%
  summarise(min_q = min(qtr), max_q = max(qtr), .by = c(iso3c, country_name)) %>%
  rowwise() %>%
  mutate(q_seq = list(seq(min_q, max_q, by = 0.25))) %>%
  ungroup() %>%
  unnest(q_seq) %>%
  transmute(iso3c, country_name, Quarter = format(q_seq, "Q%q.%Y"))

ed_q_weekly_full <- q_axis %>%
  left_join(ed_q_weekly, by = c("iso3c","country_name","Quarter")) %>%
  arrange(iso3c, Quarter)


names(ed_q_weekly_full)[names(ed_q_weekly_full) == "iso3c"] <- "Country"


##add missings from monthly data

monthly<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/world_mortality_monthly.csv", header=TRUE, sep=",")
keep <- c("JPN","CRI","TUR")
# Optional: auf JPN/CRI/TUR einschränken
keep <- c("JPN","CRI","TUR")
m <- monthly %>%
  filter(time_unit == "monthly", iso3c %in% keep) %>%
  mutate(
    year  = as.integer(year),
    time  = as.integer(time)  # Monat 1..12
  ) %>%
  filter(!is.na(year), !is.na(time), time >= 1L, time <= 12L) %>%
  mutate(
    month_date = as.Date(sprintf("%d-%02d-01", year, time)),
    qtr        = as.yearqtr(month_date),
    Quarter    = format(qtr, "Q%q.%Y")
  )

# 1) Einfache Quartalssummen (nur vorhandene Quartale)
q_monthly <- m %>%
  group_by(iso3c, country_name, Quarter) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE), .groups = "drop") %>%
  arrange(iso3c, Quarter)

# 2) Vollständige Quartalsachse je Land (fehlende Quartale -> NA)
q_axis <- m %>%
  summarise(min_q = min(qtr), max_q = max(qtr), .by = c(iso3c, country_name)) %>%
  rowwise() %>%
  mutate(q_seq = list(seq(min_q, max_q, by = 0.25))) %>%
  ungroup() %>%
  unnest(q_seq) %>%
  transmute(iso3c, country_name, Quarter = format(q_seq, "Q%q.%Y"))

q_monthly_full <- q_axis %>%
  left_join(q_monthly, by = c("iso3c","country_name","Quarter")) %>%
  arrange(iso3c, Quarter)

names(q_monthly_full)[names(q_monthly_full) == "iso3c"] <- "Country"



# 1) Drei Länder aus den neuen Daten extrahieren und Spalten matchen
add3 <- q_monthly_full %>%
  filter(Country %in% c("JPN","CRI","TUR")) %>%
  select(Country, Quarter, deaths)

# 2) Sicherheit: nur Zeilen, die im Altbestand noch nicht existieren (Key = Country+Quarter)
add3_unique <- anti_join(add3,
                         ed_q_weekly_full %>% select(Country, Quarter),
                         by = c("Country","Quarter"))

# 3) Anhängen
ed_q_full_plus <- bind_rows(
  ed_q_weekly_full %>% select(Country, Quarter, deaths),
  add3_unique
) %>%
  arrange(Country, Quarter)

# Kontrolle
table(ed_q_full_plus$Country %in% c("JPN","CRI","TUR"))
ed_q_full_plus %>% count(Country) %>% arrange(Country)


outfile <- "ed_q_full_plus.csv"
write.csv(ed_q_full_plus, outfile, row.names = FALSE, fileEncoding = "UTF-8")

##missings behandeln


# 0) Harmonisieren
ed_std <- ed_q_full_plus %>%
  mutate(
    Country = as.character(Country),
    Quarter = as.character(Quarter),
    Quarter = str_replace_all(Quarter, "\\s+", "")
  )

# 1) Geeignete Wertspalte ermitteln und auf 'deaths' mappen
prio <- c("deaths", "deaths_q_prefW", "deaths_w", "deaths_m", "deaths_q_sum")
cand <- intersect(prio, names(ed_std))
if (length(cand) == 0) {
  cand <- grep("^deaths", names(ed_std), ignore.case = TRUE, value = TRUE)
}
stopifnot(length(cand) >= 1)
val_col <- cand[1]

ed_vals <- ed_std %>%
  transmute(Country, Quarter, deaths = .data[[val_col]])

# 2) Zielraster 2015Q1–2024Q4
quarters  <- paste0("Q", rep(1:4, times = 10), ".", rep(2015:2024, each = 4))
countries <- sort(unique(ed_vals$Country))
grid <- tidyr::expand_grid(Country = countries, Quarter = quarters)

# 3) Left-Join → fehlende als NA; nur Zielspalten behalten
panel_1520 <- grid %>%
  left_join(ed_vals, by = c("Country","Quarter")) %>%
  arrange(Country, Quarter)

# 4) Checks
stopifnot(nrow(panel_1520) == length(countries) * length(quarters))
sum(is.na(panel_1520$deaths))  # sollte 24 sein
head(panel_1520)

# 1) Harmonisieren
deaths_df <- panel_1520 %>%
  transmute(
    Country = as.character(Country),
    Quarter = str_replace_all(as.character(Quarter), "\\s+", ""),
    deaths_q = deaths
  )

qdata_std <- qdata %>%
  mutate(
    Country = as.character(Country),
    Quarter = str_replace_all(as.character(Quarter), "\\s+", "")
  )

# 2) Schlüssel-Eindeutigkeit prüfen
stopifnot(
  deaths_df %>% count(Country, Quarter) %>% filter(n > 1) %>% nrow() == 0,
  qdata_std %>% count(Country, Quarter) %>% filter(n > 1) %>% nrow() == 0
)

# 3) Join (fügt Spalte deaths_q hinzu, NAs bleiben NAs)
qdata <- qdata_std %>%
  left_join(deaths_df, by = c("Country","Quarter"))

# 4) Checks
stopifnot(nrow(qdata) == nrow(qdata_std))


rm(deaths_df, ed, ed_q_full_plus, ed_q_weekly, ed_q_weekly_flex, ed_q_weekly_full, ed_std, 
   es_vals, ed2, full_std, missing_weeks, na_check, panel_1520, panel_flex_2015_2024, q_monthly, q_monthly_full, wk, wk_std, qdata_std,
   q_axis, oxd_fix, m, grid, add3, add3_unique, codes_by_cat, conflicts, ed_vals, gdp_lookup, monthly, na_per_col, fm1_q2, rows_with_na_count,
   total_na)

##GIVES THE EXCESS MORTALITY DATA


#-------------------------------------------------------------------------------
##GETS THE HOSPITALIZATION DATA
##From: https://ourworldindata.org/covid-hospitalizations


hosp_d<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/covid-hospitalizations.csv", header=TRUE, sep=",")


# 1) Welche Länder sind im Datensatz?
countries_in_ed <- hosp_d %>%
  distinct(iso_code) %>%
  pull(iso_code)

# 2) Fehlende OECD-Länder (in OECD-Liste, aber nicht im Datensatz)
missing_in_ed <- setdiff(oecd_countries, countries_in_ed)

# 3) Länder im Datensatz, die NICHT in deiner OECD-Liste sind (Optional)
extra_in_ed <- setdiff(countries_in_ed, oecd_countries)

missing_in_ed


# --- 2) Filter: nur OECD-38 ---
hosp_d <- hosp_d %>%
  filter(iso_code %in% oecd_countries)

hosp_d <- hosp_d %>% select(-entity)

hosp_d <- hosp_d %>%
  rename(
    Country = iso_code,
  )


# 0) (Optional aber empfohlen) Duplikate checken
dups <- hosp_d %>%
  count(Country, date, indicator) %>%
  filter(n > 1)

print(dups, n = 50)

# 1) Wide bauen
hosp_d <- hosp_d %>%
  mutate(
    date = as.Date(date),
    indicator = make.names(indicator)  # macht gültige Spaltennamen
  ) %>%
  pivot_wider(
    names_from  = indicator,
    values_from = value,
    values_fn   = list(value = mean)   # falls Duplikate: nimm den Mittelwert
    # Alternative: max, min, sum, first, etc.
  )

# Check
glimpse(hosp_d)
head(hosp_d)

##GIBT UNS DAILY HOSPITAL DATA
##ES FEHLEN DIE WERTE FÜR CRI, COL, MEX, NZL, TUR-> SIEHE NOTES (UNTITLED 1)
# ==============================================================================
##GET THE R RATE DATA

rrate_d<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/r_rate.csv", header=TRUE, sep=",")

drop_these <- c("Kosovo", "Micronesia", "Summer Olympics 2020", "Winter Olympics 2022", "World")


# ------------------------------------------------------------------------------
# 1) OECD-Filter + ISO3 + Datum (ohne zu kollabieren)
# ------------------------------------------------------------------------------

rrate_oecd <- rrate_d %>%
  filter(!Country.Region %in% drop_these) %>%
  mutate(
    iso3 = countrycode(Country.Region, origin = "country.name", destination = "iso3c"),
    Date = as.Date(Date)
  ) %>%
  filter(!is.na(iso3)) %>%
  filter(iso3 %in% oecd_countries) %>%
  transmute(
    Country = iso3,
    Date,
    days_infectious,
    R, ci_95_u, ci_95_l
  )

# Check: pro Country-Date solltest du jetzt 6 Zeilen haben
rrate_oecd %>% count(Country, Date) %>% count(n)

# ------------------------------------------------------------------------------
# 2) Wide: eine Zeile pro Country-Date, aber Spalten für days_infectious (5..10)
# ------------------------------------------------------------------------------

rrate_wide <- rrate_oecd %>%
  pivot_wider(
    names_from  = days_infectious,
    values_from = c(R, ci_95_u, ci_95_l),
    names_glue  = "{.value}_di{days_infectious}"
  )

# Check: jetzt eindeutig
stopifnot(nrow(rrate_wide) == nrow(distinct(rrate_wide, Country, Date)))



##NO MISSINGS
##GIVES US DAILY VALUES ABOUT THE R NUMBER WITH DIFFEREN DEFINITION

rm(rrate_oecd, rrate_d, dups)

#------------------------------------------------------------------------------
## Get the P_Values

p_values<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/excess_mortality.csv", header=TRUE, sep=",")

#p_values<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/p_values.csv", header=TRUE, sep=",")


oecd_iso3 <- c("AUS","AUT","BEL","CAN","CHL","COL","CRI","CZE","DNK","EST","FIN","FRA","DEU","GRC",
               "HUN","ISL","IRL","ISR","ITA","JPN","KOR","LVA","LTU","LUX","MEX","NLD","NZL","NOR",
               "POL","PRT","SVK","SVN","ESP","SWE","CHE","TUR","GBR","USA")


p_values_oecd <- p_values %>%
  mutate(entity = str_trim(entity)) %>%
  filter(!entity %in% c("Kosovo","Transnistria")) %>%             # nicht-OECD/para-staatlich entfernen
  mutate(
    entity = dplyr::recode(entity,                               # Synonyme vereinheitlichen
                           "England and Wales" = "United Kingdom",
                           "Northern Ireland"  = "United Kingdom",
                           "Scotland"          = "United Kingdom",
                           "UK"                = "United Kingdom",
                           "Republic of Korea" = "South Korea",
                           "Korea, Rep."       = "South Korea",
                           "Korea, South"      = "South Korea",
                           "Czech Republic"    = "Czechia",
                           "Slovak Republic"   = "Slovakia",
                           "Türkiye"           = "Turkey"
    ),
    iso3 = countrycode(entity, "country.name", "iso3c", warn = FALSE)
  ) %>%
  filter(iso3 %in% oecd_iso3) %>%
  select(-iso3)

## remove everything after 2024
p_values_oecd <- p_values_oecd %>%
  mutate(date = ymd(date)) %>%
  filter(date <= ymd("2024-12-31"))

p_values_oecd <- p_values_oecd %>%
  mutate(entity = str_trim(entity)) %>%
  mutate(entity = countrycode(
    entity, "country.name", "iso3c", warn = FALSE,
    custom_match = c(
      "UK" = "GBR", "England and Wales" = "GBR", "Scotland" = "GBR", "Northern Ireland" = "GBR",
      "Republic of Korea" = "KOR", "Korea, Rep." = "KOR", "Korea, South" = "KOR",
      "Czech Republic" = "CZE", "Czechia" = "CZE",
      "Slovak Republic" = "SVK",
      "Türkiye" = "TUR",
      "United States of America" = "USA"
    )
  ))



present <- sort(unique(p_values_oecd$entity))
missing <- setdiff(oecd_iso3, present)
extras  <- setdiff(present, oecd_iso3)

list(
  all_38_present = length(missing) == 0 && all(oecd_iso3 %in% present),
  n_distinct_in_data = length(present),
  missing = missing,
  extras  = extras
)

# Optional: tabellarische Übersicht (TRUE = vorhanden)
print(tibble::tibble(iso3 = oecd_iso3) |>
  left_join(tibble::tibble(entity = present, in_data = TRUE), by = c("iso3" = "entity")) |>
  mutate(in_data = tidyr::replace_na(in_data, FALSE)), n=Inf)


p_values_oecd$time_unit

# 1) Liste der Länder mit weekly-Daten (ISO3 in `entity`)
weekly_countries <- p_values_oecd %>%
  mutate(time_unit = str_to_lower(time_unit)) %>%
  filter(time_unit == "weekly") %>%
  distinct(entity) %>%
  arrange(entity) %>%
  pull(entity)
weekly_countries

# 2) Häufigkeitstabelle je Land und Zeiteinheit
timeunit_table <- p_values_oecd %>%
  mutate(time_unit = str_to_lower(time_unit)) %>%
  count(entity, time_unit, name = "n") %>%
  pivot_wider(names_from = time_unit, values_from = n, values_fill = 0) %>%
  arrange(entity)
print(timeunit_table, n=Inf)

##GBR are not correctly mapped

# 1) UK-Teile vor dem Mapping entfernen
drop_uk_parts <- c("England and Wales","Scotland","Northern Ireland")

p_values_oecd <- p_values %>%
  filter(!entity %in% drop_uk_parts) %>%
  mutate(entity = str_trim(entity),
         entity = recode(entity, "UK"="United Kingdom", "Türkiye"="Turkey",
                         "Czech Republic"="Czechia", "Slovak Republic"="Slovakia",
                         "Republic of Korea"="South Korea", "Korea, Rep."="South Korea",
                         "Korea, South"="South Korea")) %>%
  mutate(entity = countrycode(entity, "country.name", "iso3c", warn = FALSE)) %>%
  filter(entity %in% oecd_iso3)

# 2) Kontrolle: Zeilenzahl je Land + Frequenz
p_values_oecd %>% count(entity, time_unit) %>% arrange(entity, desc(n))

##no mixed, only JPN, CRI and TUR are on monthly base


# erwartet: p_values_oecd mit Spalten entity (ISO3), date (YYYY-MM-DD), time_unit
monthly_countries <- c("CRI","JPN","TUR")

tmp <- p_values_oecd %>%
  mutate(time_unit = str_to_lower(time_unit),
         date = ymd(date))

# Weekly -> Quartal (alle außer CRI/JPN/TUR), Monthly -> Quartal (nur CRI/JPN/TUR)
q_agg <- bind_rows(
  tmp %>% filter(time_unit == "weekly",  !entity %in% monthly_countries),
  tmp %>% filter(time_unit == "monthly",  entity %in% monthly_countries)
) %>%
  mutate(yq = as.yearqtr(date))

# Spaltenklassen/Regeln
num_cols  <- names(q_agg)[sapply(q_agg, is.numeric)]
num_cols  <- setdiff(num_cols, "time") # Index nicht aggregieren

mean_cols <- grep("^p_|per_million|share|rate|ratio|pct|percent|avg", num_cols,
                  value = TRUE, ignore.case = TRUE)
last_cols <- grep("^cum_", num_cols, value = TRUE, ignore.case = TRUE)
sum_cols  <- setdiff(num_cols, c(mean_cols, last_cols))

last_non_na <- function(x){ y <- x[!is.na(x)]; if(length(y)==0) NA_real_ else y[length(y)] }

# Quartalsaggregation
p_quarterly <- q_agg %>%
  arrange(entity, date) %>%
  group_by(entity, yq) %>%
  summarise(
    across(all_of(sum_cols),  ~sum(.x, na.rm = TRUE)),
    across(all_of(mean_cols), ~mean(.x, na.rm = TRUE)),
    across(all_of(last_cols), ~last_non_na(.x)),
    .groups = "drop"
  ) %>%
  mutate(Quarter = sprintf("Q%d.%d", as.integer(format(yq, "%q")), as.integer(format(yq, "%Y")))) %>%
  relocate(entity, Quarter) %>%
  arrange(entity, yq) %>%
  select(-yq)


p_quarterly <- p_quarterly %>%
  mutate(yq = as.yearqtr(Quarter, format = "Q%q.%Y")) %>%
  filter(yq <= as.yearqtr("2024 Q4")) %>%
  select(-yq)

colnames(p_quarterly)


##TAKE: P PROJECTION ALL AGES AND CUM EXCESS PER MILLION PROJ ALL AGES

p_add <- p_quarterly %>%
  rename(Country = entity) %>%
  mutate(
    Quarter = as.character(Quarter)
  ) %>%
  select(
    Country,
    Quarter,
    p_proj_all_ages,                    # monatlicher P-Score (aggregiert auf Quartal)
    cum_excess_per_million_proj_all_ages,
    p_avg_all_ages# kumulative Excess Deaths pro Mio.
  ) %>%
  distinct(Country, Quarter, .keep_all = TRUE)

# 2) Merge in den Hauptdatensatz
qdata <- qdata %>%
  mutate(Quarter = as.character(Quarter)) %>%
  left_join(p_add, by = c("Country","Quarter"))

qdata <- qdata %>%
  relocate(p_proj_all_ages, cum_excess_per_million_proj_all_ages, p_proj_all_ages,
           .after = deaths_q)

excess_m<-p_values_oecd

##GIVES US EXCESS MORTALITY ABSOLUT (SIMPLE AVERAGE), P-VALUES AND PER 1 MILLION INHABITANS PROJECTON BASED ON THE MODELL BY https://ourworldindata.org/excess-mortality-covid
rm(tmp, timeunit_table, p_quarterly, p_add, q_agg)

#-------------------------------------------------------------------------------
##LOADING THE ECONOMIST DATA FOR POTENTIAL ROBUSTNESS CHECKS->ONLY LOADING, CLEANING AND PICKING HAS TO BE DONE
#-------------------------------------------------------------------------------
economist<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/excess_mortality_economist.csv", header=TRUE, sep=",")

head(economist)

oecd_iso3 <- c("AUS","AUT","BEL","CAN","CHL","COL","CRI","CZE","DNK","EST","FIN","FRA","DEU","GRC",
               "HUN","ISL","IRL","ISR","ITA","JPN","KOR","LVA","LTU","LUX","MEX","NLD","NZL","NOR",
               "POL","PRT","SVK","SVN","ESP","SWE","CHE","TUR","GBR","USA")


economist_oecd <- economist %>%
  mutate(entity = str_trim(country)) %>%
  filter(!entity %in% c("Kosovo","Transnistria")) %>%             # nicht-OECD/para-staatlich entfernen
  mutate(
    entity = dplyr::recode(country,                               # Synonyme vereinheitlichen
                           "England and Wales" = "United Kingdom",
                           "Northern Ireland"  = "United Kingdom",
                           "Scotland"          = "United Kingdom",
                           "UK"                = "United Kingdom",
                           "Republic of Korea" = "South Korea",
                           "Korea, Rep."       = "South Korea",
                           "Korea, South"      = "South Korea",
                           "Czech Republic"    = "Czechia",
                           "Slovak Republic"   = "Slovakia",
                           "Türkiye"           = "Turkey"
    ),
    iso3 = countrycode(country, "country.name", "iso3c", warn = FALSE)
  ) %>%
  filter(iso3 %in% oecd_iso3) %>%
  select(-iso3)

## remove everything after 2024
economist_oecd <- economist_oecd %>%
  mutate(date = ymd(date)) %>%
  filter(date <= ymd("2024-12-31"))

economist_oecd <- economist_oecd %>%
  mutate(entity = str_trim(entity)) %>%
  mutate(entity = countrycode(
    entity, "country.name", "iso3c", warn = FALSE,
    custom_match = c(
      "UK" = "GBR", "England and Wales" = "GBR", "Scotland" = "GBR", "Northern Ireland" = "GBR",
      "Republic of Korea" = "KOR", "Korea, Rep." = "KOR", "Korea, South" = "KOR",
      "Czech Republic" = "CZE", "Czechia" = "CZE",
      "Slovak Republic" = "SVK",
      "Türkiye" = "TUR",
      "United States of America" = "USA"
    )
  ))



present <- sort(unique(economist_oecd$entity))
missing <- setdiff(oecd_iso3, present)
extras  <- setdiff(present, oecd_iso3)

list(
  all_38_present = length(missing) == 0 && all(oecd_iso3 %in% present),
  n_distinct_in_data = length(present),
  missing = missing,
  extras  = extras
)


colnames(economist_oecd)
#-------------------------------------------------------------------------------
##noch bereinigen und variablen auswählen
##There also information about Vaccinations by age and manufacture, Testing, R Rate, Attitudes (YouGov) and Donations
rm(economist)
#-------------------------------------------------------------------------------
##GET THE Google Mobility Data
#-------------------------------------------------------------------------------
gm<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/google_mobility.csv", header=TRUE, sep=",")



google_mobility <- gm %>%
  mutate(entity = str_trim(country)) %>%
  filter(!entity %in% c("Kosovo","Transnistria")) %>%             # nicht-OECD/para-staatlich entfernen
  mutate(
    entity = dplyr::recode(entity,                               # Synonyme vereinheitlichen
                           "England and Wales" = "United Kingdom",
                           "Northern Ireland"  = "United Kingdom",
                           "Scotland"          = "United Kingdom",
                           "UK"                = "United Kingdom",
                           "Republic of Korea" = "South Korea",
                           "Korea, Rep."       = "South Korea",
                           "Korea, South"      = "South Korea",
                           "Czech Republic"    = "Czechia",
                           "Slovak Republic"   = "Slovakia",
                           "Türkiye"           = "Turkey"
    ),
    iso3 = countrycode(entity, "country.name", "iso3c", warn = FALSE)
  ) %>%
  filter(iso3 %in% oecd_iso3) %>%
  select(-iso3)

## remove everything after 2024
google_mobility <- google_mobility %>%
  mutate(date = ymd(date)) %>%
  filter(date <= ymd("2024-12-31"))

google_mobility <- google_mobility %>%
  mutate(entity = str_trim(entity)) %>%
  mutate(entity = countrycode(
    entity, "country.name", "iso3c", warn = FALSE,
    custom_match = c(
      "UK" = "GBR", "England and Wales" = "GBR", "Scotland" = "GBR", "Northern Ireland" = "GBR",
      "Republic of Korea" = "KOR", "Korea, Rep." = "KOR", "Korea, South" = "KOR",
      "Czech Republic" = "CZE", "Czechia" = "CZE",
      "Slovak Republic" = "SVK",
      "Türkiye" = "TUR",
      "United States of America" = "USA"
    )
  ))


present <- sort(unique(google_mobility$entity))
missing <- setdiff(oecd_iso3, present)
extras  <- setdiff(present, oecd_iso3)

list(
  all_38_present = length(missing) == 0 && all(oecd_iso3 %in% present),
  n_distinct_in_data = length(present),
  missing = missing,
  extras  = extras
)
#-------------------------------------------------------------------------------
##GIVES THE MOBILITY DATA WITH ONLY ICELAND IS MISSING

#-------------------------------------------------------------------------------
## Get the debt data


debt <- read_excel("debtq.xlsx") %>%
  mutate(
    Country = toupper(trimws(Country)),
    Quarter = as.character(Quarter)
  ) %>%
  distinct(Country, Quarter, .keep_all = TRUE)

debtc <- read_excel("chile_debtpr.xlsx") %>%
  mutate(
      Quarter = as.character(Quarter)
  ) %>%
  distinct(Quarter, .keep_all = TRUE)

debt <- debt %>% filter(Country != "KOR")
##adding CHile Data from national accounts
debtc <- debtc %>%
  mutate(
    Quarter = format(as.yearqtr(as.character(Quarter), format = "%YQ%q"), "Q%q.%Y")
  )

debtc <- debtc %>% dplyr::mutate(Country = "CHL")

debt <- dplyr::bind_rows(
  debt %>% dplyr::select(Country, Quarter, DebtTot_CG_nom),
  debtc %>% dplyr::select(Country, Quarter, DebtTot_CG_nom)
) %>%
  dplyr::arrange(Country, Quarter)


##adding LTU data from Eurostat

debtc <- read_excel("lithuania_gov_debt.xlsx") %>%
  mutate(
    Quarter = as.character(Quarter)
  ) %>%
  distinct(Quarter, .keep_all = TRUE)

debtc <- debtc %>%
  mutate(
    Quarter = format(as.yearqtr(as.character(Quarter), format = "%Y-Q%q"), "Q%q.%Y")
  )

debtc <- debtc %>% dplyr::mutate(Country = "LTU")

debt <- dplyr::bind_rows(
  debt %>% dplyr::select(Country, Quarter, DebtTot_CG_nom),
  debtc %>% dplyr::select(Country, Quarter, DebtTot_CG_nom)
) %>%
  dplyr::arrange(Country, Quarter)

##adding korea data from BIS

debtk <- read_excel("korea_gov_debt.xlsx") %>%
  mutate(
    Quarter = as.character(Quarter)
  ) %>%
  distinct(Quarter, .keep_all = TRUE)

debtk <- debtk %>%
  mutate(
    Quarter = format(as.yearqtr(as.character(Quarter), format = "%Y-Q%q"), "Q%q.%Y")
  )

debtk <- debtk %>% dplyr::mutate(Country = "KOR")

debt <- dplyr::bind_rows(
  debt %>% dplyr::select(Country, Quarter, DebtTot_CG_nom),
  debtk %>% dplyr::select(Country, Quarter, DebtTot_CG_nom)
) %>%
  dplyr::arrange(Country, Quarter)

##add to main dataset

qdata <- qdata %>%
  mutate(
    Country = toupper(trimws(Country)),
    Quarter = as.character(Quarter)
  ) %>%
  left_join(debt, by = c("Country","Quarter"), suffix = c("", ".debt"))


## Problem: Inflation in for example TUR, build a deflator and bereinige die werte

# ==============================================================================
# Build a Deflator
# ==============================================================================

qdata_clean <- qdata %>%
  mutate(yq = as.yearqtr(as.character(Quarter), format = "Q%q.%Y"))

# -------------------- 1. GDP Deflator berechnen --------------------
# GDP Deflator = (nominelles GDP / reales GDP) * 100
qdata_with_deflator <- qdata_clean %>%
  arrange(Country, yq) %>%
  group_by(Country) %>%
  mutate(
    # Deflator für jedes Quartal
    gdp_deflator = (nGDP / rGDP_LC) * 100
  ) %>%
  ungroup()

# Prüfen
cat("\n=== GDP Deflator für KOR & TUR (erste 5 Quartale) ===\n")
print(qdata_with_deflator %>% 
        filter(Country %in% c("AUS","KOR", "TUR")) %>%
        select(Country, Quarter, yq, nGDP, rGDP_LC, gdp_deflator) %>%
        group_by(Country) %>%
        slice_head(n = 5))

# -------------------- 2. Basis-Deflator festlegen (Q4.2019 = 100) --------------------
# Für jedes Land den Deflator von Q4.2019 als Basis nehmen
base_deflator <- qdata_with_deflator %>%
  filter(yq == as.yearqtr("2015 Q4")) %>%
  select(Country, base_deflator = gdp_deflator)

cat("\n=== Basis Deflator Q4.2015 für ausgewählte Länder ===\n")
print(base_deflator %>% filter(Country %in% c("KOR", "TUR", "DEU", "USA", "CHL")))

# -------------------- 3. Debt_ar berechnen (inflationsbereinigt) --------------------
qdata_final <- qdata_with_deflator %>%
  left_join(base_deflator, by = "Country") %>%
  mutate(
    # Inflationsfaktor relativ zu Q4.2019
    inflation_index = gdp_deflator / base_deflator,
    
    # Debt_ar: Schulden in 2019 Q4 Preisen
    Debt_ar = DebtTot_CG_nom / inflation_index
  )


# -------------------- Vergleich Inflation über Länder --------------------
cat("\n=== INFLATION VERGLEICH (Q4.2019 vs Q4.2023) ===\n")
inflation_comparison <- qdata_final %>%
  filter(yq == as.yearqtr("2023 Q4")) %>%
  select(Country, inflation_index) %>%
  mutate(
    cumulative_inflation_pct = (inflation_index - 1) * 100
  ) %>%
  arrange(desc(inflation_index))

print(inflation_comparison, n = 20)

qdata<-qdata_final



#-------------------------------------------------------------------------------
##remove and move not used dataframes and variables
rm(bf_gdp_2020, bf_gdp_all_wide, bf_gdp_2020_wide, bf_gdp_all, by_month_2020, conflicts, daily_counts, events_q, gdp_lookup, 
   gdp2, irl_fc, mdd, mdd_irl, spe, spe_fix, totals_2020, pop, monthly, kor_add, debtc, debt, debtk, add3, add3_unique, base_deflator,
   inflation_comparison, qdata_with_deflator, qdata_clean, qdata_final, gm, countries_in_ed, economist)

##dropping last variables
#drop_cols <- c("Trend.EmpRate.15.74", "Deflator.tgross.cf", "gggi_ggi", "wdi_unempedua", "wdi_unempedub", "wdi_unempedui", "wdi_unempmilo")
#data <- dplyr::select(data, -any_of(drop_cols))


# ==============================================================================-
# Generate Outcome Variables-> Quarterly
# ==============================================================================

vars <- c("Country","Quarter","qtime", "TimeIndex", "TreatmentGroup", "qcovid", "qtreatment", "qstrict", "Qpopulation_th", "QReal.GDP.Growth", "nGDP", "rGDP","rGDP_LC", "GDP", "NGDP_2019", "QPrivate.Consumption", "deaths_q", "ConfirmedDeaths", "ConfirmedCases", "p_value_all_ages", "DebtTot_CG_nom", "StringencyIndex_Average", "broad_fiscal_size_q", "broad_fiscal_gdp_q")
qdata <- qdata %>% select(any_of(vars), everything())

##define variable names in a consistent way: a= absolute values, g = growth rate, n = nominal i(local currency), r = real (in USD), s = share of GDP, all variables on quarterly base otherwise is stated

names(qdata)[names(qdata) == "nGDP"] <- "nGDP_an"
names(qdata)[names(qdata) == "rGDP"] <- "rGDP_ar"
names(qdata)[names(qdata) == "GDP"] <- "GDP_index_n"
names(qdata)[names(qdata) == "NGDP_2019"] <- "nGDP_2019_an"
names(qdata)[names(qdata) == "debt"] <- "debt_an"
names(qdata)[names(qdata) == "QReal.GDP.Growth"] <- "QReal.GDP.Growth_gr"
names(qdata)[names(qdata) == "QPrivate.Consumption"] <- "QPrivate.Consumption.gr"
names(qdata)[names(qdata) == "deaths_q"] <- "excess.deaths_a"
names(qdata)[names(qdata) == "broad_fiscal_size_q"] <- "broad_fiscal_size_an"
names(qdata)[names(qdata) == "broad_fiscal_gdp_q"] <- "broad_fiscal_gdp_sn"
names(qdata)[names(qdata) == "ConfirmedCases"] <- "ConfirmedCases.a"
names(qdata)[names(qdata) == "ConfirmedDeaths"] <- "ConfirmedDeaths.a"
names(qdata)[names(qdata) == "DebtTot_CG_nom"] <- "Debt_an"

attr(qdata$QReal.GDP.Growth_gr, "label") <- "YoY, real, calendar and seasonal adjusted"
attr(qdata$nGDP_an, "label") <- "nominal GDP in LC"
attr(qdata$rGDP_ar, "label") <- "real GDP in $ (Annual Level)"
attr(qdata$rGDP_LC, "label") <- "real GDP in LC"
attr(qdata$nGDP_2019_an, "label") <- "Yearly GDP 2019 nominal"
attr(qdata$QPrivate.Consumption.gr, "label") <- "YoY, real, alendar and seasonal adjusted"
attr(qdata$Debt_an, "label") <- "absolute and nominal values in LC"
attr(qdata$Debt_ar, "label") <- "absolute but real values in LC"

vars <- c("Country","Quarter","qtime", "TimeIndex", "TreatmentGroup", "qcovid", "qtreatment", "qstrict", "StringencyIndex_Average", "Debt_an", "Debt_ar", "inflation_index", "base_deflator", "gdp_deflator", "p_value_all_ages")
qdata <- qdata %>% select(any_of(vars), everything())

#-------------------------------------------------------------------------------
## 1. GDP Outcome Calculating
# Quartalswachstumsrate vs. 2019 berechnen

# BIP pro Kopf berechnen und Wachstum vs. 2019
data_capita <- qdata %>%
  arrange(Country, Quarter) %>%
  group_by(Country) %>%
  mutate(
    # Quartal und Jahr extrahieren
    quarter_only = str_extract(Quarter, "Q[1-4]"),
    year_only = as.numeric(str_extract(Quarter, "\\d{4}")),
    
    # BIP pro Kopf berechnen (real GDP / Bevölkerung)
    rGDP_per_capita =(rGDP_ar / Qpopulation_th) * 100 ,
    
    # BIP pro Kopf für 2019 Quartale finden
    rGDP_pc_2019_Q1 = first(rGDP_per_capita[year_only == 2019 & quarter_only == "Q1"]),
    rGDP_pc_2019_Q2 = first(rGDP_per_capita[year_only == 2019 & quarter_only == "Q2"]),
    rGDP_pc_2019_Q3 = first(rGDP_per_capita[year_only == 2019 & quarter_only == "Q3"]),
    rGDP_pc_2019_Q4 = first(rGDP_per_capita[year_only == 2019 & quarter_only == "Q4"]),
    
    # Entsprechenden 2019-Wert zuweisen
    rGDP_pc_2019 = case_when(
      quarter_only == "Q1" ~ rGDP_pc_2019_Q1,
      quarter_only == "Q2" ~ rGDP_pc_2019_Q2,
      quarter_only == "Q3" ~ rGDP_pc_2019_Q3,
      quarter_only == "Q4" ~ rGDP_pc_2019_Q4
    ),
    
    # Wachstumsrate BIP pro Kopf vs. 2019 berechnen
    rGDP_pc_growth_vs_2019 = (rGDP_per_capita / rGDP_pc_2019 - 1) * 100
  ) %>%
  ungroup() %>%
  select(-rGDP_pc_2019_Q1, -rGDP_pc_2019_Q2, -rGDP_pc_2019_Q3, -rGDP_pc_2019_Q4)

# Labels hinzufügen
attr(data_capita$rGDP_per_capita, "label") <- "Reales BIP pro Kopf (US Dollar)"
attr(data_capita$rGDP_pc_growth_vs_2019, "label") <- "Wachstum des realen BIP pro Kopf vs. entsprechendes Quartal 2019 (%)"


# Datum für Plot erstellen
data_capita_plot <- data_capita %>%
  mutate(
    date = ymd(paste0(year_only, "-", 
                      case_when(
                        quarter_only == "Q1" ~ "01-01",
                        quarter_only == "Q2" ~ "04-01", 
                        quarter_only == "Q3" ~ "07-01",
                        quarter_only == "Q4" ~ "10-01"
                      )))
  )

# Basis Plot für BIP pro Kopf
ggplot(data_capita_plot, aes(x = date, y = rGDP_pc_growth_vs_2019, group = Country)) +
  geom_line(alpha = 0.3, color = "gray60") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Reales BIP pro Kopf Wachstum im Vergleich zu 2019",
    subtitle = "Quartalsdaten, Basis: jeweiliges Quartal 2019 = 0%",
    x = "Quartal",
    y = "BIP pro Kopf Wachstum vs. 2019 (%)",
    caption = "Quelle: Eigene Berechnung auf Basis von rGDP_ar und Qpopulation_th"
  ) +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(legend.position = "none")

# Ausgewählte Länder hervorheben
highlight_countries <- c("USA", "DEU", "CHN", "GBR", "FRA", "IRL", "TUR")  # Anpassen an Ihre Daten

ggplot(data_capita_plot, aes(x = date, y = rGDP_pc_growth_vs_2019)) +
  # Alle Länder als Hintergrund
  geom_line(aes(group = Country), alpha = 0.2, color = "gray70") +
  # Hervorgehobene Länder
  geom_line(data = data_capita_plot %>% filter(Country %in% highlight_countries),
            aes(color = Country), size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
  # COVID-Periode markieren
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dotted", 
             color = "blue", alpha = 0.5) +
  annotate("text", x = as.Date("2020-03-01"), y = max(data_capita_plot$rGDP_pc_growth_vs_2019, na.rm = TRUE),
           label = "COVID Start", hjust = -0.1, vjust = 1, color = "blue", size = 3) +
  labs(
    title = "Reales BIP pro Kopf Wachstum im Vergleich zu 2019",
    subtitle = "Hervorgehoben: USA, Deutschland, China, UK, Frankreich",
    x = "Quartal", 
    y = "BIP pro Kopf Wachstum vs. 2019 (%)",
    color = "Land"
  ) +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_brewer(palette = "Set1")


## Kombinierte Variable berechnen ab 2019 qoq auf die Quarter von 2019 fixiert, vorher normal yoy (alle Daten sind Saison und kalenderbereinigt)

qdata <- data_capita %>%
  arrange(Country, TimeIndex) %>%
  group_by(Country) %>%
  mutate(
    # Quartal/Jahr deterministisch aus TimeIndex
    quarter_num  = ((TimeIndex - 1L) %% 4L) + 1L,
    quarter_only = paste0("Q", quarter_num),
    year_only    = 2015L + ((TimeIndex - 1L) %/% 4L),
    
    # 1) QoQ-Wachstum korrekt (innerhalb Country, nach TimeIndex)
    rGDP_pc_qoq  = (rGDP_per_capita / lag(rGDP_per_capita) - 1) * 100,
    
    # 2) 2019-Baselines je Quarter
    rGDP_pc_2019_Q1 = first(rGDP_per_capita[year_only == 2019 & quarter_num == 1]),
    rGDP_pc_2019_Q2 = first(rGDP_per_capita[year_only == 2019 & quarter_num == 2]),
    rGDP_pc_2019_Q3 = first(rGDP_per_capita[year_only == 2019 & quarter_num == 3]),
    rGDP_pc_2019_Q4 = first(rGDP_per_capita[year_only == 2019 & quarter_num == 4]),
    
    rGDP_pc_2019 = case_when(
      quarter_num == 1 ~ rGDP_pc_2019_Q1,
      quarter_num == 2 ~ rGDP_pc_2019_Q2,
      quarter_num == 3 ~ rGDP_pc_2019_Q3,
      quarter_num == 4 ~ rGDP_pc_2019_Q4
    ),
    
    # 3) Abweichung ggü. 2019 (same quarter)
    rGDP_pc_vs_2019 = if_else(is.finite(rGDP_pc_2019),
                              (rGDP_per_capita / rGDP_pc_2019 - 1) * 100,
                              NA_real_),
    
    # 4) Kombinierte Metrik: <2019 = QoQ, >=2019 = vs.2019
    rGDP_pc_combined_correct = if_else(year_only < 2019, rGDP_pc_qoq, rGDP_pc_vs_2019)
  ) %>%
  ungroup() %>%
  select(-starts_with("rGDP_pc_2019_Q"))


qdata %>% group_by(Country) %>% summarise(order_ok = all(diff(TimeIndex) >= 0))




names(qdata)[names(qdata) == "rGDP_pc_combined_correct"] <- "rGDP_pc_combined"
attr(qdata$rGDP_pc_combined,  "label") <- "Kombinierte Wachstumsrate: Vor 2019 qoq, Ab 2019 vs. Quartal 2019"
names(qdata)[names(qdata) == "rGDP_pc_growth_vs_2019"] <- "rGDP_pc_growth_2019"
attr(qdata$rGDP_pc_growth_2019,  "label") <- "Wachstum vs. jeweiliges Quartal 2019 (%)"


vars <- c("Country","Quarter","qtime", "TimeIndex", "TreatmentGroup", "qcovid", "qtreatment", "qstrict", "StringencyIndex_Average", "Debt_an", "Debt_ar", "inflation_index", "base_deflator", "gdp_deflator", "p_value_all_ages", 
          "QReal.GDP.Growth_gr", "rGDP_per_capita", "rGDP_pc_combined", "rGDP_pc_growth_2019")
qdata <- qdata %>% select(any_of(vars), everything())

data_plot<-qdata

# Datum für Plot erstellen
data_plot <- qdata %>%
  mutate(
    date = ymd(paste0(year_only, "-", 
                      case_when(
                        quarter_only == "Q1" ~ "01-01",
                        quarter_only == "Q2" ~ "04-01", 
                        quarter_only == "Q3" ~ "07-01",
                        quarter_only == "Q4" ~ "10-01"
                      )))
  )



# Plot der kombinierten Metrik ab 2016 mit allen Quartalen
highlight_countries <- c("USA", "DEU", "CHN", "GBR", "FRA", "IRL")

ggplot(data_plot %>% filter(year_only >= 2016), aes(x = date, y = rGDP_pc_combined)) +
  geom_line(aes(group = Country), alpha = 0.2, color = "gray70") +
  geom_line(data = data_plot %>% filter(Country %in% highlight_countries, year_only >= 2016),
            aes(color = Country), size = 1.2) +
  geom_vline(xintercept = as.Date("2020-01-01"), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  annotate("text", x = as.Date("2017-07-01"), y = 8, 
           label = "Quartal-over-Quartal\nWachstum", color = "darkgreen", size = 3) +
  annotate("text", x = as.Date("2021-01-01"), y = 8, 
           label = "Vergleich mit\nQuartal 2019", color = "darkred", size = 3) +
  labs(
    title = "Reales BIP pro Kopf Entwicklung - Kombinierte Metrik",
    subtitle = "Vor 2019: qoq Wachstum | Ab 2019: Vergleich mit entsprechendem Quartal 2019",
    x = "Quartal",
    y = "Wachstumsrate (%)",
    color = "Land"
  ) +
  theme_minimal() +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%Q") +
  scale_color_brewer(palette = "Set1") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## Change of the Metric for IRL
# Zuerst den GDP_index_n für Irland in Wachstumsrate vs. 2019 umrechnen
data_plot_corrected <- data_plot %>%
  group_by(Country) %>%
  mutate(
    # Für Irland: GDP_index_n zu Wachstumsrate vs. 2019 umrechnen
    # 1. GDP_index_n Wert für Q1 2019 finden (Referenz für Vergleich)
    gdp_index_2019q1 = ifelse(Country == "IRL", 
                              first(GDP_index_n[year_only == 2019 & quarter_only == "Q1"]), 
                              NA_real_),
    
    # 2. Wachstumsrate vs. 2019 für Irland berechnen
    # (GDP_index_n / Referenzwert 2019 - 1) * 100
    irl_growth_vs_2019 = ifelse(Country == "IRL",
                                (GDP_index_n / gdp_index_2019q1 - 1) * 100,
                                NA_real_),
    
    # 3. Kombinierte Metrik: Für Irland die umgerechnete Rate, für andere normal
    rGDP_pc_corrected = ifelse(Country == "IRL", irl_growth_vs_2019, rGDP_pc_combined)
  ) %>%
  ungroup()

# Plot mit korrigierten Irland-Daten
ggplot(data_plot_corrected %>% filter(year_only >= 2016), 
       aes(x = date, y = rGDP_pc_corrected)) +
  geom_line(aes(group = Country), alpha = 0.2, color = "gray70") +
  geom_line(data = data_plot_corrected %>% 
              filter(Country %in% highlight_countries, year_only >= 2016),
            aes(color = Country), size = 1.2) +
  geom_vline(xintercept = as.Date("2019-01-01"), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  annotate("text", x = as.Date("2017-07-01"), y = 8, 
           label = "Quartal-over-Quartal\nWachstum", color = "darkgreen", size = 3) +
  annotate("text", x = as.Date("2021-01-01"), y = 8, 
           label = "Vergleich mit\nQuartal 2019", color = "darkred", size = 3) +
  labs(
    title = "Reales BIP pro Kopf Entwicklung - Korrigierte Metrik",
    subtitle = "Irland: GDP_index_n (Referenz Q1.2020) zu Wachstum vs. 2019 umgerechnet",
    x = "Quartal",
    y = "Wachstumsrate vs. 2019 (%)",
    color = "Land"
  ) +
  theme_minimal() +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-Q%q") +
  scale_color_brewer(palette = "Set1") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



##Mit INDEX, nicht sicher ob das hilfreich ist
# Kumulierter Index Plot (empfohlen)
index_2019 <- data_plot %>%
  group_by(Country) %>%
  mutate(
    # Index auf Q1 2019 = 100 setzen
    index_base = GDP_index_n / first(GDP_index_n[year_only == 2019 & quarter_only == "Q1"]) * 100
  ) %>%
  ungroup()

ggplot(index_2019, aes(x = date, y = index_base)) +
  geom_line(aes(group = Country), alpha = 0.2, color = "gray70") +
  geom_line(data = index_2019 %>% filter(Country %in% highlight_countries),
            aes(color = Country), size = 1.2) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "red") +
  geom_vline(xintercept = as.Date("2019-01-01"), linetype = "dotted", color = "blue") +
  labs(
    title = "Reales BIP pro Kopf - Index (Q1 2019 = 100)",
    subtitle = "Kumulierte Entwicklung ausgehend von Pre-COVID Niveau",
    x = "Quartal",
    y = "BIP pro Kopf Index (Q1 2019 = 100)",
    color = "Land"
  ) +
  theme_minimal() +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")


##Letzter Schritt: Variablen umbenenennen und korrekt ordnen
# Schritt 1: Nur die relevanten Variablen aus data_plot_corrected auswählen
clean_variables <- data_plot_corrected %>%
  select(Country, Quarter, 
         rGDP_per_capita,
         rGDP_pc_combined,
         rGDP_pc_corrected,
         rGDP_pc_vs_2019,
  )

# Schritt 2: Labels zuweisen (falls noch nicht geschehen)
attr(clean_variables$rGDP_per_capita, "label") <- "Reales BIP pro Kopf (US Dollar)"
attr(clean_variables$rGDP_pc_combined, "label") <- "Kombinierte Wachstumsrate pro Kopf: Vor 2019 qoq, Ab 2019 vs. Quartal 2019"
attr(clean_variables$rGDP_pc_corrected, "label") <- "Bereinigte Wachstumsrate pro Kopf (Irland korrigiert)"
attr(clean_variables$rGDP_pc_vs_2019, "label") <- "Wachstum pro Kopf vs. jeweiliges Quartal 2019 (%)"



##Plot für average excess GDP
# Datum aus Quarter korrekt erstellen und nach Jahr filtern
avg_pc_growth <- qdata %>%
  mutate(
    quarter_only = str_extract(Quarter, "Q[1-4]"),
    year_only = as.numeric(str_extract(Quarter, "\\d{4}")),
    date = ymd(paste0(year_only, "-", 
                      case_when(
                        quarter_only == "Q1" ~ "01-01",
                        quarter_only == "Q2" ~ "04-01", 
                        quarter_only == "Q3" ~ "07-01",
                        quarter_only == "Q4" ~ "10-01"
                      )))
  ) %>%
  # Nur ab 2016 für bessere Übersicht
  filter(year_only >= 2016) %>%
  group_by(date) %>%
  summarise(
    avg_growth = mean(rGDP_pc_combined, na.rm = TRUE),
    se = sd(rGDP_pc_combined, na.rm = TRUE) / sqrt(n()),
    upper = avg_growth + 1.96 * se,
    lower = avg_growth - 1.96 * se
  )

# Plot mit korrigierter X-Achse
ggplot(avg_pc_growth, aes(x = date, y = avg_growth)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "purple", alpha = 0.2) +
  geom_line(color = "purple", size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = as.Date("2019-10-01"), linetype = "dotted", color = "blue", alpha = 0.7) +
  labs(
    title = "Durchschnittliches BIP pro Kopf Wachstum vs. 2019",
    subtitle = "95% Konfidenzintervall, Quartalsdaten ab 2016",
    x = "Quartal",
    y = "BIP pro Kopf Wachstum vs. 2019 (%)"
  ) +
  theme_minimal() +
  scale_x_date(
    date_breaks = "3 months",
    labels = function(x) paste0(lubridate::year(x), "-Q", lubridate::quarter(x))
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



##mit normalen QReal.GDP.Growth
# Datum aus Quarter korrekt erstellen und nach Jahr filtern
avg_pc_growth <- qdata %>%
  mutate(
    quarter_only = str_extract(Quarter, "Q[1-4]"),
    year_only = as.numeric(str_extract(Quarter, "\\d{4}")),
    date = ymd(paste0(year_only, "-", 
                      case_when(
                        quarter_only == "Q1" ~ "01-01",
                        quarter_only == "Q2" ~ "04-01", 
                        quarter_only == "Q3" ~ "07-01",
                        quarter_only == "Q4" ~ "10-01"
                      )))
  ) %>%
  # Nur ab 2016 für bessere Übersicht
  filter(year_only >= 2016) %>%
  group_by(date) %>%
  summarise(
    avg_growth = mean(QReal.GDP.Growth_gr, na.rm = TRUE),
    se = sd(QReal.GDP.Growth_gr, na.rm = TRUE) / sqrt(n()),
    upper = avg_growth + 1.96 * se,
    lower = avg_growth - 1.96 * se
  )

# Plot mit korrigierter X-Achse
ggplot(avg_pc_growth, aes(x = date, y = avg_growth)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "purple", alpha = 0.2) +
  geom_line(color = "purple", size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = as.Date("2019-10-01"), linetype = "dotted", color = "blue", alpha = 0.7) +
  labs(
    title = "Durchschnittliches BIP pro Kopf Wachstum vs. 2019",
    subtitle = "95% Konfidenzintervall, Quartalsdaten ab 2016",
    x = "Quartal",
    y = "BIP pro Kopf Wachstum vs. 2019 (%)"
  ) +
  scale_x_date(
    date_breaks = "3 months",
    labels = function(x) paste0(lubridate::year(x), "-Q", lubridate::quarter(x))
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#===============================================================================
##2. Create DEBT
#==============================================================================

#-------------------------------------------------------------------------------
#BESTAND AN DEBT REAL IN LC->GROWTH RATE nach 2019 auf den entsprechend Quartalswert festgesetzt


qdata <- qdata %>%
  arrange(Country, TimeIndex) %>%
  group_by(Country) %>%
  mutate(
    # Quartal/Jahr deterministisch aus TimeIndex
    quarter_num  = ((TimeIndex - 1L) %% 4L) + 1L,
    quarter_only = paste0("Q", quarter_num),
    year_only    = 2015L + ((TimeIndex - 1L) %/% 4L),

    # 1) QoQ-Wachstum korrekt (innerhalb Country, nach TimeIndex)
    Debt_qoq = (Debt_ar / dplyr::lag(Debt_ar) - 1) * 100,

    # 2) 2019-Baselines je Quarter
    debt_2019_Q1 = dplyr::first(Debt_ar[year_only == 2019 & quarter_num == 1]),
    debt_2019_Q2 = dplyr::first(Debt_ar[year_only == 2019 & quarter_num == 2]),
    debt_2019_Q3 = dplyr::first(Debt_ar[year_only == 2019 & quarter_num == 3]),
    debt_2019_Q4 = dplyr::first(Debt_ar[year_only == 2019 & quarter_num == 4]),

    debt_2019 = dplyr::case_when(
      quarter_num == 1 ~ debt_2019_Q1,
      quarter_num == 2 ~ debt_2019_Q2,
      quarter_num == 3 ~ debt_2019_Q3,
      quarter_num == 4 ~ debt_2019_Q4
    ),

    # 3) Abweichung ggü. 2019 (same quarter)
    Debt_growth_vs_2019 = dplyr::if_else(
      is.finite(debt_2019),
      (Debt_ar / debt_2019 - 1) * 100,
      NA_real_
    ),

    # 4) Kombinierte Metrik: <2019 = QoQ, >=2019 = vs.2019
    Debt_combined_correct = dplyr::if_else(year_only < 2019, Debt_qoq, Debt_growth_vs_2019)
  ) %>%
  ungroup() %>%
  select(-dplyr::starts_with("debt_2019_Q"))

# Ordnungscheck
qdata %>% group_by(Country) %>% summarise(order_ok = all(diff(TimeIndex) >= 0), .groups = "drop")

#-------------------------------------------------------------------------------
##BESTAND AN DEBT NOMINAL IN LC-> GROWTH RATE nach 2019 auf den entsprechend Quartalswert festgesetzt

qdata <- qdata %>%
  arrange(Country, TimeIndex) %>%
  group_by(Country) %>%
  mutate(
    # Quartal/Jahr deterministisch aus TimeIndex
    quarter_num  = ((TimeIndex - 1L) %% 4L) + 1L,
    quarter_only = paste0("Q", quarter_num),
    year_only    = 2015L + ((TimeIndex - 1L) %/% 4L),
    
    # 1) QoQ-Wachstum korrekt (innerhalb Country, nach TimeIndex)
    Debt_qoq = (Debt_an / dplyr::lag(Debt_an) - 1) * 100,
    
    # 2) 2019-Baselines je Quarter
    debt_2019_Q1 = dplyr::first(Debt_an[year_only == 2019 & quarter_num == 1]),
    debt_2019_Q2 = dplyr::first(Debt_an[year_only == 2019 & quarter_num == 2]),
    debt_2019_Q3 = dplyr::first(Debt_an[year_only == 2019 & quarter_num == 3]),
    debt_2019_Q4 = dplyr::first(Debt_an[year_only == 2019 & quarter_num == 4]),
    
    debt_2019 = dplyr::case_when(
      quarter_num == 1 ~ debt_2019_Q1,
      quarter_num == 2 ~ debt_2019_Q2,
      quarter_num == 3 ~ debt_2019_Q3,
      quarter_num == 4 ~ debt_2019_Q4
    ),
    
    # 3) Abweichung ggü. 2019 (same quarter)
    Debt_growth_vs_2019_n = dplyr::if_else(
      is.finite(debt_2019),
      (Debt_an / debt_2019 - 1) * 100,
      NA_real_
    ),
    
    # 4) Kombinierte Metrik: <2019 = QoQ, >=2019 = vs.2019
    Debt_combined_correct_n = dplyr::if_else(year_only < 2019, Debt_qoq, Debt_growth_vs_2019_n)
  ) %>%
  ungroup() %>%
  select(-dplyr::starts_with("debt_2019_Q"))

# Ordnungscheck
qdata %>% group_by(Country) %>% summarise(order_ok = all(diff(TimeIndex) >= 0), .groups = "drop")



# Umbenennungen & Labels
names(qdata)[names(qdata) == "Debt_combined_correct"] <- "DebtR_combined"
attr(qdata$DebtR_combined,"label") <- "Kombinierte Schuldenänderung: Vor 2019 qoq, Ab 2019 vs. Quartal 2019 real"
names(qdata)[names(qdata) == "Debt_combined_correct_n"] <- "DebtN_combined"
attr(qdata$DebtN_combined,"label") <- "Kombinierte Schuldenänderung: Vor 2019 qoq, Ab 2019 vs. Quartal 2019 nominal"

names(qdata)[names(qdata) == "Debt_growth_vs_2019"] <- "Debt_growth_2019"
attr(qdata$Debt_growth_2019,        "label") <- "Schuldenänderung vs. jeweiliges Quartal 2019 (%)"

# Variablenauswahl (ergänzt um neue Schulden-Variablen)
vars <- c(
  "Country","Quarter","qtime","TimeIndex","TreatmentGroup","qcovid","qtreatment","qstrict",
  "StringencyIndex_Average","Debt_an","Debt_ar","inflation_index","base_deflator","gdp_deflator",
  "p_value_all_ages","QReal.GDP.Growth_gr",
  "DebtR_combined","DebtN_combined","Debt_growth_2019"
)
qdata <- qdata %>% select(any_of(vars), everything())



# 4) Gewünschte Reihenfolge herstellen, ohne Duplikate zu erzeugen
vars <- c(
  "Country","Quarter","qtime","TimeIndex","TreatmentGroup","qcovid","qtreatment","qstrict",
  "StringencyIndex_Average","Debt_an","Debt_ar","Debt_qoq", "DebtR_combined", "DebtN_combined","Debt_growth_2019",
  "inflation_index","base_deflator","gdp_deflator","p_value_all_ages", "nGDP_an", "rGDP_ar", "rGDP_LC", "QReal.GDP.Growth_gr", "rGDP_per_capita",
  "rGDP_pc_combined", "rGDP_pc_qoq"
)

qdata <- qdata %>% dplyr::relocate(dplyr::any_of(vars))

qdata <- dplyr::select(qdata, -rGDP_pc_vs_2019)


#-------------------------------------------------------------------------------
##DEBT SHARE OF GDP & GROWTH RATE-> SHARE >2019 AUF WERT JAHR 2019 GDP FESTGESETZT, GROWTH RATE SOMIT AUCH
##NOMINAL IN LC
qdata <- qdata %>%
  arrange(Country, TimeIndex) %>%
  # Zeitachsen bauen
  group_by(Country) %>%
  mutate(
    quarter_num = ((TimeIndex - 1L) %% 4L) + 1L,
    year_only   = 2015L + ((TimeIndex - 1L) %/% 4L)
  ) %>%
  # tatsächliches Jahres-BIP je Land/Jahr: Summe der 4 Quartale (nominal)
  group_by(Country, year_only) %>%
  mutate(
    GDP_year_actual = sum(nGDP_an, na.rm = TRUE)
  ) %>%
  # je Land: 2019er Jahres-BIP fixieren und Denominator bauen
  group_by(Country) %>%
  mutate(
    # fixes Jahres-BIP 2019 pro Land
    GDP_year_2019 = dplyr::first(GDP_year_actual[year_only == 2019]),
    
    # Denominator:
    #  <2019: aktuelles Jahres-BIP
    # >=2019: fixes 2019er Jahres-BIP
    GDP_denom = dplyr::if_else(
      year_only < 2019,
      GDP_year_actual,
      GDP_year_2019
    ),
    
    # Schuldenquote mit Jahres-BIP im Nenner
    Debt_pct_2019base = dplyr::if_else(
      !is.na(GDP_denom) & GDP_denom > 0,
      100 * Debt_an / GDP_denom,
      NA_real_
    ),
    
    # QoQ-Wachstumsrate dieser Quote
    Debt_pct_2019base_growth =
      (Debt_pct_2019base / dplyr::lag(Debt_pct_2019base) - 1) * 100
  ) %>%
  # Referenz-Quote 2019 je Land und Quartal holen
  group_by(Country, quarter_num) %>%
  mutate(
    Debt_pct_2019base_ref =
      dplyr::first(Debt_pct_2019base[year_only == 2019]),
    
    # Kombi-Variable:
    # <2019: QoQ-Wachstum,
    # >=2019: Abweichung ggü. 2019-Quartalsquote
    Debt_pct_2019_change = dplyr::if_else(
      year_only < 2019,
      Debt_pct_2019base_growth,
      dplyr::if_else(
        !is.na(Debt_pct_2019base_ref) & Debt_pct_2019base_ref != 0,
        (Debt_pct_2019base / Debt_pct_2019base_ref - 1) * 100,
        NA_real_
      )
    )
  ) %>%
  ungroup() %>%
  select(
    -GDP_year_actual,
    -GDP_year_2019,
    -GDP_denom,
    -Debt_pct_2019base_growth,
    -Debt_pct_2019base_ref
  )



# Labels
names(qdata)[names(qdata) == "Debt_pct_2019base"] <- "DebtN_share2019"
attr(qdata$DebtN_share2019,          "label") <- "Nominale Staatsschulden in % des nominalen BIP (nGDP_an) >2019 mit aktuellem GDP, ab 2019 auf 2019 GDP bezogen"

names(qdata)[names(qdata) == "Debt_pct_2019_change"] <- "DebtN_share2019_growth"
attr(qdata$DebtN_share2019_growth,"label") <- "Nominale Growth Rate der Staatsschulden QoQ mit >2019 festgesetzten Werte"
# Optional: nach vorn ziehen
qdata <- qdata %>% relocate(any_of(c("DebtN_share2019","DebtN_share2019_growth")), .after = Debt_ar)

pair <- qdata %>%
  select(DebtN_combined, DebtN_share2019_growth)

View(pair)

rm(economist, avg_pc_growth, clean_variables, data_capita, data_capita_plot, data_plot, data_plot_corrected, index_2019, mx_irag, pair, mx_hosp_d)

#-------------------------------------------------------------------------------
#REAL IN LC
qdata <- qdata %>%
  arrange(Country, TimeIndex) %>%
  # Zeitachsen bauen
  group_by(Country) %>%
  mutate(
    quarter_num = ((TimeIndex - 1L) %% 4L) + 1L,
    year_only   = 2015L + ((TimeIndex - 1L) %/% 4L)
  ) %>%
  # tatsächliches Jahres-BIP je Land/Jahr: Summe der 4 Quartale (nominal)
  group_by(Country, year_only) %>%
  mutate(
    GDP_year_actual = sum(rGDP_LC, na.rm = TRUE)
  ) %>%
  # je Land: 2019er Jahres-BIP fixieren und Denominator bauen
  group_by(Country) %>%
  mutate(
    # fixes Jahres-BIP 2019 pro Land
    GDP_year_2019 = dplyr::first(GDP_year_actual[year_only == 2019]),
    
    # Denominator:
    #  <2019: aktuelles Jahres-BIP
    # >=2019: fixes 2019er Jahres-BIP
    GDP_denom = dplyr::if_else(
      year_only < 2019,
      GDP_year_actual,
      GDP_year_2019
    ),
    
    # Schuldenquote mit Jahres-BIP im Nenner
    DebtR_pct_2019base = dplyr::if_else(
      !is.na(GDP_denom) & GDP_denom > 0,
      100 * Debt_ar / GDP_denom,
      NA_real_
    ),
    
    # QoQ-Wachstumsrate dieser Quote
    DebtR_pct_2019base_growth =
      (DebtR_pct_2019base / dplyr::lag(DebtR_pct_2019base) - 1) * 100
  ) %>%
  # Referenz-Quote 2019 je Land und Quartal holen
  group_by(Country, quarter_num) %>%
  mutate(
    DebtR_pct_2019base_ref =
      dplyr::first(DebtR_pct_2019base[year_only == 2019]),
    
    # Kombi-Variable:
    # <2019: QoQ-Wachstum,
    # >=2019: Abweichung ggü. 2019-Quartalsquote
    DebtR_pct_2019_change = dplyr::if_else(
      year_only < 2019,
      DebtR_pct_2019base_growth,
      dplyr::if_else(
        !is.na(DebtR_pct_2019base_ref) & DebtR_pct_2019base_ref != 0,
        (DebtR_pct_2019base / DebtR_pct_2019base_ref - 1) * 100,
        NA_real_
      )
    )
  ) %>%
  ungroup() %>%
  select(
    -GDP_year_actual,
    -GDP_year_2019,
    -GDP_denom,
    -DebtR_pct_2019base_growth,
    -DebtR_pct_2019base_ref
  )



# Labels
names(qdata)[names(qdata) == "DebtR_pct_2019base"] <- "DebtR_share2019"
attr(qdata$DebtR_share2019,          "label") <- "Reale Staatsschulden in % des realen BIP (nGDP_LC) in LC >2019 mit aktuellem GDP, ab 2019 auf 2019 GDP bezogen"


names(qdata)[names(qdata) == "DebtR_pct_2019_change"] <- "DebtR_share2019_growth"
attr(qdata$DebtR_share2019_growth,          "label") <- "Reale Growth Rate der Staatsschulden QoQ mit >2019 festgesetzten Werte"
# Optional: nach vorn ziehen
qdata <- qdata %>% relocate(any_of(c("DebtR_share2019","DebtR_share2019_growth")), .after = DebtN_share2019_growth)


###GIBT UNS: DEBT BESTAND VERÄNDERUNG (REAL UND NOMINELL) GEBUNDEN AN GDP 2019 UND SCHULDENQUOTE VERÄNDERUNG GEBUNDEN AN GDP (3 mAIN vARIABLEN)
rm(avg_pc_growth, clean_variables, data_capita, data_capita_plot, data_plot, data_plot_corrected, index_2019, pair)
# ==============================================================================
# CREATING OUTPUT GAP WITH HP
# =============================================================================

# ============================================
# SCHRITT 1: Daten vorbereiten
# ============================================

qdata2 <- rGDP_LC %>%
  mutate(
    # A) Konvertiere Quarter-Format "Q1.2015" zu Datum
    year  = as.numeric(str_sub(Quarter, 4, 7)),   # "Q1.2015" → 2015
    q_num = as.numeric(str_sub(Quarter, 2, 2)),   # "Q1.2015" → 1
    
    # Konvertiere zu Datum (Q1 = Jan 1, Q2 = Apr 1, Q3 = Jul 1, Q4 = Oct 1)
    quarter_date = ymd(paste0(year, "-", (q_num - 1) * 3 + 1, "-01")),
    
    # B) Log-Transformation des GDP
    log_gdp = log(rGDP_LC)
  ) %>%
  arrange(Country, quarter_date)

# Checks wie gehabt
qdata2 %>%
  select(Country, Quarter, quarter_date, rGDP_LC) %>%
  head(20)

qdata2 %>%
  group_by(Country) %>%
  summarise(
    n_quarters     = n(),
    first_q        = min(quarter_date),
    last_q         = max(quarter_date),
    n_missing_gdp  = sum(is.na(rGDP_LC))
  ) %>%
  print(n = Inf)

# ============================================
# SCHRITT 2: Pre-COVID Daten filtern (JETZT: 2010–2019)
# ============================================

pre_covid_data <- qdata2 %>%
  filter(quarter_date >= as.Date("2015-01-01") &
           quarter_date <= as.Date("2019-12-31"))

# Check: Wie viele Beobachtungen pro Land?
pre_covid_data %>%
  count(Country) %>%
  print(n = Inf)

# Visualisierung: Pre-COVID Log GDP
pre_covid_data %>%
  filter(Country %in% c("AUS", "DEU", "USA", "ITA", "FRA", "SWE")) %>%
  ggplot(aes(x = quarter_date, y = log_gdp, color = Country)) +
  geom_line() +
  labs(title = "Pre-COVID Log Real GDP (2010-2019)",
       subtitle = "Sollte glatte, aufwärts-gerichtete Linien zeigen",
       y = "Log Real GDP",
       x = "") +
  theme_minimal()

# ============================================
# SCHRITT 3: HP-Filter anwenden (pro Land)
# ============================================

countries <- unique(pre_covid_data$Country)

cat("Berechne HP-Trends für", length(countries), "Länder...\n")

calculate_hp_trend <- function(country_name) {
  
  country_data <- pre_covid_data %>%
    filter(Country == country_name) %>%
    arrange(quarter_date)
  
  if (nrow(country_data) < 16) {
    warning(paste("Not enough data for", country_name, 
                  "- only", nrow(country_data), "quarters - skipping"))
    return(NULL)
  }
  
  if (any(is.na(country_data$log_gdp))) {
    warning(paste("Missing GDP data for", country_name, "- skipping"))
    return(NULL)
  }
  
  hp_result <- hpfilter(country_data$log_gdp, 
                        freq = 1600, 
                        type = "lambda")
  
  trend_series <- hp_result$trend
  
  # Durchschnitt der letzten 4 Quarters (= 1 Jahr)
  trend_growth_quarterly <- mean(diff(tail(trend_series, 5)))
  trend_growth_annual    <- (exp(trend_growth_quarterly * 4) - 1) * 100
  
  cat(country_name, "- Trend growth:", round(trend_growth_annual, 2), "% p.a.\n")
  
  last_trend_value <- tail(trend_series, 1)
  last_date        <- max(country_data$quarter_date)
  
  # Extrapoliere bis 2023Q4 (unverändert)
  future_dates <- seq.Date(last_date + months(3), 
                           as.Date("2024-12-31"), 
                           by = "quarter")
  n_forecast  <- length(future_dates)
  
  trend_forecast <- last_trend_value + trend_growth_quarterly * (1:n_forecast)
  
  tibble(
    Country      = country_name,
    quarter_date = c(country_data$quarter_date, future_dates),
    log_gdp_trend = c(trend_series, trend_forecast),
    gdp_trend     = exp(c(trend_series, trend_forecast)),
    trend_type    = c(rep("historical", length(trend_series)), 
                      rep("forecast",  n_forecast))
  )
}

hp_trends <- map_dfr(countries, calculate_hp_trend)

cat("\nHP-Trends berechnet für", length(unique(hp_trends$Country)), "Länder.\n")

hp_trends %>%
  count(Country, trend_type) %>%
  pivot_wider(names_from = trend_type, values_from = n) %>%
  print(n = Inf)

# ============================================
# VISUALISIERUNG: Trend Check
# ============================================

sample_countries <- c("AUS", "DEU", "USA", "GBR", "ITA", "SWE", "CHE", "JPN")

hp_trends %>%
  filter(Country %in% sample_countries) %>%
  left_join(pre_covid_data %>% select(Country, quarter_date, log_gdp),
            by = c("Country", "quarter_date")) %>%
  ggplot(aes(x = quarter_date)) +
  geom_line(aes(y = log_gdp), color = "black", alpha = 0.5, size = 0.8) +
  geom_line(aes(y = log_gdp_trend, color = trend_type), size = 1) +
  facet_wrap(~Country, scales = "free_y", ncol = 2) +
  labs(title = "HP-Trend Estimation and Extrapolation",
       subtitle = "Black: Actual GDP | Blue: Historical Trend | Red: Forecast",
       y = "Log Real GDP",
       x = "") +
  theme_minimal() +
  scale_color_manual(values = c("historical" = "blue", "forecast" = "red"),
                     name = "Trend Type")

# ============================================
# OPTION 1: Alle 38 Länder in einem Plot
# ============================================

all_countries <- unique(hp_trends$Country)

cat("Plotte", length(all_countries), "Länder...\n")

p_all <- hp_trends %>%
  left_join(pre_covid_data %>% select(Country, quarter_date, log_gdp),
            by = c("Country", "quarter_date")) %>%
  ggplot(aes(x = quarter_date)) +
  geom_line(aes(y = log_gdp), color = "black", alpha = 0.5, size = 0.6,
            na.rm = TRUE) +
  geom_line(aes(y = log_gdp_trend, color = trend_type), size = 0.8) +
  facet_wrap(~Country, scales = "free_y", ncol = 6) +
  labs(title = "HP-Trend Estimation and Extrapolation - All OECD Countries",
       subtitle = "Black: Actual GDP (Pre-COVID) | Blue: Historical Trend | Red: Forecast",
       y = "Log Real GDP",
       x = "") +
  theme_minimal() +
  scale_color_manual(values = c("historical" = "blue", "forecast" = "red"),
                     name = "Trend Type",
                     labels = c("Historical (HP)", "Forecast")) +
  theme(
    legend.position = "bottom",
    strip.text      = element_text(size = 7, face = "bold"),
    axis.text       = element_text(size = 6),
    axis.title      = element_text(size = 8)
  )

ggsave("hp_trends_all_countries.png", 
       plot  = p_all, 
       width = 16, 
       height = 20, 
       dpi   = 300)

print(p_all)

cat("✓ Gespeichert: hp_trends_all_countries.png\n")

# ============================================
# SCHRITT 4: Merge mit Original-Daten
# ============================================

qdata_with_trend <- qdata2 %>%
  left_join(hp_trends %>% select(Country, quarter_date, log_gdp_trend, gdp_trend),
            by = c("Country", "quarter_date"))

missing_trends <- sum(is.na(qdata_with_trend$log_gdp_trend))
cat("\nMissing trends:", missing_trends, "rows\n")

if (missing_trends > 0) {
  cat("Countries with missing trends:\n")
  qdata_with_trend %>%
    filter(is.na(log_gdp_trend)) %>%
    count(Country) %>%
    print()
}

# ============================================
# SCHRITT 5: Output Gap berechnen (y_t)
# ============================================

qdata_with_trend <- qdata_with_trend %>%
  mutate(
    y_t    = (log_gdp - log_gdp_trend) * 100,
    y_t_pct = (rGDP_LC - gdp_trend) / gdp_trend * 100
  )

# ============================================
# SCHRITT 6: Validierung
# ============================================

cat("\n=== VALIDIERUNG ===\n")

# Pre-COVID Gap jetzt 2010–2019
pre_covid_gaps <- qdata_with_trend %>%
  filter(quarter_date >= as.Date("2010-01-01"),
         quarter_date <= as.Date("2019-12-31")) %>%
  pull(y_t)

cat("\nPre-COVID Output Gap (2010-2019):\n")
cat("  Mean:", round(mean(pre_covid_gaps, na.rm = TRUE), 2), "%\n")
cat("  SD:",   round(sd(pre_covid_gaps,   na.rm = TRUE), 2), "%\n")

covid_gaps <- qdata_with_trend %>%
  filter(quarter_date >= as.Date("2020-01-01"),
         quarter_date <= as.Date("2020-12-31")) %>%
  pull(y_t)

cat("\nCOVID-2020 Output Gap:\n")
cat("  Mean:", round(mean(covid_gaps, na.rm = TRUE), 2), "%\n")
cat("  Min:",  round(min(covid_gaps,   na.rm = TRUE), 2), "%\n")

cat("\nAm stärksten betroffene Länder (worst output gap 2020-2021):\n")
qdata_with_trend %>%
  filter(quarter_date >= as.Date("2020-01-01"),
         quarter_date <= as.Date("2021-12-31")) %>%
  group_by(Country) %>%
  summarise(
    worst_gap    = min(y_t, na.rm = TRUE),
    worst_quarter = Quarter[which.min(y_t)],
    mean_gap      = mean(y_t, na.rm = TRUE)
  ) %>%
  arrange(worst_gap) %>%
  head(10) %>%
  print()




# ============================================
# VISUALISIERUNG: Output Gaps
# ============================================

# A) GDP vs. Trend über Zeit
qdata_with_trend %>%
  filter(Country %in% sample_countries,
         quarter_date >= as.Date("2010-01-01")) %>%
  ggplot(aes(x = quarter_date)) +
  geom_line(aes(y = log_gdp, color = "Actual GDP"), size = 0.8) +
  geom_line(aes(y = log_gdp_trend, color = "HP Trend"), 
            linetype = "dashed", size = 0.8) +
  geom_vline(xintercept = as.Date("2020-01-01"), 
             linetype = "dotted", alpha = 0.5) +
  facet_wrap(~Country, scales = "free_y", ncol = 2) +
  labs(title = "Real GDP vs. HP-Trend (2015-2023)",
       subtitle = "Dotted line: COVID-19 begins",
       y = "Real GDP (absolute values)",
       x = "",
       color = "") +
  theme_minimal() +
  scale_color_manual(values = c("Actual GDP" = "black", 
                                "HP Trend" = "red")) +
  theme(legend.position = "bottom")

qdata_with_trend %>%
  filter(quarter_date >= as.Date("2010-01-01")) %>%
  ggplot(aes(x = quarter_date)) +
  geom_line(aes(y = log_gdp, color = "Actual log GDP"), size = 0.8) +
  geom_line(aes(y = log_gdp_trend, color = "HP Trend (log)"), 
            linetype = "dashed", size = 0.8) +
  geom_vline(xintercept = as.Date("2020-01-01"),
             linetype = "dotted", alpha = 0.5) +
  facet_wrap(~Country, scales = "free_y", ncol = 6) +
  labs(title = "Log Real GDP vs. Log HP-Trend (2010–2023)",
       y = "log GDP",
       x = "",
       color = "") +
  theme_minimal() +
  scale_color_manual(values = c("Actual log GDP" = "black",
                                "HP Trend (log)" = "red")) +
  theme(legend.position = "bottom")



# B) Output Gap über Zeit
qdata_with_trend %>%
  filter(Country %in% sample_countries,
         quarter_date >= as.Date("2019-01-01")) %>%
  ggplot(aes(x = quarter_date, y = y_t, color = Country, group = Country)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = as.Date("2020-01-01"), 
             linetype = "dotted", alpha = 0.5) +
  labs(title = "Output Gap (y_t) during COVID-19",
       subtitle = "Negative values = GDP below potential",
       y = "Output Gap (% deviation from trend)",
       x = "",
       color = "Country") +
  theme_minimal()

# C) Heatmap: Output Gaps über Zeit und Länder
qdata_with_trend %>%
  filter(quarter_date >= as.Date("2019-01-01"),
         !is.na(y_t)) %>%
  ggplot(aes(x = quarter_date, y = Country, fill = y_t)) +
  geom_tile() +
  geom_vline(xintercept = as.Date("2020-01-01"), 
             color = "white", linetype = "dashed", size = 1) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue",
                       midpoint = 0,
                       name = "Output Gap (%)") +
  labs(title = "Output Gap Heatmap: All Countries",
       subtitle = "Red = Below potential, Blue = Above potential",
       x = "", y = "Country") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))


# ============================================
# BONUS: Quick descriptive table
# ============================================

# Deskriptive Tabelle für Paper
desc_table <- qdata_with_trend %>%
  filter(quarter_date >= as.Date("2020-01-01")) %>%
  group_by(Country) %>%
  summarise(
    `Mean Output Gap` = round(mean(y_t, na.rm = TRUE), 2),
    `Min Output Gap` = round(min(y_t, na.rm = TRUE), 2),
    `Max Output Gap` = round(max(y_t, na.rm = TRUE), 2),
    `SD Output Gap` = round(sd(y_t, na.rm = TRUE), 2),
  ) %>%
  arrange(`Min Output Gap`)

print(desc_table, n = Inf)


output_gap_2010 <- qdata_with_trend



qdata <- qdata %>%
  left_join(
    qdata_with_trend %>%
      select(Country, Quarter,
             log_gdp_trend, gdp_trend,
             y_t, y_t_pct, log_gdp),             
    by = c("Country", "Quarter")
  )




qdata <- qdata %>%
  select(Country, Quarter, qtime, TimeIndex, TreatmentGroup, qcovid, qtreatment, qstrict, StringencyIndex_Average, log_gdp, log_gdp_trend, 
         gdp_trend, y_t, y_t_pct, nGDP_an, rGDP_ar, rGDP_LC, QReal.GDP.Growth_gr, rGDP_per_capita, rGDP_pc_combined, rGDP_pc_qoq, rGDP_pc_growth_2019, everything())

qdata$rGDP_per_capita<-qdata$rGDP_per_capita*10

##GIBT UNS DIE OUTPUT GAP VARIABLEN (Y_T)



# ==============================================================================
# DEBT GAP BERECHNEN (nominal UND real Debt in Local Currency, LC)
# ==============================================================================

qdata2 <- qdata %>%
  mutate(
    year  = as.numeric(str_sub(Quarter, 4, 7)),
    q_num = as.numeric(str_sub(Quarter, 2, 2)),
    quarter_date = ymd(paste0(year, "-", (q_num - 1) * 3 + 1, "-01")),
    
    # Nominal
    log_debt = log(Debt_an),
    
    # Real (NEU)
    log_debt_r = log(Debt_ar)
  ) %>%
  arrange(Country, quarter_date)

# Checks
qdata2 %>%
  select(Country, Quarter, quarter_date, Debt_an, Debt_ar) %>%
  head(20)

qdata2 %>%
  group_by(Country) %>%
  summarise(
    n_quarters      = n(),
    first_q         = min(quarter_date),
    last_q          = max(quarter_date),
    n_missing_debt_n = sum(is.na(Debt_an)),
    n_missing_debt_r = sum(is.na(Debt_ar))
  ) %>%
  print(n = Inf)

# ============================================
# SCHRITT 2: Pre-COVID Daten filtern (2015–2019)
# ============================================

pre_covid_data <- qdata2 %>%
  filter(quarter_date >= as.Date("2015-01-01") &
           quarter_date <= as.Date("2019-12-31"))

# Check: Wie viele Beobachtungen pro Land?
pre_covid_data %>%
  count(Country) %>%
  print(n = Inf)

# Visualisierung: Pre-COVID Log Debt (nominal vs. real)
pre_covid_data %>%
  filter(Country %in% c("AUS", "DEU", "USA", "ITA", "FRA", "SWE")) %>%
  ggplot(aes(x = quarter_date, color = Country)) +
  geom_line(aes(y = log_debt, linetype = "Nominal")) +
  geom_line(aes(y = log_debt_r, linetype = "Real")) +
  labs(title = "Pre-COVID Log Debt (2015–2019)",
       subtitle = "Nominal vs. Real (LC)",
       y = "Log Debt (LC)",
       x = "") +
  theme_minimal()

# ============================================
# SCHRITT 3: HP-Filter anwenden (pro Land) - NOMINAL UND REAL
# ============================================

countries <- unique(pre_covid_data$Country)

cat("Berechne HP-Trends für", length(countries), "Länder...\n")

calculate_hp_trend <- function(country_name) {
  
  country_data <- pre_covid_data %>%
    filter(Country == country_name) %>%
    arrange(quarter_date)
  
  if (nrow(country_data) < 16) {
    warning(paste("Not enough data for", country_name, 
                  "- only", nrow(country_data), "quarters - skipping"))
    return(NULL)
  }
  
  if (any(is.na(country_data$log_debt)) | any(is.na(country_data$log_debt_r))) {
    warning(paste("Missing debt data for", country_name, "- skipping"))
    return(NULL)
  }
  
  # HP-Filter für NOMINAL
  hp_result <- hpfilter(country_data$log_debt, 
                        freq = 1600, 
                        type = "lambda")
  trend_series <- hp_result$trend
  trend_growth_quarterly <- mean(diff(tail(trend_series, 5)))
  trend_growth_annual <- (exp(trend_growth_quarterly * 4) - 1) * 100
  
  # HP-Filter für REAL (NEU)
  hp_result_r <- hpfilter(country_data$log_debt_r, 
                          freq = 1600, 
                          type = "lambda")
  trend_series_r <- hp_result_r$trend
  trend_growth_quarterly_r <- mean(diff(tail(trend_series_r, 5)))
  trend_growth_annual_r <- (exp(trend_growth_quarterly_r * 4) - 1) * 100
  
  cat(country_name, 
      "- Nominal:", round(trend_growth_annual, 2), "% p.a.",
      "| Real:", round(trend_growth_annual_r, 2), "% p.a.\n")
  
  last_trend_value <- tail(trend_series, 1)
  last_trend_value_r <- tail(trend_series_r, 1)
  last_date <- max(country_data$quarter_date)
  
  # Extrapoliere bis 2024-12-31
  future_dates <- seq.Date(last_date + months(3), 
                           as.Date("2024-12-31"), 
                           by = "quarter")
  n_forecast <- length(future_dates)
  
  trend_forecast <- last_trend_value + trend_growth_quarterly * (1:n_forecast)
  trend_forecast_r <- last_trend_value_r + trend_growth_quarterly_r * (1:n_forecast)
  
  tibble(
    Country        = country_name,
    quarter_date   = c(country_data$quarter_date, future_dates),
    # Nominal
    log_debt_trend = c(trend_series, trend_forecast),
    debt_trend     = exp(c(trend_series, trend_forecast)),
    # Real (NEU)
    log_debt_trend_r = c(trend_series_r, trend_forecast_r),
    debt_trend_r     = exp(c(trend_series_r, trend_forecast_r)),
    trend_type     = c(rep("historical", length(trend_series)), 
                       rep("forecast",  n_forecast))
  )
}

hp_trends <- map_dfr(countries, calculate_hp_trend)

cat("\nHP-Trends berechnet für", length(unique(hp_trends$Country)), "Länder.\n")

hp_trends %>%
  count(Country, trend_type) %>%
  pivot_wider(names_from = trend_type, values_from = n) %>%
  print(n = Inf)

# ============================================
# VISUALISIERUNG: Trend Check (Nominal vs. Real)
# ============================================

sample_countries <- c("AUS", "DEU", "USA", "GBR", "ITA", "SWE", "CHE", "JPN")

# Nominal
hp_trends %>%
  filter(Country %in% sample_countries) %>%
  left_join(pre_covid_data %>% select(Country, quarter_date, log_debt),
            by = c("Country", "quarter_date")) %>%
  ggplot(aes(x = quarter_date)) +
  geom_line(aes(y = log_debt), color = "black", alpha = 0.5, size = 0.8) +
  geom_line(aes(y = log_debt_trend, color = trend_type), size = 1) +
  facet_wrap(~Country, scales = "free_y", ncol = 2) +
  labs(title = "HP-Trend Estimation and Extrapolation (NOMINAL Debt, LC)",
       subtitle = "Black: Actual Debt | Blue: Historical Trend | Red: Forecast",
       y = "Log Nominal Debt (LC)",
       x = "") +
  theme_minimal() +
  scale_color_manual(values = c("historical" = "blue", "forecast" = "red"),
                     name = "Trend Type")

# Real (NEU)
hp_trends %>%
  filter(Country %in% sample_countries) %>%
  left_join(pre_covid_data %>% select(Country, quarter_date, log_debt_r),
            by = c("Country", "quarter_date")) %>%
  ggplot(aes(x = quarter_date)) +
  geom_line(aes(y = log_debt_r), color = "black", alpha = 0.5, size = 0.8) +
  geom_line(aes(y = log_debt_trend_r, color = trend_type), size = 1) +
  facet_wrap(~Country, scales = "free_y", ncol = 2) +
  labs(title = "HP-Trend Estimation and Extrapolation (REAL Debt, LC)",
       subtitle = "Black: Actual Debt | Blue: Historical Trend | Red: Forecast",
       y = "Log Real Debt (LC)",
       x = "") +
  theme_minimal() +
  scale_color_manual(values = c("historical" = "blue", "forecast" = "red"),
                     name = "Trend Type")

# ============================================
# Alle 38 Länder in einem Plot (Nominal)
# ============================================

all_countries <- unique(hp_trends$Country)

cat("Plotte", length(all_countries), "Länder...\n")

p_all <- hp_trends %>%
  left_join(pre_covid_data %>% select(Country, quarter_date, log_debt),
            by = c("Country", "quarter_date")) %>%
  ggplot(aes(x = quarter_date)) +
  geom_line(aes(y = log_debt), color = "black", alpha = 0.5, size = 0.6,
            na.rm = TRUE) +
  geom_line(aes(y = log_debt_trend, color = trend_type), size = 0.8) +
  facet_wrap(~Country, scales = "free_y", ncol = 6) +
  labs(title = "HP-Trend Estimation and Extrapolation - All OECD Countries (Nominal Debt, LC)",
       subtitle = "Black: Actual Debt (Pre-COVID) | Blue: Historical Trend | Red: Forecast",
       y = "Log Nominal Debt (LC)",
       x = "") +
  theme_minimal() +
  scale_color_manual(values = c("historical" = "blue", "forecast" = "red"),
                     name = "Trend Type",
                     labels = c("Historical (HP)", "Forecast")) +
  theme(
    legend.position = "bottom",
    strip.text      = element_text(size = 7, face = "bold"),
    axis.text       = element_text(size = 6),
    axis.title      = element_text(size = 8)
  )

ggsave("hp_trends_all_countries_nominal.png", 
       plot  = p_all, 
       width = 16, 
       height = 20, 
       dpi   = 300)

print(p_all)

cat("✓ Gespeichert: hp_trends_all_countries_nominal.png\n")

# Alle 38 Länder (Real) (NEU)
p_all_r <- hp_trends %>%
  left_join(pre_covid_data %>% select(Country, quarter_date, log_debt_r),
            by = c("Country", "quarter_date")) %>%
  ggplot(aes(x = quarter_date)) +
  geom_line(aes(y = log_debt_r), color = "black", alpha = 0.5, size = 0.6,
            na.rm = TRUE) +
  geom_line(aes(y = log_debt_trend_r, color = trend_type), size = 0.8) +
  facet_wrap(~Country, scales = "free_y", ncol = 6) +
  labs(title = "HP-Trend Estimation and Extrapolation - All OECD Countries (Real Debt, LC)",
       subtitle = "Black: Actual Debt (Pre-COVID) | Blue: Historical Trend | Red: Forecast",
       y = "Log Real Debt (LC)",
       x = "") +
  theme_minimal() +
  scale_color_manual(values = c("historical" = "blue", "forecast" = "red"),
                     name = "Trend Type",
                     labels = c("Historical (HP)", "Forecast")) +
  theme(
    legend.position = "bottom",
    strip.text      = element_text(size = 7, face = "bold"),
    axis.text       = element_text(size = 6),
    axis.title      = element_text(size = 8)
  )

ggsave("hp_trends_all_countries_real.png", 
       plot  = p_all_r, 
       width = 16, 
       height = 20, 
       dpi   = 300)

print(p_all_r)

cat("✓ Gespeichert: hp_trends_all_countries_real.png\n")

# ============================================
# SCHRITT 4: Merge mit Original-Daten
# ============================================

qdata_with_trend <- qdata2 %>%
  left_join(hp_trends %>% select(Country, quarter_date, 
                                 log_debt_trend, debt_trend,
                                 log_debt_trend_r, debt_trend_r),
            by = c("Country", "quarter_date"))

missing_trends <- sum(is.na(qdata_with_trend$log_debt_trend))
missing_trends_r <- sum(is.na(qdata_with_trend$log_debt_trend_r))
cat("\nMissing trends (nominal):", missing_trends, "rows\n")
cat("Missing trends (real):", missing_trends_r, "rows\n")

if (missing_trends > 0) {
  cat("Countries with missing trends:\n")
  qdata_with_trend %>%
    filter(is.na(log_debt_trend)) %>%
    count(Country) %>%
    print()
}

# ============================================
# SCHRITT 5: Debt Gap berechnen (nominal UND real)
# ============================================

qdata_with_trend <- qdata_with_trend %>%
  mutate(
    # Nominal
    d_t     = (log_debt - log_debt_trend) * 100,
    d_t_pct = (Debt_an - debt_trend) / debt_trend * 100,
    
    # Real (NEU)
    d_t_r     = (log_debt_r - log_debt_trend_r) * 100,
    d_t_pct_r = (Debt_ar - debt_trend_r) / debt_trend_r * 100
  )

# ============================================
# SCHRITT 6: Validierung
# ============================================

cat("\n=== VALIDIERUNG ===\n")

# Pre-COVID Gap (2010–2019)
pre_covid_gaps <- qdata_with_trend %>%
  filter(quarter_date >= as.Date("2010-01-01"),
         quarter_date <= as.Date("2019-12-31"))

cat("\nPre-COVID Debt Gap NOMINAL (2010-2019):\n")
cat("  Mean:", round(mean(pre_covid_gaps$d_t, na.rm = TRUE), 2), "%\n")
cat("  SD:",   round(sd(pre_covid_gaps$d_t,   na.rm = TRUE), 2), "%\n")

cat("\nPre-COVID Debt Gap REAL (2010-2019):\n")
cat("  Mean:", round(mean(pre_covid_gaps$d_t_r, na.rm = TRUE), 2), "%\n")
cat("  SD:",   round(sd(pre_covid_gaps$d_t_r,   na.rm = TRUE), 2), "%\n")

covid_gaps <- qdata_with_trend %>%
  filter(quarter_date >= as.Date("2020-01-01"),
         quarter_date <= as.Date("2020-12-31"))

cat("\nCOVID-2020 Debt Gap NOMINAL:\n")
cat("  Mean:", round(mean(covid_gaps$d_t, na.rm = TRUE), 2), "%\n")
cat("  Max:",  round(max(covid_gaps$d_t,  na.rm = TRUE), 2), "%\n")

cat("\nCOVID-2020 Debt Gap REAL:\n")
cat("  Mean:", round(mean(covid_gaps$d_t_r, na.rm = TRUE), 2), "%\n")
cat("  Max:",  round(max(covid_gaps$d_t_r,  na.rm = TRUE), 2), "%\n")

# Korrelation zwischen nominal und real
cat("\nKorrelation nominal vs. real Debt Gap:\n")
cat("  d_t vs d_t_r:", round(cor(qdata_with_trend$d_t, qdata_with_trend$d_t_r, use = "complete.obs"), 3), "\n")

cat("\nAm stärksten betroffene Länder (höchster REAL debt gap 2020-2021):\n")
qdata_with_trend %>%
  filter(quarter_date >= as.Date("2020-01-01"),
         quarter_date <= as.Date("2021-12-31")) %>%
  group_by(Country) %>%
  summarise(
    max_gap_nominal = max(d_t, na.rm = TRUE),
    max_gap_real    = max(d_t_r, na.rm = TRUE),
    worst_quarter   = Quarter[which.max(d_t_r)],
    mean_gap_real   = mean(d_t_r, na.rm = TRUE)
  ) %>%
  arrange(desc(max_gap_real)) %>%
  head(10) %>%
  print()

# ============================================
# VISUALISIERUNG: Debt Gaps (Nominal vs. Real)
# ============================================

# A) Debt vs. Trend über Zeit (Nominal)
qdata_with_trend %>%
  filter(Country %in% sample_countries,
         quarter_date >= as.Date("2010-01-01")) %>%
  ggplot(aes(x = quarter_date)) +
  geom_line(aes(y = log_debt, color = "Actual Debt (log)"), size = 0.8) +
  geom_line(aes(y = log_debt_trend, color = "HP Trend (log)"), 
            linetype = "dashed", size = 0.8) +
  geom_vline(xintercept = as.Date("2020-01-01"), 
             linetype = "dotted", alpha = 0.5) +
  facet_wrap(~Country, scales = "free_y", ncol = 2) +
  labs(title = "Log NOMINAL Debt (LC) vs. HP-Trend (2010–2024)",
       subtitle = "Dotted line: COVID-19 begins",
       y = "Log Nominal Debt (LC)",
       x = "",
       color = "") +
  theme_minimal() +
  scale_color_manual(values = c("Actual Debt (log)" = "black", 
                                "HP Trend (log)" = "red")) +
  theme(legend.position = "bottom")

# A2) Debt vs. Trend über Zeit (Real) (NEU)
qdata_with_trend %>%
  filter(Country %in% sample_countries,
         quarter_date >= as.Date("2010-01-01")) %>%
  ggplot(aes(x = quarter_date)) +
  geom_line(aes(y = log_debt_r, color = "Actual Debt (log)"), size = 0.8) +
  geom_line(aes(y = log_debt_trend_r, color = "HP Trend (log)"), 
            linetype = "dashed", size = 0.8) +
  geom_vline(xintercept = as.Date("2020-01-01"), 
             linetype = "dotted", alpha = 0.5) +
  facet_wrap(~Country, scales = "free_y", ncol = 2) +
  labs(title = "Log REAL Debt (LC) vs. HP-Trend (2010–2024)",
       subtitle = "Dotted line: COVID-19 begins",
       y = "Log Real Debt (LC)",
       x = "",
       color = "") +
  theme_minimal() +
  scale_color_manual(values = c("Actual Debt (log)" = "black", 
                                "HP Trend (log)" = "red")) +
  theme(legend.position = "bottom")

# B) Debt Gap über Zeit (Nominal vs. Real)
qdata_with_trend %>%
  filter(Country %in% sample_countries,
         quarter_date >= as.Date("2019-01-01")) %>%
  pivot_longer(cols = c(d_t, d_t_r), 
               names_to = "type", 
               values_to = "gap") %>%
  mutate(type = ifelse(type == "d_t", "Nominal", "Real")) %>%
  ggplot(aes(x = quarter_date, y = gap, color = Country, linetype = type)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = as.Date("2020-01-01"), 
             linetype = "dotted", alpha = 0.5) +
  labs(title = "Debt Gap (d_t) during COVID-19: Nominal vs. Real",
       subtitle = "Positive values = Debt above HP-trend",
       y = "Debt Gap (% deviation from trend)",
       x = "",
       color = "Country",
       linetype = "Type") +
  theme_minimal()

# C) Heatmap: Debt Gaps über Zeit und Länder (Real)
qdata_with_trend %>%
  filter(quarter_date >= as.Date("2019-01-01"),
         !is.na(d_t_r)) %>%
  ggplot(aes(x = quarter_date, y = Country, fill = d_t_r)) +
  geom_tile() +
  geom_vline(xintercept = as.Date("2020-01-01"), 
             color = "white", linetype = "dashed", size = 1) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0,
                       name = "Real Debt Gap (%)") +
  labs(title = "REAL Debt Gap Heatmap: All Countries",
       subtitle = "Blue = Below trend, Red = Above trend",
       x = "", y = "Country") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

# ============================================
# BONUS: Quick descriptive table
# ============================================

desc_table <- qdata_with_trend %>%
  filter(quarter_date >= as.Date("2020-01-01")) %>%
  group_by(Country) %>%
  summarise(
    `Mean Nominal Gap` = round(mean(d_t, na.rm = TRUE), 2),
    `Max Nominal Gap`  = round(max(d_t, na.rm = TRUE), 2),
    `Mean Real Gap`    = round(mean(d_t_r, na.rm = TRUE), 2),
    `Max Real Gap`     = round(max(d_t_r, na.rm = TRUE), 2),
  ) %>%
  arrange(desc(`Max Real Gap`))

print(desc_table, n = Inf)

# ============================================
# FINAL: Merge mit qdata
# ============================================

qdata <- qdata %>%
  left_join(
    qdata_with_trend %>%
      select(Country, Quarter,
             # Nominal
             log_debt_trend, debt_trend, d_t, d_t_pct, log_debt,
             # Real (NEU)
             log_debt_trend_r, debt_trend_r, d_t_r, d_t_pct_r, log_debt_r),
    by = c("Country", "Quarter")
  )

# Check
cat("\n=== FINAL CHECK ===\n")
cat("Variablen in qdata:\n")
names(qdata)[grepl("d_t|debt", names(qdata), ignore.case = TRUE)]


##debt gap erstellt, d_t und d_t_pct = nominal und d_t_r und d_t_pct_r = real

# =============================================================================
####################DATA LOADING AND MUTATION FINISHED#########################
# =============================================================================

# Merge: qdata as base, add fm1_q and oxd_q
merged_data <- qdata %>%
  left_join(oxd_q, by = c("Country", "Quarter"))

# Check
cat("Result: ", nrow(merged_data), " rows (should be ", nrow(qdata), ")\n", sep = "")
cat("Variables: ", ncol(merged_data), "\n")



qdata <- qdata %>%
  left_join(
    oxd_q,
    by = c("Country", "Quarter")
  )





rm(desc_table, hp_trends, output_gap_2010, pre_covid_data, pre_covid_gaps, qdata_with_trend, covid_gaps, average_data, avg_pc_growth, base_deflator, clean_variables, codes_by_cat, data_capita, data_capita_plot, data_plot, data_plot_corrected, decline_2022,
   desc_table, economist, ed_vals, filtered_data, country_descriptives, p_add, p_quarterly, p_values, p_values, yearly_comparison, total_na, tmp, 
   timeunit_table, special_check, rows_with_na_count, quartile_summary, qdata_with_deflator, qdata_with_trend, outliner_summary, outlier_analysis, inflation_comparison,
   outlier_summary, na_per_col, hp_trends)





# Die gelisteten Variablen aus dem Datensatz entfernen
qdata3 <- subset(qdata, select = -c(TreatmentGroup, qcovid, qstrict, base_deflator, gdp_deflator, GDP_index_n,
  C1M_School.closing, C1M_Flag, 
  C2M_Workplace.closing, C2M_Flag, 
  C3M_Cancel.public.events, C3M_Flag, 
  C4M_Restrictions.on.gatherings, C4M_Flag, 
  C5M_Close.public.transport, C5M_Flag, 
  C6M_Stay.at.home.requirements, C6M_Flag, 
  C7M_Restrictions.on.internal.movement, C7M_Flag, 
  C8EV_International.travel.controls, 
  E1_Income.support, E1_Flag, 
  E2_Debt.contract.relief, 
  E3_Fiscal.measures, 
  E4_International.support, 
  H1_Public.information.campaigns, H1_Flag, 
  H2_Testing.policy, 
  H3_Contact.tracing, 
  H4_Emergency.investment.in.healthcare, 
  H5_Investment.in.vaccines, 
  H6M_Facial.Coverings, H6M_Flag, 
  H7_Vaccination.policy, H7_Flag, 
  H8M_Protection.of.elderly.people, H8M_Flag, 
  V1_Vaccine.Prioritisation..summary., 
  V2A_Vaccine.Availability..summary., 
  V2D_Medically..clinically.vulnerable..Non.elderly., 
  V2E_Education, 
  V2F_Frontline.workers...non.healthcare., 
  V2G_Frontline.workers...healthcare., 
  V3_Vaccine.Financial.Support..summary., 
  V4_Mandatory.Vaccination..summary., 
  GovernmentResponseIndex_Average, 
  ContainmentHealthIndex_Average, 
  EconomicSupportIndex,SortKey, ab_us, on_us, health_us, 
  nhealth_us, acc_us, of_us, be_us, 
  gu_us, qf_us, ab_per, on_per, 
  ab_ppp, on_ppp, health_per, health_ppp, 
  nhealth_per, nhealth_ppp, acc_per, acc_ppp, 
  of_per, of_ppp, be_per, be_ppp, 
  gu_per, gu_ppp, qf_us.1, qf_ppp, 
  realgdp_ppp, realgdp_per, 
  total_netspend_usd, total_moving_usd, 
  share_onw0_mo, share_on_mo, share_onw0_t, share_on_t, 
  share_nhealth_mo, share_nhealth_t, share_health_mo, share_health_t, 
  share_acc_mo, share_acc_t, share_nhealth_on, share_health_on, 
  share_of_mo, share_of_t, share_be_mo, share_be_t, 
  share_gu_mo, share_gu_t, share_be_of, share_gu_of, 
  total_intensity_ab, total_intensity_per, total_intensity_ppp, total_mo_per,
  C1_NV_adj, C2_NV_adj, C3_NV_adj, C4_NV_adj, C5_NV_adj, C6_NV_adj, 
  C7_NV_adj, C8_NV_adj, C1_V_adj, C2_V_adj, C3_V_adj, C4_V_adj, 
  C5_V_adj, C6_V_adj, C7_V_adj, C8_V_adj, N_days, 
  C2_PopWeighted, C3_PopWeighted, C4_PopWeighted, C5_PopWeighted, 
  C6_PopWeighted, C7_PopWeighted, C8_PopWeighted, 
  H1_adj, H2_adj, H3_adj, H6_PopWeighted, H7_adj, GovernmentResponseIndex_NonVaccinated, GovernmentResponseIndex_SimpleAverage,
  StringencyIndex_NV, 
  StringencyIndex_V, 
  StringencyH1_NV, 
  StringencyH1_V, 
  ControlIndex_NV, 
  ControlIndex_V, 
  StringencyIndex_NonVaccinated, 
  StringencyIndex_Vaccinated, 
  StringencyIndex_SimpleAverage, 
  StringencyIndex_WeightedAverage, 
  ContainmentHealthIndex_NonVaccinated, 
  ContainmentHealthIndex_Vaccinated, 
  ContainmentHealthIndex_SimpleAverage, 
  ContainmentHealthIndex_WeightedAverage, 
  GovernmentResponseIndex_NonVaccinated, 
  GovernmentResponseIndex_Vaccinated, 
  GovernmentResponseIndex_SimpleAverage, 
  GovernmentResponseIndex_WeightedAverage,H8_PopWeighted, C1_PopWeighted, qrel_year, time, yq, Year  
))

colnames(qdata3)

#==============================================================================
##Umbenenung finaler Datensaätze:


#Main Quarterly Dataset with all Outcomes
qdata<-qdata3

#Excess Deaths weekly
excess_w<-excess_m
p_values_oecd_w<-p_values_oecd
economist_w<-economist_oecd

#Fiscal Measures Daily
fm_d<-fm1

#Oxford und eigenen Indexe für S
oxd_d<-oxd_indices
#Alle Oxoford Daten für alle Länder
oxd_spatial_d<-oxd_spatial

#Hilfsdatensets
google_mobility_d<- google_mobility
hosp_d<-hosp_d

#Speichern finale Datensätze für Analyse


# 1. Deinen Zielordner definieren
safedata <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/Database/Analyse"

# 2. Dateipfad für die RData-Datei erstellen
pathdata<- file.path(safedata, "datafordescriptives.RData")

# 3. Alle Datensätze in diese eine Datei speichern
save(qdata, excess_w, p_values_oecd_w, economist_w, fm_d, 
     oxd_d, oxd_spatial_d, google_mobility_d, hosp_d, 
     file = pathdata)


