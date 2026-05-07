suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(lubridate)
  library(readxl); library(fixest); library(modelsummary); library(data.table)
})

conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflicts_prefer(dplyr::lag)
conflicted::conflicts_prefer(dplyr::first)
conflicted::conflicts_prefer(dplyr::coalesce)
conflicted::conflicts_prefer(dplyr::recode)
conflicted::conflicts_prefer(lubridate::quarter)
conflicted::conflicts_prefer(lubridate::year)
conflicted::conflicts_prefer(dplyr::last)
conflicted::conflicts_prefer(data.table::isoweek)
conflicted::conflicts_prefer(data.table::month)
conflicted::conflicts_prefer(dplyr::lead)
conflicted::conflicts_prefer(lubridate::union)
conflicted::conflicts_prefer(modelsummary::SD)

set.seed(1234)
options(scipen = 999)

safedata <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/processed"
load(file.path(safedata, "dataforanalysis.RData"))

fm1 <- read_excel("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/raw/fiscal measures/fiscal_classified_v1_7.xlsx")
fm1 <- fm1 %>%
  mutate(YQ = paste0("Q", Quarter, ".", Year),
         YQ_ord = factor(YQ, levels = sort(unique(paste0("Q", Quarter, ".", Year))), ordered = TRUE)) %>%
  filter(!PolicyCode %in% c(5, 6, 11, 12, 15, 16))

pandemic_qs <- c("Q1.2019","Q2.2019","Q3.2019","Q4.2019",
                 "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                 "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
                 "Q1.2022","Q2.2022","Q3.2022","Q4.2022")

# --- Minimal data build ---
df_qdata <- qdata %>% filter(Quarter %in% pandemic_qs) %>%
  select(Country, Quarter, y_t_pct, DebtR_share2019, DebtN_share2019,
         p_proj_all_ages, Qpopulation_th)

df_fiscal <- fm1 %>% filter(broad_fiscal == 1) %>%
  mutate(Quarter = as.character(YQ_ord)) %>% filter(Quarter %in% pandemic_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(F_CP = sum(broad_fiscal_gdp[transmission_channel == "CP"], na.rm=TRUE),
            F_DI = sum(broad_fiscal_gdp[transmission_channel == "DI"], na.rm=TRUE),
            .groups="drop")

# CP sub-components from fm1
fiscal_panel <- fm1 %>%
  filter(broad_fiscal != 0) %>%
  mutate(
    Quarter_fmt = paste0("Q", Quarter, ".", Year),
    CP_above = ifelse(transmission_channel == "CP" & category == 1, broad_fiscal_gdp, 0),
    CP_below = ifelse(transmission_channel == "CP" & category == 2, broad_fiscal_gdp, 0)
  ) %>%
  group_by(Country, Quarter_fmt) %>%
  summarise(F_CP_above = sum(CP_above, na.rm=TRUE),
            F_CP_below = sum(CP_below, na.rm=TRUE), .groups="drop") %>%
  rename(Quarter = Quarter_fmt)

fiscal_subcomp_d <- fm1 %>%
  filter(broad_fiscal == 1) %>%
  mutate(
    Quarter_fmt = paste0("Q", Quarter, ".", Year),
    CP_above = ifelse(transmission_channel == "CP" & category == 1, broad_fiscal_gdp, 0),
    CP_loans = ifelse(transmission_channel == "CP" & category == 2 &
                        PolicyCode %in% c("40","41"), broad_fiscal_gdp, 0),
    CP_guar  = ifelse(transmission_channel == "CP" & category == 2 &
                        PolicyCode == "43", broad_fiscal_gdp, 0)
  ) %>%
  group_by(Country, Quarter_fmt) %>%
  summarise(F_CP_above_3 = sum(CP_above, na.rm=TRUE) * 100,
            F_CP_loans = sum(CP_loans, na.rm=TRUE) * 100,
            F_CP_guar  = sum(CP_guar, na.rm=TRUE) * 100,
            .groups="drop") %>%
  rename(Quarter = Quarter_fmt) %>%
  mutate(F_CP_guar_adj = F_CP_guar * 0.35)

df <- df_qdata %>%
  left_join(df_fiscal, by=c("Country","Quarter")) %>%
  left_join(fiscal_panel, by=c("Country","Quarter")) %>%
  mutate(Quarter = factor(Quarter, levels = pandemic_qs, ordered = TRUE),
         F_CP = replace_na(F_CP, 0), F_DI = replace_na(F_DI, 0),
         F_CP_above = replace_na(F_CP_above, 0),
         F_CP_below = replace_na(F_CP_below, 0)) %>%
  arrange(Country, Quarter)

# Scale
df <- df %>%
  mutate(F_CP = F_CP * 100, F_DI = F_DI * 100,
         F_CP_above = F_CP_above * 100, F_CP_below = F_CP_below * 100,
         F_CP_below_adj_mid = F_CP_below * 0.35)

# Lags
df <- df %>%
  group_by(Country) %>% arrange(Quarter) %>%
  mutate(F_DI_lag1 = lag(F_DI, 1)) %>% ungroup()

# t_idx
df$t_idx <- match(as.character(df$Quarter), pandemic_qs)

# debt FD (chronological)
pdataD <- df %>%
  filter(Quarter %in% c("Q3.2019","Q4.2019",
                        "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                        "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
                        "Q1.2022","Q2.2022")) %>%
  arrange(Country, t_idx) %>%
  group_by(Country) %>%
  mutate(debt_dR = DebtR_share2019 - lag(DebtR_share2019, 1)) %>%
  ungroup() %>%
  filter(Quarter != "Q3.2019")

# Merge 3-way sub-components
pdataD <- pdataD %>%
  left_join(fiscal_subcomp_d, by=c("Country","Quarter")) %>%
  mutate(across(c(F_CP_above_3, F_CP_loans, F_CP_guar, F_CP_guar_adj),
                ~ replace_na(.x, 0)),
         F_DI_lag1 = replace_na(F_DI_lag1, 0))

debt_sub <- pdataD$t_idx >= 5 & pdataD$t_idx <= 14
cat(sprintf("N: %d\n\n", sum(debt_sub)))

# --- 5 Models ---
d1_ols <- feols(debt_dR ~ y_t_pct + F_CP + F_DI_lag1,
  data = pdataD, subset = debt_sub, cluster = ~Country)

d2_pooled <- feols(debt_dR ~ y_t_pct + F_CP + F_DI_lag1 | Country,
  data = pdataD, subset = debt_sub, cluster = ~Country)

d3_split_raw <- feols(debt_dR ~ y_t_pct + F_CP_above + F_CP_below + F_DI_lag1 | Country,
  data = pdataD, subset = debt_sub, cluster = ~Country)

d4_split_adj <- feols(debt_dR ~ y_t_pct + F_CP_above + F_CP_below_adj_mid + F_DI_lag1 | Country,
  data = pdataD, subset = debt_sub, cluster = ~Country)

d5_3way <- feols(debt_dR ~ y_t_pct + F_CP_above_3 + F_CP_loans + F_CP_guar_adj + F_DI_lag1 | Country,
  data = pdataD, subset = debt_sub, cluster = ~Country)

# --- Print all ---
for (nm in c("d1_ols","d2_pooled","d3_split_raw","d4_split_adj","d5_3way")) {
  cat(sprintf("\n=== %s ===\n", nm))
  print(summary(get(nm)))
}
