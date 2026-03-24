library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

base_dir <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files"
ox <- read_csv(file.path(base_dir, "data/raw/oxford stringency/Oxcnat.csv"),
               show_col_types = FALSE)

oecd <- c("AUS","AUT","BEL","CAN","CHL","COL","CRI","CZE","DNK","EST",
  "FIN","FRA","DEU","GRC","HUN","ISL","IRL","ISR","ITA","JPN",
  "LVA","LTU","LUX","MEX","NLD","NZL","NOR","POL","PRT","SVK",
  "SVN","KOR","ESP","SWE","CHE","TUR","GBR","USA")

ox2 <- ox %>%
  filter(CountryCode %in% oecd, Jurisdiction == "NAT_TOTAL") %>%
  mutate(date = ymd(Date), Country = CountryCode,
         S = as.numeric(StringencyIndex_Average)) %>%
  filter(!is.na(S)) %>%
  select(Country, date, S)

# Full 2020
cat("=== S cross-country correlation: full 2020 ===\n")
d2020 <- ox2 %>% filter(date >= "2020-01-01", date <= "2020-12-31")
w2020 <- d2020 %>% pivot_wider(names_from = Country, values_from = S)
cm2020 <- cor(w2020 %>% select(-date), use = "pairwise.complete.obs")
lt2020 <- cm2020[lower.tri(cm2020)]
cat(sprintf("  Mean r = %.3f, Median r = %.3f\n", mean(lt2020), median(lt2020)))

# By half-year
cat("\n=== S by half-year ===\n")
for (period in list(
  list("H1 2020 (Jan-Jun)", "2020-01-01", "2020-06-30"),
  list("H2 2020 (Jul-Dec)", "2020-07-01", "2020-12-31"),
  list("H1 2021 (Jan-Jun)", "2021-01-01", "2021-06-30"),
  list("H2 2021 (Jul-Dec)", "2021-07-01", "2021-12-31")
)) {
  d <- ox2 %>% filter(date >= period[[2]], date <= period[[3]])
  w <- d %>% pivot_wider(names_from = Country, values_from = S)
  cm <- cor(w %>% select(-date), use = "pairwise.complete.obs")
  lt <- cm[lower.tri(cm)]
  cat(sprintf("  %s: mean r = %.3f, median r = %.3f\n",
              period[[1]], mean(lt, na.rm = TRUE), median(lt, na.rm = TRUE)))
}

# By quarter
cat("\n=== S by quarter ===\n")
for (yr in 2020:2021) {
  for (qt in 1:4) {
    start <- as.Date(paste0(yr, "-", (qt-1)*3+1, "-01"))
    end <- start + months(3) - days(1)
    d <- ox2 %>% filter(date >= start, date <= end)
    if (nrow(d) == 0) next
    w <- d %>% pivot_wider(names_from = Country, values_from = S)
    cm <- cor(w %>% select(-date), use = "pairwise.complete.obs")
    lt <- cm[lower.tri(cm)]
    cat(sprintf("  Q%d.%d: mean r = %.3f, median r = %.3f\n",
                qt, yr, mean(lt, na.rm = TRUE), median(lt, na.rm = TRUE)))
  }
}
