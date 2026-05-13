# =============================================================================
#  DATA QUALITY DIAGNOSTICS — Pandemic-Trilemma input datasets
#  Runs read-only checks (no writes back to processed/raw). Output is captured
#  via sink() to Files/output/data_diagnostics.log.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(tibble)
  library(stringr)
  library(tidyr)
})

base <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma"
log_path <- file.path(base, "Files/output/data_diagnostics.log")
sink(log_path, split = TRUE)

cat(strrep("=", 78), "\n")
cat("  DATA QUALITY DIAGNOSTICS — ", format(Sys.time()), "\n")
cat(strrep("=", 78), "\n\n")

# -------- helpers ------------------------------------------------------------
hdr <- function(s) {
  cat("\n", strrep("=", 78), "\n", "  ", s, "\n", strrep("=", 78), "\n", sep = "")
}
sub <- function(s) cat("\n--- ", s, " ---\n", sep = "")

na_summary <- function(df, vars = NULL, top = 25) {
  if (is.null(vars)) vars <- colnames(df)
  vars <- intersect(vars, colnames(df))
  n   <- nrow(df)
  res <- tibble(
    var = vars,
    n_na = vapply(vars, function(v) sum(is.na(df[[v]])), integer(1)),
    pct_na = round(100 * vapply(vars, function(v) mean(is.na(df[[v]])), numeric(1)), 2)
  ) %>% arrange(desc(n_na))
  print(res %>% head(top), n = top)
  invisible(res)
}

range_summary <- function(df, vars) {
  vars <- intersect(vars, colnames(df))
  tibble(
    var = vars,
    n   = vapply(vars, function(v) sum(!is.na(df[[v]])), integer(1)),
    min = vapply(vars, function(v) suppressWarnings(min(df[[v]], na.rm = TRUE)), numeric(1)),
    p01 = vapply(vars, function(v) suppressWarnings(as.numeric(quantile(df[[v]], 0.01, na.rm = TRUE))), numeric(1)),
    p50 = vapply(vars, function(v) suppressWarnings(median(df[[v]], na.rm = TRUE)), numeric(1)),
    mean = vapply(vars, function(v) suppressWarnings(mean(df[[v]], na.rm = TRUE)), numeric(1)),
    p99 = vapply(vars, function(v) suppressWarnings(as.numeric(quantile(df[[v]], 0.99, na.rm = TRUE))), numeric(1)),
    max = vapply(vars, function(v) suppressWarnings(max(df[[v]], na.rm = TRUE)), numeric(1)),
    inf = vapply(vars, function(v) sum(is.infinite(df[[v]])), integer(1))
  ) %>% print(n = Inf)
}

dup_check <- function(df, keys) {
  keys <- intersect(keys, colnames(df))
  if (!length(keys)) { cat("  (no keys present)\n"); return(invisible(NULL)) }
  d <- df %>% count(across(all_of(keys))) %>% filter(n > 1)
  cat(sprintf("  Duplicate (%s) rows: %d\n", paste(keys, collapse = "+"), nrow(d)))
  if (nrow(d)) print(d %>% head(20))
  invisible(d)
}

country_coverage <- function(df, country = "Country", time = "Quarter") {
  if (!country %in% colnames(df)) { cat("  (no Country col)\n"); return(invisible(NULL)) }
  cat(sprintf("  Countries: %d unique\n", n_distinct(df[[country]])))
  print(sort(unique(df[[country]])))
  if (time %in% colnames(df)) {
    cat(sprintf("\n  Periods: %d unique\n", n_distinct(df[[time]])))
    tab <- table(df[[country]])
    cat(sprintf("  Obs per country: min = %d, max = %d, modal = %d\n",
                min(tab), max(tab), as.integer(names(which.max(table(tab))))))
  }
}

# -------- load processed RData ------------------------------------------------
hdr("LOAD dataforanalysis.RData")
env <- new.env()
load(file.path(base, "Files/data/processed/dataforanalysis.RData"), envir = env)
cat("Objects in RData:\n")
print(ls(envir = env))
for (obj in ls(envir = env)) {
  o <- get(obj, envir = env)
  cat(sprintf("  %-22s class=%s dim=%s\n",
              obj, paste(class(o), collapse = "/"),
              if (is.data.frame(o)) sprintf("%d x %d", nrow(o), ncol(o)) else
                if (is.vector(o) || is.list(o)) as.character(length(o)) else "—"))
}

# ============================================================================
# qdata  -> state variables y_k, b_k, demographic / fiscal-capacity controls
# ============================================================================
hdr("qdata — state variables")
qdata <- env$qdata
cat("Structure (str()):\n"); str(qdata, list.len = 50, give.attr = FALSE)
cat("\nClass of Quarter: "); print(class(qdata$Quarter))

sub("Country coverage")
country_coverage(qdata)

sub("Quarter range")
print(table(qdata$Quarter))

sub("Duplicates on (Country, Quarter)")
dup_check(qdata, c("Country", "Quarter"))

sub("NA counts on key variables")
key_q <- c("y_t_pct", "y_t", "d_t_pct", "d_t_pct_r", "d_t",
           "DebtN_share2019", "DebtR_share2019",
           "DebtN_share2019_growth", "DebtR_share2019_growth",
           "Qpopulation_th", "nGDP_2019_an", "inflation_index",
           "vax_rate", "rGDP_pc_2019", "debt_2019",
           "StringencyIndex_PopWeighted",
           "p_proj_all_ages", "p_avg_all_ages",
           "excess.deaths_a", "ConfirmedDeaths.a")
na_summary(qdata, key_q)

sub("Range summaries")
range_summary(qdata, key_q)

sub("Output-gap sanity (should be near 0 in 2019, large negative in Q2.2020)")
qdata %>%
  filter(Quarter %in% c("Q1.2019","Q2.2019","Q3.2019","Q4.2019",
                        "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                        "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
                        "Q1.2022")) %>%
  group_by(Quarter) %>%
  summarise(n = sum(!is.na(y_t_pct)),
            mean_y = mean(y_t_pct, na.rm = TRUE),
            sd_y   = sd(y_t_pct, na.rm = TRUE),
            min_y  = min(y_t_pct, na.rm = TRUE),
            max_y  = max(y_t_pct, na.rm = TRUE),
            .groups = "drop") %>% print(n = Inf)

sub("Stringency index source range (should be 0-1 or 0-100)")
range_summary(qdata, "StringencyIndex_PopWeighted")

sub("Vaccination rate range (should be 0-1 fraction)")
range_summary(qdata, "vax_rate")

# ============================================================================
# theta_quarterly_full -> infection prevalence
# ============================================================================
hdr("theta_quarterly_full — infection prevalence")
theta <- env$theta_quarterly_full
cat("Columns:\n"); print(colnames(theta))
cat("Quarter class: "); print(class(theta$YQ))

sub("Country coverage")
country_coverage(theta, time = "YQ")

sub("Duplicates on (Country, YQ)")
dup_check(theta, c("Country", "YQ"))

sub("NA counts on theta_* columns")
na_summary(theta, grep("^theta_", colnames(theta), value = TRUE))

sub("Range summary — theta should be in [0,1]")
range_summary(theta, c("theta_mean", "theta_mean_l2", "theta_mean_l4",
                       "theta_mean_lo", "theta_mean_hi"))

sub("Out-of-bounds theta_mean (>1 or <0)")
oob <- theta %>% filter(theta_mean > 1 | theta_mean < 0)
cat(sprintf("  Rows with theta_mean outside [0,1]: %d\n", nrow(oob)))
if (nrow(oob)) print(head(oob, 10))

# ============================================================================
# panel_w -> stringency daily panel
# ============================================================================
hdr("panel_w — daily stringency")
panel_w <- env$panel_w
cat("Columns:\n"); print(colnames(panel_w))

sub("Date range")
cat("  min:", as.character(min(panel_w$date, na.rm = TRUE)),
    "  max:", as.character(max(panel_w$date, na.rm = TRUE)), "\n")

sub("Country coverage")
country_coverage(panel_w, time = "date")

sub("Duplicates on (Country, date)")
dup_check(panel_w, c("Country", "date"))

sub("NA on S_mean / S_max")
na_summary(panel_w, c("S_mean", "S_max"))

sub("Stringency value range (raw scale)")
range_summary(panel_w, c("S_mean", "S_max"))

# ============================================================================
# hosp_d -> hospitalisations
# ============================================================================
hdr("hosp_d — hospitalisations")
hosp <- env$hosp_d
cat("Columns:\n"); print(colnames(hosp))
cat("Date range: ", as.character(min(hosp$date, na.rm = TRUE)),
    " -> ", as.character(max(hosp$date, na.rm = TRUE)), "\n")

sub("Country coverage")
country_coverage(hosp, time = "date")

sub("NA counts")
na_summary(hosp, c("Weekly.new.hospital.admissions.per.million",
                   "Daily.ICU.occupancy.per.million",
                   "Daily.hospital.occupancy.per.million"))

sub("Range")
range_summary(hosp, c("Weekly.new.hospital.admissions.per.million",
                      "Daily.ICU.occupancy.per.million",
                      "Daily.hospital.occupancy.per.million"))

# ============================================================================
# google_mobility_d -> mobility
# ============================================================================
hdr("google_mobility_d — Google Mobility")
mob <- env$google_mobility_d
cat("Columns:\n"); print(colnames(mob))
cat("Date range: ", as.character(min(mob$date, na.rm = TRUE)),
    " -> ", as.character(max(mob$date, na.rm = TRUE)), "\n")

sub("Unique 'place' categories")
print(table(mob$place))

sub("Unique countries")
cat(sprintf("  N = %d\n", n_distinct(mob$country)))
print(sort(unique(mob$country)))

sub("NA on 'trend'")
na_summary(mob, "trend")

sub("Range of 'trend' (Google mobility units, typically -100 to +100)")
range_summary(mob, "trend")

# ============================================================================
# pdata (legacy object, used downstream) and fm (legacy object)
# ============================================================================
hdr("pdata (legacy)")
if (exists("pdata", envir = env)) {
  pd <- env$pdata
  cat("Class:", paste(class(pd), collapse = "/"),
      " dim:", nrow(pd), "x", ncol(pd), "\n")
  cat("Columns (first 80):\n"); print(head(colnames(pd), 80))
} else cat("  (not present)\n")

hdr("fm (legacy)")
if (exists("fm", envir = env)) {
  fm <- env$fm
  cat("Class:", paste(class(fm), collapse = "/"),
      " dim:", nrow(fm), "x", ncol(fm), "\n")
  cat("Columns:\n"); print(colnames(fm))
} else cat("  (not present)\n")

# ============================================================================
# fm1 — fiscal_classified_v1_7.xlsx
# ============================================================================
hdr("fm1 — fiscal_classified_v1_7.xlsx")
fm1 <- readxl::read_excel(
  file.path(base, "Files/data/raw/fiscal measures/fiscal_classified_v1_7.xlsx")
)
cat("Dim:", nrow(fm1), "x", ncol(fm1), "\n")
cat("Columns:\n"); print(colnames(fm1))

sub("Country coverage")
cat(sprintf("  N = %d unique\n", n_distinct(fm1$Country)))
print(sort(unique(fm1$Country)))

sub("Year / Quarter coverage")
print(table(fm1$Year, fm1$Quarter, useNA = "ifany"))

sub("transmission_channel distribution")
print(table(fm1$transmission_channel, useNA = "ifany"))

sub("category distribution (above-the-line vs below-the-line)")
print(table(fm1$category, useNA = "ifany"))

sub("PolicyCode x transmission_channel")
print(table(fm1$PolicyCode, fm1$transmission_channel, useNA = "ifany"))

sub("broad_fiscal flag distribution")
print(table(fm1$broad_fiscal, useNA = "ifany"))

sub("broad_fiscal_gdp range (should be small fractions ~ 0-0.20)")
range_summary(fm1, "broad_fiscal_gdp")

sub("Suspicious entries: broad_fiscal_gdp < 0 or > 0.50")
susp <- fm1 %>% filter(broad_fiscal_gdp < 0 | broad_fiscal_gdp > 0.50)
cat(sprintf("  N = %d\n", nrow(susp)))
if (nrow(susp)) print(head(susp %>% select(Country, Year, Quarter, PolicyCode,
                                            transmission_channel, category, broad_fiscal_gdp), 20))

sub("NA on broad_fiscal_gdp / transmission_channel / category")
na_summary(fm1, c("broad_fiscal_gdp", "transmission_channel", "category",
                  "PolicyCode", "Year", "Quarter", "broad_fiscal"))

sub("Cross-check: PolicyCodes 5/6/11/12/15/16 (excluded by analysis.R)")
print(fm1 %>%
  filter(PolicyCode %in% c(5, 6, 11, 12, 15, 16) |
         PolicyCode %in% c("5","6","11","12","15","16")) %>%
  group_by(PolicyCode, transmission_channel, category) %>%
  summarise(n = n(),
            total_vol_pct = round(sum(broad_fiscal_gdp, na.rm = TRUE) * 100, 3),
            .groups = "drop"))

# ============================================================================
# CROSS-DATASET consistency: do qdata / theta / panel_w / fm1 cover the same
# 38 countries?
# ============================================================================
hdr("CROSS-DATASET country coverage")
cset <- function(x) sort(unique(x))
c_q   <- cset(env$qdata$Country)
c_th  <- cset(env$theta_quarterly_full$Country)
c_str <- cset(env$panel_w$Country)
c_hosp<- cset(env$hosp_d$Country)
c_fm  <- cset(fm1$Country)

cat(sprintf("  qdata:   %d\n  theta:   %d\n  string.: %d\n  hosp:    %d\n  fm1:     %d\n",
            length(c_q), length(c_th), length(c_str), length(c_hosp), length(c_fm)))

cat("\n  In qdata but NOT in theta:\n");    print(setdiff(c_q,  c_th))
cat("\n  In theta but NOT in qdata:\n");    print(setdiff(c_th, c_q))
cat("\n  In qdata but NOT in panel_w:\n");  print(setdiff(c_q,  c_str))
cat("\n  In qdata but NOT in hosp_d:\n");   print(setdiff(c_q,  c_hosp))
cat("\n  In qdata but NOT in fm1:\n");      print(setdiff(c_q,  c_fm))
cat("\n  In fm1 but NOT in qdata:\n");      print(setdiff(c_fm, c_q))

# ============================================================================
# Recreate `df` (master merge) using the analysis-script logic, then run the
# same diagnostics you'd want on the merged panel — focused on the V14 sample
# window (Q4.2019 - Q2.2022).
# ============================================================================
hdr("MASTER MERGE (replicated minimal pipeline) — V14 window checks")

pandemic_qs <- c(
  "Q1.2019","Q2.2019","Q3.2019","Q4.2019",
  "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
  "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
  "Q1.2022","Q2.2022","Q3.2022","Q4.2022"
)

# (1) qdata block
df_q <- env$qdata %>%
  filter(Quarter %in% pandemic_qs) %>%
  select(Country, Quarter, y_t_pct, d_t_pct,
         DebtN_share2019, DebtR_share2019,
         StringencyIndex_PopWeighted,
         p_proj_all_ages, p_avg_all_ages,
         Qpopulation_th, vax_rate)

# (2) theta block
df_th <- env$theta_quarterly_full %>%
  mutate(Quarter = as.character(YQ)) %>%
  filter(Quarter %in% pandemic_qs) %>%
  select(Country, Quarter, theta_mean)

# (3) fiscal block (filtered like analysis.R does)
fm1f <- fm1 %>%
  filter(!PolicyCode %in% c(5,6,11,12,15,16),
         !PolicyCode %in% c("5","6","11","12","15","16")) %>%
  mutate(Quarter = paste0("Q", Quarter, ".", Year)) %>%
  filter(Quarter %in% pandemic_qs)

df_fc <- fm1f %>%
  filter(broad_fiscal == 1) %>%
  group_by(Country, Quarter) %>%
  summarise(F_CP = sum(broad_fiscal_gdp[transmission_channel == "CP"], na.rm = TRUE),
            F_DI = sum(broad_fiscal_gdp[transmission_channel == "DI"], na.rm = TRUE),
            F_H  = sum(broad_fiscal_gdp[transmission_channel == "H"],  na.rm = TRUE),
            .groups = "drop")

# (4) stringency block
df_str <- env$panel_w %>%
  filter(!is.na(S_mean)) %>%
  mutate(year    = lubridate::year(date),
         quarter = lubridate::quarter(date),
         Quarter = paste0("Q", quarter, ".", year)) %>%
  filter(Quarter %in% pandemic_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(S_mean_pw = mean(S_mean, na.rm = TRUE),
            S_max_pw  = max(S_max, na.rm = TRUE),
            .groups = "drop")

df <- df_q %>%
  left_join(df_th,  by = c("Country","Quarter")) %>%
  left_join(df_fc,  by = c("Country","Quarter")) %>%
  left_join(df_str, by = c("Country","Quarter")) %>%
  mutate(F_CP = tidyr::replace_na(F_CP, 0),
         F_DI = tidyr::replace_na(F_DI, 0),
         F_H  = tidyr::replace_na(F_H,  0))

cat("Merged df dimensions:", nrow(df), "x", ncol(df), "\n")

sub("Country-quarter coverage on the merged df (V14 window Q4.2019-Q2.2022)")
v14 <- df %>% filter(Quarter %in% c("Q4.2019","Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                                     "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
                                     "Q1.2022","Q2.2022"))
cat(sprintf("  V14 N = %d, countries = %d, quarters = %d\n",
            nrow(v14), n_distinct(v14$Country), n_distinct(v14$Quarter)))
tab <- table(v14$Country)
cat(sprintf("  Obs per country (V14): min = %d, max = %d, modal = %d\n",
            min(tab), max(tab), as.integer(names(which.max(table(tab))))))
short <- names(tab)[tab < max(tab)]
if (length(short)) {
  cat("  Countries with < max obs (potential merge gaps):\n")
  print(tab[short])
}

sub("NA counts on key variables in V14 window")
na_summary(v14, c("y_t_pct","d_t_pct","theta_mean",
                  "S_mean_pw","S_max_pw","p_proj_all_ages","vax_rate",
                  "F_CP","F_DI","F_H","DebtN_share2019","DebtR_share2019"))

sub("Stringency unit check inside V14 window")
range_summary(v14, c("S_mean_pw", "S_max_pw"))

sub("Vaccination rate range in V14 window (expect 0-1 fraction)")
range_summary(v14, "vax_rate")

sub("Fiscal aggregates in V14 window — flag negative values")
cat(sprintf("  F_CP < 0: %d  |  F_DI < 0: %d  |  F_H < 0: %d\n",
            sum(v14$F_CP < 0, na.rm = TRUE),
            sum(v14$F_DI < 0, na.rm = TRUE),
            sum(v14$F_H  < 0, na.rm = TRUE)))
range_summary(v14, c("F_CP","F_DI","F_H"))

sub("Quarters where F_CP, F_DI, F_H are ALL exactly zero (no measures recorded)")
zero_qs <- v14 %>% filter(F_CP == 0, F_DI == 0, F_H == 0)
cat(sprintf("  N = %d  (countries=%d, quarters=%d)\n",
            nrow(zero_qs), n_distinct(zero_qs$Country), n_distinct(zero_qs$Quarter)))
if (nrow(zero_qs)) print(zero_qs %>% count(Quarter) %>% arrange(Quarter))

sub("Q2.2020 output-gap distribution (should be deeply negative)")
print(v14 %>% filter(Quarter == "Q2.2020") %>%
      summarise(mean_y = mean(y_t_pct, na.rm = TRUE),
                median_y = median(y_t_pct, na.rm = TRUE),
                min_y = min(y_t_pct, na.rm = TRUE),
                max_y = max(y_t_pct, na.rm = TRUE),
                n_pos = sum(y_t_pct > 0, na.rm = TRUE)))
cat("  Countries with POSITIVE output gap in Q2.2020 (suspicious):\n")
print(v14 %>% filter(Quarter == "Q2.2020", y_t_pct > 0) %>%
      select(Country, y_t_pct))

sub("Theta sanity in V14 window — theta_mean should be in [0,1]")
oob_th <- v14 %>% filter(!is.na(theta_mean), theta_mean > 1 | theta_mean < 0)
cat(sprintf("  Rows with theta_mean outside [0,1]: %d\n", nrow(oob_th)))
if (nrow(oob_th)) print(head(oob_th %>% select(Country, Quarter, theta_mean), 20))

cat("\n", strrep("=", 78), "\n  END OF DIAGNOSTICS\n", strrep("=", 78), "\n", sep = "")
sink()
cat("Log written to:", log_path, "\n")
