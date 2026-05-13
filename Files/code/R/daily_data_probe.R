# =============================================================================
#  DAILY-DATA PROBE — inventory candidate objects in dataforanalysis.RData
#  and the raw sources to determine what is truly daily vs weekly.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})

base <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma"
log_path <- file.path(base, "Files/output/daily_data_probe.log")
sink(log_path, split = TRUE)

cat(strrep("=", 78), "\n  DAILY-DATA PROBE — ", format(Sys.time()), "\n", strrep("=", 78), "\n", sep = "")

env <- new.env()
load(file.path(base, "Files/data/processed/dataforanalysis.RData"), envir = env)

cat("\nObjects available:\n")
for (obj in ls(envir = env)) {
  o <- get(obj, envir = env)
  cat(sprintf("  %-22s  class=%s  dim=%s\n",
              obj, paste(class(o), collapse = "/"),
              if (is.data.frame(o)) sprintf("%d x %d", nrow(o), ncol(o)) else NA))
}

infer_freq <- function(df, country_col = "Country", date_col = "date") {
  if (!country_col %in% names(df) || !date_col %in% names(df)) return("(no Country/date columns)")
  s <- df %>% arrange(.data[[country_col]], .data[[date_col]]) %>%
       group_by(.data[[country_col]]) %>%
       mutate(diff_days = as.numeric(.data[[date_col]] - lag(.data[[date_col]]))) %>%
       ungroup() %>% pull(diff_days)
  s <- s[!is.na(s) & s > 0]
  med <- median(s)
  q <- quantile(s, c(0.05, 0.5, 0.95))
  sprintf("median %.0f d  (5/50/95: %.0f / %.0f / %.0f)", med, q[1], q[2], q[3])
}

dump_block <- function(name, df, key_cols = NULL, peek_cols = NULL) {
  cat("\n", strrep("-", 78), "\n  ", name, "\n", strrep("-", 78), "\n", sep = "")
  cat(sprintf("  dim = %d x %d\n", nrow(df), ncol(df)))
  cat("  columns (first 60): ", paste(head(names(df), 60), collapse = ", "), "\n\n", sep = "")
  date_candidates <- intersect(c("date", "Date", "DATE", "day", "Day"), names(df))
  ctry_candidates <- intersect(c("Country", "country", "iso3", "ISO3"), names(df))
  if (length(date_candidates) && length(ctry_candidates)) {
    cat(sprintf("  Inferred sampling frequency: %s\n",
                infer_freq(df, ctry_candidates[1], date_candidates[1])))
    cat(sprintf("  Date range: %s -> %s\n",
                as.character(min(df[[date_candidates[1]]], na.rm = TRUE)),
                as.character(max(df[[date_candidates[1]]], na.rm = TRUE))))
    cat(sprintf("  Unique countries: %d\n", n_distinct(df[[ctry_candidates[1]]])))
  } else {
    cat("  (no Country+date pair found — skipping freq inference)\n")
  }
  if (!is.null(peek_cols)) {
    peek <- intersect(peek_cols, names(df))
    if (length(peek)) {
      cat("  Variable summaries (selected):\n")
      for (v in peek) {
        x <- df[[v]]
        if (is.numeric(x)) {
          cat(sprintf("    %-25s  N=%d  NA=%d  min=%.3g  med=%.3g  max=%.3g\n",
                      v, sum(!is.na(x)), sum(is.na(x)),
                      suppressWarnings(min(x, na.rm = TRUE)),
                      suppressWarnings(median(x, na.rm = TRUE)),
                      suppressWarnings(max(x, na.rm = TRUE))))
        }
      }
    }
  }
}

# ----- Candidate objects ----------------------------------------------------
for (obj in c("oxd_d", "oxd_spatial_d", "hosp_d", "google_mobility_d",
              "panel_w", "panel_hosp", "p_values_oecd_w", "hosp_w")) {
  if (exists(obj, envir = env)) {
    df <- get(obj, envir = env)
    if (is.data.frame(df)) {
      peek_set <- intersect(names(df),
                            c("S_mean", "S_max", "StringencyIndex_Average",
                              "StringencyIndex_PopWeighted", "S_mean_pw",
                              "p_proj", "p_proj_all_ages", "p_avg",
                              "excess", "excess_pm", "p_score", "p_value",
                              "theta_hat", "theta_predicted", "theta_mean",
                              "deaths_confirmed_pm", "cases_pm", "deaths_w",
                              "cases_w"))
      dump_block(obj, df, peek_cols = peek_set)
    }
  }
}

# ----- Cross-check the WEEKLY panel for hidden daily granularity ------------
cat("\n", strrep("=", 78), "\n",
    "  panel_w — frequency cross-check (date column may be week-ending)\n",
    strrep("=", 78), "\n", sep = "")
if (exists("panel_w", envir = env)) {
  pw <- env$panel_w
  cat("  date column distinct values per Country (first country):\n")
  c1 <- sort(unique(pw$Country))[1]
  d1 <- pw %>% filter(Country == c1) %>% pull(date)
  cat(sprintf("    Country = %s, N_dates = %d, head: %s\n",
              c1, length(d1), paste(head(sort(d1), 5), collapse = ", ")))
  diffs <- diff(sort(unique(d1)))
  cat(sprintf("    Date diffs (head): %s\n",
              paste(head(as.numeric(diffs), 10), collapse = ", ")))
}

# ----- True-daily candidates: oxd_d --------------------------------------
cat("\n", strrep("=", 78), "\n",
    "  Promising DAILY-frequency variables (per object)\n",
    strrep("=", 78), "\n", sep = "")
if (exists("oxd_d", envir = env)) {
  od <- env$oxd_d
  cat("\n--- oxd_d candidate columns matching S / stringency / mortality ---\n")
  cands <- grep("string|exces|mort|theta|p_proj|p_avg|p_score|S_mean|S_max",
                names(od), ignore.case = TRUE, value = TRUE)
  print(cands)
}
if (exists("oxd_spatial_d", envir = env)) {
  ods <- env$oxd_spatial_d
  cat("\n--- oxd_spatial_d candidate columns (S_spatial / SI) ---\n")
  cands <- grep("string|S_|spatial|excess|p_proj|theta",
                names(ods), ignore.case = TRUE, value = TRUE)
  print(head(cands, 60))
}

# ----- Search raw daily directories for excess-mortality daily files ---------
cat("\n", strrep("=", 78), "\n",
    "  Raw daily files under Files/data/raw/outcomes and controls/Excess Mortality\n",
    strrep("=", 78), "\n", sep = "")
em_root <- file.path(base, "Files/data/raw/outcomes and controls/Excess Mortality")
if (dir.exists(em_root)) {
  fs <- list.files(em_root, recursive = TRUE, full.names = FALSE)
  cat(sprintf("  N = %d files\n", length(fs)))
  print(head(fs, 30))
} else {
  cat("  (directory not found)\n")
}

cat("\n", strrep("=", 78), "\n  END\n", strrep("=", 78), "\n", sep = "")
sink()
cat("Log: ", log_path, "\n")
