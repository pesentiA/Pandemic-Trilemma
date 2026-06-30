# Make DI lag-1 the main debt specification: replace F_DI -> F_DI_lag1
# throughout the debt section (STAGE 4 .. STAGE 5) and mark the main spec.
# Two scoped replacements that do NOT touch the debt_DI0/1/2 lag-selection
# block (which uses "+ F_DI + as.numeric(...)", not "+ F_DI + F_H_lag1").
# Atomic read-write, backup, line-by-line change report.

dir <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/code/R"
f   <- file.path(dir, "analysis.R")
bak <- file.path(dir, "_rerun_20260615", "analysis_premark_DIlag1.R")

raw  <- readBin(f, "raw", file.info(f)$size); crlf <- any(raw == as.raw(13L))
L    <- readLines(f, warn = FALSE)
con0 <- file(bak, open = "wb"); writeLines(L, con0, sep = "\n", useBytes = TRUE); close(con0)

s4 <- grep("STAGE 4 - DEBT EQUATION", L, fixed = TRUE)[1]
s5 <- grep("STAGE 5 - DESCRIPTIVE COMPARISONS", L, fixed = TRUE)[1]
stopifnot(!is.na(s4), !is.na(s5), s4 < s5)
rng  <- s4:(s5 - 1L)
orig <- L[rng]; seg <- orig

seg <- gsub("+ F_DI + F_H_lag1", "+ F_DI_lag1 + F_H_lag1", seg, fixed = TRUE)  # formula spots
seg <- gsub('"F_DI"', '"F_DI_lag1"', seg, fixed = TRUE)                         # quoted spots
seg <- gsub("#Main 15.06.2026-> y_lag1 macht mehr sinn",
            "#Main 16.06.2026 -> y_lag1 + DI at lag 1 (F_DI_lag1)  === CHOSEN MAIN SPECIFICATION ===",
            seg, fixed = TRUE)
seg <- gsub("#   DI  contemporaneous; health spending at lag 1",
            "#   DI  at lag 1 (F_DI_lag1); health spending at lag 1",
            seg, fixed = TRUE)

chg <- which(seg != orig)
cat(sprintf("Debt section = lines [%d, %d]. Changed %d lines:\n", s4, s5 - 1L, length(chg)))
for (i in chg) cat(sprintf("  L%d\n    - %s\n    + %s\n", rng[i], trimws(orig[i]), trimws(seg[i])))

if (length(chg) == 0L) stop("No changes made -- aborting (already converted?).")
L[rng] <- seg
con <- file(f, open = "wb"); writeLines(L, con, sep = if (crlf) "\r\n" else "\n", useBytes = TRUE); close(con)
cat(sprintf("\nWritten OK. CRLF=%s. Backup: %s\n", crlf, bak))
