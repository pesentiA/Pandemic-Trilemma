# Splice the corrected V15 debt robustness battery into analysis.R,
# replacing the stale "DEBT EQUATION V14 - ROBUSTNESS BATTERY" block.
# Locates the block by anchor strings (robust to line shifts), backs up first,
# and preserves the file's existing line-ending convention.

f    <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/code/R/analysis.R"
batt <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/code/R/_rerun_20260615/debt_robustness_V15.R"
bak  <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/code/R/_rerun_20260615/analysis_prepatch_debt.R"

raw  <- readBin(f, "raw", file.info(f)$size)
crlf <- any(raw == as.raw(13L))
L    <- readLines(f, warn = FALSE)

con0 <- file(bak, open = "wb"); writeLines(L, con0, sep = "\n", useBytes = TRUE); close(con0)

h  <- grep("DEBT EQUATION V14 - ROBUSTNESS BATTERY", L, fixed = TRUE)
s5 <- grep("STAGE 5 - DESCRIPTIVE COMPARISONS", L, fixed = TRUE)
stopifnot(length(h) == 1L, length(s5) == 1L)

start <- h - 1L     # the "# ====" rule directly above the V14 header
keep  <- s5 - 1L    # the "# ====" rule directly above the STAGE 5 header
new   <- readLines(batt, warn = FALSE)
out   <- c(L[seq_len(start - 1L)], new, "", L[seq(keep, length(L))])

con <- file(f, open = "wb")
writeLines(out, con, sep = if (crlf) "\r\n" else "\n", useBytes = TRUE)
close(con)

cat(sprintf("OK: replaced lines %d..%d (%d old) with %d new lines + 1 blank. CRLF=%s. File now %d lines.\n",
            start, keep - 1L, keep - start, length(new), crlf, length(out)))
cat(sprintf("Backup written to: %s\n", bak))
