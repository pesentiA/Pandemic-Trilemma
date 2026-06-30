# Insert the Below-Stock composition split into the output V14 battery,
# right after the decay-stock check (anchor: "##decay machen wir nicht").
# Atomic read-insert-write; backup; CRLF-preserving; idempotency-guarded.

dir <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/code/R"
f   <- file.path(dir, "analysis.R")
rd  <- file.path(dir, "_rerun_20260615")
bak <- file.path(rd, "analysis_prebelowsplit.R")
blk <- readLines(file.path(rd, "block_belowsplit.txt"), warn = FALSE)

raw  <- readBin(f, "raw", file.info(f)$size); crlf <- any(raw == as.raw(13L))
L    <- readLines(f, warn = FALSE)
con0 <- file(bak, open = "wb"); writeLines(L, con0, sep = "\n", useBytes = TRUE); close(con0)

if (any(grepl("BELOW-STOCK COMPOSITION", L, fixed = TRUE)))
  stop("Below-stock split already present; aborting.")
a <- grep("##decay machen wir nicht", L, fixed = TRUE)
if (length(a) != 1L) stop(sprintf("anchor matches = %d", length(a)))

out <- append(L, blk, after = a)
con <- file(f, open = "wb")
writeLines(out, con, sep = if (crlf) "\r\n" else "\n", useBytes = TRUE); close(con)
cat(sprintf("OK: inserted %d lines after line %d (\"##decay machen wir nicht\"). CRLF=%s. Now %d lines.\n",
            length(blk), a, crlf, length(out)))
