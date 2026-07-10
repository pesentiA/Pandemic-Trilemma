# Insert the DI-relevance check into the output V14 battery, right after the
# Below-Stock composition split. Atomic; backup; CRLF-preserving; guarded.

dir <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/code/R"
f   <- file.path(dir, "analysis.R")
rd  <- file.path(dir, "_rerun_20260615")
bak <- file.path(rd, "analysis_predirelevance.R")
blk <- readLines(file.path(rd, "block_di_relevance.txt"), warn = FALSE)

raw  <- readBin(f, "raw", file.info(f)$size); crlf <- any(raw == as.raw(13L))
L    <- readLines(f, warn = FALSE)
con0 <- file(bak, open = "wb"); writeLines(L, con0, sep = "\n", useBytes = TRUE); close(con0)

if (any(grepl("DI CHANNEL RELEVANCE / IDENTIFICATION", L, fixed = TRUE)))
  stop("DI relevance check already present; aborting.")
a <- grep("# cumulative stock is trend-sensitive, as for the aggregate Below-Stock channel.)", L, fixed = TRUE)
if (length(a) != 1L) stop(sprintf("anchor matches = %d", length(a)))

out <- append(L, blk, after = a)
con <- file(f, open = "wb")
writeLines(out, con, sep = if (crlf) "\r\n" else "\n", useBytes = TRUE); close(con)
cat(sprintf("OK: inserted %d lines after line %d. CRLF=%s. Now %d lines.\n", length(blk), a, crlf, length(out)))
