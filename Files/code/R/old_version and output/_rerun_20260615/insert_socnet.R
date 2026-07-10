# Insert the (11) social-net split into the V15 debt battery + bullet 11 into
# the to-do list. Atomic read-insert-write keyed on anchor strings (robust to
# the live editing of analysis.R). Backs up first; preserves line endings.

dir <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/code/R"
f   <- file.path(dir, "analysis.R")
rd  <- file.path(dir, "_rerun_20260615")
bak <- file.path(rd, "analysis_prepatch_socnet.R")

b11  <- readLines(file.path(rd, "block11_battery.txt"), warn = FALSE)
bl11 <- readLines(file.path(rd, "bullet11_list.txt"),  warn = FALSE)

raw  <- readBin(f, "raw", file.info(f)$size)
crlf <- any(raw == as.raw(13L))
L    <- readLines(f, warn = FALSE)
con0 <- file(bak, open = "wb"); writeLines(L, con0, sep = "\n", useBytes = TRUE); close(con0)

a1 <- grep('show(d_noout, "(10) EXCLUDING TUR + IRL")', L, fixed = TRUE)
a2 <- grep("#     This checks whether the main coefficients are driven by outliers.", L, fixed = TRUE)
if (length(a1) != 1L) stop(sprintf("battery anchor matches=%d", length(a1)))
if (length(a2) != 1L) stop(sprintf("bullet anchor matches=%d", length(a2)))

# Idempotency guard: bail if already inserted.
if (any(grepl("(11) SAMPLE SPLIT: SOCIAL SAFETY NET", L, fixed = TRUE)))
  stop("(11) block already present; aborting to avoid duplicate.")

out <- L
out <- append(out, b11,  after = a1)   # higher line first
out <- append(out, bl11, after = a2)   # a2 < a1, index unaffected

con <- file(f, open = "wb")
writeLines(out, con, sep = if (crlf) "\r\n" else "\n", useBytes = TRUE)
close(con)
cat(sprintf("OK: (11) battery block after line %d (+%d lines); bullet 11 after line %d (+%d lines). CRLF=%s. Now %d lines.\n",
            a1, length(b11), a2, length(bl11), crlf, length(out)))
