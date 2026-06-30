# =====================================================================
# Debt equation V15 - sample split by social safety net (high vs low),
# using the SAME country groupings as the output-gap robustness section.
# =====================================================================
run_dir <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/code/R/_rerun_20260615"
snap    <- file.path(run_dir, "analysis_snapshot3.R")
logf    <- file.path(run_dir, "debt_socnet_log.txt")

con <- file(logf, open = "wt", encoding = "UTF-8")
sink(con); sink(con, type = "message")
cat("=== DEBT V15: HIGH vs LOW SOCIAL-NET SPLIT ===\n\n")

# Rebuild pdataD through the V15 spec (resilient, isolated env).
src <- readLines(snap, warn = FALSE)
cut <- grep("DEBT EQUATION V15 - ROBUSTNESS BATTERY", src, fixed = TRUE)[1]
src <- src[seq_len(cut - 1L)]
for (pk in c("polars", "fwildclusterboot", "summclust"))
  src <- gsub(sprintf("^\\s*library\\(%s\\).*$", pk), sprintf('cat("[skip] %s\\\\n")', pk), src)
src <- sub('^\\s*safeplots\\s*<-.*$', sprintf('safeplots <- "%s"', file.path(run_dir, "out_figures")), src)
src <- sub('^\\s*safetable\\s*<-.*$', sprintf('safetable <- "%s"', file.path(run_dir, "out_tables")), src)
exprs <- parse(text = paste(src, collapse = "\n"))
E <- new.env(parent = globalenv())
for (i in seq_along(exprs)) try(eval(exprs[[i]], envir = E), silent = TRUE)
suppressMessages(library(fixest))
pdataD <- get("pdataD", envir = E)

# Same social-net groupings as the output robustness battery.
high_socnet <- c("FRA","FIN","BEL","DNK","ITA","AUT","SWE","DEU","NOR",
                 "ESP","GRC","PRT","LUX","NLD","JPN","GBR","CZE","SVN","POL")
low_socnet  <- c("USA","KOR","MEX","CHL","TUR","IRL","AUS","NZL","CAN",
                 "CHE","ISR","COL","CRI","EST","LVA","LTU","HUN","SVK","ISL")

fml <- debt_dR_adj ~ y_lag1 + F_CP_above_3 + F_CP_loans_mid +
       F_CP_guar_lo + F_DI + F_H_lag1 | Country
sub <- ~ t_idx >= 4 & t_idx <= 16
ssc_v15 <- ssc(K.adj = TRUE, G.adj = TRUE)

m_full <- feols(fml, pdataD, subset = sub)
m_hi   <- feols(fml, pdataD[pdataD$Country %in% high_socnet, ], subset = sub)
m_lo   <- feols(fml, pdataD[pdataD$Country %in% low_socnet, ],  subset = sub)

cat("\n========== FULL SAMPLE (reference) ==========\n")
print(summary(m_full, cluster = ~ Country, ssc = ssc_v15))
cat("\n========== HIGH SOCIAL-NET (", length(high_socnet), "countries) ==========\n")
print(summary(m_hi, cluster = ~ Country, ssc = ssc_v15))
cat("\n========== LOW SOCIAL-NET (", length(low_socnet), "countries) ==========\n")
print(summary(m_lo, cluster = ~ Country, ssc = ssc_v15))

cat("\n========== SIDE-BY-SIDE (etable) ==========\n")
print(etable(list("Full" = m_full, "High soc.net" = m_hi, "Low soc.net" = m_lo),
             cluster = ~ Country, ssc = ssc(adj = TRUE, cluster.adj = TRUE),
             keep = c("y_lag1","F_CP_above_3","F_CP_loans_mid","F_CP_guar_lo","F_DI","F_H_lag1")))

cat("\n=== DONE ===\n")
sink(type = "message"); sink(); close(con)
