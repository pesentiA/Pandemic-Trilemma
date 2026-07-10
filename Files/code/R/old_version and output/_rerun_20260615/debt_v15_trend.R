# =====================================================================
# Main debt spec V15 (y_lag1 + F_DI_lag1) WITH a linear quarter trend.
# =====================================================================
run_dir <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/code/R/_rerun_20260615"
snap    <- file.path(run_dir, "analysis_snapshot7.R")
logf    <- file.path(run_dir, "debt_v15_trend_log.txt")

con <- file(logf, open = "wt", encoding = "UTF-8")
sink(con); sink(con, type = "message")
cat("=== DEBT V15 (y_lag1, F_DI_lag1) + linear quarter trend ===\n\n")

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

sub <- ~ t_idx >= 4 & t_idx <= 16
S15 <- ssc(K.adj = TRUE, G.adj = TRUE)

m_base  <- feols(debt_dR_adj ~ y_lag1 + F_CP_above_3 + F_CP_loans_mid +
                   F_CP_guar_lo + F_DI_lag1 + F_H_lag1 | Country,
                 pdataD, subset = sub)
m_trend <- feols(debt_dR_adj ~ y_lag1 + F_CP_above_3 + F_CP_loans_mid +
                   F_CP_guar_lo + F_DI_lag1 + F_H_lag1 + as.numeric(t_idx) | Country,
                 pdataD, subset = sub)

cat("\n========== V15 baseline (no trend) ==========\n")
print(summary(m_base, cluster = ~ Country, ssc = S15))
cat("\n========== V15 + linear quarter trend ==========\n")
print(summary(m_trend, cluster = ~ Country, ssc = S15))

cat("\n========== SIDE-BY-SIDE ==========\n")
print(etable(list("V15 (no trend)" = m_base, "V15 + lin. trend" = m_trend),
             cluster = ~ Country, ssc = ssc(adj = TRUE, cluster.adj = TRUE),
             keep = c("y_lag1","F_CP_above_3","F_CP_loans_mid","F_CP_guar_lo",
                      "F_DI_lag1","F_H_lag1","as.numeric")))
cat("\n=== DONE ===\n")
sink(type = "message"); sink(); close(con)
