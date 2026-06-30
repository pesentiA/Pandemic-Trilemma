# =====================================================================
# V15 debt spec with DI at lag 1 (F_DI -> F_DI_lag1), vs. the V15 baseline.
# =====================================================================
run_dir <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/code/R/_rerun_20260615"
snap    <- file.path(run_dir, "analysis_snapshot4.R")
logf    <- file.path(run_dir, "debt_v15_DIlag1_log.txt")

con <- file(logf, open = "wt", encoding = "UTF-8")
sink(con); sink(con, type = "message")
cat("=== V15 DEBT: DI contemporaneous vs DI lag-1 ===\n\n")

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
ssc_v15 <- ssc(K.adj = TRUE, G.adj = TRUE)

m_base <- feols(debt_dR_adj ~ y_lag1 + F_CP_above_3 + F_CP_loans_mid +
                  F_CP_guar_lo + F_DI + F_H_lag1 | Country, pdataD, subset = sub)
m_dil1 <- feols(debt_dR_adj ~ y_lag1 + F_CP_above_3 + F_CP_loans_mid +
                  F_CP_guar_lo + F_DI_lag1 + F_H_lag1 | Country, pdataD, subset = sub)

cat("\n========== V15 BASELINE: F_DI (contemporaneous) ==========\n")
print(summary(m_base, cluster = ~ Country, ssc = ssc_v15))
cat("\n========== V15 with DI at LAG 1: F_DI_lag1 ==========\n")
print(summary(m_dil1, cluster = ~ Country, ssc = ssc_v15))

cat("\n========== SIDE-BY-SIDE ==========\n")
print(etable(list("V15 (F_DI)" = m_base, "V15 (F_DI_lag1)" = m_dil1),
             cluster = ~ Country, ssc = ssc(adj = TRUE, cluster.adj = TRUE),
             keep = c("y_lag1","F_CP_above_3","F_CP_loans_mid","F_CP_guar_lo",
                      "F_DI","F_DI_lag1","F_H_lag1")))

cat("\n=== DONE ===\n")
sink(type = "message"); sink(); close(con)
