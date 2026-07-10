# =====================================================================
# Debt main-results table: 3 specs (Coarse / Headline / Preferred) under
# DI contemporaneous (F_DI) vs DI at lag 1 (F_DI_lag1).
# =====================================================================
run_dir <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/code/R/_rerun_20260615"
snap    <- file.path(run_dir, "analysis_snapshot4.R")
logf    <- file.path(run_dir, "debt_table_DIlag1_log.txt")

con <- file(logf, open = "wt", encoding = "UTF-8")
sink(con); sink(con, type = "message")
cat("=== DEBT MAIN TABLE: F_DI vs F_DI_lag1 ===\n\n")

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

below <- list(Coarse    = "F_CP_below_flow",
              Headline  = c("F_CP_loans", "F_CP_guar"),
              Preferred = c("F_CP_loans_mid", "F_CP_guar_lo"))
sub <- ~ t_idx >= 4 & t_idx <= 16
ssc_v15 <- ssc(K.adj = TRUE, G.adj = TRUE)

for (di in c("F_DI", "F_DI_lag1")) {
  for (nm in names(below)) {
    rhs <- c("y_lag1", "F_CP_above_3", below[[nm]], di, "F_H_lag1")
    fml <- as.formula(paste("debt_dR_adj ~", paste(rhs, collapse = " + "), "| Country"))
    m   <- feols(fml, pdataD, subset = sub)
    cat(sprintf("\n========== %s  |  DI = %s ==========\n", nm, di))
    print(summary(m, cluster = ~ Country, ssc = ssc_v15))
  }
}
cat("\n=== DONE ===\n")
sink(type = "message"); sink(); close(con)
