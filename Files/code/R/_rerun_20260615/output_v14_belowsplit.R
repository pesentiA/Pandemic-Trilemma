# =====================================================================
# Output equation V14 with Below-Stock split into loans-stock + guar-stock.
# Baseline: F_CP_belowstock = F_CP_loans_stock + F_CP_guar_stock.
# =====================================================================
run_dir <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/code/R/_rerun_20260615"
snap    <- file.path(run_dir, "analysis_snapshot5.R")
logf    <- file.path(run_dir, "output_v14_belowsplit_log.txt")

con <- file(logf, open = "wt", encoding = "UTF-8")
sink(con); sink(con, type = "message")
cat("=== OUTPUT V14: Below-Stock split (loans vs guarantees) ===\n\n")

src <- readLines(snap, warn = FALSE)
cut <- grep("STAGE 4 - DEBT EQUATION", src, fixed = TRUE)[1]
src <- src[seq_len(cut - 1L)]
for (pk in c("polars", "fwildclusterboot", "summclust"))
  src <- gsub(sprintf("^\\s*library\\(%s\\).*$", pk), sprintf('cat("[skip] %s\\\\n")', pk), src)
src <- sub('^\\s*safeplots\\s*<-.*$', sprintf('safeplots <- "%s"', file.path(run_dir, "out_figures")), src)
src <- sub('^\\s*safetable\\s*<-.*$', sprintf('safetable <- "%s"', file.path(run_dir, "out_tables")), src)
exprs <- parse(text = paste(src, collapse = "\n"))
E <- new.env(parent = globalenv())
for (i in seq_along(exprs)) try(eval(exprs[[i]], envir = E), silent = TRUE)
suppressMessages(library(fixest))
df_bin <- get("df_bin", envir = E)

sub <- ~ t_idx >= 4 & t_idx <= 14
ssc14 <- ssc(K.adj = TRUE, G.adj = TRUE)

m_base <- feols(y_t_pct ~ y_lag1 + S_mean_tw + F_CP_above_flow_lag2 +
                  F_CP_belowstock + F_DI_lag1 * S_mean_tw | Country,
                df_bin, subset = sub)
m_split <- feols(y_t_pct ~ y_lag1 + S_mean_tw + F_CP_above_flow_lag2 +
                   F_CP_loans_stock + F_CP_guar_stock + F_DI_lag1 * S_mean_tw | Country,
                 df_bin, subset = sub)

cat("\n========== V14 BASELINE (aggregate Below-Stock) ==========\n")
print(summary(m_base, cluster = ~ Country, ssc = ssc14))
cat("\n========== V14 with Below-Stock SPLIT (loans-stock + guar-stock) ==========\n")
print(summary(m_split, cluster = ~ Country, ssc = ssc14))

cat("\nCor(loans_stock, guar_stock) =",
    round(cor(df_bin$F_CP_loans_stock, df_bin$F_CP_guar_stock, use = "complete.obs"), 3), "\n")

cat("\n========== SIDE-BY-SIDE ==========\n")
print(etable(list("V14 (aggregate below)" = m_base, "V14 (loans/guar split)" = m_split),
             cluster = ~ Country, ssc = ssc(adj = TRUE, cluster.adj = TRUE),
             keep = c("y_lag1","S_mean_tw","F_CP_above_flow_lag2","F_CP_belowstock",
                      "F_CP_loans_stock","F_CP_guar_stock","F_DI_lag1","S_mean_tw:F_DI_lag1")))

cat("\n=== DONE ===\n")
sink(type = "message"); sink(); close(con)
