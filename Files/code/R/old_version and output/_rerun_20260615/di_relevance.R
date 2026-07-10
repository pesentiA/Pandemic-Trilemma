# =====================================================================
# DI channel relevance tests for the output V14 spec (verify numbers).
# =====================================================================
run_dir <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/code/R/_rerun_20260615"
snap    <- file.path(run_dir, "analysis_snapshot6.R")
logf    <- file.path(run_dir, "di_relevance_log.txt")

con <- file(logf, open = "wt", encoding = "UTF-8")
sink(con); sink(con, type = "message")
cat("=== DI RELEVANCE TESTS (output V14) ===\n\n")

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
S14 <- ssc(K.adj = TRUE, G.adj = TRUE)

v14 <- feols(y_t_pct ~ y_lag1 + S_mean_tw + F_CP_above_flow_lag2 +
               F_CP_belowstock + F_DI_lag1 * S_mean_tw | Country, df_bin, subset = sub)

cat("\n--- TEST 1: joint Wald  H0: alpha_DI = alpha_S,DI = 0 ---\n")
print(wald(v14, keep = "F_DI_lag1", cluster = ~ Country, ssc = S14))

cat("\n--- TEST 2: DI without interaction (unconditional) ---\n")
v14_noint <- feols(y_t_pct ~ y_lag1 + S_mean_tw + F_CP_above_flow_lag2 +
                     F_CP_belowstock + F_DI_lag1 | Country, df_bin, subset = sub)
print(summary(v14_noint, cluster = ~ Country, ssc = S14))

cat("\n--- TEST 3: collinearity of DI level vs DI x S ---\n")
es <- subset(df_bin, t_idx >= 4 & t_idx <= 14)
dm <- es$F_DI_lag1; di <- es$F_DI_lag1 * es$S_mean_tw; ok <- is.finite(dm) & is.finite(di)
cat(sprintf("  corr = %.4f ;  nonzero DI quarters: %d of %d\n",
            cor(dm[ok], di[ok]), sum(dm[ok] != 0), sum(ok)))

cat("\n--- TEST 4: marginal dY/dDI across stringency ---\n")
b <- coef(v14); V <- vcov(v14, cluster = ~ Country, ssc = S14)
nmI <- grep(":", grep("F_DI_lag1", names(b), value = TRUE), value = TRUE)
cat(sprintf("  zero-crossing S* = %.2f\n", -b["F_DI_lag1"] / b[nmI]))
for (s in c(0, quantile(es$S_mean_tw, c(.10,.50,.90), na.rm = TRUE))) {
  me <- b["F_DI_lag1"] + b[nmI] * s
  se <- sqrt(V["F_DI_lag1","F_DI_lag1"] + s^2 * V[nmI,nmI] + 2*s*V["F_DI_lag1",nmI])
  cat(sprintf("  S=%5.1f : dY/dDI = %+6.3f  [%+6.3f, %+6.3f]%s\n",
              s, me, me-1.96*se, me+1.96*se,
              if (me-1.96*se <= 0 & me+1.96*se >= 0) "  (CI incl. 0)" else ""))
}
cat("\n=== DONE ===\n")
sink(type = "message"); sink(); close(con)
