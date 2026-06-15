# =====================================================================
# Independent verification of the V14 wild-cluster bootstrap + jackknife
# (fwildclusterboot / summclust are unavailable for R 4.6.0 binaries,
#  so we reimplement the Cameron-Gelbach-Miller 2008 WCR bootstrap-t by
#  hand in feols, and a leave-one-cluster-out jackknife.)
# =====================================================================

run_dir <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/code/R/_rerun_20260615"
snap    <- file.path(run_dir, "analysis_snapshot.R")
logf    <- file.path(run_dir, "boot_log.txt")

con <- file(logf, open = "wt", encoding = "UTF-8")
sink(con); sink(con, type = "message")
cat("=== BOOTSTRAP / JACKKNIFE VERIFICATION ===\n\n")

# ---- 1. Rebuild df_bin via the resilient pipeline (isolated env) ----
src <- readLines(snap, warn = FALSE)
cut <- grep("STAGE 4 - DEBT EQUATION", src, fixed = TRUE)[1]
src <- src[seq_len(cut - 1L)]
for (pk in c("polars", "fwildclusterboot", "summclust"))
  src <- gsub(sprintf("^\\s*library\\(%s\\).*$", pk),
              sprintf('cat("[skip] %s\\\\n")', pk), src)
src <- sub('^\\s*safeplots\\s*<-.*$', sprintf('safeplots <- "%s"', file.path(run_dir, "out_figures")), src)
src <- sub('^\\s*safetable\\s*<-.*$', sprintf('safetable <- "%s"', file.path(run_dir, "out_tables")), src)
exprs <- parse(text = paste(src, collapse = "\n"))
E <- new.env(parent = globalenv())
for (i in seq_along(exprs)) try(eval(exprs[[i]], envir = E), silent = TRUE)

suppressMessages(library(fixest))
df_bin <- get("df_bin", envir = E)

bs <- subset(df_bin, t_idx >= 4 & t_idx <= 14)
bs$DI_S <- bs$F_DI_lag1 * bs$S_mean_tw
rhs <- c("y_lag1", "S_mean_tw", "F_CP_above_flow_lag2", "F_CP_belowstock", "F_DI_lag1", "DI_S")
fml_full <- as.formula(paste("y_t_pct ~", paste(rhs, collapse = " + "), "| Country"))

full <- feols(fml_full, data = bs, cluster = ~ Country)
ct   <- coeftable(full)
tobs <- ct[, "t value"]; names(tobs) <- rownames(ct)
cat("V14 bootstrap-sample fit (CRV1), N =", nobs(full), ":\n")
print(round(ct, 4)); cat("\n")

# ---- 2. WCR bootstrap-t (impose null, Rademacher), B = 9999 ----
params  <- c("F_CP_above_flow_lag2", "F_CP_belowstock", "F_DI_lag1", "DI_S", "S_mean_tw", "y_lag1")
B       <- 9999
clusters <- unique(bs$Country)
G        <- length(clusters)
gid      <- match(bs$Country, clusters)
set.seed(16031995)

cat(sprintf("Wild-cluster restricted bootstrap-t (Rademacher, B=%d, G=%d clusters)\n", B, G))
cat("------------------------------------------------------------------\n")
cat(sprintf("%-22s %9s %9s %12s\n", "param", "t_obs", "CRV1-p", "WCR-p"))
for (p in params) {
  rhs_r <- setdiff(rhs, p)
  fml_r <- as.formula(paste("y_t_pct ~", paste(rhs_r, collapse = " + "), "| Country"))
  mr    <- feols(fml_r, data = bs, cluster = ~ Country)
  yhat  <- predict(mr); uhat <- resid(mr)
  tstar <- numeric(B)
  for (b in seq_len(B)) {
    v <- sample(c(-1, 1), G, replace = TRUE)[gid]
    bs$ystar <- yhat + v * uhat
    mb <- feols(as.formula(paste("ystar ~", paste(rhs, collapse = " + "), "| Country")),
                data = bs, cluster = ~ Country)
    tstar[b] <- coeftable(mb)[p, "t value"]
  }
  wcr_p  <- mean(abs(tstar) >= abs(tobs[p]))
  crv1_p <- ct[p, "Pr(>|t|)"]
  cat(sprintf("%-22s %9.3f %9.4f %12.4f\n", p, tobs[p], crv1_p, wcr_p))
}

# ---- 3. Leave-one-cluster-out jackknife for Above-Flow ----
cat("\nLeave-one-cluster-out jackknife: F_CP_above_flow_lag2\n")
cat("------------------------------------------------------------------\n")
b_full <- coef(full)["F_CP_above_flow_lag2"]
jk <- sapply(clusters, function(g) {
  m <- feols(fml_full, data = subset(bs, Country != g))
  coef(m)["F_CP_above_flow_lag2"]
})
jk_mean <- mean(jk)
jk_se   <- sqrt((G - 1) / G * sum((jk - jk_mean)^2))
infl    <- clusters[order(abs(jk - b_full), decreasing = TRUE)][1:3]
cat(sprintf("full coef       = %.4f\n", b_full))
cat(sprintf("jackknife mean  = %.4f\n", jk_mean))
cat(sprintf("jackknife range = [%.4f, %.4f]\n", min(jk), max(jk)))
cat(sprintf("jackknife SE    = %.4f  (CRV1 SE = %.4f)\n", jk_se, ct["F_CP_above_flow_lag2", "Std. Error"]))
cat(sprintf("most influential clusters: %s\n", paste(infl, collapse = ", ")))

cat("\n=== DONE ===\n")
sink(type = "message"); sink(); close(con)
