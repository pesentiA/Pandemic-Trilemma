# =====================================================================
# Verify the adjusted V15 debt robustness battery executes and reproduces.
# 1. Rebuild pdataD through the V15 main spec (resilient, isolated env).
# 2. Source the new battery (debt_robustness_V15.R) on that pdataD.
# 3. Hand-roll the V15 wild-cluster bootstrap (fwildclusterboot unavailable).
# =====================================================================
run_dir <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/code/R/_rerun_20260615"
snap    <- file.path(run_dir, "analysis_snapshot2.R")
batt    <- file.path(run_dir, "debt_robustness_V15.R")
logf    <- file.path(run_dir, "debt_v15_log.txt")

con <- file(logf, open = "wt", encoding = "UTF-8")
sink(con); sink(con, type = "message")
cat("=== V15 DEBT ROBUSTNESS VERIFICATION ===\n\n")

# ---- 1. Rebuild pdataD up to the (old) battery header ----
src <- readLines(snap, warn = FALSE)
cut <- grep("DEBT EQUATION V14 - ROBUSTNESS BATTERY", src, fixed = TRUE)[1]
src <- src[seq_len(cut - 1L)]
for (pk in c("polars", "fwildclusterboot", "summclust"))
  src <- gsub(sprintf("^\\s*library\\(%s\\).*$", pk), sprintf('cat("[skip] %s\\\\n")', pk), src)
src <- sub('^\\s*safeplots\\s*<-.*$', sprintf('safeplots <- "%s"', file.path(run_dir, "out_figures")), src)
src <- sub('^\\s*safetable\\s*<-.*$', sprintf('safetable <- "%s"', file.path(run_dir, "out_tables")), src)
exprs <- parse(text = paste(src, collapse = "\n"))
E <- new.env(parent = globalenv())
for (i in seq_along(exprs)) try(eval(exprs[[i]], envir = E), silent = TRUE)
suppressMessages(library(fixest))

if (!exists("pdataD", envir = E)) stop("pdataD not built")
cat("pdataD rebuilt. V15 columns present:\n")
for (v in c("debt_dR_adj","y_lag1","F_CP_above_3","F_CP_loans_mid","F_CP_guar_lo","F_DI","F_H_lag1"))
  cat(sprintf("  %-18s %s\n", v, v %in% names(get("pdataD", E))))

# ---- 2. Source the new battery on E ----
cat("\n\n############### NEW V15 BATTERY OUTPUT ###############\n")
bexpr <- parse(text = paste(readLines(batt, warn = FALSE), collapse = "\n"))
nerr <- 0
for (i in seq_along(bexpr)) {
  r <- tryCatch(withVisible(eval(bexpr[[i]], envir = E)),
                error = function(e) { cat(sprintf("\n[[BATTERY EXPR %d ERROR]] %s\n", i, conditionMessage(e))); nerr <<- nerr + 1; NULL })
  if (!is.null(r) && isTRUE(r$visible)) tryCatch(print(r$value), error = function(e) cat("[print failed]\n"))
}
cat(sprintf("\n[battery expressions errored: %d]\n", nerr))

# ---- 3. Hand-rolled V15 wild-cluster restricted bootstrap-t ----
cat("\n\n############### V15 WILD-CLUSTER BOOTSTRAP (hand-rolled) ###############\n")
pdataD <- get("pdataD", E)
vars <- c("debt_dR_adj","y_lag1","F_CP_above_3","F_CP_loans_mid","F_CP_guar_lo","F_DI","F_H_lag1","Country","t_idx")
bs <- pdataD[pdataD$t_idx >= 4 & pdataD$t_idx <= 16, vars]
bs <- bs[stats::complete.cases(bs), ]
rhs <- c("y_lag1","F_CP_above_3","F_CP_loans_mid","F_CP_guar_lo","F_DI","F_H_lag1")
full <- feols(as.formula(paste("debt_dR_adj ~", paste(rhs, collapse=" + "), "| Country")),
              data = bs, cluster = ~ Country)
ct <- coeftable(full); tobs <- ct[, "t value"]; names(tobs) <- rownames(ct)
cat(sprintf("V15 bootstrap-sample fit, N = %d:\n", nobs(full))); print(round(ct, 4))
B <- 9999; cl <- unique(bs$Country); G <- length(cl); gid <- match(bs$Country, cl)
set.seed(16031995)
cat(sprintf("\nWCR-t (Rademacher, B=%d, G=%d):\n", B, G))
cat(sprintf("%-18s %9s %9s %10s\n", "param", "t_obs", "CRV1-p", "WCR-p"))
for (p in rhs) {
  mr <- feols(as.formula(paste("debt_dR_adj ~", paste(setdiff(rhs, p), collapse=" + "), "| Country")),
              data = bs, cluster = ~ Country)
  yh <- predict(mr); uh <- resid(mr); tstar <- numeric(B)
  for (b in seq_len(B)) {
    bs$ystar <- yh + sample(c(-1, 1), G, replace = TRUE)[gid] * uh
    tstar[b] <- coeftable(feols(as.formula(paste("ystar ~", paste(rhs, collapse=" + "), "| Country")),
                                data = bs, cluster = ~ Country))[p, "t value"]
  }
  cat(sprintf("%-18s %9.3f %9.4f %10.4f\n", p, tobs[p], ct[p, "Pr(>|t|)"], mean(abs(tstar) >= abs(tobs[p]))))
}

cat("\n=== DONE ===\n")
sink(type = "message"); sink(); close(con)
