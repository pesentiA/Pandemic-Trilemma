# =====================================================================
# Resilient runner for the Pandemic-Trilemma output-gap + V14 robustness
# Re-run requested 2026-06-15. Executes the FROZEN snapshot from line 1
# through the end of the V14 robustness battery (just before STAGE 4 -
# DEBT EQUATION).
#
# Design notes:
#  * Script runs in an ISOLATED env (script_env) so its rm(list=ls())
#    cannot wipe the runner's own loop state.
#  * Each top-level expression is eval'd with a per-expression error
#    trap, so missing-package blocks (wild-cluster bootstrap, summclust
#    jackknife, sf/gt maps) skip gracefully instead of halting.
#  * withVisible() reproduces REPL auto-printing so regression summaries
#    print exactly as in a normal source() run.
#  * safeplots / safetable are redirected into this sandbox so the
#    canonical output tables/figures are NOT overwritten.
# =====================================================================

run_dir <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/code/R/_rerun_20260615"
snap    <- file.path(run_dir, "analysis_snapshot.R")
logf    <- file.path(run_dir, "run_log.txt")
out_tab <- file.path(run_dir, "out_tables")
out_fig <- file.path(run_dir, "out_figures")
dir.create(out_tab, showWarnings = FALSE, recursive = TRUE)
dir.create(out_fig, showWarnings = FALSE, recursive = TRUE)

con <- file(logf, open = "wt", encoding = "UTF-8")
sink(con, split = FALSE)
sink(con, type = "message")

cat("=== RESILIENT RE-RUN START ===\n")
cat("Snapshot:", snap, "\n\n")

src <- readLines(snap, warn = FALSE)
cut <- grep("STAGE 4 - DEBT EQUATION", src, fixed = TRUE)
if (length(cut) < 1) stop("Could not locate 'STAGE 4 - DEBT EQUATION' boundary.")
cut <- cut[1]
cat(sprintf("Total snapshot lines: %d | cutting at line %d (STAGE 4 header)\n",
            length(src), cut))
src <- src[seq_len(cut - 1L)]

# Cosmetic neutralisation of hard library() calls for known-missing pkgs.
neutralize <- function(x, pkg) {
  pat <- sprintf("^\\s*library\\(%s\\).*$", pkg)
  gsub(pat, sprintf('cat("[auto-skip] library(%s) - not installed\\\\n")', pkg), x)
}
for (pk in c("polars", "fwildclusterboot", "summclust")) src <- neutralize(src, pk)

# Redirect output dirs into the sandbox (non-destructive re-run).
src <- sub('^\\s*safeplots\\s*<-.*$', sprintf('safeplots <- "%s"', out_fig), src)
src <- sub('^\\s*safetable\\s*<-.*$', sprintf('safetable <- "%s"', out_tab), src)

code  <- paste(src, collapse = "\n")
exprs <- tryCatch(parse(text = code),
                  error = function(e) { cat("PARSE ERROR:", conditionMessage(e), "\n"); NULL })

if (is.null(exprs)) {
  cat("\n!!! Snapshot did not parse; aborting.\n")
} else {
  cat(sprintf("Parsed %d top-level expressions.\n\n", length(exprs)))
  ctrl <- new.env()
  ctrl$err_idx <- integer(0)
  ctrl$err_msg <- character(0)
  script_env <- new.env(parent = globalenv())
  for (i in seq_along(exprs)) {
    res <- tryCatch(
      withVisible(eval(exprs[[i]], envir = script_env)),
      error = function(e) {
        cat(sprintf("\n[[EXPR %d ERROR]] %s\n", i, conditionMessage(e)))
        ctrl$err_idx <- c(ctrl$err_idx, i)
        ctrl$err_msg <- c(ctrl$err_msg, conditionMessage(e))
        NULL
      }
    )
    if (!is.null(res) && isTRUE(res$visible)) {
      tryCatch(print(res$value), error = function(e) cat("[print failed]\n"))
    }
  }
  cat("\n\n================ RE-RUN SUMMARY ================\n")
  cat(sprintf("Expressions evaluated : %d\n", length(exprs)))
  cat(sprintf("Expressions errored   : %d\n", length(ctrl$err_idx)))
  if (length(ctrl$err_idx)) {
    for (k in seq_along(ctrl$err_idx))
      cat(sprintf("  - expr %d: %s\n", ctrl$err_idx[k], ctrl$err_msg[k]))
  }
  cat("===============================================\n")
}

cat("\n=== RESILIENT RE-RUN END ===\n")
sink(type = "message")
sink()
close(con)
