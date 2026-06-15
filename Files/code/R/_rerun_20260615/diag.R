cat("=== .libPaths() ===\n"); print(.libPaths())
cat("\n=== R_LIBS_USER env ===\n"); cat(Sys.getenv("R_LIBS_USER"), "\n")
cat("\n=== n packages per libpath ===\n")
for (p in .libPaths()) cat(p, ":", length(list.dirs(p, recursive = FALSE)), "\n")
cat("\n=== key pkg presence ===\n")
for (pk in c("polars","fwildclusterboot","summclust","fixest","plm","car",
             "TwoWayFEWeights","DIDmultiplegtDYN","clubSandwich"))
  cat(sprintf("  %-18s %s\n", pk, requireNamespace(pk, quietly = TRUE)))
