base_dir <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files"
load(file.path(base_dir, "data/processed/dataforanalysis.RData"))
cat("qdata quarters:\n")
print(sort(unique(qdata$Quarter)))
cat("\nMin year:", min(qdata$year_only, na.rm = TRUE), "\n")
cat("Max year:", max(qdata$year_only, na.rm = TRUE), "\n")
