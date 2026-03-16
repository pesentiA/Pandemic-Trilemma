# Minimal runner: load data, build pdata, produce descriptive time-series plots
script_path <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/code/R/analysis.R"
.script_lines <- readLines(script_path)

cat("=== Running data preparation (lines 1-628) ===\n")
eval(parse(text = .script_lines[1:628]))

cat("\n=== Running plot code (lines 1751-end) ===\n")
.plot_code <- .script_lines[1751:length(.script_lines)]
eval(parse(text = .plot_code))

cat("\n=== Done. Plots saved to:", safeplots, "===\n")
