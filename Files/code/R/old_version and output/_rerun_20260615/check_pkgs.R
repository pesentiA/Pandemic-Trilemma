req <- c("polars","DIDmultiplegtDYN","fwildclusterboot","summclust","clubSandwich","boot",
  "plm","car","TwoWayFEWeights","fixest","dplyr","tidyr","stringr","lubridate","readxl",
  "conflicted","modelsummary","sandwich","lmtest","purrr","magrittr","tidyverse","scales",
  "ggplot2","did2s","haven","data.table","gtools","gt","gridExtra","huxtable","xtable",
  "stargazer","AER","psych","pastecs","kableExtra","patchwork","mFilter","countrycode",
  "corrplot","sf","RColorBrewer","UpSetR","jtools","fBasics","Synth","causalweight",
  "expss","knitr","rnaturalearth","rnaturalearthdata","foreign","did","ggExtra")
inst <- rownames(installed.packages())
miss <- setdiff(req, inst)
cat("R:", R.version.string, "\n")
cat("Installed among required:", length(intersect(req, inst)), "/", length(req), "\n")
cat("MISSING (", length(miss), "):\n", sep = "")
if (length(miss)) cat(paste0("  - ", miss), sep = "\n") else cat("  none\n")
cat("\n")
