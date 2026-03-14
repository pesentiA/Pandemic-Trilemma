###OLD APPROACH



####TWFE----

# (1) Simple DiD, without fixed effects
----------------------------------------------------
  
  did1 <- feols(Real.GDP.Growth ~ strict + covid + treatment, data)
  did1 <- summary(did1, cluster = "Country")
  did1
  
  # (2) Simple DiD with Yearly fixed effects only
  # Yearly fixed effects absorb the covid variable
  # ----------------------------------------------------
  
  
  did2 <- feols(Real.GDP.Growth ~ strict + treatment | time, data)
  did2 <- summary(did2, cluster = "Country")
  did2
  
  
  # (3) Fixed effects with panel data, with year and location fixed effects
  # Country fixed effects absorb the strict variable
  # ----------------------------------------------------
  
  # Table 3 HP
  # Panel A, column (3)
  did3 <- feols(Real.GDP.Growth ~ treatment  | time + Country, data)
  did3 <- summary(did3, cluster = "Country")
  did3
  
  
  # (4) Event study
  # Year x treatment interaction terms, normalized to pre-treatment period (time = 0), i.e. the omitted reference 'category'
  # ----------------------------------------------------
  
  did4 <- feols(Real.GDP.Growth ~ i(time, strict==1, 0) |time + Country,data)
  did4 <- summary(did4, cluster = "Country")
  did4
  
  #i(period, treatment group, reference point) creates a set of interact terms
  
  # Plot the event study with iplot (coefplot only shows non-omitted regressors)
  iplot(did4) 
  
  
  
  
  ##ALL ECONOMIC
  
  dependent_var_eco<-c("Real.GDP.Growth", "Private.Consumption", 
                       "Net.National.Income", "Gross.National.Income","GNI.Growth", 
                       "Net.Lending.Borrowing.Government", "Gov.Cons",
                       "Government.Debt.Ratio", "X", "M","Exports", "Imports", "Integration", "Trade.in.Goods.and.Services", "Trade.Balance",
                       "BOP","GB","Gross.Cap.form", "X68_Gross_capital_formation",  "Inflation_Rate", "CPI", "Unemployment.Rate", "Youth.Unemployment.Rate",
                       "Older.Persons.Unemployment.Rate")
  
  #"GDP.per.Capita", "GDP.per.Hour.Worked",  "Household.Spending", "GDPGRO",
  
  # Funktion definieren
  run_analysis <- function(dependent_var_eco) {
    
    # (1) Einfache DiD
    did1 <- feols(as.formula(paste(dependent_var_eco, "~ strict + covid + treatment")), data)
    did1 <- summary(did1, cluster = "Country")
    
    # (2) Einfache DiD mit jährlichen Fixed Effects
    did2 <- feols(as.formula(paste(dependent_var_eco, "~ strict + treatment | time")), data)
    did2 <- summary(did2, cluster = "Country")
    
    # (3) Fixed Effects mit Panel-Daten
    did3 <- feols(as.formula(paste(dependent_var_eco, "~ treatment  | time + Country")), data)
    did3 <- summary(did3, cluster = "Country")
    
    # (4) Event Study
    did4 <- feols(as.formula(paste(dependent_var_eco, "~ i(time, strict==1, 0) | time + Country")), data)
    did4 <- summary(did4, cluster = "Country")
    
    # Ergebnisse zusammenfassen und zurückgeben
    results_eco <- etable(did1, did2, did3, did4, 
                          cluster="Country", 
                          headers = c("Standard DiD", "Yearly FE", 
                                      "Country + Yearly FE", "Event study"))
    
    return(results_eco)
  }
  
  results_list_eco <- lapply(dependent_var_eco, run_analysis)
  
  results_list_eco
  page(results_list_eco, method = "print")
  
  
  
  
  
  # Funktion definieren
  run_analysis <- function(dependent_var_ct) {
    
    # (1) Einfache DiD
    did1 <- feols(as.formula(paste(dependent_var_ct, "~ strict + covid + treatment")), data)
    did1 <- summary(did1, cluster = "Country")
    
    # (2) Einfache DiD mit jährlichen Fixed Effects
    did2 <- feols(as.formula(paste(dependent_var_ct, "~ strict + treatment | time")), data)
    did2 <- summary(did2, cluster = "Country")
    
    # (3) Fixed Effects mit Panel-Daten
    did3 <- feols(as.formula(paste(dependent_var_ct, "~ treatment  | time + Country")), data)
    did3 <- summary(did3, cluster = "Country")
    
    # (4) Event Study
    did4 <- feols(as.formula(paste(dependent_var_ct, "~ i(time, strict==1, 0) | time + Country")), data)
    did4 <- summary(did4, cluster = "Country")
    
    # Ergebnisse zusammenfassen und zurückgeben
    results_ct <- etable(did1, did2, did3, did4, 
                         cluster="Country", 
                         headers = c("Standard DiD", "Yearly FE", 
                                     "Country + Yearly FE", "Event study"))
    
    return(results_ct)
  }
  
  results_list_ct <- lapply(dependent_var_ct, run_analysis)
  
  # Ergebnisse anzeigen
  results_list_ct
  page(results_list_ct, method = "print")
  
  
  
  
  
  extract_event_study_latex <- function(results_for_outcome) {
    # Nehmen Sie nur das Event Study-Modell und konvertieren Sie es in LaTeX
    latex_output <- etable(results_for_outcome$did4, 
                           cluster="Country", 
                           type="latex",
                           title="Event Study Ergebnisse")
    return(latex_output)
  }
  
  
  file_conn <- file("event_study_results_all_outcomes_soc.tex")
  for (result in results_list_ct) {
    writeLines(result$did4, file_conn)
    writeLines("\\newpage", file_conn)  # Fügt eine neue Seite zwischen den outcomes hinzu
  }
  close(file_conn)
  
  # Überprüfen Sie den Inhalt des ersten Elements
  cat(results_list_eco[[1]]$did4)
  # Datei zum Schreiben öffnen
  file_conn <- file("event_study_results_all_outcomes_soc.tex", open = "wt")
  
  # Durch die Ergebnisse gehen und in die Datei schreiben
  for (result in results_list_ct) {
    writeLines(result$did4, file_conn)
    writeLines("\\newpage\n", file_conn)  # Fügt eine neue Seite zwischen den outcomes hinzu
  }
  
  # Datei schließen
  close(file_conn)
  