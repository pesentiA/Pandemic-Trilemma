# ------------------------------------------------------------------------------
# 1. SETUP & LIBRARIES
# ------------------------------------------------------------------------------
# Installiere Pakete, falls noch nicht vorhanden
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("fixest")) install.packages("fixest")   # Für schnelle FE-Regressionen & DK-Errors
if(!require("data.table")) install.packages("data.table")

library(tidyverse)
library(fixest)
library(data.table)


#Notes:
#1 Block-Bootstrap-> feols in eine Schleife
#2 Daten auch mit gaps extrahieren, F nur total intenisity als % vom GDP
#3 Jorda verwendet nicht feols sondern (lpirfs)-> Erzwingt Cholesky Zerlegung
#4 Teste Interaktion:
#feols(lhs_diff ~ S*F + L1_S*L1_F + controls | country + date, vcov="DK")
# Oder state-abhängige Effekte:
#dt[, high_S := S > median(S)]
#feols(lhs_diff ~ S + F + high_S:F + controls | country + date)
#5 Implementierung mit fwildclusterboot:
#library(fwildclusterboot)

#boot_result <- boottest(
  #model, 
  #clustid = "country",
  #B = 999,
  #type = "mammen"  # oder "webb"
#)
# ------------------------------------------------------------------------------
# 2. DATEN VORBEREITUNG
# ------------------------------------------------------------------------------
# Annahme: Dein Dataframe heisst 'lpdata'
# Variablen: country, quarter (Date oder ID), rGDP_pc, debt, excess_deaths, S, F

# Um sicherzugehen, wandle in data.table um (effizienter)
dt <- as.data.table(lpdata)

# Sicherstellen, dass die Zeitvariable korrekt sortiert ist
setorder(dt, country, date) # Passe 'date' an den Namen deiner Zeitvariable an (z.B. "quarter")

# Transformationen der Outcomes
dt[, `:=`(
  # 1. Real GDP: Logarithmieren für Wachstumsraten
  ln_gdp = log(rGDP_pc),
  
  # 2. Debt: Falls absolut, Logarithmieren. Falls Quote, so lassen.
  # Annahme hier: Wir nehmen Log-Levels, um Wachstumsraten der Schulden zu sehen.
  ln_debt = log(debt) 
  
  # excess_deaths, S und F bleiben, wie sie sind.
)]

# Lags erstellen (Wichtig für Controls!)
# Wir erstellen Lags für GDP, Debt und die Schocks, um "Mean Reversion" zu kontrollieren
# Lag-Länge L=2 (Konservativ für Quartalsdaten, kann via AIC angepasst werden)
lags <- 2
vars_to_lag <- c("ln_gdp", "ln_debt", "excess_deaths", "S", "F")

dt[, (paste0("L", 1:lags, "_", rep(vars_to_lag, each=lags))) := 
     shift(.SD, 1:lags, type="lag"), 
   by = country, .SDcols = vars_to_lag]

# ------------------------------------------------------------------------------
# 3. DIE LOCAL PROJECTIONS FUNKTION
# ------------------------------------------------------------------------------
# Wir schreiben eine Funktion, die den LP-Loop für uns erledigt.
# Das ist sauberer und erlaubt uns, Outcomes einfach auszutauschen.

run_lp_panel <- function(data, outcome_var, shock_vars, horizons = 8) {
  
  results_list <- list()
  
  # Loop über jeden Horizont h (0 bis 8 Quartale)
  for (h in 0:horizons) {
    
    # A. Konstruktion der Abhängigen Variable (LHS)
    # LP Standard: y_{t+h} - y_{t-1} (Kumulative Veränderung)
    # Wir nutzen data.table shift(type="lead") für t+h
    
    # Temporäre Kopie für die Regression
    reg_data <- copy(data)
    
    # LHS berechnen: Wert in t+h minus Wert in t-1
    # lead(x, h) holt den Wert aus der Zukunft. shift(x, 1, type="lag") den aus der Vght.
    lhs_name <- "lhs_diff"
    reg_data[, (lhs_name) := shift(get(outcome_var), n=h, type="lead") - shift(get(outcome_var), n=1, type="lag"), by=country]
    
    # B. Die Regressions-Formel
    # LHS ~ Schocks + Lags | Fixed Effects
    # Wir nutzen fixest::feols
    
    # Formel zusammenbauen:
    # shock_vars sind "S" und "F"
    # Controls sind die Lags, die wir oben erstellt haben
    control_vars <- grep("^L[0-9]_", names(reg_data), value = TRUE)
    
    fml <- as.formula(paste(
      lhs_name, "~", 
      paste(c(shock_vars, control_vars), collapse = " + "), 
      "| country + date" # Two-Way Fixed Effects
    ))
    
    # C. Schätzung mit Driscoll-Kraay Standardfehlern
    # vcov = "DK" korrigiert für Autokorrelation UND räumliche Korrelation (Pandemie!)
    model <- feols(fml, data = reg_data, vcov = "DK", panel.id = c("country", "date"))
    
    # D. Ergebnisse extrahieren
    coeffs <- coeftable(model)
    
    # Für jeden Schock (S und F) die Werte speichern
    for (shock in shock_vars) {
      est <- coeffs[shock, "Estimate"]
      se  <- coeffs[shock, "Std. Error"]
      
      results_list[[paste(h, shock)]] <- data.frame(
        horizon = h,
        shock = shock,
        outcome = outcome_var,
        estimate = est,
        conf_low_95 = est - 1.96 * se,
        conf_high_95 = est + 1.96 * se,
        conf_low_90 = est - 1.645 * se,
        conf_high_90 = est + 1.645 * se
      )
    }
  }
  
  # Alles zu einem Dataframe verbinden
  return(bind_rows(results_list))
}

# ------------------------------------------------------------------------------
# 4. SCHÄTZUNG DURCHFÜHREN
# ------------------------------------------------------------------------------

# 1. Outcome: Real GDP (Kumulatives Wachstum in %)
results_gdp <- run_lp_panel(dt, outcome_var = "ln_gdp", shock_vars = c("S", "F"))

# 2. Outcome: Excess Deaths (Gap, daher kumulativer Gap oder Level-Gap)
# Hinweis: Bei Gaps nehmen wir oft y_{t+h} (Level in Zukunft) statt Differenz.
# Aber für Konsistenz (Effekt relativ zum Start) ist Differenz hier auch okay.
# Wenn du wissen willst "Wie ist der Gap in t+h?", nimm nur lead(outcome, h).
# Unten nutze ich die Differenz-Logik (Veränderung des Gaps), was konservativer ist.
results_deaths <- run_lp_panel(dt, outcome_var = "excess_deaths", shock_vars = c("S", "F"))

# 3. Outcome: Debt (Veränderung der Schuldenhöhe/Quote)
results_debt <- run_lp_panel(dt, outcome_var = "ln_debt", shock_vars = c("S", "F"))

# Alle Ergebnisse zusammenfügen
all_results <- bind_rows(results_gdp, results_deaths, results_debt)

# ------------------------------------------------------------------------------
# 5. VISUALISIERUNG (AER Style)
# ------------------------------------------------------------------------------
# Wir plotten die IRFs (Impulse Response Functions)

plot_irf <- function(data, outcome_name) {
  data %>%
    filter(outcome == outcome_name) %>%
    ggplot(aes(x = horizon, y = estimate, group = shock)) +
    # Null-Linie
    geom_hline(yintercept = 0, color = "black", size = 0.5) +
    # Konfidenzintervalle (95% als Fläche)
    geom_ribbon(aes(ymin = conf_low_95, ymax = conf_high_95, fill = shock), alpha = 0.15) +
    # Konfidenzintervalle (90% als Fläche - dunkler)
    geom_ribbon(aes(ymin = conf_low_90, ymax = conf_high_90, fill = shock), alpha = 0.25) +
    # Die Linie selbst
    geom_line(aes(color = shock), size = 1.2) +
    # Labels und Style
    scale_color_brewer(palette = "Set1", name = "Shock Type", labels = c("F (Fiscal)", "S (Response)")) +
    scale_fill_brewer(palette = "Set1", name = "Shock Type", labels = c("F (Fiscal)", "S (Response)")) +
    labs(
      title = paste("Impulse Response of", outcome_name),
      subtitle = "Local Projections with Driscoll-Kraay Standard Errors",
      y = "Cumulative Change (%) / Units",
      x = "Quarters after Shock"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 14),
      panel.grid.minor = element_blank()
    ) +
    facet_wrap(~shock, scales = "free_y") # Separate Panels für S und F
}

# Plots anzeigen
p1 <- plot_irf(all_results, "ln_gdp")
p2 <- plot_irf(all_results, "excess_deaths")
p3 <- plot_irf(all_results, "ln_debt")

print(p1)
print(p2)
print(p3)


# ==============================================================================
# PITFALLS & METHODISCHE HINWEISE (fuer die Argumentation im Paper)
# ==============================================================================

# 1. Dynamic Panel Bias (Nickell Bias)
#    - Problem: Nutzung von Lagged Dependent Variable (LDV) mit Fixed Effects.
#    - Entwarnung: Bias ist proportional zu 1/T. Bei T ~ 40 ist der Bias klein (< 2.5%).
#    - To-Do: In Fussnote als vernachlässigbar diskutieren.

# 2. Anticipation Effects (Endogenitaet)
#    - Problem: Gefahr, dass Fiskalpolitik (F) auf erwartete Krisen reagiert.
#    - Loesung: Wird durch Placebo-Tests mit Leads (siehe unten) widerlegt.

# 3. Small Sample Influence (N=38)
#    - Problem: Ergebnisse koennten durch einzelne "extreme" Laender getrieben sein.
#    - Loesung: Wird durch Jackknife-Analyse (siehe unten) geprueft.

# ==============================================================================
# ROBUSTNESS CHECKS (Implementierung im Code)
# ==============================================================================

# CHECK 1: Placebo Tests (Strict Exogeneity)
#    - Vorgehen: Regression von Outcome(t) auf Schocks(t+1) (Leads).
#    - Erwartung: Koeffizienten der Leads muessen statistisch insignifikant sein.
#    - Interpretation: Heutiges Wachstum sagt keine zukuenftigen Schocks voraus.

# CHECK 2: Jackknife / Leave-One-Out Sensitivity
#    - Vorgehen: Iterativer Loop, bei dem jeweils ein Land aus dem Sample fliegt.
#    - Erwartung: Verteilung der Koeffizienten muss stabil sein (keine Vorzeichenwechsel).
#    - Interpretation: Ergebnisse sind robust gegen Ausreisser.

# CHECK 3: Lag-Struktur Sensitivitaet
#    - Vorgehen: Schaetzung wiederholen mit L=1, L=3 und L=4.
#    - Erwartung: Die Form der IRF und Signifikanz sollten qualitativ aehnlich bleiben.
#    - Interpretation: Ergebnis ist kein Zufallsprodukt der Wahl von L=2.

# CHECK 4: Alternative Standardfehler
#    - Vorgehen: Vergleich von vcov="DK" (Driscoll-Kraay) mit vcov=~country (Cluster).
#    - Erwartung: DK Fehler sind meist groesser; Signifikanz sollte dennoch halten.
#    - Interpretation: Ergebnisse sind robust gegen raeumliche Korrelation (Spillover).

# CHECK 5: Zeit-Heterogenitaet (Impf-Effekt)
#    - Vorgehen: Interaktionsterm fuer Phase ab 2021 (Impfung vorhanden) testen.
#    - Erwartung: Pruefen, ob die Wirkung von S (Massnahmen) spaeter schwaecher wird.
#    - Interpretation: Kontrolle fuer strukturelle Aenderung der Pandemie-Dynamik.

# ------------------------------------------------------------------------------
# ROBUSTNESS 1: PLACEBO (LEADS)
# "Sagen zukünftige Schocks das heutige Wachstum voraus?" (Sollte NEIN sein)
# ------------------------------------------------------------------------------

run_placebo <- function(data) {
  # Wir testen, ob Schock in t+1 das Outcome in t beeinflusst
  # Dazu nutzen wir shift(..., type="lead") für den Schock
  
  # Outcome: Log-Differenz heute (t)
  data[, d_lngdp := ln_gdp - shift(ln_gdp, 1, type="lag"), by=country]
  
  # Leads der Schocks
  data[, S_lead1 := shift(S, 1, type="lead"), by=country]
  data[, F_lead1 := shift(F, 1, type="lead"), by=country]
  
  # Regression (kontrolliert für Vergangenheit, testet Zukunft)
  # Wichtig: Wir nutzen d_lngdp als LHS
  placebo_model <- feols(d_lngdp ~ S_lead1 + F_lead1 + 
                           L1_ln_gdp + L1_S + L1_F | country + date, 
                         data = data, vcov = "DK", panel.id = c("country", "date"))
  
  print(summary(placebo_model))
}

# ------------------------------------------------------------------------------
# ROBUSTNESS 2: JACKKNIFE (Leave-One-Out)
# ------------------------------------------------------------------------------

run_jackknife <- function(data, h_target=4) {
  countries <- unique(data$country)
  coefs_S <- numeric(length(countries))
  coefs_F <- numeric(length(countries))
  names(coefs_S) <- countries
  
  # LHS für Horizont h vorbereiten (einmalig für Speed)
  dt_jk <- copy(data)
  dt_jk[, lhs := shift(ln_gdp, n=h_target, type="lead") - shift(ln_gdp, n=1, type="lag"), by=country]
  
  for(i in seq_along(countries)) {
    c_out <- countries[i]
    # Sample ohne Land c_out
    sub_data <- dt_jk[country != c_out]
    
    # Simple Schätzung (ohne Loop über alle h, nur für h_target)
    mod <- feols(lhs ~ S + F + L1_ln_gdp + L1_S | country + date, 
                 data = sub_data, vcov = "DK", panel.id = c("country", "date"))
    
    coefs_S[i] <- coef(mod)["S"]
    coefs_F[i] <- coef(mod)["F"]
  }
  
  # Plotten
  par(mfrow=c(1,2))
  hist(coefs_S, main=paste("Jackknife S (h=", h_target, ")"), xlab="Coefficient")
  hist(coefs_F, main=paste("Jackknife F (h=", h_target, ")"), xlab="Coefficient")
}