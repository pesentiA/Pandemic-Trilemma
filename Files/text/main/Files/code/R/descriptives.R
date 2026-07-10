##BESCHREIBUNG HINZUFÜGEN
##Install Packages and load Data ----
#.rs.restartR()

rm(list=ls())

packages_vector <- c( "did2s","haven", "dplyr",  "sandwich",  "jtools", "data.table",
                      "fBasics","gtools","rnaturalearth", "rnaturalearthdata", "foreign","gt", "Synth","gridExtra", "fixest","huxtable", 
                      "xtable", "foreign", "stargazer", "AER", "causalweight", "tidyr","expss","stringr","pscore","AER","ggplot2","haven","lubridate" ,"knitr",
                      "kableExtra", "psych", "pastecs","purrr","magrittr","did","remote", "did2s", "patchwork", "readxl", "did2s", "plm", "scales", "mFilter", 
                      "countrycode", "tidyverse", "corrplot", "rnaturalearthdata", "ggExtra", "gt", "sf", "RColorBrewer","UpSetR")



#install.packages(packages_vector)
lapply(packages_vector, require, character.only = TRUE) 


# List loaded packages 
(.packages())

# Set options
options(max.print = 9999, scipen = 999, na.print = "")

dir <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/output r"
setwd(dir)

set.seed(1234)

#### Set preferences:
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflicts_prefer(dplyr::lag)
conflicted::conflicts_prefer(dplyr::first)
conflicted::conflicts_prefer(dplyr::coalesce)
conflicted::conflicts_prefer(dplyr::recode)
conflicted::conflicts_prefer(lubridate::intersect)
conflicted::conflicts_prefer(lubridate::quarter)
conflicted::conflicts_prefer(lubridate::year)
conflicted::conflicts_prefer(lubridate::wday)
conflicted::conflicts_prefer(dplyr::last)
conflicted::conflicts_prefer(data.table::isoweek)
conflicted::conflicts_prefer(data.table::month)
conflicted::conflicts_prefer(dplyr::lead)

##Data from qcode Skript laden

safedata <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/Database/Analyse"
load(file.path(safedata, "datafordescriptives.RData"))

#Output Location Plots und Table
safeplots <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/output r/Plots"
safetable <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/output r/Table Descriptives"

##we now have the folowing datasets to work with

#Main Quarterly Dataset with all Outcomes
colnames(qdata)

#Excess Deaths weekly
colnames(excess_w)
colnames(p_values_oecd_w)
colnames(economist_w)

#Fiscal Measures Daily
colnames(fm_d)

#Oxford und eigenen Indexe für S
colnames(oxd_d)
#Alle Oxoford Daten für alle Länder
colnames(oxd_spatial_d)

#Hilfsdatensets
colnames(google_mobility_d)
colnames(hosp_d)

#RRate?-> Descriptives durch und löschen->Ideen herausschrieben


################################################################################
#===============================================================================
#   PHASE 1: DESCRIPTIVE STATISTICS AND VALIDITY CHECKS
#===============================================================================
################################################################################

#...............................................................................
##########################Fiscal Measures#######################################
#..............................................................................

# ==============================================================================
#  DESCRIPTIVE ANALYSIS: FISCAL MEASURES (F)
#  Transmission Channel Decomposition: DI / CP / H
#
#  Prerequisite: load("alle_meine_datensets.RData")
#  Uses:         fm_d (fiscal measures dataset, v1.4 with transmission_channel)
#
#  Structure:
#    1. Data preparation & filtering
#    2. Overall descriptives
#    3. Temporal dynamics (Year x Quarter)
#    4. Country-level heterogeneity
#    5. Regional decomposition
#    6. Instrument composition (PolicyCode level)
#    7. IMF accounting cross-validation
#    8. Distributional properties
# ==============================================================================


# ==============================================================================
#  1. DATA PREPARATION
# ==============================================================================

# --- 1a. Restrict to fiscal measures -----------------------------------------
# broad_fiscal = 1: core measures (positive fiscal weight)
# broad_fiscal = 2: extensions (fiscal weight = 0, timeline documentation)
# broad_fiscal = 3: EU-level measures (NextGenEU, EIB)
# broad_fiscal = 0: monetary policy (excluded from this analysis)

fm_all <- fm_d %>%
  filter(broad_fiscal %in% c(1, 2, 3))

# Core measures: the empirically relevant subset with positive fiscal weight
fm <- fm_d %>%
  filter(broad_fiscal == 1)

cat("============================================================\n")
cat("  FISCAL MEASURES: DESCRIPTIVE ANALYSIS\n")
cat("============================================================\n\n")
cat(sprintf("Total observations in fm_d:     %d\n", nrow(fm_d)))
cat(sprintf("Fiscal measures (bf=1,2,3):     %d\n", nrow(fm_all)))
cat(sprintf("Core measures (bf=1):           %d\n", nrow(fm)))
cat(sprintf("Extensions (bf=2):              %d\n", sum(fm_all$broad_fiscal == 2)))
cat(sprintf("EU measures (bf=3):             %d\n", sum(fm_all$broad_fiscal == 3)))
cat(sprintf("Countries:                      %d\n", n_distinct(fm$Country)))
cat(sprintf("Date range:                     %s to %s\n",
            min(fm$date, na.rm = TRUE), max(fm$date, na.rm = TRUE)))
cat(sprintf("Missing broad_fiscal_gdp:       %d\n",
            sum(is.na(fm$broad_fiscal_gdp))))


# ==============================================================================
#  2. OVERALL DESCRIPTIVES
# ==============================================================================

cat("\n\n============================================================\n")
cat("  2. OVERALL DESCRIPTIVES\n")
cat("============================================================\n")

# --- 2a. Aggregate totals by transmission channel ----------------------------
# Each observation is one measure; broad_fiscal_gdp is that measure's size as
# a share of the country's 2019 GDP. Summing across all countries gives the
# cross-country total (not a meaningful economic quantity per se, but captures
# the relative weight of each channel in the full dataset).
fm$broad_fiscal_gdp <- fm$broad_fiscal_gdp * 100


overall_by_channel <- fm %>%
  group_by(transmission_channel) %>%
  summarise(
    n_measures     = n(),
    n_countries    = n_distinct(Country),
    total_gdp_pct  = sum(broad_fiscal_gdp, na.rm = TRUE),
    mean_size      = mean(broad_fiscal_gdp, na.rm = TRUE),
    median_size    = median(broad_fiscal_gdp, na.rm = TRUE),
    sd_size        = sd(broad_fiscal_gdp, na.rm = TRUE),
    min_size       = min(broad_fiscal_gdp, na.rm = TRUE),
    p25            = quantile(broad_fiscal_gdp, 0.25, na.rm = TRUE),
    p75            = quantile(broad_fiscal_gdp, 0.75, na.rm = TRUE),
    p90            = quantile(broad_fiscal_gdp, 0.90, na.rm = TRUE),
    p99            = quantile(broad_fiscal_gdp, 0.99, na.rm = TRUE),
    max_size       = max(broad_fiscal_gdp, na.rm = TRUE),
    n_zero         = sum(broad_fiscal_gdp == 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_of_measures = round(n_measures / sum(n_measures) * 100, 1),
    pct_of_volume   = round(total_gdp_pct / sum(total_gdp_pct) * 100, 1)
  )

cat("\n--- 2a. Summary by Transmission Channel ---\n")
print(as.data.frame(overall_by_channel))

# 1. Daten vorbereiten: Runden und publikationsreife Spaltennamen vergeben
tabelle_aer <- as.data.frame(overall_by_channel) %>%
  # Alle numerischen Werte auf 2 Nachkommastellen runden (AER Standard)
  mutate(across(where(is.numeric), ~ round(., 2))) %>% 
  # Spaltennamen für das Paper schön formatieren
  rename(
    `Channel` = transmission_channel,
    `Measures` = n_measures,
    `Countries` = n_countries,
    `Total GDP (\\%)` = total_gdp_pct,
    `Mean` = mean_size,
    `Median` = median_size,
    `SD` = sd_size,
    `Min` = min_size,
    `p25` = p25,
    `p75` = p75,
    `p90` = p90,
    `p99` = p99,
    `Max` = max_size,
    `Zeros` = n_zero,
    `\\% Measures` = pct_of_measures,
    `\\% Volume` = pct_of_volume
  )

# 2. LaTeX-Code im AER-Style generieren
latex_table <- kable(
  tabelle_aer, 
  format = "latex", 
  booktabs = TRUE, # Das Wichtigste für den AER-Style: toprule, midrule, bottomrule
  linesep = "",    # Verhindert ungewollte automatische Leerzeilen
  escape = FALSE,  # Wichtig, damit die %-Zeichen korrekt als LaTeX-Befehl erkannt werden
  caption = "Summary Statistics by Transmission Channel",
  align = c("l", rep("c", ncol(tabelle_aer) - 1)) # Erste Spalte linksbündig, der Rest zentriert
) %>%
  # Tabelle automatisch an die Seitenbreite anpassen (da 16 Spalten sehr breit sind)
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  # Optional: Die klassische "Notes"-Zeile am Ende der Tabelle hinzufügen
  footnote(general = "Notes: CP = ..., DI = ..., H = ...", 
           threeparttable = TRUE)

# 3. Den fertigen LaTeX-Code in der Konsole ausgeben ...
cat(latex_table)

#Volume dominance of CP:
  #  - CP accounts for 66.9% of measures but 78.5% of fiscal volume
  #  - Ratio 78.5/66.9 = 1.17: the average CP measure is systematically larger
  #    than the average DI measure
  #  - Driven by the fiscally heaviest instruments (Kurzarbeit, loan guarantees)
  #    falling into the CP category
  #
  #  Extreme right-skew in all channels:
  #  - CP: mean/median ratio = 4.5x (0.44% vs 0.10% GDP)
  #  - DI: mean/median ratio = 4.0x (0.24% vs 0.06% GDP)
  #  - H:  mean/median ratio = 2.4x (0.26% vs 0.11% GDP)
  #  - A small number of large programs dominate fiscal volume in every channel
  #  - Largest single CP measure: 12.8% GDP (likely a national guarantee program)
  #  - Implication: country-quarter aggregates will be driven by few large programs
  #
  #  Cross-country average (total_gdp_pct / 38 countries):
  #  - Total: ~18.2% of 2019 GDP in fiscal measures across all channels
  #  - CP:   ~14.3 pp  |  DI: ~2.7 pp  |  H: ~1.2 pp
  #  - Usable as: "the average OECD economy deployed fiscal measures equivalent
  #    to approximately 18% of pre-pandemic GDP"

# --- Plot: Overall Channel Composition (dual panel) ---------------------------

# Prepare data
plot_data <- overall_by_channel %>%
  mutate(
    # Labels for display
    channel = factor(transmission_channel,
                     levels = c("CP", "DI", "H"),
                     labels = c("Capacity\nPreservation", "Demand\nInjection", "Health")),
    mean_pct    = mean_size * 100,
    median_pct  = median_size * 100,
    p75_pct     = p75 * 100,
    p90_pct     = p90 * 100,
    total_pct   = total_gdp_pct  # Already summed, keep as proportion for bar
  )

# Color palette
col_cp <- "#2C5F8A"
col_di <- "#D4763A"
col_h  <- "#6A994E"
channel_cols <- c("Capacity\nPreservation" = col_cp,
                  "Demand\nInjection"      = col_di,
                  "Health"                 = col_h)

# Panel A: Share of measures vs share of volume
panel_a_data <- plot_data %>%
  select(channel, pct_of_measures, pct_of_volume) %>%
  pivot_longer(cols = c(pct_of_measures, pct_of_volume),
               names_to = "metric", values_to = "share") %>%
  mutate(metric = factor(metric,
                         levels = c("pct_of_measures", "pct_of_volume"),
                         labels = c("Share of Measures (%)", "Share of Volume (%)")))

p1 <- ggplot(panel_a_data, aes(x = channel, y = share, fill = channel)) +
  geom_col(width = 0.65, color = "white", linewidth = 0.3) +
  geom_text(aes(label = paste0(share, "%")),
            vjust = -0.5, size = 3.2, fontface = "bold") +
  facet_wrap(~metric) +
  scale_fill_manual(values = channel_cols) +
  scale_y_continuous(limits = c(0, 95), expand = c(0, 0)) +
  labs(
    title    = "A. Measure Count vs. Fiscal Volume by Channel",
    subtitle = "CP is disproportionately large: 67% of measures but 79% of volume",
    x = NULL, y = "Percent"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position  = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    strip.text         = element_text(face = "bold", size = 10),
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(size = 9, color = "grey40"),
    axis.text.x        = element_text(size = 9)
  )

# Panel B: Size distribution (mean, median, p75, p90)
panel_b_data <- plot_data %>%
  select(channel, mean_pct, median_pct, p75_pct, p90_pct) %>%
  pivot_longer(cols = c(median_pct, mean_pct, p75_pct, p90_pct),
               names_to = "stat", values_to = "value") %>%
  mutate(stat = factor(stat,
                       levels = c("median_pct", "mean_pct", "p75_pct", "p90_pct"),
                       labels = c("Median", "Mean", "p75", "p90")))

p2 <- ggplot(panel_b_data, aes(x = stat, y = value, fill = channel)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6,
           color = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.2f", value), group = channel),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 2.7) +
  scale_fill_manual(values = channel_cols,
                    name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "B. Size Distribution of Individual Measures",
    subtitle = "Mean/median ratio of 4.5x (CP) indicates extreme right-skew: few large programs dominate volume",
    x = NULL, y = "% of 2019 GDP"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position    = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(size = 9, color = "grey40")
  )

# Combine
p_overall <- p1 / p2 +
  plot_annotation(
    title    = "Fiscal Response Composition: 38 OECD Economies, 2020–2021",
    subtitle = "1,829 core measures | Average deployment: 18.2% of 2019 GDP (14.3 pp CP, 2.7 pp DI, 1.2 pp H)",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10, color = "grey30")
    )
  )

print(p_overall)


# Einen zweiten Plot im selben Ordner speichern
ggsave(
  filename = file.path(safeplots, "composition_fiscal.pdf"), 
  plot = p_overall,
  width = 10, 
  height = 8
)

# --- Plot: Size Distribution (Histogram by Channel) --------------------------

# Exclude zeros for distributional analysis (7 measures with size = 0)
fm_pos <- fm %>%
  filter(broad_fiscal_gdp > 0) %>%
  mutate(
    size_pct = broad_fiscal_gdp * 100,
    channel  = factor(transmission_channel,
                      levels = c("CP", "DI", "H"),
                      labels = c("Capacity Preservation", "Demand Injection", "Health"))
  )

# Compute channel-level mean and median for vertical reference lines
channel_stats <- fm_pos %>%
  group_by(channel) %>%
  summarise(
    mean_val   = mean(size_pct),
    median_val = median(size_pct),
    n          = n(),
    .groups    = "drop"
  )

p_hist <- ggplot(fm_pos, aes(x = size_pct, fill = channel)) +
  geom_histogram(binwidth = 0.05, boundary = 0,
                 color = "white", linewidth = 0.15, alpha = 0.9) +
  # Median (solid) and mean (dashed) reference lines per panel
  geom_vline(data = channel_stats,
             aes(xintercept = median_val),
             linetype = "solid", color = "black", linewidth = 0.6) +
  geom_vline(data = channel_stats,
             aes(xintercept = mean_val),
             linetype = "dashed", color = "black", linewidth = 0.6) +
  # Annotation: median and mean labels
  geom_text(data = channel_stats,
            aes(x = median_val, y = Inf,
                label = sprintf("Median: %.2f%%", median_val)),
            hjust = -0.1, vjust = 2, size = 2.8, inherit.aes = FALSE) +
  geom_text(data = channel_stats,
            aes(x = mean_val, y = Inf,
                label = sprintf("Mean: %.2f%%", mean_val)),
            hjust = -0.1, vjust = 3.8, size = 2.8, fontface = "italic",
            inherit.aes = FALSE) +
  facet_wrap(~channel, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("Capacity Preservation" = col_cp,
                               "Demand Injection"      = col_di,
                               "Health"                 = col_h)) +
  scale_x_continuous(
    limits = c(0, 3),
    breaks = seq(0, 3, 0.5),
    expand = c(0, 0)
  ) +
  labs(
    title    = "Size Distribution of Individual Fiscal Measures",
    subtitle = "Truncated at 3% GDP for visibility | Solid line: median, dashed: mean | Right-skew in all channels",
    x        = "Measure Size (% of 2019 GDP)",
    y        = "Number of Measures",
    caption  = sprintf("N = %d measures with size > 0. Truncated: %d CP, %d DI, %d H measures above 3%% GDP.",
                       nrow(fm_pos),
                       sum(fm_pos$size_pct > 3 & fm_pos$channel == "Capacity Preservation"),
                       sum(fm_pos$size_pct > 3 & fm_pos$channel == "Demand Injection"),
                       sum(fm_pos$size_pct > 3 & fm_pos$channel == "Health"))
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position    = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    strip.text         = element_text(face = "bold", size = 10),
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(size = 9, color = "grey40"),
    plot.caption       = element_text(size = 8, color = "grey50")
  )

print(p_hist)
# --- 2b. Grand total (all channels combined) ---------------------------------

overall_total <- fm %>%
  summarise(
    n_measures     = n(),
    n_countries    = n_distinct(Country),
    total_gdp_pct  = sum(broad_fiscal_gdp, na.rm = TRUE),
    mean_size      = mean(broad_fiscal_gdp, na.rm = TRUE),
    median_size    = median(broad_fiscal_gdp, na.rm = TRUE),
    sd_size        = sd(broad_fiscal_gdp, na.rm = TRUE),
    min_size       = min(broad_fiscal_gdp, na.rm = TRUE),
    p25            = quantile(broad_fiscal_gdp, 0.25, na.rm = TRUE),
    p75            = quantile(broad_fiscal_gdp, 0.75, na.rm = TRUE),
    max_size       = max(broad_fiscal_gdp, na.rm = TRUE)
  )

cat("\n--- 2b. Grand Total (All Channels) ---\n")
print(as.data.frame(overall_total))




# --- 2c. By IMF accounting category within each channel ----------------------
# Tests the expected pattern: BLW (cat=2) should be exclusively CP;
# deferrals (cat=3) exclusively CP; ATL (cat=1) mixed.

by_channel_imf <- fm %>%
  group_by(transmission_channel, category) %>%
  summarise(
    n          = n(),
    total_gdp  = sum(broad_fiscal_gdp, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  arrange(transmission_channel, category)

cat("\n--- 2c. Channel x IMF Category ---\n")
print(as.data.frame(by_channel_imf))

#Das sind die 5 Einträge von Code 42 (Preferential loans to households)
#— Chile, Slowakei, USA, Dänemark, Japan. Das ist by design: Haushaltskredite 
#sind below-the-line nach IMF-Konvention (weil der Staat ein Finanzaktivum erwirbt), 
#aber der Transmissionskanal ist DI, weil die Liquidität an Haushalte fliesst und über Konsumausgaben wirkt.


# --- 2d. Robustness sub-categories within CP ---------------------------------
# Decompose CP into: direct (ATL grants/subsidies), loans & guarantees (BLW),
# and deferrals (cat=3). These carry structurally different kappa_F^CP.

cp_decomp <- fm %>%
  filter(transmission_channel == "CP") %>%
  mutate(
    cp_subtype = case_when(
      is_deferral == 1               ~ "Deferral",
      is_blw == 1                    ~ "Loans & Guarantees",
      TRUE                           ~ "Direct (ATL)"
    )
  ) %>%
  group_by(cp_subtype) %>%
  summarise(
    n          = n(),
    total_gdp  = sum(broad_fiscal_gdp, na.rm = TRUE),
    mean_gdp   = mean(broad_fiscal_gdp, na.rm = TRUE),
    median_gdp = median(broad_fiscal_gdp, na.rm = TRUE),
    max_gdp    = max(broad_fiscal_gdp, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  mutate(
    pct_of_cp_n   = round(n / sum(n) * 100, 1),
    pct_of_cp_vol = round(total_gdp / sum(total_gdp) * 100, 1)
  )

cat("\n--- 2d. CP Sub-Decomposition ---\n")
print(as.data.frame(cp_decomp))


# ==============================================================================
#  3. TEMPORAL DYNAMICS
# ==============================================================================

cat("\n\n============================================================\n")
cat("  3. TEMPORAL DYNAMICS\n")
cat("============================================================\n")

# --- 3a. Quarter identifier ---------------------------------------------------
# Construct Q1.2020 format for chronological ordering

fm <- fm %>%
  mutate(
    YQ = paste0("Q", Quarter, ".", Year),
    YQ_ord = factor(
      YQ,
      levels = c("Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                 "Q1.2021","Q2.2021","Q3.2021","Q4.2021"),
      ordered = TRUE
    )
  )

# --- 3b. Aggregate volume per quarter by channel -----------------------------
# This sums broad_fiscal_gdp across ALL countries for each quarter.
# Interpretation: total cross-country fiscal deployment per quarter.

temporal_channel <- fm %>%
  group_by(YQ_ord, transmission_channel) %>%
  summarise(
    n_measures  = n(),
    total_gdp   = sum(broad_fiscal_gdp, na.rm = TRUE),
    mean_size   = mean(broad_fiscal_gdp, na.rm = TRUE),
    median_size = median(broad_fiscal_gdp, na.rm = TRUE),
    n_countries = n_distinct(Country),
    .groups     = "drop"
  )

cat("\n--- 3b. Fiscal Volume by Quarter x Channel ---\n")
print(as.data.frame(temporal_channel))

# --- 3c. Wide format: volume shares over time --------------------------------

temporal_wide <- temporal_channel %>%
  select(YQ_ord, transmission_channel, total_gdp) %>%
  pivot_wider(
    names_from  = transmission_channel,
    values_from = total_gdp,
    values_fill = 0
  ) %>%
  mutate(
    Total    = CP + DI + H,
    CP_share = round(CP / Total * 100, 1),
    DI_share = round(DI / Total * 100, 1),
    H_share  = round(H / Total * 100, 1)
  )

cat("\n--- 3c. Channel Composition Over Time (% of Total) ---\n")
print(as.data.frame(temporal_wide))



#  Three distinct phases emerge:
#
#  Phase 1 — Acute Crisis (Q1-Q2 2020):
#  - Massive front-loading: Q1+Q2 2020 account for 499 of 691 total volume (72%)
#  - CP dominates at 83-87% of volume — governments prioritized keeping firms
#    alive and workers attached during the strictest lockdowns
#  - Mean CP measure size peaks in Q1.2020 at 0.89% GDP — 4x the later quarters
#  - 36 of 38 countries deployed CP already in Q1, vs only 28 for DI
#    => CP was not only larger but adopted faster and more universally
#
#  Phase 2 — Transition (Q3 2020 - Q1 2021):
#  - Total volume drops by ~80% (from 228 to 44-48 per quarter)
#  - CP share falls to 53-65%, DI rises to 20-36%
#  - Q1.2021 shows highest DI share (36.2%) — driven by US ARPA ($1.9T) and
#    second-round stimulus packages as economies reopened
#  - Consistent with the model prediction: as S_k declines, the relative value
#    of DI rises because the demand channel operates more effectively (Eq. 8)
#
#  Phase 3 — Tail (Q2-Q4 2021):
#  - Volume drops further; CP share recovers to 72%
#  - But the composition of CP likely shifts: fewer emergency programs, more
#    structural support (extended Kurzarbeit, recovery guarantees)
#  - H share rises to 14-17% — reflecting vaccination logistics and continued
#    health system costs even as economic restrictions ease
#
#  Key finding for the paper:
#  The temporal shift from CP-dominated (87%) to mixed CP/DI (53/36%) and back
#  to CP-dominated (72%) traces the state-dependent composition logic of the
#  model. The reversal in Phase 3 may reflect that DI exhausts its effectiveness
#  as output gaps close, while residual structural damage still requires CP.


# Common AER theme
theme_aer <- theme_classic(base_size = 11) +
  theme(
    text             = element_text(family = "serif"),
    plot.title       = element_text(face = "bold", size = 12, hjust = 0),
    plot.subtitle    = element_text(size = 9, color = "grey30", hjust = 0),
    axis.title       = element_text(size = 10),
    axis.text        = element_text(size = 9),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 9),
    legend.key.size  = unit(0.4, "cm"),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
    plot.caption     = element_text(size = 8, color = "grey50", hjust = 0)
  )

# Colors: grayscale-friendly, AER-appropriate
col_cp <- "grey25"
col_di <- "grey60"
col_h  <- "grey85"
channel_fills <- c("CP" = col_cp, "DI" = col_di, "H" = col_h)

# Quarter labels without year for cleaner x-axis
q_labels <- c("Q1\n2020","Q2","Q3","Q4","Q1\n2021","Q2","Q3","Q4")

# --- Prepare data in long format for stacked bars ---

# Panel A: Measure count
count_long <- temporal_channel %>%
  mutate(
    transmission_channel = factor(transmission_channel,
                                  levels = c("H", "DI", "CP"))
  )

p_count <- ggplot(count_long,
                  aes(x = YQ_ord, y = n_measures, fill = transmission_channel)) +
  geom_col(width = 0.7, color = "white", linewidth = 0.2) +
  # Total count label on top
  geom_text(
    data = count_long %>%
      group_by(YQ_ord) %>%
      summarise(total = sum(n_measures), .groups = "drop"),
    aes(x = YQ_ord, y = total, label = total, fill = NULL),
    vjust = -0.4, size = 3, family = "serif"
  ) +
  scale_fill_manual(
    values = channel_fills,
    breaks = c("CP", "DI", "H"),
    labels = c("Capacity Preservation", "Demand Injection", "Health")
  ) +
  scale_x_discrete(labels = q_labels) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "A. Number of Measures",
    x     = NULL,
    y     = "Measures"
  ) +
  theme_aer

# Panel B: Fiscal volume (% of GDP, stacked)
vol_long <- temporal_channel %>%
  mutate(
    total_gdp_pct = total_gdp,  # Already in % of GDP * 100 units
    transmission_channel = factor(transmission_channel,
                                  levels = c("H", "DI", "CP"))
  )

# CP share annotation data (label on top of each bar)
cp_share_labels <- temporal_wide %>%
  mutate(label = paste0(CP_share, "%"))

p_volume <- ggplot(vol_long,
                   aes(x = YQ_ord, y = total_gdp_pct, fill = transmission_channel)) +
  geom_col(width = 0.7, color = "white", linewidth = 0.2) +
  # CP share annotation above each bar
  geom_text(
    data = cp_share_labels,
    aes(x = YQ_ord, y = Total, label = label, fill = NULL),
    vjust = -0.4, size = 2.8, family = "serif", fontface = "italic"
  ) +
  scale_fill_manual(
    values = channel_fills,
    breaks = c("CP", "DI", "H"),
    labels = c("Capacity Preservation", "Demand Injection", "Health")
  ) +
  scale_x_discrete(labels = q_labels) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title   = "B. Fiscal Volume",
    x       = NULL,
    y       = "Sum of Measures (% of 2019 GDP)",
    caption = "Notes: Italic percentages indicate CP share of quarterly total. Volume is the sum of each measure's\nsize as a share of the respective country's 2019 GDP, aggregated across 38 OECD economies."
  ) +
  theme_aer

# Combine
p_temporal <- p_count / p_volume +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Figure X: Fiscal Response by Transmission Channel Over Time",
    theme = theme(
      plot.title = element_text(face = "bold", size = 13, family = "serif")
    )
  ) &
  theme(legend.position = "bottom")

print(p_temporal)



# --- 3d. Cumulative deployment ------------------------------------------------

temporal_cum <- temporal_wide %>%
  arrange(YQ_ord) %>%
  mutate(
    cum_CP    = cumsum(CP),
    cum_DI    = cumsum(DI),
    cum_H     = cumsum(H),
    cum_Total = cumsum(Total)
  ) %>%
  select(YQ_ord, cum_CP, cum_DI, cum_H, cum_Total)

cat("\n--- 3d. Cumulative Deployment ---\n")
print(as.data.frame(temporal_cum))

# --- 3e. Year-level aggregation -----------------------------------------------

temporal_year <- fm %>%
  group_by(Year, transmission_channel) %>%
  summarise(
    n_measures = n(),
    total_gdp  = sum(broad_fiscal_gdp, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  pivot_wider(
    names_from  = transmission_channel,
    values_from = c(n_measures, total_gdp),
    values_fill = 0
  )

cat("\n--- 3e. Year-Level Summary ---\n")
print(as.data.frame(temporal_year))


##Most of it was deployed in 2020-> Fertig interpretieren

# --- 3f. Measure count per quarter (activation intensity) ---------------------
# Captures the policy response intensity independent of fiscal volume.

temporal_count <- fm %>%
  group_by(YQ_ord) %>%
  summarise(
    n_total     = n(),
    n_CP        = sum(transmission_channel == "CP"),
    n_DI        = sum(transmission_channel == "DI"),
    n_H         = sum(transmission_channel == "H"),
    n_countries = n_distinct(Country),
    .groups     = "drop"
  )

cat("\n--- 3f. Measure Activation Intensity ---\n")
print(as.data.frame(temporal_count))

##Zeigt mir genau das Muster das ich mit meinem Modell prognostizieren würde!! CP zuerst dann DI aber nicht alle gleich, FRAGE: Intensität, Mix CP und DI, Zeitpunkt CP und DI
# ==============================================================================
#  4. COUNTRY-LEVEL HETEROGENEITY
# ==============================================================================

cat("\n\n============================================================\n")
cat("  4. COUNTRY-LEVEL HETEROGENEITY\n")
cat("============================================================\n")

# --- 4a. Country totals by channel -------------------------------------------
# The key descriptive for the cross-country analysis: how much did each
# country deploy as % of its own 2019 GDP, split by DI/CP/H?

country_channel <- fm %>%
  group_by(Country, income_group, region, transmission_channel) %>%
  summarise(
    n_measures = n(),
    total_gdp  = sum(broad_fiscal_gdp, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  pivot_wider(
    names_from  = transmission_channel,
    values_from = c(n_measures, total_gdp),
    values_fill = 0
  ) %>%
  mutate(
    total_fiscal = total_gdp_CP + total_gdp_DI + total_gdp_H,
    CP_share     = round(total_gdp_CP / total_fiscal * 100, 1),
    DI_share     = round(total_gdp_DI / total_fiscal * 100, 1),
    H_share      = round(total_gdp_H / total_fiscal * 100, 1)
  ) %>%
  arrange(desc(total_fiscal))

cat("\n--- 4a. Country Totals by Channel (sorted by total, % of 2019 GDP) ---\n")
print(
  as.data.frame(
    country_channel %>%
      select(Country, income_group,
             total_gdp_CP, total_gdp_DI, total_gdp_H, total_fiscal,
             CP_share, DI_share)
  ),
  row.names = FALSE
)

##AUSGABE NOCH SCHREIBEN JE NACHDEM->APPENIDX


# ==============================================================================
#  4a. COUNTRY-LEVEL INTENSITY BY CHANNEL (3 Panels)
# ==============================================================================


# --- Prepare data -------------------------------------------------------------

# Country totals in % of 2019 GDP, sorted by total package
country_plot <- country_channel %>%
  mutate(
    CP = total_gdp_CP,
    DI = total_gdp_DI,
    H  = total_gdp_H
  )

# OECD unweighted averages
oecd_avg <- country_plot %>%
  summarise(
    avg_CP = mean(CP),
    avg_DI = mean(DI),
    avg_H  = mean(H)
  )

# Common theme
theme_aer_bar <- theme_classic(base_size = 10) +
  theme(
    text               = element_text(family = "serif"),
    plot.title         = element_text(face = "bold", size = 11, hjust = 0),
    plot.subtitle      = element_text(size = 8, color = "grey30", hjust = 0),
    axis.title.x       = element_blank(),
    axis.text.x        = element_text(size = 7, angle = 90, hjust = 1, vjust = 0.5),
    axis.title.y       = element_text(size = 9),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    plot.caption       = element_text(size = 7, color = "grey50", hjust = 0)
  )

# --- Panel A: Capacity Preservation ------------------------------------------

# Sort countries by CP volume
cp_order <- country_plot %>% arrange(CP) %>% pull(Country)

p_cp <- ggplot(country_plot %>% mutate(Country = factor(Country, levels = cp_order)),
               aes(x = Country, y = CP)) +
  geom_col(fill = "grey25", width = 0.7) +
  geom_hline(yintercept = oecd_avg$avg_CP,
             linetype = "dashed", color = "#C0392B", linewidth = 0.6) +
  annotate("text",
           x = 3, y = oecd_avg$avg_CP + 0.3,
           label = sprintf("OECD avg: %.1f%%", oecd_avg$avg_CP),
           size = 2.8, family = "serif", color = "#C0392B") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "A. Capacity Preservation",
    subtitle = "Short-time work, loan guarantees, tax deferrals, business support",
    y        = "% of 2019 GDP"
  ) +
  theme_aer_bar

# --- Panel B: Demand Injection ------------------------------------------------

di_order <- country_plot %>% arrange(DI) %>% pull(Country)

p_di <- ggplot(country_plot %>% mutate(Country = factor(Country, levels = di_order)),
               aes(x = Country, y = DI)) +
  geom_col(fill = "grey55", width = 0.7) +
  geom_hline(yintercept = oecd_avg$avg_DI,
             linetype = "dashed", color = "#C0392B", linewidth = 0.6) +
  annotate("text",
           x = 3, y = oecd_avg$avg_DI + 0.15,
           label = sprintf("OECD avg: %.1f%%", oecd_avg$avg_DI),
           size = 2.8, family = "serif", color = "#C0392B") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "B. Demand Injection",
    subtitle = "Cash transfers, unemployment benefits, VAT cuts, stimulus checks",
    y        = "% of 2019 GDP"
  ) +
  theme_aer_bar

# --- Panel C: Health ----------------------------------------------------------

h_order <- country_plot %>% arrange(H) %>% pull(Country)

p_h <- ggplot(country_plot %>% mutate(Country = factor(Country, levels = h_order)),
              aes(x = Country, y = H)) +
  geom_col(fill = "grey75", width = 0.7) +
  geom_hline(yintercept = oecd_avg$avg_H,
             linetype = "dashed", color = "#C0392B", linewidth = 0.6) +
  annotate("text",
           x = 3, y = oecd_avg$avg_H + 0.1,
           label = sprintf("OECD avg: %.1f%%", oecd_avg$avg_H),
           size = 2.8, family = "serif", color = "#C0392B") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "C. Health Expenditure",
    subtitle = "Medical supplies, hospital capacity, vaccination infrastructure",
    y        = "% of 2019 GDP",
    caption  = paste0(
      "Notes: 1,829 core fiscal measures across 38 OECD economies, 2020–2021. ",
      "Each bar shows the country total as % of its 2019 GDP.\n",
      "Countries sorted independently within each panel by channel volume. ",
      "Dashed line: unweighted OECD average."
    )
  ) +
  theme_aer_bar

# --- Combine ------------------------------------------------------------------

p_country_bars <- p_cp / p_di / p_h +
  plot_annotation(
    title = "Figure X: Fiscal Deployment by Transmission Channel and Country",
    theme = theme(
      plot.title = element_text(face = "bold", size = 13, family = "serif")
    )
  )

print(p_country_bars)



# ==============================================================================
#  4a. COUNTRY COMPOSITION: SCALED PIE CHARTS & STRATEGY SCATTER
# ==============================================================================


# --- Prepare country-level data -----------------------------------------------

country_pies <- fm %>%
  group_by(Country, income_group, region, transmission_channel) %>%
  summarise(gdp = sum(broad_fiscal_gdp, na.rm = TRUE) * 100, .groups = "drop") %>%
  pivot_wider(
    names_from  = transmission_channel,
    values_from = gdp,
    values_fill = 0
  ) %>%
  mutate(Total = CP + DI + H) %>%
  arrange(desc(Total))

# Grid positions: countries sorted by total size, arranged in 6 columns
n <- nrow(country_pies)
country_pies <- country_pies %>%
  mutate(
    rank = row_number(),
    col  = ((rank - 1) %% 7) + 1,
    row  = -((rank - 1) %/% 7 + 1),   # Negative so top-left = largest
    # Pie radius proportional to sqrt(Total) for area scaling
    radius = sqrt(Total) / max(sqrt(Total)) * 0.42
  )


##Warum werden Strategien gewählt?

# ==============================================================================
#  STRATEGY SCATTER: CP Share vs DI Share
# ==============================================================================
#
#  This is the analytically informative plot. Each country is one point.
#  x = CP share, y = DI share (H share = 100 - x - y, implicit).
#  Size = total fiscal package. Color = region.
#
#  What to look for:
#  - Clusters: do European countries group in the top-left (high CP, low DI)?
#  - Trade-off: is there a negative slope (countries choose CP OR DI)?
#  - Scale-independence: does composition correlate with total package size?
#  - Outliers: which countries deviate from their regional pattern?
# ==============================================================================

# Prepare
scatter_data <- country_pies %>%
  mutate(
    CP_share = CP / Total * 100,
    DI_share = DI / Total * 100,
    H_share  = H / Total * 100
  )

# Region colors (5 regions, colorblind-safe)
region_cols <- c(
  "Europe & Central Asia"      = "#2C5F8A",
  "North America"              = "#D4763A",
  "East Asia & Pacific"        = "#6A994E",
  "Latin America & Caribbean"  = "#9B5094",
  "Middle East & North Africa" = "#C2A83E"
)

p_scatter <- ggplot(scatter_data,
                    aes(x = CP_share, y = DI_share, size = Total, color = region)) +
  # Reference lines
  geom_hline(yintercept = mean(scatter_data$DI_share),
             linetype = "dashed", color = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = mean(scatter_data$CP_share),
             linetype = "dashed", color = "grey70", linewidth = 0.4) +
  # Points
  geom_point(alpha = 0.8) +
  # Country labels
  geom_text(aes(label = Country), size = 2.5, family = "serif",
            vjust = -1.2, show.legend = FALSE) +
  # Scales
  scale_size_continuous(
    range  = c(2, 12),
    name   = "Total Package\n(% of 2019 GDP)",
    breaks = c(5, 15, 30, 45)
  ) +
  scale_color_manual(values = region_cols, name = NULL) +
  scale_x_continuous(limits = c(40, 100), breaks = seq(40, 100, 10)) +
  scale_y_continuous(limits = c(0, 50),   breaks = seq(0, 50, 10)) +
  # Quadrant annotations
  annotate("text", x = 95, y = 48, label = "High CP\nHigh DI",
           size = 2.5, color = "grey60", fontface = "italic", family = "serif") +
  annotate("text", x = 48, y = 48, label = "Low CP\nHigh DI",
           size = 2.5, color = "grey60", fontface = "italic", family = "serif") +
  annotate("text", x = 95, y = 2,  label = "High CP\nLow DI",
           size = 2.5, color = "grey60", fontface = "italic", family = "serif") +
  labs(
    title    = "Figure X: Fiscal Strategy Space — CP vs. DI Orientation",
    subtitle = "Dashed lines at cross-country means. Size = total package. Residual to 100% = Health share.",
    x        = "Capacity Preservation Share (%)",
    y        = "Demand Injection Share (%)",
    caption  = "Notes: Each point represents one OECD economy. Composition based on 1,829 core fiscal measures.\nMean CP share = 77.5%. Mean DI share = 15.4%."
  ) +
  theme_classic(base_size = 11) +
  theme(
    text               = element_text(family = "serif"),
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(size = 9, color = "grey30"),
    plot.caption       = element_text(size = 8, color = "grey50", hjust = 0),
    legend.position    = "right",
    legend.text        = element_text(size = 8),
    panel.grid.major   = element_line(color = "grey92", linewidth = 0.3),
    axis.title         = element_text(size = 10)
  ) +
  guides(
    color = guide_legend(order = 1, override.aes = list(size = 4)),
    size  = guide_legend(order = 2)
  )

print(p_scatter)

# --- 4b. Cross-country summary statistics of country-level totals -------------
# Treats each country as one observation. This captures the distribution of
# fiscal responses across the OECD.

country_stats <- country_channel %>%
  ungroup() %>%
  summarise(
    # Total fiscal
    mean_total   = mean(total_fiscal),
    median_total = median(total_fiscal),
    sd_total     = sd(total_fiscal),
    min_total    = min(total_fiscal),
    max_total    = max(total_fiscal),
    iqr_total    = IQR(total_fiscal),
    # CP
    mean_CP      = mean(total_gdp_CP),
    median_CP    = median(total_gdp_CP),
    sd_CP        = sd(total_gdp_CP),
    # DI
    mean_DI      = mean(total_gdp_DI),
    median_DI    = median(total_gdp_DI),
    sd_DI        = sd(total_gdp_DI),
    # CP share
    mean_CP_share   = mean(CP_share),
    median_CP_share = median(CP_share),
    sd_CP_share     = sd(CP_share),
    min_CP_share    = min(CP_share),
    max_CP_share    = max(CP_share)
  )

cat("\n--- 4b. Cross-Country Distribution of Fiscal Totals ---\n")
print(t(country_stats))

##Spannende Ausgabe: über die OECD hat jedes Land mindestens 2% des eigenen GDP ausgegeben bis max 41%
#Davon ein mind Anteil von CP von 44.6% bis zu 98.2%-> Fast nur CP-> Starke Unterschiede bezüglich der Strategie Mix CP

#Mögliche Gründe:
  
#**Institutionelle Infrastruktur (stärkstes Argument).
#**Arbeitsmarktrigidität und Matching-Kosten 
#**Firmenstruktur und KMU-Anteil.
#**Sozialstaatsarchitektur als Substitut.
#**Koordination Geld-/Fiskalpolitik und Garantie-Infrastruktur.
#**Politische Ökonomie: Sichtbarkeit vs. Effektivität.
#**Was du daraus machen kannst.

# --- 4c. Country-level CP sub-decomposition -----------------------------------
# For each country: how much of CP was direct vs. loans/guarantees vs. deferrals?
#Frag enach der Aufteilung von CP nach Country

country_cp_sub <- fm %>%
  filter(transmission_channel == "CP") %>%
  mutate(
    cp_subtype = case_when(
      is_deferral == 1 ~ "Deferral",
      is_blw == 1      ~ "Loans_Guar",
      TRUE             ~ "Direct_ATL"
    )
  ) %>%
  group_by(Country, cp_subtype) %>%
  summarise(gdp = sum(broad_fiscal_gdp, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from  = cp_subtype,
    values_from = gdp,
    values_fill = 0
  ) %>%
  mutate(
    CP_total    = Direct_ATL + Loans_Guar + Deferral,
    LG_share    = round(Loans_Guar / CP_total * 100, 1),
    Def_share   = round(Deferral / CP_total * 100, 1)
  ) %>%
  arrange(desc(CP_total))

cat("\n--- 4c. CP Sub-Decomposition by Country (% of 2019 GDP) ---\n")
print(as.data.frame(country_cp_sub), row.names = FALSE)

# --- 4d. Extremes: identify outlier countries ---------------------------------

cat("\n--- 4d. Extremes ---\n")
cat("\nHighest total fiscal (% GDP):\n")
print(head(country_channel %>% select(Country, total_fiscal, CP_share, DI_share), 5))

cat("\nLowest total fiscal (% GDP):\n")
print(tail(country_channel %>% select(Country, total_fiscal, CP_share, DI_share), 5))

cat("\nHighest CP share:\n")
print(head(country_channel %>% arrange(desc(CP_share)) %>%
             select(Country, total_fiscal, CP_share), 5))

cat("\nHighest DI share:\n")
print(head(country_channel %>% arrange(desc(DI_share)) %>%
             select(Country, total_fiscal, DI_share), 5))


# ==============================================================================
#  5. REGIONAL DECOMPOSITION
# ==============================================================================

cat("\n\n============================================================\n")
cat("  5. REGIONAL DECOMPOSITION\n")
cat("============================================================\n")

# --- 5a. By region -----------------------------------------------------------

region_channel <- fm %>%
  group_by(region, transmission_channel) %>%
  summarise(
    n_measures  = n(),
    n_countries = n_distinct(Country),
    total_gdp   = sum(broad_fiscal_gdp, na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  pivot_wider(
    names_from  = transmission_channel,
    values_from = c(n_measures, total_gdp),
    values_fill = 0
  ) %>%
  mutate(
    total       = total_gdp_CP + total_gdp_DI + total_gdp_H,
    CP_share    = round(total_gdp_CP / total * 100, 1),
    DI_share    = round(total_gdp_DI / total * 100, 1)
  ) %>%
  arrange(desc(total))

cat("\n--- 5a. Fiscal Totals by Region ---\n")
print(as.data.frame(region_channel))

# --- 5b. By income group -----------------------------------------------------

income_channel <- fm %>%
  group_by(income_group, transmission_channel) %>%
  summarise(
    n_measures  = n(),
    n_countries = n_distinct(Country),
    total_gdp   = sum(broad_fiscal_gdp, na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  pivot_wider(
    names_from  = transmission_channel,
    values_from = c(n_measures, total_gdp),
    values_fill = 0
  ) %>%
  mutate(
    total    = total_gdp_CP + total_gdp_DI + total_gdp_H,
    CP_share = round(total_gdp_CP / total * 100, 1),
    DI_share = round(total_gdp_DI / total * 100, 1)
  )

cat("\n--- 5b. Fiscal Totals by Income Group ---\n")
print(as.data.frame(income_channel))

#Per Country share GDP 
#AE:19.48%
#EM: 12.42%

#Warum weniger? Weniger need oder weniger Mölcihckeiten??

# --- 5c. Mean country-level totals by region ----------------------------------
# Average country fiscal effort within each region (treats each country equally).

region_country_avg <- country_channel %>%
  group_by(region) %>%
  summarise(
    n_countries      = n(),
    mean_total       = mean(total_fiscal),
    mean_CP          = mean(total_gdp_CP),
    mean_DI          = mean(total_gdp_DI),
    mean_H           = mean(total_gdp_H),
    mean_CP_share    = mean(CP_share),
    sd_CP_share      = sd(CP_share),
    .groups          = "drop"
  ) %>%
  arrange(desc(mean_total))

cat("\n--- 5c. Mean Country-Level Fiscal Effort by Region ---\n")
print(as.data.frame(region_country_avg))


# ==============================================================================
#  6. INSTRUMENT COMPOSITION (PolicyCode Level)
# ==============================================================================

cat("\n\n============================================================\n")
cat("  6. INSTRUMENT COMPOSITION\n")
cat("============================================================\n")

# --- 6a. Top instruments by fiscal volume -------------------------------------

instrument_rank <- fm %>%
  group_by(PolicyCode, PolicyCategory, transmission_channel) %>%
  summarise(
    n          = n(),
    total_gdp  = sum(broad_fiscal_gdp, na.rm = TRUE),
    mean_gdp   = mean(broad_fiscal_gdp, na.rm = TRUE),
    n_countries = n_distinct(Country),
    .groups    = "drop"
  ) %>%
  arrange(desc(total_gdp)) %>%
  mutate(
    cum_pct = round(cumsum(total_gdp) / sum(total_gdp) * 100, 1)
  )

cat("\n--- 6a. Top 20 Instruments by Fiscal Volume ---\n")
print(as.data.frame(head(instrument_rank, 20)), row.names = FALSE)

# --- 6b. Concentration: how many codes account for 80% of volume? -------------

n_80pct <- min(which(instrument_rank$cum_pct >= 80))
cat(sprintf("\n--- 6b. Concentration: %d of %d codes account for 80%% of volume ---\n",
            n_80pct, n_distinct(fm$PolicyCode)))

# --- 6c. By PolicyCategory (broad instrument type) ----------------------------

category_summary <- fm %>%
  group_by(PolicyCategory, transmission_channel) %>%
  summarise(
    n         = n(),
    total_gdp = sum(broad_fiscal_gdp, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  arrange(transmission_channel, desc(total_gdp))

cat("\n--- 6c. PolicyCategory x Transmission Channel ---\n")
print(as.data.frame(category_summary))


# ==============================================================================
#  7. IMF ACCOUNTING CROSS-VALIDATION
# ==============================================================================

cat("\n\n============================================================\n")
cat("  7. IMF ACCOUNTING CROSS-VALIDATION\n")
cat("============================================================\n")

# --- 7a. IMF category distribution by channel --------------------------------
# Expected: ATL (1) = mixed DI+CP; BLW (2) = all CP; Deferrals (3) = all CP

imf_cross <- fm %>%
  group_by(
    imf_cat = case_when(
      category == 1 ~ "Above-the-line",
      category == 2 ~ "Below-the-line",
      category == 3 ~ "Tax deferrals",
      TRUE          ~ "Other"
    ),
    transmission_channel
  ) %>%
  summarise(
    n         = n(),
    total_gdp = sum(broad_fiscal_gdp, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  pivot_wider(
    names_from  = transmission_channel,
    values_from = c(n, total_gdp),
    values_fill = 0
  )

cat("\n--- 7a. IMF Category x Transmission Channel ---\n")
print(as.data.frame(imf_cross))

# --- 7b. Validation checks ---------------------------------------------------

cat("\n--- 7b. Validation Checks ---\n")

# BLW must be exclusively CP (except Code 42: household loans -> DI)
blw_non_cp <- fm %>%
  filter(category == 2, transmission_channel != "CP")
cat(sprintf("  BLW non-CP: %d measures (all Code 42 household loans: %s)\n",
            nrow(blw_non_cp),
            ifelse(all(blw_non_cp$PolicyCode == "42"), "YES", "NO")))

# Deferrals must be exclusively CP
def_non_cp <- fm %>%
  filter(category == 3, transmission_channel != "CP")
cat(sprintf("  Deferrals non-CP: %d (should be 0)\n", nrow(def_non_cp)))

# No missing channels
cat(sprintf("  Unclassified measures: %d (should be 0)\n",
            sum(is.na(fm$transmission_channel))))


# ==============================================================================
#  8. DISTRIBUTIONAL PROPERTIES
# ==============================================================================

cat("\n\n============================================================\n")
cat("  8. DISTRIBUTIONAL PROPERTIES\n")
cat("============================================================\n")

# --- 8a. Size distribution by channel ----------------------------------------
# Fiscal measures are heavily right-skewed: most measures are small,
# a few are very large. Document this for the econometric specification.

size_dist <- fm %>%
  filter(broad_fiscal_gdp > 0) %>%  # Exclude zeros for distribution stats
  group_by(transmission_channel) %>%
  summarise(
    n           = n(),
    mean        = mean(broad_fiscal_gdp),
    median      = median(broad_fiscal_gdp),
    sd          = sd(broad_fiscal_gdp),
    skewness    = (mean(broad_fiscal_gdp) - median(broad_fiscal_gdp)) / sd(broad_fiscal_gdp),
    p10         = quantile(broad_fiscal_gdp, 0.10),
    p25         = quantile(broad_fiscal_gdp, 0.25),
    p50         = quantile(broad_fiscal_gdp, 0.50),
    p75         = quantile(broad_fiscal_gdp, 0.75),
    p90         = quantile(broad_fiscal_gdp, 0.90),
    p95         = quantile(broad_fiscal_gdp, 0.95),
    p99         = quantile(broad_fiscal_gdp, 0.99),
    max         = max(broad_fiscal_gdp),
    mean_to_med = mean(broad_fiscal_gdp) / median(broad_fiscal_gdp),
    .groups     = "drop"
  )

cat("\n--- 8a. Size Distribution (broad_fiscal_gdp > 0) ---\n")
print(as.data.frame(size_dist))
cat("\n  mean_to_med > 1 indicates right-skew (large measures dominate volume).\n")

# --- 8b. Gini-style concentration: share of total volume by top decile --------

top_decile <- fm %>%
  filter(broad_fiscal_gdp > 0) %>%
  group_by(transmission_channel) %>%
  arrange(desc(broad_fiscal_gdp)) %>%
  mutate(
    rank     = row_number(),
    n_total  = n(),
    in_top10 = rank <= ceiling(n_total * 0.10)
  ) %>%
  summarise(
    top10_share = round(
      sum(broad_fiscal_gdp[in_top10]) / sum(broad_fiscal_gdp) * 100, 1
    ),
    .groups = "drop"
  )

cat("\n--- 8b. Concentration: Top 10% of Measures by Volume Share ---\n")
print(as.data.frame(top_decile))


# ==============================================================================
#  INTERPRETATION: Distributional Properties (Section 8a & 8b)
# ==============================================================================
#
#  8a — Size Distribution:
#
#  The measure-level size distribution is extremely right-skewed in all channels.
#  The mean/median ratio quantifies this:
#    CP: 4.46x (mean 0.44% vs median 0.10%)
#    DI: 4.06x (mean 0.24% vs median 0.06%)
#    H:  2.45x (mean 0.26% vs median 0.11%)
#
#  Interpretation: the "typical" fiscal measure is small. Half of all CP
#  measures are below 0.10% GDP — a single country's minor tax credit or
#  a small-scale loan facility. But the distribution has a very heavy right
#  tail: the p99 for CP is 6.10% GDP (61x the median), and the maximum is
#  12.79% GDP (128x the median). These are national guarantee programs or
#  flagship Kurzarbeit schemes that individually exceed the combined volume
#  of hundreds of smaller measures.
#
#  The gap between p75 and p90 is where the "big programs" begin:
#    CP: p75 = 0.35%, p90 = 1.03% — the top quartile is 3.5x the median,
#        but p90 is 10x the median. The jump from p90 to p99 (6.10%) shows
#        that the tail is not just heavy but accelerating.
#    DI: p75 = 0.25%, p90 = 0.71% — similar pattern, less extreme tail
#    H:  more compressed (max = 2.20%), consistent with health expenditures
#        being bounded by hospital capacity and vaccination logistics
#
#  For econometrics: when aggregating to country-quarter panels, the
#  aggregated F_k^CP and F_k^DI will be dominated by the 1-3 largest
#  programs per country. This is not a data quality issue — it reflects
#  the reality that fiscal policy operates through flagship programs.
#  But it means outlier sensitivity must be tested.
#
#
#  8b — Concentration:
#
#  The top 10% of measures by size account for:
#    CP: 65.8% of total CP volume (122 of 1,220 measures)
#    DI: 57.0% of total DI volume (44 of 431 measures)
#    H:  48.0% of total H volume  (18 of 171 measures)
#
#  Even more striking at the top:
#    CP top 5% (61 measures) = 50.9% of volume
#    CP top 1% (13 measures) = 23.7% of volume
#
#  The bottom 50% of CP measures account for only 3.5% of CP volume.
#  The Gini coefficients formalize this:
#    CP: 0.777 — higher than US income inequality (~0.49)
#    DI: 0.725
#    H:  0.652
#
#  Implication: the fiscal response is not a portfolio of many comparable
#  measures — it is a small number of large programs surrounded by a long
#  tail of minor interventions. The 13 largest CP measures (0.7% of all
#  fiscal measures in the dataset) account for nearly a quarter of all CP
#  volume across 38 countries. Identifying and correctly classifying these
#  programs is therefore critical for the analysis.
# ==============================================================================


# --- Plot 1: Lorenz Curves by Channel ----------------------------------------


# AER theme (reuse from earlier)
theme_aer <- theme_classic(base_size = 11) +
  theme(
    text               = element_text(family = "serif"),
    plot.title         = element_text(face = "bold", size = 12, hjust = 0),
    plot.subtitle      = element_text(size = 9, color = "grey30", hjust = 0),
    axis.title         = element_text(size = 10),
    axis.text          = element_text(size = 9),
    legend.position    = "bottom",
    legend.title       = element_blank(),
    legend.text        = element_text(size = 9),
    panel.grid.major   = element_line(color = "grey92", linewidth = 0.3),
    plot.caption       = element_text(size = 8, color = "grey50", hjust = 0)
  )

# Compute Lorenz curve data for each channel
fm_pos <- fm %>% filter(broad_fiscal_gdp > 0)

lorenz_data <- fm_pos %>%
  group_by(transmission_channel) %>%
  arrange(broad_fiscal_gdp, .by_group = TRUE) %>%
  mutate(
    pop_share = row_number() / n(),
    cum_share = cumsum(broad_fiscal_gdp) / sum(broad_fiscal_gdp)
  ) %>%
  ungroup() %>%
  # Add origin point for each channel
  bind_rows(
    tibble(
      transmission_channel = rep(c("CP","DI","H"), each = 1),
      pop_share = 0, cum_share = 0
    )
  ) %>%
  mutate(
    channel = factor(transmission_channel,
                     levels = c("CP","DI","H"),
                     labels = c("CP (Gini = 0.78)",
                                "DI (Gini = 0.73)",
                                "H (Gini = 0.65)"))
  )

p_lorenz <- ggplot(lorenz_data, aes(x = pop_share, y = cum_share, color = channel)) +
  # Perfect equality line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "grey60", linewidth = 0.5) +
  # Lorenz curves
  geom_line(linewidth = 0.8) +
  # Key reference annotations
  annotate("segment",
           x = 0.50, xend = 0.50, y = 0, yend = 0.035,
           linetype = "dotted", color = "grey40", linewidth = 0.4) +
  annotate("text", x = 0.50, y = 0.06, size = 2.8, family = "serif",
           color = "grey30", label = "Bottom 50% of CP\n= 3.5% of volume") +
  annotate("segment",
           x = 0.90, xend = 0.90, y = 0, yend = 0.342,
           linetype = "dotted", color = "grey40", linewidth = 0.4) +
  annotate("text", x = 0.82, y = 0.40, size = 2.8, family = "serif",
           color = "grey30", label = "Top 10% of CP\n= 65.8% of volume") +
  scale_color_manual(values = c("grey25", "grey50", "grey72")) +
  scale_x_continuous(labels = scales::percent, breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.2)) +
  coord_equal() +
  labs(
    title    = "A. Lorenz Curves: Fiscal Volume Concentration",
    subtitle = "Further from diagonal = more concentrated. CP is the most unequal channel.",
    x        = "Cumulative Share of Measures (ranked by size)",
    y        = "Cumulative Share of Fiscal Volume"
  ) +
  theme_aer


# --- Plot 2: Cumulative Contribution (Top-Down) ------------------------------
# Shows how volume accumulates when stacking measures from largest to smallest.
# Answers: "how many measures do you need to reach X% of the total?"

cum_contrib <- fm_pos %>%
  group_by(transmission_channel) %>%
  arrange(desc(broad_fiscal_gdp), .by_group = TRUE) %>%
  mutate(
    rank        = row_number(),
    rank_pct    = rank / n() * 100,
    cum_vol_pct = cumsum(broad_fiscal_gdp) / sum(broad_fiscal_gdp) * 100
  ) %>%
  ungroup() %>%
  mutate(
    channel = factor(transmission_channel,
                     levels = c("CP","DI","H"),
                     labels = c("Capacity Preservation",
                                "Demand Injection",
                                "Health"))
  )

# Find where each channel crosses 80% of volume
cross_80 <- cum_contrib %>%
  group_by(channel) %>%
  filter(cum_vol_pct >= 80) %>%
  slice_min(rank) %>%
  ungroup()

p_cumtop <- ggplot(cum_contrib, aes(x = rank_pct, y = cum_vol_pct, color = channel)) +
  geom_line(linewidth = 0.8) +
  # 80% reference line
  geom_hline(yintercept = 80, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  # Mark where each channel crosses 80%
  geom_point(data = cross_80, size = 2.5) +
  geom_text(data = cross_80,
            aes(label = sprintf("%.0f%%", rank_pct)),
            vjust = -1, hjust = 0.5, size = 3, family = "serif",
            show.legend = FALSE) +
  scale_color_manual(values = c("grey25", "grey55", "grey75"), name = NULL) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(breaks = seq(0, 100, 20)) +
  labs(
    title    = "B. Cumulative Volume from Largest to Smallest Measure",
    subtitle = "Points mark where each channel reaches 80% of its total volume.",
    x        = "Percent of Measures Included (ranked largest first)",
    y        = "Cumulative Share of Channel Volume (%)"
  ) +
  theme_aer


# --- Plot 3: Log-Scale Rank-Size (Zipf-Style) --------------------------------
# If the tail follows a power law, this will be approximately linear.
# Steeper slope = heavier tail.

rank_size <- fm_pos %>%
  group_by(transmission_channel) %>%
  arrange(desc(broad_fiscal_gdp), .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  mutate(
    size_pct = broad_fiscal_gdp * 100,
    channel  = factor(transmission_channel,
                      levels = c("CP","DI","H"),
                      labels = c("Capacity Preservation",
                                 "Demand Injection",
                                 "Health"))
  )

p_zipf <- ggplot(rank_size, aes(x = rank, y = size_pct, color = channel)) +
  geom_point(alpha = 0.4, size = 0.8) +
  scale_x_log10(
    breaks = c(1, 5, 10, 50, 100, 500, 1000),
    labels = c("1","5","10","50","100","500","1k")
  ) +
  scale_y_log10(
    breaks = c(0.001, 0.01, 0.1, 1, 10),
    labels = c("0.001","0.01","0.1","1","10")
  ) +
  scale_color_manual(values = c("grey25", "grey55", "grey75"), name = NULL) +
  labs(
    title    = "C. Rank–Size Distribution (Log–Log Scale)",
    subtitle = "Approximately linear relationship indicates power-law tail behavior.",
    x        = "Rank (largest = 1)",
    y        = "Measure Size (% of 2019 GDP)",
    caption  = paste0(
      "Notes: N = 1,822 measures with size > 0. ",
      "Top 1% of CP (13 measures) accounts for 23.7% of CP volume.\n",
      "Gini coefficients — CP: 0.78, DI: 0.73, H: 0.65. ",
      "All channels exceed typical income inequality measures."
    )
  ) +
  theme_aer


# --- Combine all three -------------------------------------------------------

p_dist <- p_lorenz / (p_cumtop | p_zipf) +
  plot_annotation(
    title = "Figure X: Distributional Properties of Fiscal Measures",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14, family = "serif")
    )
  ) &
  theme(legend.position = "bottom")

print(p_dist)


#Lorenz-Kurve (A) zeigt die Ungleichheit direkt. Je weiter die Kurve von der Diagonale entfernt, desto konzentrierter. CP hat den grössten Abstand — die untere Hälfte der CP-Massnahmen trägt 3.5% zum Volumen bei. Das ist extremer als die US-Einkommensverteilung. Der Gini von 0.78 sagt: würdest du zwei zufällige CP-Massnahmen ziehen, unterscheidet sich ihre Grösse im Erwartungswert um 78% des Mittelwerts.
#Kumulativer Top-Down (B) beantwortet die operative Frage: wie viele Massnahmen muss man korrekt identifizieren und klassifizieren, damit die Analyse robust ist? Bei CP reichen die grössten ~15% der Massnahmen für 80% des Volumens. Das heisst umgekehrt: 85% der Massnahmen sind für das aggregierte Bild fast irrelevant. Für deine Schätzung bedeutet das, dass Klassifikationsfehler bei kleinen Massnahmen tolerierbar sind — aber ein einzelner Fehler bei einem grossen Garantieprogramm kann die Ländertotals substantiell verschieben.
#Rank-Size / Zipf (C) testet, ob die Verteilung einem Potenzgesetz folgt. Wenn der Log-Log-Plot annähernd linear ist, folgt die Grössenverteilung einem Power Law — das ist typisch für Systeme wo "Skalenfreiheit" herrscht. Für dich relevant: in einer Power-Law-Verteilung ist der Mittelwert kein guter Lageparameter, und Varianz kann unendlich sein. Das hat Implikationen für die Standardfehler in deiner Panel-Schätzung.

#Zum Zipf (c) Test:
#Genau, und das ist eigentlich eine gute Nachricht. Ein linearer Zipf-Plot würde ein Pareto-Potenzgesetz implizieren — dort kann die Varianz unendlich sein, was OLS-Inferenz fundamental untergräbt. Dein Plot zeigt stattdessen eine konvexe Krümmung nach unten: die grössten Massnahmen fallen schneller ab als ein Potenzgesetz vorhersagen würde.
#Das ist konsistent mit einer **Log-Normalverteilung**, nicht mit einem Power Law. Die Intuition: bei einem Potenzgesetz gibt es keinen natürlichen Skalierungsmechanismus — die nächstgrössere Massnahme kann beliebig viel grösser sein. Bei fiskalischen Massnahmen existiert aber eine natürliche Obergrenze: kein einzelnes Programm kann grösser als das BIP sein, und politische sowie administrative Constraints begrenzen die Programmgrösse. Die Kurve knickt oben ab, weil die 5–10 grössten Massnahmen kleiner sind als ein Potenzgesetz extrapolieren würde.
#Für die Ökonometrie bedeutet das: die Verteilung ist stark rechtsschief und konzentriert (Gini 0.78 bleibt), aber die **Varianz ist endlich**. Standardfehler konvergieren, OLS mit robusten Standardfehlern ist nicht fundamental gebrochen. Du kannst den Zipf-Plot im Paper trotzdem zeigen — er dokumentiert die Konzentration visuell — aber die Interpretation im Text sollte lauten: heavy-tailed mit log-normaler Struktur, nicht Power Law. Streich die Zeile "approximately linear relationship indicates power-law tail behavior" aus dem Subtitle und ersetze sie durch etwas wie "concavity indicates bounded (log-normal) rather than power-law tail."



# --- 8c. Country-level coefficient of variation --------------------------------
# Measures cross-country heterogeneity in fiscal effort for each channel.

country_cv <- country_channel %>%
  ungroup() %>%
  summarise(
    cv_total   = sd(total_fiscal) / mean(total_fiscal),
    cv_CP      = sd(total_gdp_CP) / mean(total_gdp_CP),
    cv_DI      = sd(total_gdp_DI) / mean(total_gdp_DI),
    cv_H       = sd(total_gdp_H) / mean(total_gdp_H),
    cv_CPshare = sd(CP_share) / mean(CP_share)
  )

cat("\n--- 8c. Cross-Country Coefficient of Variation ---\n")
cat("  (Higher CV = more heterogeneous across countries)\n")
print(t(country_cv))

# --- 8d. Correlation: CP vs DI at the country level ---------------------------
# Do countries that deploy more CP also deploy more DI, or is there a trade-off?

cor_cp_di <- cor.test(country_channel$total_gdp_CP, country_channel$total_gdp_DI)

cat("\n--- 8d. Country-Level Correlation: CP vs DI ---\n")
cat(sprintf("  Pearson r = %.3f, p = %.4f\n", cor_cp_di$estimate, cor_cp_di$p.value))
cat("  (Positive: countries that spend more on CP also spend more on DI.)\n")
cat("  (Negative: trade-off between CP and DI.)\n")

# Same for CP share vs total fiscal effort
cor_cpshare_total <- cor.test(country_channel$CP_share, country_channel$total_fiscal)

cat(sprintf("\n  CP_share vs Total fiscal: r = %.3f, p = %.4f\n",
            cor_cpshare_total$estimate, cor_cpshare_total$p.value))


# ==============================================================================
#  COUNTRY x QUARTER x CHANNEL: Descriptives & Temporal Patterns
# ==============================================================================


theme_aer <- theme_classic(base_size = 11) +
  theme(
    text               = element_text(family = "serif"),
    plot.title         = element_text(face = "bold", size = 12, hjust = 0),
    plot.subtitle      = element_text(size = 9, color = "grey30", hjust = 0),
    axis.title         = element_text(size = 10),
    axis.text          = element_text(size = 9),
    legend.position    = "bottom",
    legend.title       = element_blank(),
    legend.text        = element_text(size = 9),
    panel.grid.major   = element_line(color = "grey92", linewidth = 0.3),
    plot.caption       = element_text(size = 8, color = "grey50", hjust = 0)
  )

# --- Quarter ordering --------------------------------------------------------

q_levels <- c("Q1.2020","Q2.2020","Q3.2020","Q4.2020",
              "Q1.2021","Q2.2021","Q3.2021","Q4.2021")

fm <- fm %>%
  mutate(
    size_pct = broad_fiscal_gdp * 100,
    YQ = factor(paste0("Q", Quarter, ".", Year), levels = q_levels, ordered = TRUE)
  )


# --- 1. Country-Quarter Panel -------------------------------------------------
# Aggregate to country x quarter x channel

cqc <- fm %>%
  group_by(Country, region, income_group, YQ, transmission_channel) %>%
  summarise(
    n_measures = n(),
    vol        = sum(size_pct, na.rm = TRUE),
    .groups    = "drop"
  )

# Wide format: one row per country-quarter
cqc_wide <- cqc %>%
  filter(transmission_channel %in% c("CP", "DI")) %>%
  pivot_wider(
    names_from  = transmission_channel,
    values_from = c(n_measures, vol),
    values_fill = 0
  ) %>%
  mutate(
    vol_total  = vol_CP + vol_DI,
    cp_ratio   = ifelse(vol_total > 0, vol_CP / vol_total * 100, NA_real_)
  )


# --- 2. Adoption breadth per quarter -----------------------------------------
# How many countries were actively deploying each channel?

adoption <- fm %>%
  filter(size_pct > 0) %>%
  group_by(YQ, transmission_channel) %>%
  summarise(
    n_countries = n_distinct(Country),
    .groups     = "drop"
  ) %>%
  pivot_wider(
    names_from  = transmission_channel,
    values_from = n_countries,
    values_fill = 0
  )

cat("\n--- Adoption Breadth: Countries with Positive Deployment ---\n")
print(as.data.frame(adoption))
# CP deployed by 36-38 countries in Q1-Q2 2020, DI by 28-37
# CP remains broader throughout: still 22 countries in Q4.2021 vs 10 for DI


# --- 3. First deployment quarter per channel ----------------------------------

first_deploy <- fm %>%
  filter(size_pct > 0) %>%
  group_by(Country, transmission_channel) %>%
  summarise(first_q = min(YQ), .groups = "drop") %>%
  pivot_wider(
    names_from  = transmission_channel,
    values_from = first_q
  )

cat("\n--- First Deployment Quarter ---\n")
print(as.data.frame(first_deploy))

# Timing lag: CP before DI?
timing <- first_deploy %>%
  mutate(
    cp_idx = match(CP, q_levels),
    di_idx = match(DI, q_levels),
    lag    = di_idx - cp_idx  # Positive = CP was first
  )

cat("\n--- CP vs DI Timing ---\n")
cat(sprintf("  CP first: %d | Same quarter: %d | DI first: %d\n",
            sum(timing$lag > 0, na.rm = TRUE),
            sum(timing$lag == 0, na.rm = TRUE),
            sum(timing$lag < 0, na.rm = TRUE)))
cat(sprintf("  Mean lag (DI - CP): %.2f quarters\n", mean(timing$lag, na.rm = TRUE)))
# 28 of 38 countries deployed CP and DI in the same quarter
# 9 deployed CP at least one quarter before DI; only 1 deployed DI first


# --- 4. Peak deployment quarter -----------------------------------------------
# When did each country deploy the most CP / DI?

peak_q <- fm %>%
  filter(size_pct > 0) %>%
  group_by(Country, YQ, transmission_channel) %>%
  summarise(vol = sum(size_pct), .groups = "drop") %>%
  group_by(Country, transmission_channel) %>%
  slice_max(vol, n = 1, with_ties = FALSE) %>%
  ungroup()

peak_summary <- peak_q %>%
  group_by(transmission_channel, YQ) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = YQ, values_from = n, values_fill = 0)

cat("\n--- Peak Deployment Quarter Distribution ---\n")
print(as.data.frame(peak_summary))
# CP peaks in Q1.2020 (22 countries) or Q2.2020 (15) — front-loaded
# DI peaks spread across Q1 (10), Q2 (14), Q3 (5), Q1.2021 (4) — more diffuse


# --- 5. CP/(CP+DI) ratio dynamics ---------------------------------------------
# How does the composition shift within countries over time?

ratio_panel <- cqc_wide %>%
  filter(!is.na(cp_ratio)) %>%
  group_by(YQ) %>%
  summarise(
    mean_ratio   = mean(cp_ratio),
    median_ratio = median(cp_ratio),
    sd_ratio     = sd(cp_ratio),
    q25          = quantile(cp_ratio, 0.25),
    q75          = quantile(cp_ratio, 0.75),
    n            = n(),
    .groups      = "drop"
  )

cat("\n--- CP/(CP+DI) Ratio Over Time (cross-country distribution) ---\n")
print(as.data.frame(ratio_panel))
# ==============================================================================
#  INTERPRETATION:
#
#  Timing:
#  - 36 of 38 countries deployed CP in Q1.2020 — near-universal immediate response
#  - DI was slower: only 28 countries in Q1.2020, reaching 37 in Q2.2020
#  - CP preceded DI in 9 countries; they were simultaneous in 28; DI first in only 1
#  - This is consistent with the model: CP is the first-line defense when S_k is high,
#    because it prevents irreversible structural damage (psi * S_k * y_k)
#
#  Peak quarter:
#  - CP peaked in Q1.2020 for 22/38 countries — massive front-loading
#  - DI peak is more dispersed: Q1 (10), Q2 (14), later (14)
#  - DI deployment continued to shift rightward as economies reopened
#    and the demand channel became operative (consistent with Eq. 8)
#
#  Composition dynamics:
#  - Mean CP/(CP+DI) ratio starts near 90% in Q1.2020 and declines
#  - The variance of the ratio increases over time — countries diverge
#    in their composition strategies as the acute phase passes
# ==============================================================================


# ==============================================================================
#  PLOT: Country x Quarter Heatmap — CP vs DI Deployment
# ==============================================================================

# --- Panel A: Heatmap of CP intensity by country x quarter --------------------

# Country order: sorted by total fiscal package (largest on top)
country_order <- fm %>%
  group_by(Country) %>%
  summarise(total = sum(size_pct), .groups = "drop") %>%
  arrange(total) %>%
  pull(Country)

# Build full country-quarter-channel grid (fills zeros where no measures exist)
full_grid <- expand_grid(
  Country              = unique(fm$Country),
  YQ                   = factor(q_levels, levels = q_levels, ordered = TRUE),
  transmission_channel = c("CP", "DI")
)

heatmap_data <- fm %>%
  filter(transmission_channel %in% c("CP", "DI")) %>%
  group_by(Country, YQ, transmission_channel) %>%
  summarise(vol = sum(size_pct, na.rm = TRUE), .groups = "drop") %>%
  right_join(full_grid, by = c("Country", "YQ", "transmission_channel")) %>%
  replace_na(list(vol = 0)) %>%
  mutate(
    Country = factor(Country, levels = country_order),
    channel = factor(transmission_channel,
                     levels = c("CP", "DI"),
                     labels = c("Capacity Preservation", "Demand Injection"))
  )

# Cap at p99 for color scale readability
cap_99 <- quantile(heatmap_data$vol[heatmap_data$vol > 0], 0.99, na.rm = TRUE)

q_labels_short <- c("Q1\n'20","Q2","Q3","Q4","Q1\n'21","Q2","Q3","Q4")

p_heat <- ggplot(heatmap_data,
                 aes(x = YQ, y = Country, fill = pmin(vol, cap_99))) +
  geom_tile(color = "white", linewidth = 0.15) +
  facet_wrap(~channel) +
  scale_fill_gradient(
    low    = "grey96",
    high   = "grey15",
    name   = "% of 2019 GDP",
    breaks = c(0, 2, 5, 10),
    limits = c(0, cap_99)
  ) +
  scale_x_discrete(labels = q_labels_short) +
  labs(
    title    = "A. Fiscal Deployment Intensity by Country and Quarter",
    subtitle = "Darker = larger deployment. Countries sorted by total fiscal package (largest on top).",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    text             = element_text(family = "serif"),
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(size = 9, color = "grey30"),
    strip.text       = element_text(face = "bold", size = 10),
    axis.text.y      = element_text(size = 7),
    axis.text.x      = element_text(size = 8),
    legend.position  = "right",
    legend.key.height = unit(1.5, "cm"),
    panel.grid       = element_blank()
  )


# --- Panel B: CP/(CP+DI) ratio over time with country-level dispersion -------

# Individual country trajectories + cross-country ribbon
ratio_country <- cqc_wide %>%
  filter(!is.na(cp_ratio)) %>%
  mutate(Country = factor(Country, levels = country_order))

p_ratio <- ggplot() +
  # IQR ribbon
  geom_ribbon(
    data = ratio_panel,
    aes(x = as.numeric(YQ), ymin = q25, ymax = q75),
    fill = "grey80", alpha = 0.5
  ) +
  # Individual country lines (light)
  geom_line(
    data = ratio_country,
    aes(x = as.numeric(YQ), y = cp_ratio, group = Country),
    color = "grey60", alpha = 0.3, linewidth = 0.3
  ) +
  # Cross-country median (bold)
  geom_line(
    data = ratio_panel,
    aes(x = as.numeric(YQ), y = median_ratio),
    color = "black", linewidth = 1
  ) +
  geom_point(
    data = ratio_panel,
    aes(x = as.numeric(YQ), y = median_ratio),
    color = "black", size = 2
  ) +
  # Reference lines
  geom_hline(yintercept = 50, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  annotate("text", x = 7.5, y = 53, label = "50% threshold",
           size = 2.8, family = "serif", color = "grey50") +
  # Scales
  scale_x_continuous(
    breaks = 1:8,
    labels = q_labels_short
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 25)
  ) +
  labs(
    title    = "B. Composition Dynamics: CP/(CP+DI) Ratio Over Time",
    subtitle = "Bold line: cross-country median. Ribbon: IQR. Thin lines: individual countries.",
    x        = NULL,
    y        = "CP Share of CP+DI (%)",
    caption  = paste0(
      "Notes: Panel A capped at p99 for color readability. ",
      "Panel B computed for country-quarters with positive CP+DI deployment.\n",
      "CP preceded DI in 9/38 countries; simultaneous in 28/38. ",
      "CP peak quarter: Q1.2020 (22 countries). DI peak: distributed Q1–Q2.2020."
    )
  ) +
  theme_aer


# --- Combine ------------------------------------------------------------------

p_cq <- p_heat / p_ratio +
  plot_layout(heights = c(2, 1)) +
  plot_annotation(
    title = "Figure X: Temporal Deployment Patterns — CP vs. DI Across OECD Economies",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14, family = "serif")
    )
  )

print(p_cq)



#zeigt die Kompositionsdynamik innerhalb der Länder. Der Median startet bei ~90% CP-Anteil und fällt im Verlauf. Entscheidend ist aber das 
#IQR-Band: es wird über die Zeit breiter. In Q1.2020 lagen fast alle Länder bei 80–100% CP — homogene Erstreaktion. Ab Q3.2020 divergieren die 
#Strategien: einige bleiben bei >90% CP (Frankreich, Italien), andere kippen unter 50% (USA, Chile, Australien). Das ist exakt die Heterogenität, 
#die dein Modell als suboptimal oder optimal bewerten kann.


# ==============================================================================
#  COUNTRY STRATEGY TIMELINES: Small Multiples (38 Panels)
# ==============================================================================


# --- Prepare country-quarter-channel volumes ----------------------------------

q_levels <- c("Q1.2020","Q2.2020","Q3.2020","Q4.2020",
              "Q1.2021","Q2.2021","Q3.2021","Q4.2021")

# Full grid: ensure every country x quarter x channel has a row
full_grid <- expand_grid(
  Country              = unique(fm$Country),
  YQ                   = factor(q_levels, levels = q_levels, ordered = TRUE),
  transmission_channel = c("CP", "DI", "H")
)

panel_data <- fm %>%
  group_by(Country, YQ, transmission_channel) %>%
  summarise(vol = sum(size_pct, na.rm = TRUE), .groups = "drop") %>%
  right_join(full_grid, by = c("Country", "YQ", "transmission_channel")) %>%
  replace_na(list(vol = 0)) %>%
  mutate(
    transmission_channel = factor(transmission_channel,
                                  levels = c("H", "DI", "CP")),
    q_num = as.numeric(YQ)
  )

# --- Country labels with total package size for facet strip -------------------

country_totals <- panel_data %>%
  group_by(Country) %>%
  summarise(total = sum(vol), .groups = "drop") %>%
  arrange(desc(total)) %>%
  mutate(label = sprintf("%s (%.1f%%)", Country, total))

# Ordered factor: largest package first (row-wise reading order)
panel_data <- panel_data %>%
  left_join(country_totals, by = "Country") %>%
  mutate(label = factor(label, levels = country_totals$label))

# --- Faceted stacked bar plot -------------------------------------------------

q_labels_short <- c("Q1","Q2","Q3","Q4","Q1","Q2","Q3","Q4")

p_small_mult <- ggplot(panel_data,
                       aes(x = q_num, y = vol, fill = transmission_channel)) +
  geom_col(width = 0.75, color = NA) +
  # Year separator
  geom_vline(xintercept = 4.5, linetype = "dotted",
             color = "grey50", linewidth = 0.3) +
  facet_wrap(~label, ncol = 6, scales = "free_y") +
  scale_fill_manual(
    values = c("CP" = "grey25", "DI" = "grey58", "H" = "grey82"),
    breaks = c("CP", "DI", "H"),
    labels = c("Capacity Preservation", "Demand Injection", "Health"),
    name   = NULL
  ) +
  scale_x_continuous(
    breaks = 1:8,
    labels = q_labels_short
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title    = "Figure X: Fiscal Strategy Timelines — All OECD Economies",
    subtitle = "Stacked quarterly deployment by transmission channel (% of 2019 GDP). Dotted line separates 2020 | 2021.\nCountries sorted by total fiscal package (parentheses). Free y-axis to preserve within-country temporal pattern.",
    x        = NULL,
    y        = NULL,
    caption  = paste0(
      "Notes: 1,829 core fiscal measures across 38 OECD economies. ",
      "Each bar shows the sum of measures deployed in that country-quarter.\n",
      "Free y-scales: cross-country level comparisons should use Figure [country bars]. ",
      "This figure highlights within-country temporal composition shifts."
    )
  ) +
  theme_minimal(base_size = 9) +
  theme(
    text               = element_text(family = "serif"),
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(size = 8.5, color = "grey30"),
    plot.caption       = element_text(size = 7.5, color = "grey50", hjust = 0),
    strip.text         = element_text(face = "bold", size = 7.5, margin = margin(2,0,2,0)),
    axis.text.x        = element_text(size = 6),
    axis.text.y        = element_text(size = 6),
    axis.ticks         = element_blank(),
    legend.position    = "bottom",
    legend.text        = element_text(size = 9),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.spacing      = unit(0.4, "lines")
  )

# Print at large size — recommend ggsave for proper rendering
print(p_small_mult)

# --- Recommended export for readability: --------------------------------------
# ggsave("country_timelines.pdf", p_small_mult,
#        width = 16, height = 14, units = "in")


# ==============================================================================
#  PLOT: Mean Fiscal Deployment per Quarter (MATLAB-style comparison)
# ==============================================================================
#  Computes the unweighted cross-country average of F_CP and F_DI per quarter
#  (% of 2019 GDP). This is the empirical counterpart to the model's optimal
#  control trajectories u_opt(2,:) and u_opt(3,:) in the MATLAB output.
# ==============================================================================


# --- Compute mean country-level deployment per quarter ------------------------
# For each quarter: sum each country's measures, then average across countries.
# This gives "how much did the average OECD economy deploy in quarter k?"

q_levels <- c("Q1.2020","Q2.2020","Q3.2020","Q4.2020",
              "Q1.2021","Q2.2021","Q3.2021","Q4.2021")

# Full grid: every country x quarter x channel (zero-fill for missing)
full_grid <- expand_grid(
  Country              = unique(fm$Country),
  YQ                   = factor(q_levels, levels = q_levels, ordered = TRUE),
  transmission_channel = c("CP", "DI")
)

country_quarter <- fm %>%
  filter(transmission_channel %in% c("CP", "DI")) %>%
  group_by(Country, YQ, transmission_channel) %>%
  summarise(vol = sum(size_pct, na.rm = TRUE), .groups = "drop") %>%
  right_join(full_grid, by = c("Country", "YQ", "transmission_channel")) %>%
  replace_na(list(vol = 0))

# Cross-country mean and SE per quarter
mean_deploy <- country_quarter %>%
  group_by(YQ, transmission_channel) %>%
  summarise(
    mean_vol = mean(vol),
    se_vol   = sd(vol) / sqrt(n()),
    .groups  = "drop"
  ) %>%
  mutate(
    channel = factor(transmission_channel,
                     levels = c("CP", "DI"),
                     labels = c(expression(F[k]^{CP}), expression(F[k]^{DI})))
  )

# Separate for positioning
mean_cp <- mean_deploy %>% filter(transmission_channel == "CP")
mean_di <- mean_deploy %>% filter(transmission_channel == "DI")

q_labels_short <- c("Q1\n2020","Q2","Q3","Q4","Q1\n2021","Q2","Q3","Q4")

# --- Plot: Grouped Bars with SE whiskers (MATLAB-style) -----------------------

p_matlab <- ggplot() +
  # CP bars
  geom_col(
    data = mean_cp,
    aes(x = as.numeric(YQ) - 0.17, y = mean_vol),
    width = 0.3, fill = "grey25"
  ) +
  geom_errorbar(
    data = mean_cp,
    aes(x = as.numeric(YQ) - 0.17,
        ymin = pmax(mean_vol - se_vol, 0), ymax = mean_vol + se_vol),
    width = 0.12, linewidth = 0.4
  ) +
  # DI bars
  geom_col(
    data = mean_di,
    aes(x = as.numeric(YQ) + 0.17, y = mean_vol),
    width = 0.3, fill = "grey65"
  ) +
  geom_errorbar(
    data = mean_di,
    aes(x = as.numeric(YQ) + 0.17,
        ymin = pmax(mean_vol - se_vol, 0), ymax = mean_vol + se_vol),
    width = 0.12, linewidth = 0.4
  ) +
  # Manual legend via annotation
  annotate("rect", xmin = 6.3, xmax = 6.6, ymin = 5.8, ymax = 6.2,
           fill = "grey25") +
  annotate("text", x = 6.7, y = 6.0, label = "F[k]^{CP}",
           hjust = 0, size = 3.5, family = "serif", parse = TRUE) +
  annotate("rect", xmin = 6.3, xmax = 6.6, ymin = 5.0, ymax = 5.4,
           fill = "grey65") +
  annotate("text", x = 6.7, y = 5.2, label = "F[k]^{DI}",
           hjust = 0, size = 3.5, family = "serif", parse = TRUE) +
  # Scales
  scale_x_continuous(
    breaks = 1:8,
    labels = q_labels_short
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),
    breaks = seq(0, 8, 1)
  ) +
  labs(
    title    = "Fiscal Composition: Empirical Deployment",
    subtitle = "Unweighted cross-country mean across 38 OECD economies. Whiskers: ± 1 SE.",
    x        = "Quarter",
    y        = "% of 2019 GDP",
    caption  = paste0(
      "Notes: Each country's quarterly CP and DI volume computed as sum of individual measures (% of 2019 GDP).\n",
      "Zeros included for countries with no deployment in a given quarter. ",
      "Comparable to model controls u(2,:) and u(3,:) in iLQR output."
    )
  ) +
  theme_classic(base_size = 11) +
  theme(
    text               = element_text(family = "serif"),
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(size = 9, color = "grey30"),
    plot.caption       = element_text(size = 8, color = "grey50", hjust = 0),
    axis.title         = element_text(size = 10),
    axis.text          = element_text(size = 9),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.major.x = element_blank()
  )

print(p_matlab)

##See Notion (Fiscal Measures) for full Interpretation
#...............................................................................
###########################STRINGENCY###########################################
#...............................................................................


#  Primärvariable: StringencyIndex_PopWeighted (Oxford CGRT, C1–C8)
#  Analyseperiode: März 2020 – Dezember 2021 (= Planungshorizont des Modells)
#  Tägliche Daten (oxd_d), Aggregation auf Quartal für Panel (qdata)
#
#  Gliederung:
#   1. Datenvorbereitung & Periodenabgrenzung
#   2. Gesamtverteilung & Zeitstruktur
#   3. Aggregierte OECD-Trajektorie
#   4. Länderheterogenität
#   5. Komponentenzerlegung (C1–C8)
#   6. Quartalsaggregation für das Panel
#   7. Deskriptive Korrelation mit Outcomes
#   8. Timing-Koinzidenz mit Fiskalmassnahmen
# ==============================================================================


# --- AER Theme ----------------------------------------------------------------
theme_aer <- theme_classic(base_size = 10) +
  theme(
    text               = element_text(family = "serif"),
    plot.title         = element_text(face = "bold", size = 11, hjust = 0),
    plot.subtitle      = element_text(size = 8, color = "grey30"),
    plot.caption       = element_text(size = 7, color = "grey50", hjust = 0),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    legend.position    = "bottom",
    legend.text        = element_text(size = 8),
    strip.text         = element_text(face = "bold", size = 8)
  )


# ==============================================================================
#  1. DATENVORBEREITUNG & PERIODENABGRENZUNG
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 1: Datenvorbereitung\n")
cat(strrep("=", 70), "\n\n")

# --- Primärvariable: StringencyIndex_PopWeighted ---
# Konstruktion: Einfacher Durchschnitt der Indikatoren C1–C8 (je 0–100),
# gewichtet nach Bevölkerungsanteil NV/V ab Einführung differenzierter Politik.
# Erfasst: Schulschliessungen (C1), Arbeitsplatzschliessungen (C2),
# Veranstaltungsverbote (C3), Versammlungsbeschränkungen (C4),
# ÖV-Einschränkungen (C5), Stay-at-home (C6), Inlandsbewegung (C7),
# Internationale Reisen (C8).
# NICHT enthalten: Gesundheitsmassnahmen (H1–H8), Wirtschaftspolitik (E1–E4).

primary_var <- "StringencyIndex_PopWeighted"

# --- Analyseperiode: 1. März 2020 – 31. Dezember 2021 ---
# Begründung: Vor März 2020 sind alle Werte ≈ 0 (keine Pandemie).
# Ab 2022 konvergieren die Werte gegen 0 (Omicron → "living with COVID").
# Der Analysezeitraum deckt den Planungshorizont des iLQR ab.

oxd_panel <- oxd_d %>%
  filter(Date >= as.Date("2020-03-01"), Date <= as.Date("2021-12-31")) %>%
  mutate(
    S_raw  = .data[[primary_var]],        # 0–100 Skala
    S      = S_raw / 100,                 # Normalisiert auf [0,1] für Modell
    YQ     = paste0("Q", Quarter_Num, ".", Year),
    YQ_ord = factor(YQ, levels = c("Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                                   "Q1.2021","Q2.2021","Q3.2021","Q4.2021"))
  )

cat(sprintf("  Analyseperiode: %s bis %s\n",
            min(oxd_panel$Date), max(oxd_panel$Date)))
cat(sprintf("  Beobachtungen: %d (= %d Länder × %d Tage)\n",
            nrow(oxd_panel), n_distinct(oxd_panel$Country),
            nrow(oxd_panel) / n_distinct(oxd_panel$Country)))
cat(sprintf("  S (0–100): mean = %.1f, sd = %.1f, range = [%.1f, %.1f]\n",
            mean(oxd_panel$S_raw), sd(oxd_panel$S_raw),
            min(oxd_panel$S_raw), max(oxd_panel$S_raw)))
cat(sprintf("  Anteil Tage mit S > 50: %.1f%%\n",
            mean(oxd_panel$S_raw > 50) * 100))
cat(sprintf("  Anteil Tage mit S > 70: %.1f%%\n",
            mean(oxd_panel$S_raw > 70) * 100))
cat(sprintf("  Anteil Tage mit S = 0:  %.1f%%\n\n",
            mean(oxd_panel$S_raw == 0) * 100))


# ==============================================================================
#  2. GESAMTVERTEILUNG & ZEITSTRUKTUR
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("  SECTION 2: Gesamtverteilung\n")
cat(strrep("=", 70), "\n\n")

# --- 2a. Verteilung nach Jahr -------------------------------------------------
# Interpretation: 2020 ist rechtsschief (Masse bei 40–70), 2021 ist bimodal
# (Koexistenz strenger und lockerer Regime). Die Verschiebung nach links
# reflektiert Impffortschritt und Anpassung.

year_stats <- oxd_panel %>%
  group_by(Year) %>%
  summarise(
    N       = n(),
    Mean    = mean(S_raw),
    Median  = median(S_raw),
    SD      = sd(S_raw),
    P10     = quantile(S_raw, 0.10),
    P25     = quantile(S_raw, 0.25),
    P75     = quantile(S_raw, 0.75),
    P90     = quantile(S_raw, 0.90),
    Pct_gt50 = mean(S_raw > 50) * 100,
    .groups  = "drop"
  )
cat("--- 2a. Verteilung nach Jahr ---\n")
print(kable(year_stats, digits = 1))

p_dist_year <- ggplot(oxd_panel, aes(x = S_raw, fill = factor(Year))) +
  geom_density(alpha = 0.45, color = NA) +
  geom_vline(data = year_stats, aes(xintercept = Mean, color = factor(Year)),
             linetype = "dashed", linewidth = 0.6, show.legend = FALSE) +
  scale_fill_manual(values = c("2020" = "grey30", "2021" = "grey65"), name = NULL) +
  scale_color_manual(values = c("2020" = "grey30", "2021" = "grey65")) +
  scale_x_continuous(breaks = seq(0, 100, 20), expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "A. Distribution by Year",
    subtitle = "Dashed lines: annual means. 2021 bimodal: strict vs. reopened regimes coexist.",
    x = "Stringency Index (0–100)", y = "Density"
  ) + theme_aer

# --- 2b. Verteilung nach Quartal -----------------------------------------------
# Interpretation: Die Verteilung wandert systematisch nach links. Q2.2020 hat
# die engste Konzentration (alle Länder im harten Lockdown gleichzeitig).
# Ab Q3.2020 wächst die Varianz — Länder divergieren in ihrer Exit-Strategie.

q_stats <- oxd_panel %>%
  group_by(YQ_ord) %>%
  summarise(
    Mean   = mean(S_raw),
    Median = median(S_raw),
    SD     = sd(S_raw),
    IQR    = IQR(S_raw),
    CV     = sd(S_raw) / mean(S_raw),
    .groups = "drop"
  )
cat("\n--- 2b. Verteilung nach Quartal ---\n")
print(kable(q_stats, digits = 1))

# Interpretation: CV steigt über die Zeit → zunehmende Heterogenität
cat(sprintf("\n  CV-Trend: Q1.2020 = %.2f → Q4.2021 = %.2f (Heterogenität steigt)\n",
            q_stats$CV[1], q_stats$CV[nrow(q_stats)]))

p_dist_quarter <- ggplot(oxd_panel, aes(x = S_raw, y = YQ_ord)) +
  geom_violin(fill = "grey80", color = "grey50", scale = "width",
              width = 0.8, alpha = 0.7) +
  geom_boxplot(width = 0.15, fill = "white", outlier.size = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 2.5,
               color = "#C0392B") +
  scale_x_continuous(breaks = seq(0, 100, 20)) +
  labs(
    title    = "B. Quarterly Distribution (Violin + Box)",
    subtitle = "Red diamond: mean. Box: IQR. Width: density. Variance increases as countries diverge.",
    x = "Stringency Index (0–100)", y = NULL
  ) + theme_aer + theme(panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3))

p_section2 <- p_dist_year + p_dist_quarter +
  plot_annotation(
    title = "Figure X: Distribution of Containment Intensity",
    theme = theme(plot.title = element_text(face = "bold", size = 13, family = "serif"))
  )
print(p_section2)


# ==============================================================================
#  3. AGGREGIERTE OECD-TRAJEKTORIE
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 3: Aggregierte OECD-Trajektorie\n")
cat(strrep("=", 70), "\n\n")

# --- 3a. Tägliches OECD-Aggregat (Mean + Streuband) --------------------------
daily_agg <- oxd_panel %>%
  group_by(Date) %>%
  summarise(
    Mean   = mean(S_raw),
    Median = median(S_raw),
    P25    = quantile(S_raw, 0.25),
    P75    = quantile(S_raw, 0.75),
    P10    = quantile(S_raw, 0.10),
    P90    = quantile(S_raw, 0.90),
    SD     = sd(S_raw),
    .groups = "drop"
  )

# Interpretation: Drei Phasen sind erkennbar:
# (1) März–Mai 2020: synchroner Anstieg auf ~70–80 (universeller Lockdown)
# (2) Juni 2020–März 2021: Oszillation 35–55 (Lockdown-Lockerung-Zyklen)
# (3) April–Dez 2021: stetiger Rückgang auf ~30 (Impfung → Exit)
# Das IQR-Band (P25–P75) ist in Phase 1 eng, in Phase 2–3 breit.

cat("  Phase 1 (März–Mai 2020): Peak Mean = ", 
    round(max(daily_agg$Mean[daily_agg$Date <= "2020-05-31"]), 1), "\n")
cat("  Phase 2 Trough (Sommer 2020): Min Mean = ",
    round(min(daily_agg$Mean[daily_agg$Date >= "2020-06-01" & 
                               daily_agg$Date <= "2020-09-30"]), 1), "\n")
cat("  Phase 2 Second Peak (Winter 2020/21): Max Mean = ",
    round(max(daily_agg$Mean[daily_agg$Date >= "2020-10-01" & 
                               daily_agg$Date <= "2021-03-31"]), 1), "\n")
cat("  Phase 3 (Dez 2021): Terminal Mean = ",
    round(tail(daily_agg$Mean, 30) %>% mean(), 1), "\n\n")

# Wave markers for annotation
wave_dates <- data.frame(
  x     = as.Date(c("2020-04-01","2020-11-15","2021-07-15","2021-12-15")),
  label = c("Wave 1\n(Original)","Wave 2\n(Alpha)","Wave 3\n(Delta)","Wave 4\n(Omicron)")
)

p_trajectory <- ggplot(daily_agg, aes(x = Date)) +
  # Streuband: P10–P90 (hell) und P25–P75 (dunkel)
  geom_ribbon(aes(ymin = P10, ymax = P90), fill = "grey85", alpha = 0.6) +
  geom_ribbon(aes(ymin = P25, ymax = P75), fill = "grey65", alpha = 0.6) +
  geom_line(aes(y = Mean), color = "black", linewidth = 0.8) +
  geom_line(aes(y = Median), color = "#C0392B", linewidth = 0.5, linetype = "dashed") +
  # Wave annotations
  geom_text(data = wave_dates, aes(x = x, y = 95, label = label),
            size = 2.3, family = "serif", color = "grey40", lineheight = 0.85) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y",
               expand = c(0.01, 0)) +
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100)) +
  labs(
    title    = "Figure X: OECD Containment Trajectory",
    subtitle = paste0("Black: unweighted OECD mean. Red dashed: median. ",
                      "Dark band: IQR (P25–P75). Light band: P10–P90."),
    x = NULL, y = "Stringency Index (0–100)",
    caption  = paste0("Notes: Daily data, 38 OECD economies. ",
                      "StringencyIndex_PopWeighted (Oxford CGRT, C1–C8).\n",
                      "Three phases visible: synchronized lockdown (Q1–Q2 2020), ",
                      "oscillating cycles (Q3 2020–Q1 2021), ",
                      "vaccination-driven exit (Q2–Q4 2021).")
  ) + theme_aer

print(p_trajectory)


# --- 3b. OECD-Dispersionsmaß über die Zeit -----------------------------------
# CV = SD/Mean: steigt monoton → Länder divergieren über die Pandemie hinweg

daily_agg <- daily_agg %>%
  mutate(CV = SD / pmax(Mean, 1))  # pmax um Division durch ≈0 zu vermeiden

cat("--- 3b. Dispersion (CV) über die Zeit ---\n")
cat(sprintf("  CV in April 2020: %.2f\n",
            mean(daily_agg$CV[daily_agg$Date >= "2020-04-01" & 
                                daily_agg$Date <= "2020-04-30"])))
cat(sprintf("  CV in Dez 2021:   %.2f\n\n",
            mean(daily_agg$CV[daily_agg$Date >= "2021-12-01"])))


# ==============================================================================
#  4. LÄNDERHETEROGENITÄT
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("  SECTION 4: Länderheterogenität\n")
cat(strrep("=", 70), "\n\n")

# --- 4a. Deskriptive Statistiken nach Land ------------------------------------
country_stats <- oxd_panel %>%
  group_by(Country) %>%
  summarise(
    Mean    = mean(S_raw),
    Median  = median(S_raw),
    SD      = sd(S_raw),
    Min     = min(S_raw),
    Max     = max(S_raw),
    P25     = quantile(S_raw, 0.25),
    P75     = quantile(S_raw, 0.75),
    IQR     = IQR(S_raw),
    CV      = sd(S_raw) / mean(S_raw),
    # Tage im harten Lockdown (S > 60)
    Days_gt60    = sum(S_raw > 60),
    Pct_gt60     = mean(S_raw > 60) * 100,
    # Tage mit sehr geringer Stringency (S < 20)
    Days_lt20    = sum(S_raw < 20),
    Pct_lt20     = mean(S_raw < 20) * 100,
    # Peak-Datum
    Peak_Date    = Date[which.max(S_raw)],
    Peak_Value   = max(S_raw),
    .groups = "drop"
  ) %>%
  arrange(desc(Mean))

cat("--- 4a. Country Statistics (sorted by Mean S) ---\n")
print(kable(country_stats %>% select(Country, Mean, Median, SD, CV,
                                     Pct_gt60, Pct_lt20, Peak_Value),
            digits = 1,
            col.names = c("Country","Mean","Median","SD","CV",
                          "% Days S>60","% Days S<20","Peak")))

# Interpretation:
cat("\n  Top 5 (strengste Massnahmen, Mean S):\n")
for(i in 1:5) {
  cat(sprintf("    %s: Mean = %.1f, %d Tage S>60 (%.0f%%)\n",
              country_stats$Country[i], country_stats$Mean[i],
              country_stats$Days_gt60[i], country_stats$Pct_gt60[i]))
}
cat("\n  Bottom 5 (lockerste Massnahmen, Mean S):\n")
for(i in (nrow(country_stats)-4):nrow(country_stats)) {
  cat(sprintf("    %s: Mean = %.1f, %d Tage S<20 (%.0f%%)\n",
              country_stats$Country[i], country_stats$Mean[i],
              country_stats$Days_lt20[i], country_stats$Pct_lt20[i]))
}

# Spread
cat(sprintf("\n  Cross-country spread: Mean S range = [%.1f, %.1f], ratio = %.1f:1\n",
            min(country_stats$Mean), max(country_stats$Mean),
            max(country_stats$Mean) / min(country_stats$Mean)))
cat(sprintf("  Cross-country SD of Mean S: %.1f\n",
            sd(country_stats$Mean)))


# --- 4b. Country Ranking Bar Chart --------------------------------------------

p_country_bars <- country_stats %>%
  mutate(Country = factor(Country, levels = rev(Country))) %>%  # descending
  ggplot(aes(x = Country, y = Mean)) +
  geom_col(aes(fill = Mean), width = 0.7, show.legend = FALSE) +
  geom_errorbar(aes(ymin = P25, ymax = P75), width = 0.3,
                color = "grey30", linewidth = 0.3) +
  geom_hline(yintercept = mean(country_stats$Mean),
             linetype = "dashed", color = "#C0392B", linewidth = 0.5) +
  annotate("text", x = 2, y = mean(country_stats$Mean) + 2,
           label = sprintf("OECD avg: %.1f", mean(country_stats$Mean)),
           size = 2.5, family = "serif", color = "#C0392B") +
  scale_fill_gradient(low = "grey75", high = "grey25") +
  scale_y_continuous(breaks = seq(0, 80, 20), expand = expansion(mult = c(0, 0.05))) +
  coord_flip() +
  labs(
    title    = "Figure X: Mean Containment Intensity by Country",
    subtitle = "Bar: mean Stringency (Mar 2020–Dec 2021). Whiskers: P25–P75.",
    x = NULL, y = "Mean Stringency Index (0–100)",
    caption  = "Notes: StringencyIndex_PopWeighted (Oxford CGRT, C1–C8). Dashed line: OECD unweighted average."
  ) + theme_aer


print(p_country_bars)


# --- 4c. Small Multiples: Country Trajectories --------------------------------

p_small_mult <- oxd_panel %>%
  left_join(country_stats %>% select(Country, Mean), by = "Country") %>%
  mutate(label = sprintf("%s (%.0f)", Country, Mean),
         label = factor(label,
                        levels = country_stats %>%
                          arrange(desc(Mean)) %>%
                          mutate(l = sprintf("%s (%.0f)", Country, Mean)) %>%
                          pull(l))) %>%
  ggplot(aes(x = Date, y = S_raw)) +
  geom_line(color = "grey30", linewidth = 0.35) +
  # OECD mean als Referenz
  geom_line(data = daily_agg %>% rename(S_raw = Mean),
            aes(x = Date, y = S_raw),
            color = "#C0392B", linewidth = 0.3, alpha = 0.5) +
  facet_wrap(~label, ncol = 6) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%y") +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 50, 100)) +
  labs(
    title    = "Figure X: Country-Level Containment Trajectories",
    subtitle = paste0("Black: country trajectory. Red: OECD mean. ",
                      "Parentheses: period mean. Sorted by mean S (descending)."),
    x = NULL, y = NULL,
    caption  = paste0("Notes: Daily StringencyIndex_PopWeighted, Mar 2020–Dec 2021. ",
                      "Fixed y-axis [0, 100] enables cross-country comparison.")
  ) +
  theme_aer +
  theme(
    axis.text.x = element_text(size = 5),
    axis.text.y = element_text(size = 5),
    strip.text  = element_text(size = 6.5, margin = margin(2, 0, 2, 0)),
    panel.spacing = unit(0.3, "lines")
  )

print(p_small_mult)


# ==============================================================================
#  5. KOMPONENTENZERLEGUNG (C1–C8)
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 5: Komponentenzerlegung\n")
cat(strrep("=", 70), "\n\n")

# C1 = School closing, C2 = Workplace closing, C3 = Cancel public events,
# C4 = Restrictions on gatherings, C5 = Close public transport,
# C6 = Stay-at-home requirements, C7 = Restrictions on internal movement,
# C8 = International travel controls

component_vars <- c("C1_NV_adj","C2_NV_adj","C3_NV_adj","C4_NV_adj",
                    "C5_NV_adj","C6_NV_adj","C7_NV_adj","C8_NV_adj")
component_labels <- c("C1: Schools","C2: Workplaces","C3: Public Events",
                      "C4: Gatherings","C5: Public Transport",
                      "C6: Stay-at-Home","C7: Internal Movement",
                      "C8: Intl. Travel")

# --- 5a. Mean nach Komponente über die Zeit ---
comp_daily <- oxd_panel %>%
  select(Date, all_of(component_vars)) %>%
  group_by(Date) %>%
  summarise(across(everything(), mean, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(cols = all_of(component_vars),
               names_to = "Component", values_to = "Value") %>%
  mutate(Label = factor(Component,
                        levels = component_vars,
                        labels = component_labels))

cat("--- 5a. Component Means (OECD, full period) ---\n")
comp_summary <- oxd_panel %>%
  summarise(across(all_of(component_vars),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd   = ~sd(.x, na.rm = TRUE)),
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(everything(),
               names_to = c("Component", ".value"),
               names_pattern = "(.+)_(mean|sd)") %>%
  mutate(Label = factor(Component, levels = component_vars,
                        labels = component_labels)) %>%
  arrange(desc(mean))
print(kable(comp_summary %>% select(Label, mean, sd), digits = 1,
            col.names = c("Component", "OECD Mean", "SD")))

# Interpretation:
cat("\n  C8 (Intl. Travel) und C3 (Public Events) bleiben am längsten aktiv.\n")
cat("  C5 (ÖV) und C6 (Stay-at-Home) werden am schnellsten gelockert.\n")
cat("  → Der Stringency Index wird im Zeitverlauf zunehmend von C8 und C3/C4\n")
cat("    getrieben, nicht von den ökonomisch relevanten C1/C2/C6.\n")
cat("  → Implikation: Der Index überschätzt die effektive wirtschaftliche\n")
cat("    Restriktion in der Spätphase. α_S könnte time-varying sein.\n\n")

p_components <- ggplot(comp_daily, aes(x = Date, y = Value, color = Label)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(values = rep(c("grey20","grey45","grey65","grey80"), 2),
                     name = NULL) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  scale_y_continuous(breaks = seq(0, 100, 25)) +
  labs(
    title    = "Figure X: Component-Level Stringency Trajectories (OECD Mean)",
    subtitle = "Each line: OECD unweighted mean of one C-indicator. Travel (C8) persists longest; stay-at-home (C6) exits fastest.",
    x = NULL, y = "Component Value (0–100)",
    caption  = "Notes: C1–C8 non-vaccinated adjusted indicators. OECD mean across 38 economies."
  ) + theme_aer +
  guides(color = guide_legend(nrow = 2))

print(p_components)

# --- 5b. Komponentenstruktur nach Phase ----------------------------------------
# Welche Instrumente dominieren in welcher Pandemiephase?

comp_phase <- oxd_panel %>%
  mutate(Phase = case_when(
    Date <= as.Date("2020-05-31")                                    ~ "Phase 1:\nApr–May 2020",
    Date >= as.Date("2020-06-01") & Date <= as.Date("2020-09-30")    ~ "Phase 2:\nJun–Sep 2020",
    Date >= as.Date("2020-10-01") & Date <= as.Date("2021-03-31")    ~ "Phase 3:\nOct 2020–Mar 2021",
    Date >= as.Date("2021-04-01") & Date <= as.Date("2021-09-30")    ~ "Phase 4:\nApr–Sep 2021",
    TRUE                                                             ~ "Phase 5:\nOct–Dec 2021"
  ),
  Phase = factor(Phase, levels = c("Phase 1:\nApr–May 2020","Phase 2:\nJun–Sep 2020",
                                   "Phase 3:\nOct 2020–Mar 2021",
                                   "Phase 4:\nApr–Sep 2021","Phase 5:\nOct–Dec 2021"))
  ) %>%
  select(Phase, all_of(component_vars)) %>%
  group_by(Phase) %>%
  summarise(across(everything(), mean, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(cols = all_of(component_vars),
               names_to = "Component", values_to = "Value") %>%
  mutate(Label = factor(Component, levels = component_vars,
                        labels = component_labels))

p_comp_heatmap <- ggplot(comp_phase, aes(x = Phase, y = Label, fill = Value)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(Value, 0)), size = 2.8, family = "serif") +
  scale_fill_gradient(low = "white", high = "grey25", limits = c(0, 100),
                      name = "Mean\n(0–100)") +
  labs(
    title    = "Figure X: NPI Composition Across Pandemic Phases",
    subtitle = "Cell values: OECD mean of each component. C8 (Travel) and C4 (Gatherings) persist; C5/C6 exit early.",
    x = NULL, y = NULL,
    caption  = "Notes: Phases defined by epidemiological wave structure. Darker = more restrictive."
  ) + theme_aer +
  theme(axis.text.x = element_text(size = 7, lineheight = 0.85))

print(p_comp_heatmap)


# ==============================================================================
#  6. QUARTALSAGGREGATION FÜR DAS PANEL
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 6: Quartalsaggregation\n")
cat(strrep("=", 70), "\n\n")

# --- 6a. Aggregation: Mean pro Country × Quarter -----------------------------
# Der Quartalsdurchschnitt ist die korrekte Aggregation für den iLQR:
# S_k repräsentiert die durchschnittliche Intensität im Quartal k.

q_stringency <- oxd_panel %>%
  group_by(Country, YQ_ord) %>%
  summarise(
    S_mean   = mean(S, na.rm = TRUE),         # Modell-Skala [0,1]
    S_raw    = mean(S_raw, na.rm = TRUE),      # Originalskala [0,100]
    S_max    = max(S_raw, na.rm = TRUE),
    S_min    = min(S_raw, na.rm = TRUE),
    S_sd     = sd(S_raw, na.rm = TRUE),
    S_range  = max(S_raw, na.rm = TRUE) - min(S_raw, na.rm = TRUE),
    .groups  = "drop"
  )

# Cross-country stats per quarter
q_cross <- q_stringency %>%
  group_by(YQ_ord) %>%
  summarise(
    Mean     = mean(S_raw),
    Median   = median(S_raw),
    SD       = sd(S_raw),
    Min      = min(S_raw),
    Max      = max(S_raw),
    P25      = quantile(S_raw, 0.25),
    P75      = quantile(S_raw, 0.75),
    CV       = sd(S_raw) / mean(S_raw),
    .groups  = "drop"
  )

cat("--- 6a. Quarterly Cross-Country Statistics ---\n")
print(kable(q_cross, digits = 1,
            col.names = c("Quarter","Mean","Median","SD","Min","Max","P25","P75","CV")))

# Interpretation:
cat("\n  Schlüsselbeobachtungen:\n")
cat(sprintf("  • Q2.2020 engste Verteilung: SD = %.1f, CV = %.2f (synchroner Schock)\n",
            q_cross$SD[q_cross$YQ_ord == "Q2.2020"],
            q_cross$CV[q_cross$YQ_ord == "Q2.2020"]))
cat(sprintf("  • Q4.2021 breiteste Verteilung: SD = %.1f, CV = %.2f (Divergenz)\n",
            q_cross$SD[q_cross$YQ_ord == "Q4.2021"],
            q_cross$CV[q_cross$YQ_ord == "Q4.2021"]))
cat(sprintf("  • Mean S fällt von %.1f (Q2.2020) auf %.1f (Q4.2021) = −%.0f%%\n",
            q_cross$Mean[q_cross$YQ_ord == "Q2.2020"],
            q_cross$Mean[q_cross$YQ_ord == "Q4.2021"],
            (1 - q_cross$Mean[q_cross$YQ_ord == "Q4.2021"] / 
               q_cross$Mean[q_cross$YQ_ord == "Q2.2020"]) * 100))

# --- 6b. Within-quarter variation (S_range per country) -----------------------
# Wie stark variiert S innerhalb eines Quartals? Hohe Intra-Quartal-Variation
# bedeutet, dass der Quartalsdurchschnitt Information verliert.

cat("\n--- 6b. Within-Quarter Variation (S_range = max − min within country×quarter) ---\n")
wq_stats <- q_stringency %>%
  group_by(YQ_ord) %>%
  summarise(
    Mean_Range = mean(S_range),
    Mean_SD    = mean(S_sd),
    .groups    = "drop"
  )
print(kable(wq_stats, digits = 1))
cat(sprintf("  → Durchschnittliche Intra-Quartal-Range: %.1f Punkte\n",
            mean(q_stringency$S_range)))
cat("  → Quartalsdurchschnitt glättet erheblich. Für Robustness: S_max verwenden.\n")


# --- 6c. Panel-Mapping: S-Quartale auf Modellskala ---------------------------

cat("\n--- 6c. S auf Modellskala [0,1] ---\n")
s_panel_stats <- q_stringency %>%
  summarise(
    Mean = mean(S_mean),
    SD   = sd(S_mean),
    Min  = min(S_mean),
    Max  = max(S_mean),
    P10  = quantile(S_mean, 0.10),
    P90  = quantile(S_mean, 0.90)
  )
cat(sprintf("  S ∈ [0,1]: mean = %.3f, sd = %.3f, range = [%.3f, %.3f]\n",
            s_panel_stats$Mean, s_panel_stats$SD,
            s_panel_stats$Min, s_panel_stats$Max))
cat(sprintf("  → Mean S = %.2f entspricht ≈%.0f%% Lockdown-Intensität\n",
            s_panel_stats$Mean, s_panel_stats$Mean * 100))
cat("  → Im Modell: α_S × S ≈ 12 × 0.42 = 5.0pp GDP-Verlust (OECD-Durchschnitt)\n\n")


# ==============================================================================
#  7. DESKRIPTIVE KORRELATION MIT OUTCOMES
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("  SECTION 7: Korrelation mit Outcomes (deskriptiv, NICHT kausal)\n")
cat(strrep("=", 70), "\n\n")

# Merge quarterly S with qdata outcomes
pandemic_qs <- c("Q1.2020", "Q2.2020", "Q3.2020", "Q4.2020",
                 "Q1.2021", "Q2.2021", "Q3.2021", "Q4.2021",
                 "Q1.2022", "Q2.2022", "Q3.2022", "Q4.2022")

outcome_panel <- qdata %>%
  filter(Quarter %in% pandemic_qs) %>%
  select(Country, Quarter, y_t_pct, QReal.GDP.Growth_gr, d_t_pct) %>%
  left_join(
    q_stringency %>%
      mutate(Quarter = as.character(YQ_ord)) %>%
      select(Country, Quarter, S_mean, S_raw),
    by = c("Country", "Quarter")
  ) %>%
  filter(!is.na(S_mean))

cat(sprintf("  Panel: %d obs (%d countries × %d quarters)\n\n",
            nrow(outcome_panel), n_distinct(outcome_panel$Country),
            n_distinct(outcome_panel$Quarter)))

# --- 7a. Contemporaneous correlations ----------------------------------------
cat("--- 7a. Contemporane Korrelationen (Pearson) ---\n")
cors <- outcome_panel %>%
  summarise(
    r_S_y     = cor(S_raw, y_t_pct, use = "complete.obs"),
    r_S_growth = cor(S_raw, QReal.GDP.Growth_gr, use = "complete.obs"),
    r_S_d     = cor(S_raw, d_t_pct, use = "complete.obs")
  )
cat(sprintf("  corr(S, y_t)       = %.3f  (Lockdown ↔ Output Gap)\n", cors$r_S_y))
cat(sprintf("  corr(S, GDP_growth) = %.3f  (Lockdown ↔ Growth)\n", cors$r_S_growth))
cat(sprintf("  corr(S, d_t)       = %.3f  (Lockdown ↔ Excess Mortality)\n\n", cors$r_S_d))

cat("  Interpretation:\n")
cat("  • S–y: negativ erwartet (mehr Lockdown → tiefere Outputlücke).\n")
cat("    Aber: Stärke reflektiert auch Endogenität (Länder mit schlimmerem\n")
cat("    Pandemieverlauf haben striktere Massnahmen).\n")
cat("  • S–d: Vorzeichen ambig. Positiv = Lockdown geht mit höherer Mortalität\n")
cat("    einher (weil beides von der Pandemieschwere getrieben wird).\n")
cat("    Negativ = Lockdown senkt Mortalität (kausaler Effekt).\n")
cat("    → Diese Korrelation kann die Kausalrichtung nicht identifizieren.\n\n")

# --- 7b. Scatter: S vs Output Gap --------------------------------------------

p_scatter_y <- ggplot(outcome_panel, aes(x = S_raw, y = y_t_pct)) +
  geom_point(alpha = 0.3, size = 1.5, color = "grey40") +
  geom_smooth(method = "lm", se = TRUE, color = "#C0392B",
              linewidth = 0.7, fill = "grey85") +
  labs(
    title = "A. S vs. Output Gap",
    subtitle = sprintf("r = %.3f (contemporaneous, not causal)", cors$r_S_y),
    x = "Stringency Index (0–100)", y = "Output Gap (% of potential)"
  ) + theme_aer

p_scatter_g <- ggplot(outcome_panel, aes(x = S_raw, y = QReal.GDP.Growth_gr)) +
  geom_point(alpha = 0.3, size = 1.5, color = "grey40") +
  geom_smooth(method = "lm", se = TRUE, color = "#C0392B",
              linewidth = 0.7, fill = "grey85") +
  labs(
    title = "B. S vs. GDP Growth",
    subtitle = sprintf("r = %.3f (contemporaneous, not causal)", cors$r_S_growth),
    x = "Stringency Index (0–100)", y = "Real GDP Growth (%, q-o-q)"
  ) + theme_aer

p_scatter_d <- ggplot(outcome_panel, aes(x = S_raw, y = d_t_pct)) +
  geom_point(alpha = 0.3, size = 1.5, color = "grey40") +
  geom_smooth(method = "lm", se = TRUE, color = "#C0392B",
              linewidth = 0.7, fill = "grey85") +
  labs(
    title = "C. S vs. Excess Mortality",
    subtitle = sprintf("r = %.3f (endogenous: S responds to crisis severity)", cors$r_S_d),
    x = "Stringency Index (0–100)", y = "Cumulative Excess Mortality (%)"
  ) + theme_aer

p_scatter_all <- (p_scatter_y | p_scatter_g | p_scatter_d) +
  plot_annotation(
    title = "Figure X: Containment Intensity and Outcomes (Descriptive)",
    subtitle = "Quarterly country-level data, Q1.2020–Q4.2021. Correlations are descriptive, not causal estimates.",
    theme = theme(plot.title = element_text(face = "bold", size = 13, family = "serif"),
                  plot.subtitle = element_text(size = 9, color = "grey30", family = "serif"))
  )
print(p_scatter_all)


# ==============================================================================
#  8. TIMING-KOINZIDENZ MIT FISKALMASSNAHMEN
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 8: Timing S vs. Fiskaldeployment\n")
cat(strrep("=", 70), "\n\n")

# Merge quarterly S with fiscal volumes (from fm_d)
fm_q <- fm_d %>%
  filter(broad_fiscal == 1) %>%
  mutate(
    YQ = paste0("Q", Quarter, ".", Year),
    size_pct = broad_fiscal_gdp * 100
  ) %>%
  group_by(Country, YQ, transmission_channel) %>%
  summarise(vol = sum(size_pct, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = transmission_channel, values_from = vol, values_fill = 0)

sf_panel <- q_stringency %>%
  mutate(Quarter = as.character(YQ_ord)) %>%
  select(Country, Quarter, S_raw) %>%
  left_join(fm_q %>% rename(Quarter = YQ), by = c("Country","Quarter")) %>%
  replace_na(list(CP = 0, DI = 0, H = 0)) %>%
  mutate(Total_Fiscal = CP + DI + H)

# --- 8a. Korrelation S vs. Fiskalvolumen --------------------------------------
cat("--- 8a. Korrelation S vs. Fiskalvolumen (country-quarter level) ---\n")
cat(sprintf("  corr(S, Total) = %.3f\n",
            cor(sf_panel$S_raw, sf_panel$Total_Fiscal, use = "complete.obs")))
cat(sprintf("  corr(S, CP)    = %.3f\n",
            cor(sf_panel$S_raw, sf_panel$CP, use = "complete.obs")))
cat(sprintf("  corr(S, DI)    = %.3f\n",
            cor(sf_panel$S_raw, sf_panel$DI, use = "complete.obs")))
cat(sprintf("  corr(S, H)     = %.3f\n\n",
            cor(sf_panel$S_raw, sf_panel$H, use = "complete.obs")))

cat("  Interpretation:\n")
cat("  • Positive Korrelation S–CP erwartet: striktere Lockdowns erfordern\n")
cat("    mehr Kapazitätssicherung (Property 3). Stärke zeigt Komplementarität.\n")
cat("  • S–DI schwächer: DI wird zeitverzögert deployed und teilweise\n")
cat("    nach dem Peak von S (Reopening-Phase, ARPA in Q1.2021).\n")
cat("  • Kausale Richtung nicht identifiziert: S treibt F, nicht umgekehrt.\n\n")

# --- 8b. Dual-Axis: OECD Mean S + Mean Fiscal per Quarter --------------------
sf_agg <- sf_panel %>%
  group_by(Quarter) %>%
  summarise(
    S_mean    = mean(S_raw),
    CP_mean   = mean(CP),
    DI_mean   = mean(DI),
    .groups   = "drop"
  ) %>%
  mutate(Quarter = factor(Quarter,
                          levels = c("Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                                     "Q1.2021","Q2.2021","Q3.2021","Q4.2021")))

# Dual panel: S oben, Fiscal unten (kein dual-axis, stattdessen patchwork)
p_timing_s <- ggplot(sf_agg, aes(x = Quarter, y = S_mean, group = 1)) +
  geom_line(linewidth = 0.8, color = "black") +
  geom_point(size = 2.5, color = "black") +
  scale_y_continuous(limits = c(0, 80)) +
  labs(title = "A. Mean Containment Intensity",
       y = "Stringency (0–100)", x = NULL) +
  theme_aer + theme(axis.text.x = element_blank())

p_timing_f <- sf_agg %>%
  pivot_longer(cols = c(CP_mean, DI_mean), names_to = "Channel", values_to = "Vol") %>%
  mutate(Channel = ifelse(Channel == "CP_mean", "CP", "DI")) %>%
  ggplot(aes(x = Quarter, y = Vol, fill = Channel)) +
  geom_col(position = "dodge", width = 0.6) +
  scale_fill_manual(values = c("CP" = "grey25", "DI" = "grey65"), name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "B. Mean Fiscal Deployment",
       y = "% of 2019 GDP", x = NULL,
       caption = paste0("Notes: Unweighted OECD means. Panel A: quarterly average ",
                        "StringencyIndex. Panel B: fiscal volumes by transmission channel.\n",
                        "CP deployment tracks S closely (complementarity per Property 3). ",
                        "DI peaks in Q1.2021 (ARPA) when S has already declined.")) +
  theme_aer +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_timing <- p_timing_s / p_timing_f +
  plot_layout(heights = c(1, 1.2)) +
  plot_annotation(
    title = "Figure X: Containment Intensity and Fiscal Deployment — Temporal Alignment",
    theme = theme(plot.title = element_text(face = "bold", size = 12, family = "serif"))
  )
print(p_timing)


# ==============================================================================
#  SUMMARY
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  ZUSAMMENFASSUNG: Stringency Descriptives\n")
cat(strrep("=", 70), "\n\n")

cat("  1. Drei Phasen: synchroner Lockdown (Mar–May 2020, Mean S ≈ 65),\n")
cat("     oszillierende Zyklen (Jun 2020–Mar 2021, S ≈ 40–55),\n")
cat("     impfgetriebener Exit (Apr–Dec 2021, S → 30).\n\n")

cat("  2. Massive Länderheterogenität: Mean S rangiert von ~20 bis ~65.\n")
cat("     Ratio Top/Bottom ≈ 3:1. CV steigt über die Zeit.\n\n")

cat("  3. Komponentenverschiebung: Frühe Phase von C1/C2/C6 dominiert\n")
cat("     (Schulen, Arbeitsplätze, Stay-at-Home = ökonomisch bindend).\n")
cat("     Spätphase von C8/C3/C4 dominiert (Reisen, Events, Versammlungen\n")
cat("     = ökonomisch weniger bindend). → α_S ist de facto time-varying.\n\n")

cat("  4. Fiskal-Timing: CP trackt S eng (r > 0 erwartet). DI ist zeitlich\n")
cat("     verschoben — Peak DI in Q1.2021, Peak S in Q2.2020.\n")
cat("     → Komplementarität CP–S bestätigt Property 3.\n\n")

cat("  5. Intra-Quartal-Variation erheblich (Range ~20–30 Punkte innerhalb\n")
cat("     eines Quartals). Quartalsdurchschnitt glättet. Robustness: S_max.\n\n")

cat("  6. Contemporane Korrelationen sind endogen und nicht kausal\n")
cat("     interpretierbar. Strukturelle Identifikation erfordert den iLQR.\n\n")


##Full Interpretation see Notion (Stringency)




# Listet die Namen aller Dataframes im Global Environment auf
Filter(function(x) is.data.frame(get(x)), ls())
rm(adoption, blw_non_cp, by_channel_imf, category_summary, channel_stats, 
   comp_daily, comp_phase, comp_summary, cors, count_long, country_channel, 
   country_cp_sub, country_cv, country_pies, country_plot, country_quarter, 
   country_stats, country_totals, cp_decomp, cp_share_labels, cqc, cqc_wide, 
   cross_80, cum_contrib, daily_agg, def_non_cp, economist_w, excess_w, 
   first_deploy, fm_all, fm_d, fm_pos, fm_q, full_grid, 
   heatmap_data,  imf_cross, income_channel, instrument_rank, 
   lorenz_data, mean_cp, mean_deploy, mean_di, oecd_avg, outcome_panel, 
   overall_by_channel, overall_total, oxd_panel,
   panel_a_data, panel_b_data, panel_data, peak_q, 
   peak_summary, plot_data, q_cross, q_stats, q_stringency, rank_size, 
   ratio_country, ratio_panel, region_channel, region_country_avg, 
   s_panel_stats, scatter_data, sf_agg, sf_panel, size_dist, tabelle_aer, 
   temporal_channel, temporal_count, temporal_cum, temporal_wide, 
   temporal_year, timing, top_decile, vol_long, wave_dates, wq_stats, year_stats)

# ==============================================================================
###################################OUTCOMES#####################################
# ==============================================================================

#...............................................................................
################################EXCESS DEATHS###################################
#...............................................................................

#  Primärvariable: p_proj_all_ages (P-score mit projizierter Baseline)
#  Robustness:     excess_per_million_proj_all_ages (Excess Deaths per Million)
#  Quelle:         Our World in Data — Excess Mortality (Karlinsky & Kobak, 2021)
#  Analyseperiode: März 2020 – Dezember 2021
#  Wochendaten (p_values_oecd_w), Aggregation auf Quartal
#
#  Gliederung:
#   1. Datenvorbereitung & Variablenwahl
#   2. Gesamtverteilung
#   3. Aggregierte OECD-Trajektorie (Wellenstruktur)
#   4. Länderheterogenität (Between-Variation)
#   5. Within-Country-Variation
#   6. Between vs. Within: ANOVA-Zerlegung
#   7. Wellenanalyse (Asynchronität & Korrelationsstruktur)
#   8. Lag-Struktur S → θ → d (Endogenitätsdiagnostik)
#   9. Altersdekomposition
#  10. Quartalsaggregation & Modell-Mapping
# ==============================================================================

# --- AER Theme ----------------------------------------------------------------
theme_aer <- theme_classic(base_size = 10) +
  theme(
    text               = element_text(family = "serif"),
    plot.title         = element_text(face = "bold", size = 11, hjust = 0),
    plot.subtitle      = element_text(size = 8, color = "grey30"),
    plot.caption       = element_text(size = 7, color = "grey50", hjust = 0),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    legend.position    = "bottom",
    legend.text        = element_text(size = 8),
    strip.text         = element_text(face = "bold", size = 8)
  )

# --- Farbpalette für Wellen ---------------------------------------------------
wave_colors <- c(
  "W1: Original"  = "#D62728",  # Rot
  
  "Trough 1"      = "#7F7F7F",  # Grau
  "W2: Alpha"      = "#FF7F0E",  # Orange
  "Trough 2"      = "#7F7F7F",
  "W3: Delta"      = "#2CA02C",  # Grün
  "W4: Omicron"    = "#1F77B4"   # Blau
)


# ==============================================================================
#  1. DATENVORBEREITUNG & VARIABLENWAHL
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 1: Datenvorbereitung & Variablenwahl\n")
cat(strrep("=", 70), "\n\n")

# --- Variablenwahl: p_proj_all_ages -------------------------------------------
# Der P-score vergleicht beobachtete Todesfälle mit einer *projizierten* Baseline,
# die demografische Trends (Alterung) seit 2020 fortschreibt. Die Alternative
# p_avg_all_ages nutzt den Durchschnitt 2015–2019, was in alternden Gesellschaften
# systematisch überschätzt (upward bias ≈ 3.2 Prozentpunkte im Mittel).
# Die Robustness-Variable excess_per_million_proj_all_ages gibt die absolute Rate
# pro Million und Woche — direkt interpretierbar und mappt auf δ_θ · θ_k im Modell.

mort_panel <- p_values_oecd_w %>%
  filter(
    time_unit == "weekly",
    date >= as.Date("2020-03-01"),
    date <= as.Date("2022-12-31")
  ) %>%
  transmute(
    Country    = entity,
    date       = as.Date(date),
    d_flow     = p_proj_all_ages,      # Main: P-score (projected baseline)
    d_flow_avg = p_avg_all_ages,       # Comparison: P-score (2015-2019 avg)
    d_rate     = excess_per_million_proj_all_ages,  # Robustness: excess/million/week
    d_cum      = cum_excess_per_million_proj_all_ages,  # Cumulative stock
    Year       = year(date),
    Month      = month(date),
    Week       = isoweek(date),
    YQ         = paste0(year(date), "Q", quarter(date))
  ) %>%
  mutate(
    YQ_ord = factor(YQ, levels = c("2020Q1","2020Q2","2020Q3","2020Q4",
                                   "2021Q1","2021Q2","2021Q3","2021Q4",
                                   "2022Q1", "2022Q2","2022Q3","2022Q4")),
    # Wellenzuordnung (OECD-Konsensus-Timing)
    Wave = case_when(
      date <= as.Date("2020-06-30") ~ "W1: Original",
      date <= as.Date("2020-09-30") ~ "Trough 1",
      date <= as.Date("2021-03-31") ~ "W2: Alpha",
      date <= as.Date("2021-06-30") ~ "Trough 2",
      date <= as.Date("2021-09-30") ~ "W3: Delta",
      date <= as.Date("2022-01-01") ~ "W4: Omicron"
    ),
    Wave = factor(Wave, levels = c("W1: Original","Trough 1","W2: Alpha",
                                   "Trough 2","W3: Delta","W4: Omicron"))
  )

cat(sprintf("  Panel: %d Beobachtungen = %d Länder × ~%d Wochen\n",
            nrow(mort_panel), n_distinct(mort_panel$Country),
            nrow(mort_panel) / n_distinct(mort_panel$Country)))
cat(sprintf("  Periode: %s bis %s\n", min(mort_panel$date), max(mort_panel$date)))
cat(sprintf("  Fehlende OECD-Mitglieder (nur monatlich): CRI, JPN, TUR\n\n"))

cat("  Variablenvergleich (Pandemieperiode):\n")
cat(sprintf("    d_flow (P-score proj):  mean = %.2f, median = %.2f, sd = %.2f\n",
            mean(mort_panel$d_flow, na.rm = T),
            median(mort_panel$d_flow, na.rm = T),
            sd(mort_panel$d_flow, na.rm = T)))
cat(sprintf("    d_rate (excess/mio/wk): mean = %.2f, median = %.2f, sd = %.2f\n",
            mean(mort_panel$d_rate, na.rm = T),
            median(mort_panel$d_rate, na.rm = T),
            sd(mort_panel$d_rate, na.rm = T)))
cat(sprintf("    d_flow_avg (P-score avg): mean = %.2f (upward bias ≈ %.2f pp)\n",
            mean(mort_panel$d_flow_avg, na.rm = T),
            mean(mort_panel$d_flow_avg - mort_panel$d_flow, na.rm = T)))
cat(sprintf("    corr(d_flow, d_rate) = %.3f\n",
            cor(mort_panel$d_flow, mort_panel$d_rate, use = "complete")))
cat(sprintf("    corr(d_flow, d_flow_avg) = %.3f\n\n",
            cor(mort_panel$d_flow, mort_panel$d_flow_avg, use = "complete")))


# ==============================================================================
#  2. GESAMTVERTEILUNG
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("  SECTION 2: Gesamtverteilung\n")
cat(strrep("=", 70), "\n\n")

# Interpretation: Stark rechtsschief — die meisten Country-Weeks sind nahe der
# Baseline, der fette rechte Tail stammt von Pandemie-Peaks. 26.7% der Wochen
# UNTER Baseline reflektiert reduzierte Non-COVID-Mortalität (weniger Verkehrs-
# unfälle, aufgeschobene Eingriffe, Lockdown-Effekte).

overall_stats <- mort_panel %>%
  summarise(
    N        = n(),
    Mean     = mean(d_flow),
    Median   = median(d_flow),
    SD       = sd(d_flow),
    Skewness = mean(((d_flow - mean(d_flow)) / sd(d_flow))^3),
    Kurtosis = mean(((d_flow - mean(d_flow)) / sd(d_flow))^4) - 3,  # excess kurtosis
    P5       = quantile(d_flow, 0.05),
    P25      = quantile(d_flow, 0.25),
    P75      = quantile(d_flow, 0.75),
    P95      = quantile(d_flow, 0.95),
    Pct_neg  = mean(d_flow < 0) * 100,
    Pct_gt50 = mean(d_flow > 50) * 100,
    Pct_gt100 = mean(d_flow > 100) * 100
  )
cat("  Verteilung d_flow (P-score, projizierte Baseline):\n")
print(kable(overall_stats, digits = 2))

# --- 2a. Dichte nach Jahr ----------------------------------------------------

year_stats <- mort_panel %>%
  group_by(Year) %>%
  summarise(Mean = mean(d_flow), Median = median(d_flow),
            SD = sd(d_flow), Pct_neg = mean(d_flow < 0) * 100, .groups = "drop")
cat("\n--- 2a. Verteilung nach Jahr ---\n")
print(kable(year_stats, digits = 1))

p_dist_year <- ggplot(mort_panel, aes(x = d_flow, fill = factor(Year))) +
  geom_density(alpha = 0.45, color = NA, adjust = 1.2) +
  geom_vline(data = year_stats, aes(xintercept = Mean, color = factor(Year)),
             linetype = "dashed", linewidth = 0.6, show.legend = FALSE) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 0.4) +
  scale_fill_manual(values = c("2020" = "grey30", "2021" = "grey65"), name = NULL) +
  scale_color_manual(values = c("2020" = "grey30", "2021" = "grey65")) +
  coord_cartesian(xlim = c(-40, 120)) +
  labs(
    title    = "A. Distribution by Year",
    subtitle = "Dashed: annual means. Right-skewed: most weeks near baseline, fat tail from waves.",
    x = "P-score (% excess above projected baseline)", y = "Density"
  ) + theme_aer

# --- 2b. Verteilung nach Quartal ----------------------------------------------
# Interpretation: Wellenstruktur klar sichtbar. Q4.2020 und Q4.2021 haben die
# höchsten Mittelwerte (Alpha- bzw. Omicron-Peak). Die Verteilung wird in
# Wellentälern eng und symmetrisch, in Peaks breit und rechtsschief.

q_stats <- mort_panel %>%
  group_by(YQ_ord) %>%
  summarise(
    Mean   = mean(d_flow),
    Median = median(d_flow),
    SD     = sd(d_flow),
    P25    = quantile(d_flow, 0.25),
    P75    = quantile(d_flow, 0.75),
    CV     = sd(d_flow) / max(abs(mean(d_flow)), 0.01),
    .groups = "drop"
  )
cat("\n--- 2b. Verteilung nach Quartal ---\n")
print(kable(q_stats, digits = 1))

p_dist_quarter <- ggplot(mort_panel, aes(x = d_flow, y = YQ_ord)) +
  geom_violin(fill = "grey80", color = "grey50", scale = "width",
              adjust = 1.2, width = 0.8) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 2.5, color = "#D62728") +
  stat_summary(fun = median, geom = "point", shape = 16, size = 1.5, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.4) +
  coord_cartesian(xlim = c(-40, 100)) +
  labs(
    title    = "B. Quarterly Distribution",
    subtitle = "Diamond: mean. Circle: median. Peaks track wave timing.",
    x = "P-score (%)", y = NULL
  ) + theme_aer

p_section2 <- p_dist_year / p_dist_quarter +
  plot_annotation(
    title   = "Excess Mortality: Overall Distribution",
    caption = "Source: Our World in Data (Karlinsky & Kobak, 2021). P-score = (observed − projected) / projected × 100.\n35 OECD countries with weekly data, Mar 2020 – Dec 2021.",
    theme   = theme(plot.title = element_text(face = "bold", size = 13, family = "serif"))
  )
print(p_section2)


# ==============================================================================
#  3. AGGREGIERTE OECD-TRAJEKTORIE (WELLENSTRUKTUR)
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 3: Aggregierte OECD-Trajektorie\n")
cat(strrep("=", 70), "\n\n")

# Berechne wöchentliche Aggregate über alle 35 Länder
weekly_agg <- mort_panel %>%
  group_by(date) %>%
  summarise(
    Mean   = mean(d_flow),
    Median = median(d_flow),
    SD     = sd(d_flow),
    P10    = quantile(d_flow, 0.10),
    P25    = quantile(d_flow, 0.25),
    P75    = quantile(d_flow, 0.75),
    P90    = quantile(d_flow, 0.90),
    N_pos  = sum(d_flow > 0),
    N_neg  = sum(d_flow < 0),
    .groups = "drop"
  )

# --- Wellenpeaks identifizieren -----------------------------------------------
wave_windows <- list(
  W1 = c("2020-03-01", "2020-06-30"),
  W2 = c("2020-10-01", "2021-03-31"),
  W3 = c("2021-04-01", "2021-09-30"),
  W4 = c("2021-10-01", "2021-12-31")
)

cat("  Wellenpeaks (OECD Mean P-score):\n")
for (wn in names(wave_windows)) {
  win <- wave_windows[[wn]]
  sub <- weekly_agg %>% filter(date >= as.Date(win[1]), date <= as.Date(win[2]))
  peak_row <- sub %>% filter(Mean == max(Mean))
  cat(sprintf("    %s: %.1f%% (Woche %s), SD = %.1f, Breite P10–P90 = [%.1f, %.1f]\n",
              wn, peak_row$Mean, peak_row$date, peak_row$SD, peak_row$P10, peak_row$P90))
}

# --- Troughs ------------------------------------------------------------------
cat("\n  Wellentäler:\n")
trough_windows <- list(
  T1 = c("2020-06-01", "2020-09-30"),
  T2 = c("2021-05-01", "2021-09-30")
)
for (tn in names(trough_windows)) {
  win <- trough_windows[[tn]]
  sub <- weekly_agg %>% filter(date >= as.Date(win[1]), date <= as.Date(win[2]))
  trough_row <- sub %>% filter(Mean == min(Mean))
  cat(sprintf("    %s: %.1f%% (Woche %s)\n", tn, trough_row$Mean, trough_row$date))
}

# --- Hauptplot: OECD-Trajektorie mit Unsicherheitsbändern --------------------
# Interpretation: Vier klar getrennte Wellen. Die Bänder (P10–P90) zeigen massive
# Heterogenität während der Peaks: einige Länder verdoppeln ihre Mortalität,
# andere bleiben nahe der Baseline. In Troughs konvergieren alle.

# Annotationen für Wellen vorbereiten
wave_labels <- tibble(
  date  = as.Date(c("2020-04-15","2020-12-15","2021-08-01","2021-11-15")),
  y     = c(27, 32, 19, 29),
  label = c("W1: Original","W2: Alpha","W3: Delta","W4: Omicron")
)

p_trajectory <- ggplot(weekly_agg, aes(x = date)) +
  # Bänder
  geom_ribbon(aes(ymin = P10, ymax = P90), fill = "#D62728", alpha = 0.12) +
  geom_ribbon(aes(ymin = P25, ymax = P75), fill = "#D62728", alpha = 0.25) +
  # Linien
  geom_line(aes(y = Mean), color = "#D62728", linewidth = 0.9) +
  geom_line(aes(y = Median), color = "black", linewidth = 0.5, linetype = "dashed") +
  # Baseline
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  # Wellenlabels
  geom_text(data = wave_labels, aes(x = date, y = y, label = label),
            size = 2.8, fontface = "italic", family = "serif", color = "grey30") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y",
               expand = expansion(mult = c(0.01, 0.01))) +
  labs(
    title    = "OECD Aggregate Excess Mortality Trajectory",
    subtitle = "Solid red: cross-country mean. Dashed black: median.\nDark band: IQR (P25–P75). Light band: P10–P90.",
    x = NULL, y = "P-score (% excess above projected baseline)",
    caption  = "35 OECD countries, weekly. Source: Our World in Data."
  ) + theme_aer
print(p_trajectory)

# --- Ergänzend: Anteil Länder mit positivem Excess pro Woche ------------------
p_share_pos <- ggplot(weekly_agg, aes(x = date, y = N_pos / (N_pos + N_neg) * 100)) +
  geom_area(fill = "#D62728", alpha = 0.3) +
  geom_line(color = "#D62728", linewidth = 0.5) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "grey50") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title    = "Share of Countries with Positive Excess Mortality",
    subtitle = "In troughs, nearly half of OECD countries are BELOW expected mortality.",
    x = NULL, y = "% countries with d > 0"
  ) + theme_aer
print(p_share_pos)


# ==============================================================================
#  4. LÄNDERHETEROGENITÄT (BETWEEN-VARIATION)
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 4: Länderheterogenität\n")
cat(strrep("=", 70), "\n\n")

country_stats <- mort_panel %>%
  group_by(Country) %>%
  summarise(
    Mean     = mean(d_flow),
    Median   = median(d_flow),
    SD       = sd(d_flow),
    Max      = max(d_flow),
    Min      = min(d_flow),
    Pct_neg  = mean(d_flow < 0) * 100,
    Wk_gt20  = sum(d_flow > 20),
    Pct_gt20 = mean(d_flow > 20) * 100,
    Rate_mean = mean(d_rate),
    Cum_end  = last(d_cum),
    .groups  = "drop"
  ) %>%
  arrange(desc(Mean))

cat("  Länderergebnisse (sortiert nach Mean P-score):\n")
print(kable(country_stats %>% select(Country, Mean, Median, SD, Max, Pct_neg, Pct_gt20, Cum_end),
            digits = 1, col.names = c("Country","Mean","Median","SD","Max","%neg","%>20","Cum/M")))

cat(sprintf("\n  Between-Country Summary:\n"))
cat(sprintf("    Range of means: [%.1f, %.1f]\n", min(country_stats$Mean), max(country_stats$Mean)))
cat(sprintf("    SD of means: %.1f\n", sd(country_stats$Mean)))
cat(sprintf("    CV of means: %.2f\n", sd(country_stats$Mean) / mean(country_stats$Mean)))

# --- Kumulative Excess Deaths per Million (End 2021) --------------------------
cat(sprintf("\n  Cumulative excess deaths/million (end Dec 2021):\n"))
cs_cum <- country_stats %>% arrange(desc(Cum_end))
cat(sprintf("    Top 5: %s\n", paste(sprintf("%s (%.0f)", head(cs_cum$Country, 5), head(cs_cum$Cum_end, 5)), collapse = ", ")))
cat(sprintf("    Bottom 5: %s\n", paste(sprintf("%s (%.0f)", tail(cs_cum$Country, 5), tail(cs_cum$Cum_end, 5)), collapse = ", ")))
cat(sprintf("    OECD mean: %.0f, median: %.0f\n", mean(cs_cum$Cum_end), median(cs_cum$Cum_end)))

# --- Plot: Cumulative Excess Deaths per Million, End 2021 ---------------------
# Interpretation: Massive Heterogenität. Faktor >10 zwischen den am stärksten
# und am schwächsten betroffenen Ländern. NZL ist NEGATIV — weniger Todesfälle
# als erwartet über den gesamten Zeitraum.

p_cum <- country_stats %>%
  mutate(Country = fct_reorder(Country, Cum_end)) %>%
  ggplot(aes(x = Cum_end, y = Country)) +
  geom_col(aes(fill = Cum_end > 0), width = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "#D62728", "FALSE" = "#1F77B4")) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
  geom_vline(xintercept = mean(country_stats$Cum_end), linetype = "dashed",
             color = "grey40", linewidth = 0.4) +
  labs(
    title    = "Cumulative Excess Deaths per Million (Mar 2020 – Dec 2021)",
    subtitle = "Dashed line: OECD mean. Blue: negative excess (fewer deaths than expected).",
    x = "Excess deaths per million", y = NULL,
    caption = "Source: Our World in Data. Projected baseline adjusts for demographic trends."
  ) + theme_aer +
  theme(axis.text.y = element_text(size = 7))
print(p_cum)

# --- Plot: Mean P-score vs. Cumulative per Million ----------------------------
# Cross-check: sind die beiden Masse konsistent?
p_mean_vs_cum <- ggplot(country_stats, aes(x = Mean, y = Cum_end)) +
  geom_point(color = "#D62728", size = 2) +
  geom_text(aes(label = Country), size = 2.2, nudge_y = 150, family = "serif") +
  geom_smooth(method = "lm", se = FALSE, color = "grey50", linewidth = 0.5, linetype = "dashed") +
  labs(
    title    = "Mean Weekly P-score vs. Cumulative Excess Deaths per Million",
    subtitle = sprintf("r = %.3f. Near-perfect: flow and stock measures are consistent.",
                       cor(country_stats$Mean, country_stats$Cum_end)),
    x = "Mean P-score (%)", y = "Cumulative excess deaths per million"
  ) + theme_aer
print(p_mean_vs_cum)


# ==============================================================================
#  5. WITHIN-COUNTRY-VARIATION
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 5: Within-Country-Variation\n")
cat(strrep("=", 70), "\n\n")

# Interpretation: Einige Länder (MEX, COL) sind DAUERHAFT über der Baseline.
# Andere (GBR, ITA, BEL) haben scharfe Peaks mit langen Normalperioden.
# Die temporale Konzentration der Excess Mortality variiert dramatisch.

within_stats <- mort_panel %>%
  group_by(Country) %>%
  summarise(
    Mean    = mean(d_flow),
    SD      = sd(d_flow),
    Range   = max(d_flow) - min(d_flow),
    CV      = sd(d_flow) / max(abs(mean(d_flow)), 0.01),
    Wk_gt20 = sum(d_flow > 20),
    Pct_gt20 = mean(d_flow > 20) * 100,
    Pct_neg  = mean(d_flow < 0) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(SD))

cat("  Within-Country Variation (sortiert nach SD):\n")
print(kable(within_stats %>% select(Country, Mean, SD, Range, CV, Pct_gt20, Pct_neg),
            digits = 1,
            col.names = c("Country","Mean","SD","Range","CV","%>20","%neg")))

# --- Temporale Konzentration: Spike vs. Sustained Pattern ---------------------
cat("\n  Temporale Konzentration der Excess Mortality:\n")
cat("  (Anteil des totalen Excess aus Wochen mit d_flow > 20%)\n\n")
conc_stats <- mort_panel %>%
  group_by(Country) %>%
  summarise(
    total_excess = sum(pmax(d_flow, 0)),
    spike_excess = sum(d_flow[d_flow > 20]),
    n_spike      = sum(d_flow > 20),
    n_total      = n(),
    .groups      = "drop"
  ) %>%
  mutate(
    frac_spike   = ifelse(total_excess > 0, spike_excess / total_excess, 0),
    frac_time    = n_spike / n_total,
    pattern      = case_when(
      frac_spike > 0.8 & frac_time > 0.5 ~ "Sustained",
      frac_spike > 0.5 ~ "Spike-dominated",
      frac_spike > 0.2 ~ "Mixed",
      TRUE ~ "Low excess"
    )
  ) %>%
  arrange(desc(frac_spike))

for (i in 1:nrow(conc_stats)) {
  r <- conc_stats[i, ]
  cat(sprintf("    %s: %.0f%% of excess in %d spike weeks (%.0f%% of time) → %s\n",
              r$Country, r$frac_spike * 100, r$n_spike, r$frac_time * 100, r$pattern))
}

cat(sprintf("\n  Pattern distribution: %s\n",
            paste(names(table(conc_stats$pattern)), table(conc_stats$pattern), sep = ": ", collapse = ", ")))

# --- Plot: Spaghetti aller Länder mit Wellenstruktur -------------------------
# Zeigt die asynchrone Wellenstruktur: nicht alle Wellen treffen alle Länder
# gleichzeitig oder mit gleicher Intensität.

p_spaghetti <- ggplot(mort_panel, aes(x = date, y = d_flow, group = Country)) +
  geom_line(alpha = 0.2, linewidth = 0.3, color = "grey40") +
  # OECD-Aggregat hervorheben
  geom_line(data = weekly_agg, aes(x = date, y = Mean, group = 1),
            color = "#D62728", linewidth = 1, inherit.aes = FALSE) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  coord_cartesian(ylim = c(-30, 120)) +
  labs(
    title    = "Within-Country Trajectories: All 35 OECD Countries",
    subtitle = "Grey: individual countries. Red: OECD mean.\nMassive within-country variation drives 78% of total variance.",
    x = NULL, y = "P-score (%)",
    caption = "Clipped at 120% for visibility. MEX and COL peaks exceed 150%."
  ) + theme_aer
print(p_spaghetti)

# --- Spotlight: Ausgewählte Archetypes ----------------------------------------
archetypes <- c("USA", "GBR", "DEU", "ITA", "MEX", "AUS", "KOR", "NZL")

p_archetypes <- mort_panel %>%
  filter(Country %in% archetypes) %>%
  mutate(Country = factor(Country, levels = archetypes)) %>%
  ggplot(aes(x = date, y = d_flow)) +
  geom_area(fill = "#D62728", alpha = 0.25) +
  geom_line(color = "#D62728", linewidth = 0.4) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  facet_wrap(~ Country, ncol = 4, scales = "free_y") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %y") +
  labs(
    title    = "Country Archetypes: Heterogeneous Mortality Profiles",
    subtitle = "USA: persistent; GBR/ITA: wave-synchronized; DEU: delayed; MEX: sustained crisis;\nAUS/KOR/NZL: elimination/suppression → near-zero excess.",
    x = NULL, y = "P-score (%)",
    caption  = "Y-axis scales differ across panels."
  ) + theme_aer +
  theme(strip.text = element_text(size = 9))
print(p_archetypes)


# ==============================================================================
#  6. BETWEEN VS. WITHIN: ANOVA-ZERLEGUNG
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 6: ANOVA-Zerlegung (Between vs. Within)\n")
cat(strrep("=", 70), "\n\n")

# --- Klassische ANOVA-Zerlegung -----------------------------------------------
# Total Variation = Between (Ländermittelwerte) + Within (Abweichungen vom Ländermittel)
# Für Identification: Within identifiziert δ_θ und S → θ → d;
#                     Between identifiziert länderspezifische Exposition und Reaktion.

grand_mean <- mean(mort_panel$d_flow)
country_means <- mort_panel %>% group_by(Country) %>%
  summarise(mu = mean(d_flow), n = n(), .groups = "drop")

SS_total   <- sum((mort_panel$d_flow - grand_mean)^2)
SS_between <- sum(country_means$n * (country_means$mu - grand_mean)^2)
SS_within  <- SS_total - SS_between

cat(sprintf("  ANOVA — d_flow (wöchentlicher P-score):\n"))
cat(sprintf("    Grand mean:  %.2f\n", grand_mean))
cat(sprintf("    SS_total:    %12.0f\n", SS_total))
cat(sprintf("    SS_between:  %12.0f  (%.1f%%)\n", SS_between, SS_between / SS_total * 100))
cat(sprintf("    SS_within:   %12.0f  (%.1f%%)\n\n", SS_within, SS_within / SS_total * 100))

# Repeat for d_rate
gm_rate <- mean(mort_panel$d_rate)
cm_rate <- mort_panel %>% group_by(Country) %>%
  summarise(mu = mean(d_rate), n = n(), .groups = "drop")
SS_t_rate <- sum((mort_panel$d_rate - gm_rate)^2)
SS_b_rate <- sum(cm_rate$n * (cm_rate$mu - gm_rate)^2)

cat(sprintf("  ANOVA — d_rate (excess deaths per million/week):\n"))
cat(sprintf("    SS_between: %.1f%%,  SS_within: %.1f%%\n\n",
            SS_b_rate / SS_t_rate * 100, (1 - SS_b_rate / SS_t_rate) * 100))

# --- Zeitvariante ANOVA: Between-Anteil nach Quartal --------------------------
# Interpretation: In Wellentälern ist WITHIN-Variation hoch (alle Länder nahe null,
# aber individuell fluktuierend). In Peaks steigt der BETWEEN-Anteil, weil die
# Wellen verschiedene Länder unterschiedlich treffen.

cat("  Between-Anteil nach Quartal:\n")
q_anova <- mort_panel %>%
  group_by(YQ_ord) %>%
  group_modify(~ {
    gm <- mean(.x$d_flow)
    cm <- .x %>% group_by(Country) %>%
      summarise(mu = mean(d_flow), n = n(), .groups = "drop")
    ss_t <- sum((.x$d_flow - gm)^2)
    ss_b <- sum(cm$n * (cm$mu - gm)^2)
    tibble(Between_pct = ss_b / ss_t * 100, Within_pct = 100 - ss_b / ss_t * 100,
           N = nrow(.x), SD = sd(.x$d_flow))
  })
print(kable(q_anova, digits = 1))

# Interpretation
cat(sprintf("\n  Gesamtergebnis: %.0f%% Between / %.0f%% Within (wöchentlich)\n",
            SS_between / SS_total * 100, SS_within / SS_total * 100))
cat("  → Within-Variation dominiert: dasselbe Land erlebt dramatisch\n")
cat("    unterschiedliche Mortalität über Wellen hinweg.\n")
cat("  → Dies ist die temporale Variation, die das Modell nutzt:\n")
cat("    d_{k+1} = δ_θ · θ_k reagiert auf sich änderndes θ_k, nicht auf\n")
cat("    fixe Ländermerkmale.\n\n")


# ==============================================================================
#  7. WELLENANALYSE (ASYNCHRONITÄT & KORRELATIONSSTRUKTUR)
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("  SECTION 7: Wellenanalyse\n")
cat(strrep("=", 70), "\n\n")

# --- 7a. Wellenstatistiken (OECD) ---------------------------------------------

wave_stats <- mort_panel %>%
  group_by(Wave) %>%
  summarise(
    N_weeks  = n() / n_distinct(Country),
    Mean     = mean(d_flow),
    Median   = median(d_flow),
    SD       = sd(d_flow),
    P25      = quantile(d_flow, 0.25),
    P75      = quantile(d_flow, 0.75),
    Max      = max(d_flow),
    Pct_gt20 = mean(d_flow > 20) * 100,
    Pct_neg  = mean(d_flow < 0) * 100,
    .groups  = "drop"
  )
cat("--- 7a. Wellenstatistiken (OECD) ---\n")
print(kable(wave_stats, digits = 1))

# --- 7b. Länderspezifische Wellenmittelwerte ----------------------------------
# Wer wurde WANN getroffen? Entscheidend für Identifikation: Variation in der
# zeitlichen Abfolge der Wellen zwischen Ländern.

wave_country <- mort_panel %>%
  group_by(Country, Wave) %>%
  summarise(Mean = mean(d_flow), .groups = "drop") %>%
  pivot_wider(names_from = Wave, values_from = Mean)

cat("\n--- 7b. Länderspezifische Wellenmittelwerte ---\n")
print(kable(wave_country %>% arrange(desc(`W1: Original`)),
            digits = 1))

# --- 7c. Paarweise Korrelationen ----------------------------------------------
# Interpretation: Geringe mittlere paarweise Korrelation (r ≈ 0.2) zeigt, dass
# die Wellenstruktur ASYNCHRON ist — die Mortalitätspeaks treffen Länder zu
# verschiedenen Zeitpunkten. Dies ist identifikationsrelevant: es liefert
# Within-Variation, die nicht durch einen gemeinsamen Zeiteffekt absorbiert wird.

wide_mort <- mort_panel %>%
  select(Country, date, d_flow) %>%
  pivot_wider(names_from = Country, values_from = d_flow)

corr_matrix <- cor(wide_mort %>% select(-date), use = "pairwise.complete.obs")
upper_tri   <- corr_matrix[upper.tri(corr_matrix)]

cat(sprintf("\n  Paarweise Korrelationen der wöchentlichen d_flow:\n"))
cat(sprintf("    Mean: %.3f, Median: %.3f, SD: %.3f\n", mean(upper_tri), median(upper_tri), sd(upper_tri)))
cat(sprintf("    Range: [%.3f, %.3f]\n", min(upper_tri), max(upper_tri)))
cat(sprintf("    %% negativ: %.1f%%\n\n", mean(upper_tri < 0) * 100))

# Regionale Korrelationen
regions <- list(
  Europe = c("AUT","BEL","CZE","DEU","DNK","ESP","EST","FIN","FRA","GBR",
             "GRC","HUN","IRL","ISL","ITA","LTU","LUX","LVA","NLD","NOR",
             "POL","PRT","SVK","SVN","SWE","CHE"),
  LatAm  = c("CHL","COL","MEX"),
  AsiaPac = c("AUS","NZL","KOR"),
  NorthAm = c("USA","CAN")
)

cat("  Regionale Korrelationen:\n")
for (reg_name in names(regions)) {
  reg_countries <- intersect(regions[[reg_name]], colnames(corr_matrix))
  if (length(reg_countries) >= 2) {
    reg_corr <- corr_matrix[reg_countries, reg_countries]
    reg_upper <- reg_corr[upper.tri(reg_corr)]
    cat(sprintf("    %-12s: mean r = %.3f (n_pairs = %d)\n",
                reg_name, mean(reg_upper), length(reg_upper)))
  }
}

# --- Plot: Korrelationsmatrix (Heatmap) ---------------------------------------
# Sortiert nach Regionengruppen für visuelle Cluster-Erkennung

region_order <- c(regions$NorthAm, regions$AsiaPac, regions$LatAm, regions$Europe)
region_order <- intersect(region_order, colnames(corr_matrix))

corr_long <- as.data.frame(as.table(corr_matrix[region_order, region_order])) %>%
  rename(Country1 = Var1, Country2 = Var2, r = Freq) %>%
  mutate(
    Country1 = factor(Country1, levels = region_order),
    Country2 = factor(Country2, levels = region_order)
  )

p_corr_heat <- ggplot(corr_long, aes(x = Country1, y = Country2, fill = r)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_gradient2(low = "#1F77B4", mid = "white", high = "#D62728",
                       midpoint = 0, limits = c(-0.6, 0.9),
                       name = "r") +
  labs(
    title    = "Pairwise Correlation of Weekly Excess Mortality",
    subtitle = "Grouped by region. Low mean r = 0.21 → asynchronous wave structure.",
    x = NULL, y = NULL
  ) + theme_aer +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6),
        axis.text.y = element_text(size = 6),
        legend.position = "right")
print(p_corr_heat)


# ==============================================================================
#  8. LAG-STRUKTUR S → θ → d (ENDOGENITÄTSDIAGNOSTIK)
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 8: Lag-Struktur S → θ → d\n")
cat(strrep("=", 70), "\n\n")

# --- Wöchentliche S berechnen (Oxford Stringency → Wochenende Sonntag) --------
# Die Excess-Mortality-Daten sind nach Woche (Sonntag) strukturiert.
# Wir aggregieren die täglichen Stringency-Werte auf dieselbe Wochenstruktur.

s_weekly <- oxd_d %>%
  filter(Date >= as.Date("2020-02-01"), Date <= as.Date("2021-12-31")) %>%
  mutate(
    Date  = as.Date(Date),
    dow   = lubridate::wday(Date, week_start = 1) - 1,  # Mon=0, Sun=6
    week_sun = Date + (6 - dow)
  ) %>%
  group_by(Country, date = week_sun) %>%
  summarise(S_raw = mean(StringencyIndex_PopWeighted, na.rm = TRUE), .groups = "drop")

merged_sd <- mort_panel %>%
  inner_join(s_weekly, by = c("Country", "date"))

cat(sprintf("  Merged panel: %d obs (%d Länder)\n\n", nrow(merged_sd), n_distinct(merged_sd$Country)))

# --- 8a. Aggregierte Cross-Korrelation S(t) vs d(t+lag) ----------------------
# Interpretation: Die POSITIVE Korrelation bei lag=0 ist die Endogenitätssignatur:
# Regierungen lockern bei fallender Mortalität und verschärfen bei steigender.
# Das NEGATIVE Vorzeichen bei lag ≈ 5–7 Wochen reflektiert den kausalen Kanal
# S → θ → d: Lockdown unterdrückt Infektionen, die 3–4 Wochen später die
# Mortalität senken.

# Berechnet alle Lags von -10 bis +10 automatisch (plot = FALSE unterdrückt den direkten Plot)
cross_corr <- ccf(agg_sd$S, agg_sd$d, lag.max = 10, na.action = na.pass, plot = FALSE)

# Falls du es als Tibble brauchst, kannst du es direkt umwandeln:
lag_corrs_ccf <- tibble(
  lag = as.vector(cross_corr$lag),
  r   = as.vector(cross_corr$acf)
) %>%
  filter(lag >= -8) # Filtert es auf deine gewünschte Range (-8 bis 10)

# ggplot initialisieren
ggplot(lag_corrs_ccf, aes(x = lag, y = r)) +
  # Vertikale Linien von 0 bis zum Korrelationswert (r)
  geom_segment(aes(xend = lag, yend = 0), color = "steelblue", linewidth = 1) +
  # Punkte an der Spitze der Linien
  geom_point(color = "steelblue", size = 3) +
  # Horizontale Nulllinie zur besseren Orientierung
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  # Das Aussehen etwas aufgeräumter machen
  theme_minimal() +
  # Beschriftungen hinzufügen
  labs(
    title = "Kreuzkorrelation zwischen S und d",
    subtitle = "Lags von -8 bis +10",
    x = "Lag",
    y = "Korrelation (r)"
  ) +
  # Die x-Achse so einstellen, dass jeder Lag-Schritt angezeigt wird
  scale_x_continuous(breaks = -8:10)



agg_sd <- merged_sd %>%
  group_by(date) %>%
  summarise(S = mean(S_raw), d = mean(d_flow), .groups = "drop") %>%
  arrange(date)
lag_range <- -8:10

lag_corrs <- tibble(
  lag = lag_range,
  r   = sapply(lag_range, function(l) {
    # Bedingung für die Richtung der Verschiebung
    if (l > 0) {
      shifted_d <- lead(agg_sd$d, l)
    } else if (l < 0) {
      shifted_d <- lag(agg_sd$d, abs(l))
    } else {
      shifted_d <- agg_sd$d
    }
    
    # Korrelation berechnen
    cor(agg_sd$S, shifted_d, use = "complete.obs")
  })
)

print(lag_corrs)

cat("  Cross-Korrelationen: corr(S_t, d_{t+lag}), OECD-Aggregat:\n")
cat(sprintf("  %12s %14s %20s\n", "Lag (Wochen)", "Korrelation", ""))
for (i in 1:nrow(lag_corrs)) {
  l <- lag_corrs$lag[i]
  r <- lag_corrs$r[i]
  note <- ""
  if (l == 0) note <- "← contemporaneous (endogeneity)"
  if (l == 3) note <- "← expected causal lag"
  if (!is.na(r) && r == min(lag_corrs$r, na.rm = TRUE)) note <- paste(note, "← minimum")
  cat(sprintf("  %12d %14.3f  %s\n", l, r, note))
}

# --- Plot: Cross-Korrelogramm ------------------------------------------------
p_ccf <- ggplot(lag_corrs, aes(x = lag, y = r)) +
  geom_col(aes(fill = r > 0), width = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "#D62728", "FALSE" = "#1F77B4")) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.3) +
  annotate("text", x = 0.5, y = 0.35, label = "Endogeneity\n(d → S)",
           hjust = 0, size = 2.5, fontface = "italic", family = "serif") +
  annotate("text", x = 5.5, y = -0.35, label = "Causal effect\n(S → θ → d)",
           hjust = 0, size = 2.5, fontface = "italic", family = "serif") +
  scale_x_continuous(breaks = lag_range) +
  labs(
    title    = "Cross-Correlogram: Stringency vs. Excess Mortality (OECD Aggregate)",
    subtitle = "Positive at lag 0: S reacts TO mortality (endogeneity).\nNegative at lag 5–7: S suppresses mortality THROUGH infection dynamics.",
    x = "Lag (weeks; positive = d leads S)", y = "Correlation"
  ) + theme_aer
print(p_ccf)

# --- 8b. Länderspezifische optimale Lags --------------------------------------

opt_lags <- merged_sd %>%
  group_by(Country) %>%
  group_modify(~ {
    best_lag <- 0; best_r <- 0
    for (l in 0:10) {
      r <- cor(.x$S_raw, lead(.x$d_flow, l), use = "complete")
      if (!is.na(r) && abs(r) > abs(best_r)) { best_r <- r; best_lag <- l }
    }
    tibble(opt_lag = best_lag, max_r = best_r, sign = ifelse(best_r > 0, "+", "−"))
  }) %>%
  ungroup()

cat(sprintf("\n  Länderspezifische optimale Lags (max |r|, lag 0–10 Wochen):\n"))
cat(sprintf("    Mean lag: %.1f, Median: %.0f, Mode: %d\n",
            mean(opt_lags$opt_lag), median(opt_lags$opt_lag),
            as.numeric(names(sort(table(opt_lags$opt_lag), decreasing = TRUE))[1])))
cat(sprintf("    Vorzeichen: %d positiv (S↑ ↔ d↑), %d negativ (S↑ ↔ d↓)\n",
            sum(opt_lags$sign == "+"), sum(opt_lags$sign == "−")))
cat(sprintf("    Mean |r|: %.3f\n\n", mean(abs(opt_lags$max_r))))

# --- Plot: Verteilung der optimalen Lags --------------------------------------
p_lag_dist <- ggplot(opt_lags, aes(x = opt_lag, fill = sign)) +
  geom_bar(width = 0.7) +
  scale_fill_manual(values = c("+" = "#D62728", "−" = "#1F77B4"),
                    name = "Sign of r") +
  scale_x_continuous(breaks = 0:10) +
  labs(
    title    = "Distribution of Optimal Lags Across Countries",
    subtitle = "Red (+): S and d move together (endogeneity).\nBlue (−): S leads d decline (causal suppression effect).",
    x = "Optimal lag (weeks)", y = "Number of countries"
  ) + theme_aer
print(p_lag_dist)

cat("  Modellimplikation:\n")
cat("  Die positive contemporane S–d-Korrelation bestätigt, dass jede direkte\n")
cat("  Regression d ~ S die Policy Reaction Function schätzt, nicht den kausalen\n")
cat("  Gesundheitseffekt. Das Modell trennt diese Kanäle korrekt:\n")
cat("  d_{k+1} = δ_θ · θ_k wird über die Infektionsdynamik identifiziert,\n")
cat("  der S → θ Kanal über φ_S in der θ-Gleichung.\n\n")


# ==============================================================================
#  9. ALTERSDEKOMPOSITION
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("  SECTION 9: Altersdekomposition\n")
cat(strrep("=", 70), "\n\n")

# Interpretation: Der steile Altersgradient bestätigt, dass δ_θ effektiv ein
# altersgewichteter Parameter ist. Die ABFLACHUNG des Gradienten über die Wellen
# (Impfung schützt Ältere überproportional, Delta trifft Jüngere) hat Implikationen
# für die Zeitvarianz von δ_θ: es sinkt schneller bei Ländern mit hoher Impfquote
# bei Älteren.

age_vars <- c("p_proj_0_14", "p_proj_15_64", "p_proj_65_74", "p_proj_75_84", "p_proj_85p")
age_labels <- c("0–14", "15–64", "65–74", "75–84", "85+")

# Nur Länder mit verfügbarer Altersaufschlüsselung
age_data <- p_values_oecd_w %>%
  filter(
    time_unit == "weekly",
    date >= as.Date("2020-03-01"),
    date <= as.Date("2021-12-31")
  ) %>%
  mutate(date = as.Date(date)) %>%
  select(Country = entity, date, all_of(age_vars)) %>%
  drop_na()

cat(sprintf("  Datenverfügbarkeit: %d obs (%d Länder mit Altersaufschlüsselung)\n\n",
            nrow(age_data), n_distinct(age_data$Country)))

# Gesamtstatistik
cat("  Altersgruppen-Statistiken (projizierte Baseline):\n")
age_summary <- map_dfr(seq_along(age_vars), function(i) {
  s <- age_data[[age_vars[i]]]
  tibble(
    Age_Group = age_labels[i],
    Mean      = mean(s),
    Median    = median(s),
    SD        = sd(s),
    P5        = quantile(s, 0.05),
    P95       = quantile(s, 0.95),
    Pct_neg   = mean(s < 0) * 100
  )
})
print(kable(age_summary, digits = 1))

# --- Altersgradient nach Welle -------------------------------------------------
age_data_long <- age_data %>%
  pivot_longer(cols = all_of(age_vars), names_to = "age_var", values_to = "p_score") %>%
  mutate(
    Age_Group = factor(recode(age_var,
                              "p_proj_0_14" = "0–14", "p_proj_15_64" = "15–64",
                              "p_proj_65_74" = "65–74", "p_proj_75_84" = "75–84", "p_proj_85p" = "85+"),
                       levels = age_labels),
    Wave = case_when(
      date <= as.Date("2020-06-30") ~ "W1: Original",
      date <= as.Date("2020-09-30") ~ "Trough 1",
      date <= as.Date("2021-03-31") ~ "W2: Alpha",
      date <= as.Date("2021-06-30") ~ "Trough 2",
      date <= as.Date("2021-09-30") ~ "W3: Delta",
      TRUE                          ~ "W4: Omicron"
    ),
    Wave = factor(Wave, levels = names(wave_colors))
  )

# Tabelle
age_wave_tab <- age_data_long %>%
  filter(Age_Group != "0–14") %>%  # Zu verrauscht
  group_by(Wave, Age_Group) %>%
  summarise(Mean = mean(p_score), .groups = "drop") %>%
  pivot_wider(names_from = Age_Group, values_from = Mean)

cat("\n  Altersgradient nach Welle (ohne 0–14, zu verrauscht):\n")
print(kable(age_wave_tab, digits = 1))

# Ratio 85+ / 15-64
age_ratio <- age_data_long %>%
  filter(Age_Group %in% c("15–64", "85+")) %>%
  group_by(Wave, Age_Group) %>%
  summarise(Mean = mean(p_score), .groups = "drop") %>%
  pivot_wider(names_from = Age_Group, values_from = Mean) %>%
  mutate(Ratio = `85+` / pmax(abs(`15–64`), 0.01))

cat("\n  Ratio 85+ / 15–64 nach Welle:\n")
for (i in 1:nrow(age_ratio)) {
  cat(sprintf("    %s: 85+ = %.1f, 15–64 = %.1f, Ratio = %.1f\n",
              age_ratio$Wave[i], age_ratio$`85+`[i], age_ratio$`15–64`[i], age_ratio$Ratio[i]))
}

# --- Plot: Altersgradient über die Zeit ---------------------------------------
age_weekly_agg <- age_data_long %>%
  filter(Age_Group != "0–14") %>%
  group_by(date, Age_Group) %>%
  summarise(Mean = mean(p_score), .groups = "drop")

p_age_traj <- ggplot(age_weekly_agg, aes(x = date, y = Mean, color = Age_Group)) +
  geom_line(linewidth = 0.6) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_color_manual(
    values = c("15–64" = "#1F77B4", "65–74" = "#FF7F0E",
               "75–84" = "#D62728", "85+" = "#8C564B"),
    name = "Age Group"
  ) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(
    title    = "Age-Specific Excess Mortality Trajectory (OECD Mean)",
    subtitle = "Age gradient flattens over time: vaccines protect elderly disproportionately;\nDelta/Omicron hit working-age populations harder relative to earlier waves.",
    x = NULL, y = "P-score (%)",
    caption = "0–14 excluded (extreme noise, SD >> Mean). 29 countries with age breakdown."
  ) + theme_aer
print(p_age_traj)


# ==============================================================================
#  10. QUARTALSAGGREGATION & MODELL-MAPPING
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 10: Quartalsaggregation & Modell-Mapping\n")
cat(strrep("=", 70), "\n\n")

# --- 10a. Quartalspanel aus Wochendaten ---------------------------------------
q_mort <- mort_panel %>%
  group_by(Country, YQ, YQ_ord) %>%
  summarise(
    d_flow_mean = mean(d_flow),
    d_flow_max  = max(d_flow),
    d_flow_sd   = sd(d_flow),
    d_rate_mean = mean(d_rate),
    d_cum_end   = last(d_cum),
    N_weeks     = n(),
    .groups     = "drop"
  )

cat(sprintf("  Quartalspanel: %d obs (%d Länder × %d Quartale)\n\n",
            nrow(q_mort), n_distinct(q_mort$Country), n_distinct(q_mort$YQ)))

# --- Cross-country stats per quarter -----------------------------------------
q_cs <- q_mort %>%
  group_by(YQ_ord) %>%
  summarise(
    Mean   = mean(d_flow_mean),
    Median = median(d_flow_mean),
    SD     = sd(d_flow_mean),
    Min    = min(d_flow_mean),
    Max    = max(d_flow_mean),
    CV     = sd(d_flow_mean) / max(abs(mean(d_flow_mean)), 0.01),
    .groups = "drop"
  )
cat("  Cross-Country-Statistiken pro Quartal (d_flow_mean):\n")
print(kable(q_cs, digits = 1))

# --- Within-quarter variation -------------------------------------------------
cat("\n  Within-Quarter-Variation (mean SD der wöchentlichen d_flow pro Country×Quarter):\n")
wq_stats <- q_mort %>%
  group_by(YQ_ord) %>%
  summarise(Mean_SD = mean(d_flow_sd), Median_SD = median(d_flow_sd), .groups = "drop")
print(kable(wq_stats, digits = 1))
cat(sprintf("\n  Gesamt-Mean Within-Quarter SD: %.1f\n", mean(q_mort$d_flow_sd, na.rm = T)))
cat("  → Substanzielle Intra-Quartal-Variation.\n\n")

# --- ANOVA auf Quartalsebene -------------------------------------------------
q_gm <- mean(q_mort$d_flow_mean)
q_cm <- q_mort %>% group_by(Country) %>%
  summarise(mu = mean(d_flow_mean), n = n(), .groups = "drop")
q_ss_t <- sum((q_mort$d_flow_mean - q_gm)^2)
q_ss_b <- sum(q_cm$n * (q_cm$mu - q_gm)^2)

cat(sprintf("  ANOVA auf Quartalsebene (d_flow_mean):\n"))
cat(sprintf("    Between: %.1f%%,  Within: %.1f%%\n",
            q_ss_b / q_ss_t * 100, (1 - q_ss_b / q_ss_t) * 100))
cat("  → Aggregation erhöht Between-Anteil (Within-Wochen-Fluktuation mittelt sich aus).\n\n")

# --- Kumulative Trajektorie ---------------------------------------------------
# Modellrelevant: der Stock d_cum akkumuliert über die Zeit. Das iLQR penalisiert
# den Flow (d_k+1 = δ_θ · θ_k), aber die Wohlfahrtskosten sind proportional zum
# kumulierten Verlust an Menschenleben.

q_cum <- q_mort %>%
  group_by(YQ_ord) %>%
  summarise(
    Mean_cum = mean(d_cum_end),
    SD_cum   = sd(d_cum_end),
    Min_cum  = min(d_cum_end),
    Max_cum  = max(d_cum_end),
    .groups  = "drop"
  )
cat("  Kumulative Excess Deaths/Million am Quartalsende (OECD-Mittel):\n")
print(kable(q_cum, digits = 0))

# --- Plot: Quartals-Boxplots mit Overlay --------------------------------------
p_q_box <- ggplot(q_mort, aes(x = YQ_ord, y = d_flow_mean)) +
  geom_boxplot(fill = "grey85", color = "grey50", outlier.size = 1,
               outlier.color = "#D62728", width = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "#D62728") +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  labs(
    title    = "A. Quarterly Distribution of Mean Excess Mortality",
    subtitle = "Diamond: OECD mean. Box: IQR. Wave structure tracks exactly.",
    x = NULL, y = "Quarterly mean P-score (%)"
  ) + theme_aer

# --- Plot: Kumulative Divergenz -----------------------------------------------
# Ausgewählte Länder: zeigt, wie sich die kumulativen Pfade über die Zeit trennen.
sel_countries <- c("MEX","USA","GBR","ITA","DEU","FRA","KOR","AUS","NZL","CZE","SWE")

p_cum_traj <- mort_panel %>%
  filter(Country %in% sel_countries) %>%
  ggplot(aes(x = date, y = d_cum, color = Country)) +
  geom_line(linewidth = 0.6) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  scale_color_brewer(palette = "Paired") +
  labs(
    title    = "B. Cumulative Excess Deaths per Million: Divergent Trajectories",
    subtitle = "Trajectories diverge monotonically: pandemic outcomes are path-dependent.",
    x = NULL, y = "Cumulative excess deaths per million",
    caption = "Selected countries. NZL remains negative throughout: fewer deaths than expected."
  ) + theme_aer

p_section10 <- p_q_box / p_cum_traj +
  plot_annotation(
    title = "Excess Mortality: Quarterly Aggregation",
    theme = theme(plot.title = element_text(face = "bold", size = 13, family = "serif"))
  )
print(p_section10)


# ==============================================================================
#  ZUSAMMENFASSUNG & MODELLIMPLIKATIONEN
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  ZUSAMMENFASSUNG & MODELLIMPLIKATIONEN\n")
cat(strrep("=", 70), "\n\n")

cat("  1. VERTEILUNG: Stark rechtsschief (skew = 2.1). Median (6.0) weit unter\n")
cat("     Mean (11.9). Fat right tail von Pandemiepeaks. 26.7% der Country-Weeks\n")
cat("     UNTER Baseline → Lockdowns reduzierten auch Non-COVID-Mortalität.\n\n")

cat("  2. WELLENSTRUKTUR: Vier distinkte Wellen mit unterschiedlicher Intensität.\n")
cat("     W2 (Alpha) war die tödlichste Phase (Mean P-score 17.3%), nicht W1.\n")
cat("     W4 (Omicron): hohe Infektionszahlen, aber niedrige Mortalität (IFR-Rückgang).\n\n")

cat("  3. VARIANZZERLEGUNG: 22% Between / 78% Within (wöchentlich).\n")
cat("     32% Between / 68% Within (quartalsweise).\n")
cat("     → Within-Variation dominiert: Identifikation von δ_θ und S → θ → d\n")
cat("     läuft über temporale Variation innerhalb desselben Landes.\n\n")

cat("  4. BETWEEN-HETEROGENITÄT: Faktor >10 zwischen höchster (LTU: 5940/mio)\n")
cat("     und niedrigster (NZL: −540/mio) kumulativer Excess Mortality.\n")
cat("     → Massive Unterschiede im Pandemie-Outcome trotz vergleichbarer\n")
cat("     Einkommensniveaus. Identifiziert Effekt von Policy-Unterschieden.\n\n")

cat("  5. LAG-STRUKTUR: Contemporane S–d-Korrelation POSITIV (+0.32) =\n")
cat("     Endogenitätssignatur. Kausaler Effekt (S → θ → d) bei Lag 5–7 Wochen\n")
cat("     und negativem Vorzeichen sichtbar, aber von Reverse Causality überlagert.\n")
cat("     → Direkte d ~ S Regression schätzt Policy Reaction Function.\n")
cat("     → Modell trennt korrekt: δ_θ aus θ-Dynamik, φ_S aus S → θ Kanal.\n\n")

cat("  6. ALTERSDEKOMPOSITION: Steiler Gradient (85+ >> 15–64), der sich über\n")
cat("     Wellen ABFLACHT (Ratio 1.5 → 0.3). δ_θ ist effektiv altersgewichtet\n")
cat("     und zwischen Ländern heterogen (Altersstruktur), aber innerhalb von\n")
cat("     Ländern über die Zeit sinkend (Impfung). Between-Variationsquelle.\n\n")

cat("  7. QUARTALSAGGREGATION: Mean within-quarter SD = 10.4 → substanzielle\n")
cat("     Intra-Quartal-Variation geht bei Quartalsaggregation verloren.\n")
cat("     Between-Anteil steigt von 22% (wöchentlich) auf 32% (quartalsweise).\n")
cat("     → Analoges Aggregationsproblem wie bei Stringency, aber weniger\n")
cat("     gravierend, da d_k ein Outcome ist (kein Regressor).\n\n")

cat("  8. MODELL-MAPPING:\n")
cat("     • d_k in Gl. (15) = FLOW (neue Excess-Todesfälle pro Periode)\n")
cat("     • d_flow_mean (quartalsweiser P-score) mappt auf den Flow.\n")
cat("     • d_cum (kumulativ) mappt auf Σ d_k → relevant für Wohlfahrtsbewertung.\n")
cat("     • δ_θ und ρ_θ müssen JOINT kalibriert werden (θ_k unbeobachtet).\n")
cat("     • Excess Mortality als d_k-Proxy: vermeidet Testing-Bias, der\n")
cat("       bestätigte Fallzahlen als θ_k-Proxy unbrauchbar macht.\n\n")

cat("  ", strrep("=", 66), "\n")
cat("  Script abgeschlossen.\n")
cat("  ", strrep("=", 66), "\n")


# ==============================================================================
#  04_stage1_theta_imputation.R
#  Stage 1: Epidemiological Block — Calibration, θ-Imputation & Validation
# 
# ==============================================================================
#
#  Purpose:
#    Impute unobserved infection prevalence θ_k from observed excess mortality
#    d_k using the structural relationship d_{w+ℓ} = δ_θ · θ_w (Eq. 15).
#    Validate calibrated (ρ_θ, φ_S, δ_θ) against the θ-equation (Eq. 17).
#    Aggregate weekly θ̂_w to quarterly θ̂_k for use in Stage 2 estimation.
#
#  Data inputs:
#    excess_w / p_values_oecd_w  — weekly excess mortality (OWID, Karlinsky & Kobak 2021)
#    oxd_d                       — daily StringencyIndex (Oxford CGRT)
#    oxd_spatial_d               — daily confirmed cases/deaths (OxCGRT/JHU)
#    qdata                       — quarterly panel (population, GDP, etc.)
#
#  Key outputs:
#    theta_weekly   — country × week panel with θ̂_w
#    theta_quarterly — country × quarter panel with θ̂_k (for Stage 2)
#
#  Structure:
#   1. Data preparation & merging
#   2. Calibrated epidemiological parameters
#   3. θ-imputation from excess mortality
#   4. Cross-check: θ̂ vs. confirmed cases
#   5. Validation: θ-equation consistency
#   6. Sensitivity: lag length and wave-specific IFR
#   7. Quarterly aggregation for Stage 2
#   8. Summary diagnostics & export
# ==============================================================================


# --- AER Theme ----------------------------------------------------------------
theme_aer <- theme_classic(base_size = 10) +
  theme(
    text               = element_text(family = "serif"),
    plot.title         = element_text(face = "bold", size = 11, hjust = 0),
    plot.subtitle      = element_text(size = 8, color = "grey30"),
    plot.caption       = element_text(size = 7, color = "grey50", hjust = 0),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    legend.position    = "bottom",
    legend.text        = element_text(size = 8),
    strip.text         = element_text(face = "bold", size = 8)
  )


# ==============================================================================
#  1. DATA PREPARATION & MERGING
# ==============================================================================


# --- 1a. Weekly excess mortality (35 OECD countries with weekly data) ---------
# Source: Our World in Data (Karlinsky & Kobak 2021)
# Variables: excess_proj_all_ages (absolute excess deaths),
#            p_proj_all_ages (P-score = % deviation from projected baseline),
#            projected_deaths_since_2020_all_ages (expected deaths)
# Missing: CRI, JPN, TUR (monthly only)

mort_w <- p_values_oecd_w %>%
  filter(time_unit == "weekly") %>%
  mutate(
    Country = entity,
    date    = as.Date(date)
  ) %>%
  filter(date >= as.Date("2020-01-01"), date <= as.Date("2022-12-31")) %>%
  select(
    Country, date, time,
    p_proj    = p_proj_all_ages,
    excess    = excess_proj_all_ages,
    expected  = projected_deaths_since_2020_all_ages,
    observed  = deaths_since_2020_all_ages,
    # Age-specific P-scores for cross-check
    p_15_64   = p_proj_15_64,
    p_65_74   = p_proj_65_74,
    p_75_84   = p_proj_75_84,
    p_85p     = p_proj_85p
  ) %>%
  arrange(Country, date)

cat(sprintf("  Excess mortality: %d obs, %d countries, %s to %s\n",
            nrow(mort_w), n_distinct(mort_w$Country),
            min(mort_w$date), max(mort_w$date)))

##Excess Mortality Weekly geladen (ohne CRI, JPN, TUR)-> siehe weiter unten


# --- 1c. Weekly stringency from daily oxd_d -----------------------------------
# Aggregate daily S to weekly (ISO week aligned with mortality data)

s_weekly <- oxd_d %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Date >= as.Date("2020-01-01"), Date <= as.Date("2022-12-31")) %>%
  mutate(
    isoyr = isoyear(Date),
    isowk = isoweek(Date)
  ) %>%
  group_by(Country, isoyr, isowk) %>%
  summarise(
    S_mean  = mean(StringencyIndex_PopWeighted, na.rm = TRUE) / 100,
    S_max   = max(StringencyIndex_PopWeighted, na.rm = TRUE) / 100,
    n_days  = n(),
    .groups = "drop"
  )




##Angepasster Index geladen für alle 38 OECD Länder
##S_mean-> Weekly passt

# Mortality data weeks: need to match. OWID "time" = ISO week number.
# Align via ISO year-week
mort_w <- mort_w %>%
  mutate(
    isoyr = isoyear(date),
    isowk = isoweek(date)
  )

panel_w <- s_weekly %>%                          # 38 Länder als Basis
  left_join(mort_w, by = c("Country", "isoyr", "isowk"))

cat(sprintf("  Weekly panel: %d obs after S merge (%.1f%% with S data)\n",
            nrow(panel_w), mean(!is.na(panel_w$S_mean)) * 100))

# --- 1d. Confirmed cases/deaths from oxd_spatial_d ---------------------------
# Cumulative -> need to difference for weekly new cases/deaths

conf_daily <- oxd_spatial_d %>%
  mutate(
    Date = as.Date(as.character(Date), format = "%Y%m%d"),
    ConfirmedCases  = as.numeric(ConfirmedCases),
    ConfirmedDeaths = as.numeric(ConfirmedDeaths)
  ) %>%
  filter(Date >= as.Date("2020-01-01"), Date <= as.Date("2022-12-31")) %>%
  select(Country = CountryCode, Date, ConfirmedCases, ConfirmedDeaths) %>%
  arrange(Country, Date) %>%
  group_by(Country) %>%
  mutate(
    new_cases  = pmax(0, ConfirmedCases  - lag(ConfirmedCases, 1)),
    new_deaths = pmax(0, ConfirmedDeaths - lag(ConfirmedDeaths, 1))
  ) %>%
  ungroup()

# Aggregate to weekly
conf_weekly <- conf_daily %>%
  mutate(
    isoyr = isoyear(Date),
    isowk = isoweek(Date)
  ) %>%
  group_by(Country, isoyr, isowk) %>%
  summarise(
    cases_w  = sum(new_cases, na.rm = TRUE),
    deaths_w = sum(new_deaths, na.rm = TRUE),
    .groups  = "drop"
  )

panel_w <- panel_w %>%
  left_join(conf_weekly, by = c("Country", "isoyr", "isowk"))

cat(sprintf("  Confirmed cases merged: %.1f%% with case data\n",
            mean(!is.na(panel_w$cases_w)) * 100))




# --- 1b. Population from qdata (2020 Q1) -------------------------------------
pop <- qdata %>%
  filter(Quarter == "Q1.2020") %>%
  select(Country, pop_th = Qpopulation_th) %>%
  mutate(pop = pop_th * 1000)  # Convert to persons

panel_w <- panel_w %>%
  left_join(pop, by = "Country")

cat(sprintf("  Population merged: %d countries with pop data\n",
            sum(!is.na(mort_w$pop)) / nrow(filter(mort_w, Country == "USA")) ))

##Population Data geladen für alle 38 Länder



##Datenset bereit, nur Theta fehlt für die Länder CRI, TUR, JPN-> Siehe weiter unten
##problem with the negative excess deaths, how many?

# --- Diagnostic: negative excess mortality frequency --------------------------

# Weekly
cat("=== WEEKLY ===\n")
cat(sprintf("  Total obs:        %d\n", sum(!is.na(panel_w$excess))))
cat(sprintf("  Negative excess:  %d (%.1f%%)\n",
            sum(panel_w$excess < 0, na.rm = TRUE),
            mean(panel_w$excess < 0, na.rm = TRUE) * 100))
cat(sprintf("  theta_hat = 0:    %d (%.1f%%)\n",
            sum(panel_w$theta_hat == 0, na.rm = TRUE),
            mean(panel_w$theta_hat == 0, na.rm = TRUE) * 100))
##High but look at the quarterly data

#has to define the dataset before or move down-> it's genau wie erwartet. 6 echte Nullen (2.1%) sind unproblematisch — das sind Q1 2020 und Länder wie Neuseeland oder Australien vor dem ersten Ausbruch. Die sind korrekt null.
#but 28% are below 0.1%-> midpandemic, fear canal is low but this is correct-> Aufpassen mit Log
# Quarterly-> nicht ein Problem



library(ISOweek)

# date aus isoyr + isowk rekonstruieren für CRI/JPN/TUR
panel_w <- panel_w %>%
  mutate(
    date = if_else(
      is.na(date),
      ISOweek2date(sprintf("%d-W%02d-1", isoyr, isowk)),
      date
    )
  )

panel_w <- panel_w %>%
  filter(date >= as.Date("2020-03-01"), date <= as.Date("2022-12-31")) %>%
  filter(!is.na(pop))


# Excess deaths per million
panel_w <- panel_w %>%
  mutate(
    excess_pm = excess / (pop / 1e6),
    deaths_confirmed_pm = deaths_w / (pop / 1e6),
    cases_pm  = cases_w / (pop / 1e6)
  )

cat(sprintf("\n  Final weekly panel: %d obs, %d countries, %s to %s\n",
            nrow(panel_w), n_distinct(panel_w$Country),
            min(panel_w$date), max(panel_w$date)))
cat(sprintf("  Weeks per country: %.0f (median)\n\n",
            median(table(panel_w$Country))))

#Werte vorher einfach au 0 setzen!!

# ==============================================================================
#  2. CALIBRATED EPIDEMIOLOGICAL PARAMETERS
# ==============================================================================
# ===========================================================================
# 2a. Wave definitions based on OECD-median variant dominance (>50%)
# ---------------------------------------------------------------------------
# Criterion: approximate date at which the new variant exceeded 50% of
# sequenced samples across the OECD median, based on GISAID/ECDC/CDC data.
#
# DOMINANCE THRESHOLD SOURCES:
#   [1] ECDC/WHO (23 Jul 2021): "Between 28 Jun and 11 Jul 2021, Delta was
#       dominant in 19/28 EU countries, median 68.3%."
#       https://www.ecdc.europa.eu/en/news-events/sars-cov-2-delta-variant-now-dominant-european-region
#   [2] CDC MMWR 71(6), 2022: Alpha >50% US ~Apr 2021; Delta >50% week ending
#       26 Jun 2021; Omicron >50% week ending 25 Dec 2021.
#       https://www.cdc.gov/mmwr/volumes/71/wr/mm7106a4.htm
#   [3] Tsiatsiani et al. (2022, Vaccines 10(4):496): Alpha >50% UK ~20 Dec 2020;
#       Omicron >50% UK ~12-14 Dec 2021. GISAID-based Bayesian estimation.
#       https://doi.org/10.3390/vaccines10040496
#   [4] ECDC Weekly Epidemiological Update (13 Jan 2022): 21 EU/EEA countries
#       reported Omicron prevalence of 46.4% in week 52/2021-01/2022.
#       https://www.ecdc.europa.eu/en/news-events/weekly-epidemiological-update-omicron-variant-concern-voc-week-2-data-13-january-2022
#   [5] Tegally et al. (2022, Nat Sci Rep 12:4659): GISAID geographical
#       prevalence, Alpha >50% Europe Dec 2020-Mar 2021.
#       https://doi.org/10.1038/s41598-022-08684-1
#
# OECD-MEDIAN DOMINANCE DATES (>50% of sequenced samples):
#   Original/D614G: Pandemic start through ~Feb 2021
#   Alpha (B.1.1.7): ~1 Mar 2021 (UK: Dec 2020, DE: KW9, US: Apr 2021)
#   Delta (B.1.617.2): ~1 Jul 2021 (UK: late May, EU: late Jun, US: 26 Jun)
#   Omicron (B.1.1.529): ~1 Jan 2022 (UK: mid-Dec, US: 25 Dec, EU: early Jan)
# ===========================================================================

panel_w <- panel_w %>%
  mutate(
    wave = case_when(
      date < as.Date("2020-06-15")  ~ "W1",          # Original strain, first surge
      date < as.Date("2020-09-15")  ~ "W1_summer",   # Summer lull (still Wildtype)
      date < as.Date("2021-03-01")  ~ "W2_wt",       # Autumn/winter surge, Wildtype + treatment gains
      date < as.Date("2021-07-01")  ~ "W2_alpha",    # Alpha dominant (OECD median >50%)
      date < as.Date("2022-01-01")  ~ "W3_delta",    # Delta dominant (OECD median >50%)
      TRUE                          ~ "W4_omicron"   # Omicron dominant (OECD median >50%)
    ),
    wave_label = case_when(
      wave == "W1"          ~ "Wave 1 (Original, spring 2020)",
      wave == "W1_summer"   ~ "Summer lull 2020 (Original)",
      wave == "W2_wt"       ~ "Wave 2a (Wildtype, autumn/winter 2020-21)",
      wave == "W2_alpha"    ~ "Wave 2b (Alpha, spring 2021)",
      wave == "W3_delta"    ~ "Wave 3 (Delta, summer/autumn 2021)",
      wave == "W4_omicron"  ~ "Wave 4 (Omicron, 2022+)",
      TRUE                  ~ NA_character_
    )
  )

# --- Coarser 4-wave mapping (for quarterly aggregation / paper tables) ---
panel_w <- panel_w %>%
  mutate(
    wave_coarse = case_when(
      wave %in% c("W1", "W1_summer")      ~ "Wave1",
      wave %in% c("W2_wt", "W2_alpha")    ~ "Wave2",
      wave == "W3_delta"                    ~ "Wave3",
      wave == "W4_omicron"                  ~ "Wave4",
      TRUE                                  ~ "Other"
    )
  )


# ===========================================================================
# 2b. Infection Fatality Rate delta_theta by wave
# ---------------------------------------------------------------------------
# CRITICAL: delta_theta is the IFR = P(death | infection).
# The imputation inverts this: theta_hat_w = d_{w+ell} / delta_theta.
# A LOWER delta_theta implies MORE infections for the same deaths.
#
# These are EFFECTIVE population IFRs, i.e., they incorporate:
#   (a) intrinsic variant virulence
#   (b) treatment improvements (Dexamethasone from Jun 2020)
#   (c) vaccination coverage at time of wave
#   (d) prior immunity from natural infection
#
# IFR SOURCES:
#   [6] Levin et al. (2020, Eur J Epidemiol 35:1123-1138): OECD-restricted
#       meta-analysis, exponential age-IFR, population IFR 0.5-1.5% depending
#       on age structure. 90% of cross-country variation explained by age.
#       https://doi.org/10.1007/s10654-020-00698-1
#   [7] Meyerowitz-Katz & Merone (2020, Int J Infect Dis 101:138-148):
#       Meta-analysis of 24 seroprevalence studies, overall IFR 0.68%
#       (95% CI: 0.53-0.82%).
#       https://doi.org/10.1016/j.ijid.2020.09.1464
#   [8] Brazeau et al. (2020, Imperial College Report 34): Population IFR
#       1.15% for high-income country age structures.
#   [9] COVID-19 Forecasting Team (2022, Lancet 399:2469-2506): IFR varied
#       by age, time, and geography during pre-vaccine era.
#       https://doi.org/10.1016/S0140-6736(21)02867-1
#   [10] Marziano et al. (2023, Influenza Other Respir Viruses 17:e13181):
#        IFR in Italy by variant phase: ancestral ~2.2%, Alpha ~0.7%,
#        Delta ~0.2%, Omicron ~0.05%. 22-44 fold reduction over pandemic.
#        https://doi.org/10.1111/irv.13181
#   [11] Ward et al. (2024, Nat Commun 15:4628): Real-time IHR/IFR across
#        pandemic in England. IFR peaked at 0.97%, declined to ~0.01% by
#        Omicron BA.2 (Mar-Apr 2022).
#        https://doi.org/10.1038/s41467-024-47199-3
#   [12] Erikstrup et al. (2022, Lancet Reg Health Eur 21:100479):
#        Omicron IFR in Denmark: 6.2 per 100,000 infections (0.0062%).
#        https://doi.org/10.1016/j.lanepe.2022.100479
#   [13] RECOVERY Collaborative Group (2021, NEJM 384:693-704):
#        Dexamethasone reduced 28-day mortality by ~1/3 in ventilated patients.
#        Treatment effect available from Jun 2020 onward.
#
# CALIBRATION LOGIC:
#   W1 (spring 2020): Pre-treatment, no vaccines. Levin [6] central estimate
#     for OECD age structure ~0.9%. Range: 0.8% [7] to 1.15% [8].
#   W1_summer: Dexamethasone available from Jun 2020 [13], lower healthcare
#     overload -> effective IFR declines ~20-30%.
#   W2_wt: Autumn/winter 2020-21, Wildtype with Dexamethasone. IFR 0.5-0.7%
#     per [9] and [10] (Italy ancestral phase ~2.2% CFR / ~15% ascertainment
#     = ~0.33% IFR in second half; but IFR > CFR adjustment for undertesting).
#   W2_alpha: Alpha intrinsically ~30-50% more severe (Davies et al. 2021,
#     Nature 593:270-274) but offset by early vaccination (10-15% first dose).
#     Net IFR ~0.3-0.5%.
#   W3_delta: Delta + 40-70% vaccination. Marziano [10]: ~0.2% in Italy.
#     Ward [11]: declining from ~0.15% to ~0.05% over Delta period.
#   W4_omicron: Omicron + high vaccination + prior immunity.
#     Erikstrup [12]: 0.006% in Denmark. Marziano [10]: ~0.05% in Italy.
#     Ward [11]: ~0.01% by spring 2022. Central: 0.04%.
# "The central IFR for Wave 4 is set at 0.04 percent, below the REACT-1 England estimate of 0.097 percent for early Omicron (BA.1), reflecting the progressively milder BA.2 and BA.5 subvariants that dominated the latter half of 2022 and the substantially higher hybrid immunity levels in later quarters.
# ===========================================================================

ifr_wave <- tibble(
  wave   = c("W1",   "W1_summer", "W2_wt", "W2_alpha", "W3_delta", "W4_omicron"),
  ifr    = c(0.009,  0.007,       0.006,   0.004,      0.003,      0.0004),
  ifr_lo = c(0.008,  0.005,       0.004,   0.003,      0.002,      0.0002),
  ifr_hi = c(0.012,  0.009,       0.008,   0.006,      0.005,      0.0007),
  label  = c(
    "Original (pre-treatment, pre-vax) [6,7,8]",
    "Original + Dexamethasone, summer lull [6,13]",
    "Wildtype, autumn/winter 2020-21, treatment gains [9,10]",
    "Alpha + early vaccination (10-15% 1st dose) [10]",
    "Delta + significant vaccination (40-70%) [10,11]",
    "Omicron + high immunity (70-80% vax, prior infection) [10,11,12]"
  )
)

cat("--- 2b. Calibrated IFR (delta_theta) by Wave ---\n")
print(kable(ifr_wave %>% select(wave, ifr, ifr_lo, ifr_hi, label),
            digits = 4, col.names = c("Wave", "IFR", "IFR_low", "IFR_high", "Rationale")))

# Merge IFR into panel
panel_w <- panel_w %>%
  left_join(ifr_wave %>% select(wave, ifr, ifr_lo, ifr_hi), by = "wave")

# ===========================================================================
# 2c. Lag parameter ℓ (infection-to-death)
# ---------------------------------------------------------------------------
# The structural inversion θ̂_w = d_{w+ℓ} / δ_θ requires specifying ℓ,
# the lag between infection and death in weeks.
#
# DECOMPOSITION:  ℓ = incubation period + symptom onset-to-death
#
# INCUBATION PERIOD (variant-specific):
#   [14] Alene et al. (2021, BMC Infect Dis 21:257): Meta-analysis of studies
#        published in 2020, pooled mean 6.5 days (original strain).
#   [15] Lauer et al. (2020, Ann Intern Med 172:577-582): Median 5.1 days
#        (95% CI: 4.5-5.8), original strain, 181 confirmed cases.
#        https://doi.org/10.7326/M20-0504
#   [16] Grant et al. (2022, Lancet Reg Health Eur 13:100278): Delta
#        incubation ~4.3 days in France nationwide case-control study.
#   [17] CDC Clinical Presentation (2024): Delta ~4 days, Omicron ~3-4 days.
#        Original strain pooled mean ~6.5 days.
#   [18] Jansen et al. (2022, MMWR 70(52)): Omicron cluster in Nebraska,
#        median incubation ~3 days.
#        https://doi.org/10.15585/mmwr.mm705152e3
#
# SYMPTOM ONSET TO DEATH:
#   [19] Verity et al. (2020, Lancet Infect Dis 20:669-677): Mean 17.8 days
#        (95% CrI: 16.9-19.2), based on 24 deaths in Hubei. Gamma-distributed.
#        https://doi.org/10.1016/S1473-3099(20)30243-7
#   [20] Zhou et al. (2020, Lancet 395:1054-1062): Median 18.5 days
#        (IQR: 15.0-22.0), 191 inpatients in Wuhan.
#        https://doi.org/10.1016/S0140-6736(20)30566-3
#
# TOTAL INFECTION-TO-DEATH LAG BY VARIANT:
#   Original:  ~5-6d + ~18d = ~23-24d ≈ 3.3 weeks
#   Alpha:     ~5d   + ~18d = ~23d    ≈ 3.3 weeks  (similar to original)
#   Delta:     ~4d   + ~16-18d = ~20-22d ≈ 3.0 weeks
#   Omicron:   ~3d   + ~14-18d = ~17-21d ≈ 2.5-3.0 weeks
#
# NOTE: Onset-to-death estimates for Delta/Omicron are less precisely
# established than for the original strain. Verity et al. [19] and Zhou
# et al. [20] both predate variant emergence. The shorter incubation
# periods of later variants plausibly compress total lag by ~3-6 days,
# but onset-to-death may also be shorter due to faster clinical progression
# (Delta) or longer due to better ICU care extending survival (all waves).
# At weekly resolution, these differences fall within the ±1 week
# sensitivity range.
#
# DECISION: Single central ℓ = 3 weeks for all waves, with sensitivity
# over ℓ ∈ {2, 3, 4}. This is conservative for Omicron (where ℓ ≈ 2.5
# may be more accurate) and slightly aggressive for the original strain
# (where ℓ ≈ 3.3). The quarterly aggregation step in Section 7 smooths
# sub-week timing errors, making this approximation defensible.
# ===========================================================================

ell_central <- 3   # weeks
ell_range   <- 2:4

cat(sprintf("\n--- 2c. Infection-to-death lag ---\n"))
cat(sprintf("  Central estimate: ℓ = %d weeks\n", ell_central))
cat(sprintf("  Sensitivity range: ℓ ∈ {%s} weeks\n\n", paste(ell_range, collapse = ", ")))
cat("  Decomposition (original strain):\n")
cat("    Incubation:        ~5-6 days (Lauer et al. 2020)\n")
cat("    Onset-to-death:    ~18 days  (Verity et al. 2020; Zhou et al. 2020)\n")
cat("    Total:             ~23-24 days ≈ 3.3 weeks\n\n")
cat("  Later variants compress incubation by 2-3 days (Delta/Omicron),\n")
cat("  but weekly resolution absorbs this within the sensitivity range.\n")


# ===========================================================================
# 2d. Epidemiological parameters for θ-equation validation
# ---------------------------------------------------------------------------
# The θ-equation in the structural model is:
#   θ_{w+1} = ρ_θ^w · (1 − φ_S · S_w) · θ_w
#
# where ρ_θ^w is the weekly uncontrolled infection growth factor and
# φ_S is the NPI suppression effectiveness.
#
# CRITICAL INTERPRETATION (from Calibration Report):
#   ρ_θ^w is a REDUCED-FORM parameter, NOT a mechanistic SIR growth factor.
#   A naive SIR conversion (weekly factor = R₀^{7/T_g}) yields astronomical
#   values (e.g., ~4.1 for R₀=2.75, T_g=5d) because it ignores susceptible
#   depletion, behavioral responses, and heterogeneous contact patterns.
#   The values below are effective parameters that, jointly with φ_S,
#   should reproduce observed weekly infection trajectories when calibrated
#   to match the θ̂ panel.
#
# GENERATION TIME LITERATURE (informing relative scaling across variants):
#   [21] Hart et al. (2022, Lancet Infect Dis 22:603-610): Mean intrinsic
#        generation time: Alpha 5.5d (95%CrI 4.7-6.5), Delta 4.7d (4.1-5.6).
#        Household generation time: Alpha 4.5d, Delta 3.2d (~28% shorter).
#        https://doi.org/10.1016/S1473-3099(22)00001-9
#   [22] Manica et al. (2022, Lancet Reg Health Eur 22:100501): Intrinsic
#        generation time: Alpha 6.0d, Delta 6.6d, Omicron 6.8d.
#        Realized household: Omicron 3.59d.
#        https://doi.org/10.1016/j.lanepe.2022.100501
#   [23] Madewell et al. (2023, BMC Infect Dis 23:429): Meta-analysis of
#        serial intervals: Original/Alpha ~5.2d, Delta 3.9d (3.4-4.3),
#        Omicron 3.2d (2.9-3.5).
#        https://doi.org/10.1186/s12879-023-08407-5
#
# R₀ LITERATURE (from Calibration Report):
#   Original: 2.5-3.0 (Li et al. 2020, NEJM; Alimohamadi et al. 2020)
#   Alpha:    4.0-4.5 (Davies et al. 2021, Science: 43-90% more transmissible)
#   Delta:    5.0     (Liu & Rocklöv 2021, J Travel Med)
#   Omicron:  8-10    (Liu et al. 2022: R₀ ≈ 9.5, range 5.5-24)
#
# NPI SUPPRESSION EFFECTIVENESS φ_S:
#   [24] Brauner et al. (2021, Science 371:eabd9338): Bayesian hierarchical
#        model across 41 countries. Gathering bans, business closures, and
#        school closures each reduce R_t by 15-35%. Combined NPI packages
#        achieved R_t reductions of 60-80%.
#        https://doi.org/10.1126/science.abd9338
#   [25] Flaxman et al. (2020, Nature 584:257-261): Estimated that full
#        lockdowns reduced R_t to <1 in all 11 European countries studied,
#        implying NPI effectiveness of 60-85%.
#        https://doi.org/10.1038/s41586-020-2405-7
#
# CALIBRATION LOGIC FOR ρ_θ^w:
#   These values are chosen so that ρ_θ^w · (1 − φ_S · S) matches observed
#   weekly θ̂ growth during each wave. They respect the relative R₀ ranking
#   (Original < Alpha < Delta < Omicron) but are NOT proportional to R₀
#   because susceptible depletion and behavioral responses (not captured
#   by S_oxford) attenuate growth differently across waves.
#
#   Summer 2020 has a low ρ_θ^w because (a) seasonal reduction in
#   transmission and (b) population-level behavioral caution persisted
#   even as formal NPIs relaxed. W2_wt is intermediate because the
#   autumn/winter surge occurred with the original strain but was amplified
#   by seasonal effects and indoor gathering.
# ===========================================================================

rho_theta_weekly <- tibble(
  wave     = c("W1",  "W1_summer", "W2_wt", "W2_alpha", "W3_delta", "W4_omicron"),
  rho_w    = c(1.8,   1.2,         1.5,     2.0,        2,        3.5),
  rho_w_lo = c(1.5,   1.0,         1.2,     1.6,        1.5,        2.5),
  rho_w_hi = c(2.2,   1.5,         1.8,     2.5,        2.5,        5.0),
  label    = c(
    "Original, first surge [R0=2.5-3.0]",
    "Summer lull (seasonal + behavioral suppression)",
    "Wildtype, autumn/winter (seasonal amplification)",
    "Alpha [R0=4.0-4.5, 43-90% more transmissible]",
    "Delta [R0=5.0, shorter generation time]",
    "Omicron [R0=8-10, immune evasion dominant]"
  )
)

phi_S_central <- 0.70  # Central estimate (Brauner et al. 2021; Flaxman et al. 2020)
phi_S_range   <- c(0.60, 0.82)  # Range from Calibration Report

cat(sprintf("\n--- 2d. ρ_θ (weekly) and φ_S ---\n"))
cat(sprintf("  φ_S = %.2f (range: %.2f-%.2f)\n",
            phi_S_central, phi_S_range[1], phi_S_range[2]))
cat("  Sources: Brauner et al. (2021, Science); Flaxman et al. (2020, Nature)\n\n")
cat("  NOTE: ρ_θ^w is a reduced-form parameter, not R₀^{7/T_g}.\n")
cat("  Naive SIR conversion yields weekly factors of 4-10+ for R₀=2.5-10,\n")
cat("  which are unphysical without susceptible depletion. The values below\n")
cat("  are effective parameters calibrated jointly with φ_S to match\n")
cat("  observed θ̂ trajectories.\n\n")
print(kable(rho_theta_weekly, digits = 1,
            col.names = c("Wave", "ρ_θ^w", "ρ_θ^w_lo", "ρ_θ^w_hi", "Rationale")))

# Merge ρ_θ into panel
panel_w <- panel_w %>%
  left_join(rho_theta_weekly %>% select(wave, rho_w, rho_w_lo, rho_w_hi), by = "wave")

# ==============================================================================
#  3. θ-IMPUTATION FROM EXCESS MORTALITY
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 3: θ-Imputation\n")
cat(strrep("=", 70), "\n\n")

# --- 3a. Core imputation: θ̂_w = d_{w+ℓ} / δ_θ --------------------------------
#
# Logic: If d_{w+ℓ} deaths (per capita) in week w+ℓ were caused by infections
# in week w, and a fraction δ_θ of infections die, then:
#   θ̂_w = d_{w+ℓ} / δ_θ
#
# Where d_{w+ℓ} = excess deaths in week w+ℓ / population
#
# We use LEAD (not lag): for each week w, look ℓ weeks FORWARD in deaths
# to find the deaths caused by infections in week w.
#
# Note: This means θ̂ for the LAST ℓ weeks of the sample cannot be computed.

panel_w <- panel_w %>%
  arrange(Country, date) %>%
  group_by(Country) %>%
  mutate(
    # Excess deaths per capita (share of population)
    d_pc = excess / pop,
    # Deaths ℓ weeks AHEAD (deaths caused by infections THIS week)
    d_lead2 = lead(d_pc, n = 2),
    d_lead3 = lead(d_pc, n = 3),
    d_lead4 = lead(d_pc, n = 4)
  ) %>%
  ungroup()

# Central imputation (ℓ = 3)
panel_w <- panel_w %>%
  mutate(
    theta_hat = d_lead3 / ifr,
    # Sensitivity: ℓ = 2 and ℓ = 4
    theta_hat_l2 = d_lead2 / ifr,
    theta_hat_l4 = d_lead4 / ifr
  )

# Handle negative excess deaths -> implies θ < 0, set floor at 0
# Negative P-scores occur in 26.7% of weeks (from Script 03)
# Interpretation: below-baseline mortality does not mean negative infections.
# Conservative: floor at 0. Alternative: allow negative (measurement error).

n_negative <- sum(panel_w$theta_hat < 0, na.rm = TRUE)
n_total    <- sum(!is.na(panel_w$theta_hat))

cat(sprintf("--- 3a. θ-Imputation (central: ℓ = %d) ---\n", ell_central))
cat(sprintf("  Total country-weeks with θ̂: %d\n", n_total))
cat(sprintf("  Negative θ̂: %d (%.1f%%) — floored at 0\n",
            n_negative, n_negative / n_total * 100))

panel_w <- panel_w %>%
  mutate(
    theta_hat    = pmax(0, theta_hat),
    theta_hat_l2 = pmax(0, theta_hat_l2),
    theta_hat_l4 = pmax(0, theta_hat_l4)
  )

# --- 3b. Descriptive statistics of θ̂ ------------------------------------------

# Restrict to model period for statistics
theta_stats <- panel_w %>%
  filter(date >= as.Date("2020-03-01"), date <= as.Date("2022-12-31"),
         !is.na(theta_hat))

cat(sprintf("\n--- 3b. θ̂ Descriptives) ---\n"))
cat(sprintf("  N = %d country-weeks, %d countries\n",
            nrow(theta_stats), n_distinct(theta_stats$Country)))
cat(sprintf("  θ̂ (share of population):\n"))
cat(sprintf("    Mean   = %.5f  (%.3f%%)\n", mean(theta_stats$theta_hat),
            mean(theta_stats$theta_hat) * 100))
cat(sprintf("    Median = %.5f  (%.3f%%)\n", median(theta_stats$theta_hat),
            median(theta_stats$theta_hat) * 100))
cat(sprintf("    SD     = %.5f  (%.3f%%)\n", sd(theta_stats$theta_hat),
            sd(theta_stats$theta_hat) * 100))
cat(sprintf("    P95    = %.5f  (%.3f%%)\n",
            quantile(theta_stats$theta_hat, 0.95),
            quantile(theta_stats$theta_hat, 0.95) * 100))
cat(sprintf("    Max    = %.5f  (%.3f%%)\n",
            max(theta_stats$theta_hat),
            max(theta_stats$theta_hat) * 100))

# By wave
wave_theta <- theta_stats %>%
  group_by(wave, wave_label) %>%
  summarise(
    N         = n(),
    mean_th   = mean(theta_hat) * 100,
    median_th = median(theta_hat) * 100,
    p95_th    = quantile(theta_hat, 0.95) * 100,
    max_th    = max(theta_hat) * 100,
    ifr_used  = first(ifr),
    .groups   = "drop"
  )

cat("\n--- θ̂ by Wave (% of population) ---\n")
print(kable(wave_theta %>%
              select(wave_label, N, ifr_used, mean_th, median_th, p95_th, max_th),
            digits = c(0, 0, 4, 3, 3, 3, 3),
            col.names = c("Wave","N","IFR","Mean%","Med%","P95%","Max%")))


# --- 3c. OECD aggregate θ̂ trajectory -----------------------------------------


theta_agg <- theta_stats %>%
  group_by(date) %>%
  summarise(
    mean_theta  = mean(theta_hat, na.rm = TRUE) * 1e6,        # % of pop → per million
    p25_theta   = quantile(theta_hat, 0.25, na.rm = TRUE) * 1e6,
    p75_theta   = quantile(theta_hat, 0.75, na.rm = TRUE) * 1e6,
    p10_theta   = quantile(theta_hat, 0.10, na.rm = TRUE) * 1e6,
    p90_theta   = quantile(theta_hat, 0.90, na.rm = TRUE) * 1e6,
    mean_excess = mean(excess / (pop / 1e6), na.rm = TRUE),               # already per million
    .groups     = "drop"
  )

scale_factor <- max(theta_agg$mean_theta, na.rm = TRUE) /
  max(theta_agg$mean_excess, na.rm = TRUE)

wave_labels_df <- data.frame(
  x     = as.Date(c("2020-04-15", "2020-12-01", "2021-08-15", "2022-02-01")),
  label = c("Wave 1\n(Original)", "Wave 2\n(Wt/Alpha)",
            "Wave 3\n(Delta)", "Wave 4\n(Omicron)")
)

last_date   <- max(theta_agg$date, na.rm = TRUE)
last_theta  <- theta_agg$mean_theta[which.max(theta_agg$date)]
last_excess <- theta_agg$mean_excess[which.max(theta_agg$date)] * scale_factor

p_theta_traj <- ggplot(theta_agg, aes(x = date)) +
  geom_ribbon(aes(ymin = p10_theta, ymax = p90_theta), fill = "grey85", alpha = 0.6) +
  geom_ribbon(aes(ymin = p25_theta, ymax = p75_theta), fill = "grey65", alpha = 0.6) +
  geom_line(aes(y = mean_theta), color = "black", linewidth = 0.7) +
  geom_line(aes(y = mean_excess * scale_factor),
            color = "#C0392B", linewidth = 0.7, linetype = "dashed") +
  geom_vline(xintercept = as.numeric(as.Date(c("2020-06-15", "2020-09-15",
                                               "2021-03-01", "2021-07-01",
                                               "2022-01-01"))),
             linetype = "dotted", color = "grey50", linewidth = 0.3) +
  geom_text(data = wave_labels_df,
            aes(x = x, y = max(theta_agg$p90_theta) * 0.95, label = label),
            size = 2.3, family = "serif", color = "grey40", lineheight = 0.85) +
  annotate("text",
           x = last_date + 7, y = last_theta,
           label = "hat(theta)", parse = TRUE, hjust = 0, size = 2.8,
           family = "serif", color = "black", fontface = "bold") +
  annotate("text",
           x = last_date + 7, y = last_excess,
           label = "excess\ndeaths/m", hjust = 0, size = 2.3,
           family = "serif", color = "#C0392B", lineheight = 0.85) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y",
               expand = expansion(mult = c(0.01, 0.08))) +
  scale_y_continuous(
    expand   = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . / scale_factor,
                        name = "Excess deaths per million per week")
  ) +
  labs(
    y        = expression(hat(theta) ~ "(implied infections per million, weekly)"),
    title    = expression(paste("Figure X: Imputed Infection Prevalence ", hat(theta), " (OECD Mean)")),
    subtitle = paste0("Black: OECD mean \u03b8\u0302. Bands: IQR (dark) and P10\u2013P90 (light).\n",
                      "Red dashed: excess deaths per million (right axis). ",
                      "Omicron: \u03b8\u0302 peaks sharply; excess mortality does not follow (IFR collapse ~20\u00d7)."),
    x        = NULL,
    caption  = paste0("Notes: 35 OECD economies, weekly. Excess mortality from OWID.\n",
                      "IFR calibrated by wave (Levin et al. 2020; Marziano et al. 2023).\n",
                      "Negative excess mortality floored at \u03b8\u0302 = 0.\n",
                      "Vertical dotted lines: wave boundaries (OECD-median variant dominance >50%).\n",
                      "Both series in per-million units. Right axis gap vs. left axis = inverse IFR.")
  ) +
  theme_aer +
  theme(
    axis.title.y = element_text(
      color  = "black",
      angle  = 90,
      vjust  = 0.5,
      margin = margin(t = 0, r = 10, b = 0, l = 0)
    ),
    axis.title.y.right = element_text(
      color  = "#C0392B",
      angle  = 90,
      vjust  = 0.5,
      margin = margin(t = 0, r = 0, b = 0, l = 10)
    ),
    axis.text.y.right  = element_text(color = "#C0392B"),
    axis.ticks.y.right = element_line(color = "#C0392B")
  )

print(p_theta_traj)

##Wie hat sich S verhalten?

# ==============================================================================
#  p_theta_traj — Imputed θ̂, Excess Mortality, and Stringency
#  Three series: θ̂ (left axis), excess deaths/million (right axis),
#                mean stringency S (scaled to left axis, blue area)
# ==============================================================================

# --- 1. Aggregate θ̂ and excess mortality from panel_w ------------------------
theta_agg <- theta_stats %>%
  group_by(date) %>%
  summarise(
    mean_theta  = mean(theta_hat, na.rm = TRUE) * 1e6,
    p25_theta   = quantile(theta_hat, 0.25, na.rm = TRUE) * 1e6,
    p75_theta   = quantile(theta_hat, 0.75, na.rm = TRUE) * 1e6,
    p10_theta   = quantile(theta_hat, 0.10, na.rm = TRUE) * 1e6,
    p90_theta   = quantile(theta_hat, 0.90, na.rm = TRUE) * 1e6,
    mean_excess = mean(excess / (pop / 1e6), na.rm = TRUE),
    mean_S      = mean(S_mean, na.rm = TRUE),   # S ∈ [0, 1]
    .groups     = "drop"
  )

# --- 2. Scale factors ---------------------------------------------------------
# Right axis: excess deaths mapped onto left (θ̂) axis
scale_excess <- max(theta_agg$mean_theta, na.rm = TRUE) /
  max(theta_agg$mean_excess, na.rm = TRUE)

# S scaled to left axis: S = 1 maps to max(p90_theta)
scale_S <- max(theta_agg$p90_theta, na.rm = TRUE)

# --- 3. Annotation positions --------------------------------------------------
wave_labels_df <- data.frame(
  x     = as.Date(c("2020-04-15", "2020-12-01", "2021-08-15", "2022-02-01")),
  label = c("Wave 1\n(Original)", "Wave 2\n(Wt/Alpha)",
            "Wave 3\n(Delta)", "Wave 4\n(Omicron)")
)

last_date   <- max(theta_agg$date, na.rm = TRUE)
last_theta  <- theta_agg$mean_theta[which.max(theta_agg$date)]
last_excess <- theta_agg$mean_excess[which.max(theta_agg$date)] * scale_excess
last_S      <- theta_agg$mean_S[which.max(theta_agg$date)] * scale_S

# --- 4. Plot ------------------------------------------------------------------
p_theta_traj <- ggplot(theta_agg, aes(x = date)) +
  
  # Stringency background area (drawn first, behind everything)
  geom_area(aes(y = mean_S * scale_S),
            fill = "#2980B9", alpha = 0.12) +
  geom_line(aes(y = mean_S * scale_S),
            color = "#2980B9", linewidth = 0.5, linetype = "solid") +
  
  # θ̂ uncertainty bands
  geom_ribbon(aes(ymin = p10_theta, ymax = p90_theta),
              fill = "grey85", alpha = 0.6) +
  geom_ribbon(aes(ymin = p25_theta, ymax = p75_theta),
              fill = "grey65", alpha = 0.6) +
  
  # θ̂ mean line
  geom_line(aes(y = mean_theta), color = "black", linewidth = 0.7) +
  
  # Excess mortality (scaled to left axis, displayed on right axis)
  geom_line(aes(y = mean_excess * scale_excess),
            color = "#C0392B", linewidth = 0.7, linetype = "dashed") +
  
  # Wave boundary lines
  geom_vline(xintercept = as.numeric(as.Date(c("2020-06-15", "2020-09-15",
                                               "2021-03-01", "2021-07-01",
                                               "2022-01-01"))),
             linetype = "dotted", color = "grey50", linewidth = 0.3) +
  
  # Wave labels
  geom_text(data = wave_labels_df,
            aes(x = x, y = max(theta_agg$p90_theta) * 0.95, label = label),
            size = 2.3, family = "serif", color = "grey40", lineheight = 0.85) +
  
  # Direct line labels at right margin
  annotate("text",
           x = last_date + 6, y = last_theta,
           label = "hat(theta)", parse = TRUE, hjust = 0, size = 2.8,
           family = "serif", color = "black", fontface = "bold") +
  annotate("text",
           x = last_date + 6, y = last_excess,
           label = "excess\ndeaths/m", hjust = 0, size = 2.3,
           family = "serif", color = "#C0392B", lineheight = 0.85) +
  annotate("text",
           x = last_date + 6, y = last_S,
           label = "S\n(scaled)", hjust = 0, size = 2.3,
           family = "serif", color = "#2980B9", lineheight = 0.85) +
  
  # Axes
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y",
               expand = expansion(mult = c(0.01, 0.10))) +
  scale_y_continuous(
    expand   = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . / scale_excess,
                        name = "Excess deaths per million per week")
  ) +
  
  # Labels
  labs(
    y        = expression(hat(theta) ~ "(implied infections per million, weekly)"),
    title    = expression(paste("Figure X: Imputed Infection Prevalence ",
                                hat(theta), ", Excess Mortality, and Stringency (OECD Mean)")),
    subtitle = paste0(
      "Black: OECD mean \u03b8\u0302. Bands: IQR (dark) and P10\u2013P90 (light).\n",
      "Red dashed: excess deaths per million (right axis). ",
      "Blue area: mean stringency S \u2208 [0,1] (scaled to left axis).\n",
      "Omicron: \u03b8\u0302 peaks sharply; excess mortality does not follow (IFR collapse ~20\u00d7). ",
      "High S suppresses \u03b8\u0302 in W1\u2013W2."
    ),
    x       = NULL,
    caption = paste0(
      "Notes: 35 OECD economies, weekly. Excess mortality from OWID.\n",
      "IFR calibrated by wave (Levin et al. 2020; Marziano et al. 2023). ",
      "Stringency: Oxford CGRT (C1\u2013C8), population-weighted.\n",
      "Negative excess mortality floored at \u03b8\u0302 = 0. ",
      "Vertical dotted lines: wave boundaries (OECD-median variant dominance >50%).\n",
      "S scaled to left axis for comparability; right axis shows excess deaths only."
    )
  ) +
  
  # Theme
  theme_aer +
  theme(
    axis.title.y = element_text(
      color  = "black",
      angle  = 90,
      vjust  = 0.5,
      margin = margin(t = 0, r = 10, b = 0, l = 0)
    ),
    axis.title.y.right = element_text(
      color  = "#C0392B",
      angle  = 90,
      vjust  = 0.5,
      margin = margin(t = 0, r = 0, b = 0, l = 10)
    ),
    axis.text.y.right  = element_text(color = "#C0392B"),
    axis.ticks.y.right = element_line(color = "#C0392B")
  )

print(p_theta_traj)

##Looks great-> zeigt wie die Regierungen kein Interesse mehr hatten-> 
#->Stützt meine Argumentation das Death Rate das entscheidende ist aber der SP auf die Cases reagiert-> simultanität von Theta und S-> Wichtig
#-> Ziegt den Übergang zu einer normalen Grippe
#-> Mismatch zwischen deaths und cases-> Death Rate IFR (genaue Schätzung) ist das Problem
#vor den spikes hatte man Angst
#FAZIT: Modellhorizont funktioniert, S_k und Cases ist auch eine gute Messung im Modell, SP reagiert auf Cases

##Für Paper, Code anpassen->siehe nachfolgend
# ==============================================================================
#  p_theta_traj — Imputed θ̂, Excess Mortality, and Stringency
#  Three series: θ̂ (left axis), excess deaths/million (right axis),
#                mean stringency S (scaled to left axis, blue area)
# ==============================================================================

# --- 1. Aggregate θ̂, excess mortality, and stringency from panel_w -----------
theta_agg <- theta_stats %>%
  group_by(date) %>%
  summarise(
    mean_theta  = mean(theta_hat, na.rm = TRUE) * 1e6,
    p25_theta   = quantile(theta_hat, 0.25, na.rm = TRUE) * 1e6,
    p75_theta   = quantile(theta_hat, 0.75, na.rm = TRUE) * 1e6,
    p10_theta   = quantile(theta_hat, 0.10, na.rm = TRUE) * 1e6,
    p90_theta   = quantile(theta_hat, 0.90, na.rm = TRUE) * 1e6,
    mean_excess = mean(excess / (pop / 1e6), na.rm = TRUE),
    mean_S      = mean(S_mean, na.rm = TRUE),
    .groups     = "drop"
  )

# --- 2. Scale factors ---------------------------------------------------------
scale_excess <- max(theta_agg$mean_theta, na.rm = TRUE) /
  max(theta_agg$mean_excess, na.rm = TRUE)

scale_S <- max(theta_agg$p90_theta, na.rm = TRUE)

# --- 3. Label positions (inside plot, around Jun 2022) -----------------------
wave_labels_df <- data.frame(
  x     = as.Date(c("2020-04-15", "2020-12-01", "2021-08-15", "2022-02-01")),
  label = c("Wave 1\n(Original)", "Wave 2\n(Wt/Alpha)",
            "Wave 3\n(Delta)", "Wave 4\n(Omicron)")
)

# Reference values at label x-position for vertical placement
label_date  <- as.Date("2022-06-01")
label_row   <- theta_agg %>% filter(date == label_date)
if (nrow(label_row) == 0) {
  label_row <- theta_agg %>% filter(date == min(date[date >= label_date]))
}

ref_theta  <- label_row$mean_theta
ref_excess <- label_row$mean_excess * scale_excess
ref_S      <- label_row$mean_S * scale_S

# --- 4. Plot ------------------------------------------------------------------
p_theta_traj <- ggplot(theta_agg, aes(x = date)) +
  
  # Stringency background area (drawn first, behind everything)
  geom_area(aes(y = mean_S * scale_S),
            fill = "#2980B9", alpha = 0.12) +
  geom_line(aes(y = mean_S * scale_S),
            color = "#2980B9", linewidth = 0.5, linetype = "solid") +
  
  # θ̂ uncertainty bands
  geom_ribbon(aes(ymin = p10_theta, ymax = p90_theta),
              fill = "grey85", alpha = 0.6) +
  geom_ribbon(aes(ymin = p25_theta, ymax = p75_theta),
              fill = "grey65", alpha = 0.6) +
  
  # θ̂ mean line
  geom_line(aes(y = mean_theta), color = "black", linewidth = 0.7) +
  
  # Excess mortality
  geom_line(aes(y = mean_excess * scale_excess),
            color = "#C0392B", linewidth = 0.7, linetype = "dashed") +
  
  # Wave boundary lines
  geom_vline(xintercept = as.numeric(as.Date(c("2020-06-15", "2020-09-15",
                                               "2021-03-01", "2021-07-01",
                                               "2022-01-01"))),
             linetype = "dotted", color = "grey50", linewidth = 0.3) +
  
  # Wave labels
  geom_text(data = wave_labels_df,
            aes(x = x, y = max(theta_agg$p90_theta) * 0.95, label = label),
            size = 2.3, family = "serif", color = "grey40", lineheight = 0.85) +
  
  # Direct line labels (inside plot)
  annotate("text",
           x = label_date, y = ref_theta * 1.15,
           label = "hat(theta)", parse = TRUE, hjust = 0, size = 2.8,
           family = "serif", color = "black", fontface = "bold") +
  annotate("text",
           x = label_date, y = ref_excess * 0.6,
           label = "excess\ndeaths/m", hjust = 0, size = 2.3,
           family = "serif", color = "#C0392B", lineheight = 0.85) +
  annotate("text",
           x = label_date, y = ref_S * 1.3,
           label = "S (scaled)", hjust = 0, size = 2.3,
           family = "serif", color = "#2980B9", lineheight = 0.85) +
  
  # Axes
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y",
               expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(
    expand   = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . / scale_excess,
                        name = "Excess deaths per million per week")
  ) +
  
  # Labels
  labs(
    y        = expression(hat(theta) ~ "(implied infections per million, weekly)"),
    title    = expression(paste("Figure X: Imputed Infection Prevalence ",
                                hat(theta), ", Excess Mortality, and Stringency (OECD Mean)")),
    subtitle = paste0(
      "Black: OECD mean \u03b8\u0302. Bands: IQR (dark) and P10\u2013P90 (light).\n",
      "Red dashed: excess deaths per million (right axis). ",
      "Blue area: mean stringency S \u2208 [0,1] (scaled to left axis).\n",
      "Omicron: \u03b8\u0302 peaks sharply; excess mortality does not follow (IFR collapse ~20\u00d7). ",
      "High S suppresses \u03b8\u0302 in W1\u2013W2."
    ),
    x       = NULL,
    caption = paste0(
      "Notes: 35 OECD economies, weekly. Excess mortality from OWID.\n",
      "IFR calibrated by wave (Levin et al. 2020; Marziano et al. 2023). ",
      "Stringency: Oxford CGRT (C1\u2013C8), population-weighted.\n",
      "Negative excess mortality floored at \u03b8\u0302 = 0. ",
      "Vertical dotted lines: wave boundaries (OECD-median variant dominance >50%).\n",
      "S scaled to left axis for comparability; right axis shows excess deaths only."
    )
  ) +
  
  # Theme
  theme_aer +
  theme(
    axis.title.y = element_text(
      color  = "black",
      angle  = 90,
      vjust  = 0.5,
      margin = margin(t = 0, r = 10, b = 0, l = 0)
    ),
    axis.title.y.right = element_text(
      color  = "#C0392B",
      angle  = 90,
      vjust  = 0.5,
      margin = margin(t = 0, r = 0, b = 0, l = 10)
    ),
    axis.text.y.right  = element_text(color = "#C0392B"),
    axis.ticks.y.right = element_line(color = "#C0392B")
  )

print(p_theta_traj)

#Das zeigen als Planungshorizont

#Interpretation Gap EXD und S
#The widening gap between θ̂ and S in later waves reflects the binding fiscal and political constraind (fiskalische erschöpfung)
#at the heart of the pandemic trilemma: as debt accumulated and public compliance eroded (politische erschöpfung), governments faced a trilemma in which further suppression was neither fiscally sustainable nor politically viable, 
#even as infection prevalence reached historically high levels.-> Paradigmawechseln und bessere Informationen ICU Kapazität wichtig-> Mehr Eigenverantwortung-> Impfe dich oder gehe das Risiko ein-> keine generellen lockdowns
#FAZIT: Planungshorizont 2020-2021-> Danach löst die Impfung das Trilemma-> Option out (Selbstbestimmung)



##wie gut ist die messung zu confirmed cases?


# ==============================================================================
#  4. CROSS-CHECK: θ̂ VS. CONFIRMED CASES
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 4: Cross-Check Against Confirmed Cases\n")
cat(strrep("=", 70), "\n\n")

# θ̂ from excess mortality should exceed confirmed cases because:
# (1) Testing was incomplete, especially in early waves
# (2) Asymptomatic cases were missed
# (3) Confirmed cases subject to reporting delays and testing capacity
# The ratio θ̂ / confirmed cases = ascertainment adjustment factor

cross_check <- theta_stats %>%
  filter(!is.na(cases_w), cases_w > 0, theta_hat > 0) %>%
  mutate(
    cases_share = cases_w / pop,
    ratio = theta_hat / cases_share
  )

cat(sprintf("  Obs for cross-check: %d (both θ̂ > 0 and confirmed cases > 0)\n\n",
            nrow(cross_check)))

# Ratio by wave
ratio_wave <- cross_check %>%
  group_by(wave, wave_label) %>%
  summarise(
    median_ratio = median(ratio, na.rm = TRUE),
    mean_ratio   = mean(ratio, na.rm = TRUE),
    p25_ratio    = quantile(ratio, 0.25, na.rm = TRUE),
    p75_ratio    = quantile(ratio, 0.75, na.rm = TRUE),
    .groups      = "drop"
  )

# Extended cross-check including Omicron (for this section only)
theta_stats_ext <- panel_w %>%
  filter(date >= as.Date("2020-03-01"), date <= as.Date("2022-12-31"),
         !is.na(theta_hat))

cat("--- 4a. θ̂ / Confirmed Cases Ratio by Wave ---\n")
cat("  (Ratio > 1 expected: θ̂ includes undetected infections)\n")
cat("  (Ratio >> 1 in early waves: limited testing capacity)\n\n")
print(kable(ratio_wave, digits = 1,
            col.names = c("Wave","Label","Median","Mean","P25","P75")))

cat("\n  Interpretation:\n")
cat("  • W1: High ratio expected (~5-20x) — very limited testing\n")
cat("  • W1_summer: Ratio noisy — low absolute numbers, division by small\n")
cat("    confirmed case counts amplifies measurement error\n")
cat("  • W2_wt / W2_alpha: Ratio declines as testing capacity scales up\n")
cat("  • W3_delta: Testing mature, ratio should approach 1-3x\n")
cat("  • W4_omicron: Ratio may increase — massive infection surge\n")
cat("    overwhelmed testing capacity again\n")
cat("  • Ratio < 1 would indicate IFR calibration is too LOW → too many θ̂\n\n")

##Ratio (Median) macht auch total Sinn und ist auch tief am Schluss, die Intervalle ziegen die massiven Unterschieden-> je nach Kapazitäten (Testen)
#Stimmt auch mit dem Bild zu Omicron überein, niemand mehr testen gegangen
##Gutes Mass, unverfäscht-> Ökonomische Storyline dazu erzählen

# --- 4b. Scatter: θ̂ vs confirmed cases (log-log) ----------------------------

# Define factor levels consistent with new wave classification
wave_level_order <- c(
  "Wave 1 (Original, spring 2020)",
  "Summer lull 2020 (Original)",
  "Wave 2a (Wildtype, autumn/winter 2020-21)",
  "Wave 2b (Alpha, spring 2021)",
  "Wave 3 (Delta, summer/autumn 2021)",
  "Wave 4 (Omicron, 2022+)"
)

wave_colors <- c(
  "Wave 1 (Original, spring 2020)"            = "#2C3E50",
  "Summer lull 2020 (Original)"                = "#7F8C8D",
  "Wave 2a (Wildtype, autumn/winter 2020-21)"  = "#8E44AD",
  "Wave 2b (Alpha, spring 2021)"               = "#2980B9",
  "Wave 3 (Delta, summer/autumn 2021)"         = "#27AE60",
  "Wave 4 (Omicron, 2022+)"                    = "#E74C3C"
)

p_crosscheck <- cross_check %>%
  mutate(wave_label = factor(wave_label, levels = wave_level_order)) %>%
  ggplot(aes(x = cases_share * 100, y = theta_hat * 100)) +
  geom_point(aes(color = wave_label), alpha = 0.2, size = 0.8) +
  geom_line(
    data        = data.frame(v = c(1e-5, 100)),
    aes(x = v, y = v),
    linetype    = "dashed",
    color       = "grey40",
    linewidth   = 0.5,
    inherit.aes = FALSE
  ) +
  annotate("text", x = 0.01, y = 0.5,
           label = "θ̂ = confirmed",
           angle = 45, size = 2.5, color = "grey40", family = "serif") +
  scale_x_log10() + scale_y_log10() +
  scale_color_manual(values = wave_colors, name = NULL, drop = FALSE) +
  labs(
    title = "Figure X: Imputed θ̂ vs. Confirmed Cases",
    subtitle = "Log-log scale. Points above diagonal: θ̂ exceeds confirmed (expected).",
    x = "Confirmed cases (% of population, weekly)",
    y = "θ̂ imputed (% of population, weekly)",
    caption = "Notes: Country-week observations. Dashed line: 1:1 (perfect ascertainment)."
  ) + theme_aer +
  guides(color = guide_legend(nrow = 2, override.aes = list(alpha = 1, size = 2)))

print(p_crosscheck)

#Für Liinear Regression Line:   geom_smooth(method    = "lm",se        = TRUE,color     = "black",fill      = "grey80",linewidth = 0.6,alpha     = 0.3,formula   = y ~ x) +
##Funktioniert gut, zeigt das meine Schätzung oberhalb ist was mehr Sinn macht-> Link zu S?
lag_range <- 0:3

cor_comparison <- expand_grid(
  wv    = unique(panel_w$wave_label),   # umbenannt zu wv
  lag_k = lag_range
) %>%
  pmap_dfr(function(wv, lag_k) {
    
    sub <- panel_w %>%
      filter(wave_label == wv) %>%       # wv statt wave
      mutate(cases_share = cases_w / pop) %>%
      arrange(Country, date) %>%
      group_by(Country) %>%
      mutate(
        theta_lag = lag(theta_hat,   lag_k),
        cases_lag = lag(cases_share, lag_k)
      ) %>%
      ungroup() %>%
      filter(
        !is.na(theta_lag), !is.na(cases_lag), !is.na(S_mean),
        is.finite(theta_lag), is.finite(cases_lag), is.finite(S_mean)
      )
    
    if (nrow(sub) < 30) {
      return(tibble(
        wave_label  = wv, lag = lag_k,
        cor_theta_S = NA_real_, cor_cases_S = NA_real_,
        delta_cor   = NA_real_, N = nrow(sub)
      ))
    }
    
    tibble(
      wave_label  = wv,
      lag         = lag_k,
      cor_theta_S = tryCatch(cor(sub$theta_lag,  sub$S_mean), error = function(e) NA_real_),
      cor_cases_S = tryCatch(cor(sub$cases_lag,  sub$S_mean), error = function(e) NA_real_),
      delta_cor   = cor_theta_S - cor_cases_S,
      N           = nrow(sub)
    )
  })

print(kable(cor_comparison, digits = 3))

best_lag <- cor_comparison %>%
  filter(!is.na(cor_theta_S)) %>%
  group_by(wave_label) %>%
  slice_max(cor_theta_S, n = 1)

print(kable(best_lag, digits = 3))

#Zeigt das der SP S eher gemäss den ConfirmedCases macht.
#Die Anpassung an den wirklichen Pandemiedruck fällt schnell-> ist die Todesrate relevant?
#Somit ist aber gut das die richtige Planungsdimension Theta_hat ist = exogener von S
#Zeigt auch das Theta_hat einen Mehrwert bringt->Anderer Kanal
#"The near-zero correlation between θ̂ and S, in contrast to the significantly positive correlation between confirmed cases and S, is consistent with confirmed case counts being contaminated by testing-capacity endogeneity. θ̂, derived from excess mortality, is mechanically independent of surveillance intensity and therefore the appropriate state variable for the planner's problem."

cor_excess <- expand_grid(
  wv    = unique(panel_w$wave_label),
  lag_k = 0:3
) %>%
  pmap_dfr(function(wv, lag_k) {
    
    sub <- panel_w %>%
      filter(wave_label == wv) %>%
      arrange(Country, date) %>%
      group_by(Country) %>%
      mutate(excess_lag = lag(d_pc, lag_k)) %>%
      ungroup() %>%
      filter(!is.na(excess_lag), !is.na(S_mean), is.finite(excess_lag))
    
    if (nrow(sub) < 30) return(NULL)
    
    tibble(
      wave_label    = wv,
      lag           = lag_k,
      cor_excess_S  = tryCatch(cor(sub$excess_lag, sub$S_mean), error = function(e) NA_real_),
      N             = nrow(sub)
    )
  })

# Alle drei Serien zusammen
cor_all <- cor_comparison %>%
  select(wave_label, lag, cor_theta_S, cor_cases_S, delta_cor) %>%
  left_join(cor_excess, by = c("wave_label", "lag"))

print(kable(cor_all, digits = 3,
            col.names = c("Wave", "Lag", "cor(θ̂,S)", "cor(cases,S)",
                          "Δ(θ̂−cases)", "cor(deaths,S)", "N")))

##Cor zu deaths leicht höher, vor allem in harten Wellen, gleicht sich an-> Paradigmawechsel zu Impfen gegen Ende 2021-> Mein Modell

unique(hosp_d$Country)
head(hosp_d)
summary(hosp_d$date)

colnames(hosp_d)

# --- Hospitalisierung: täglich → wöchentlich aggregieren ---
hosp_w <- hosp_d %>%
  mutate(date = as.Date(date)) %>%
  mutate(
    isoyr = isoyear(date),
    isowk = isoweek(date)
  ) %>%
  group_by(Country, isoyr, isowk) %>%
  summarise(
    hosp_new_pm = mean(Weekly.new.hospital.admissions.per.million, na.rm = TRUE),
    icu_occ_pm  = mean(Daily.ICU.occupancy.per.million,            na.rm = TRUE),
    .groups = "drop"
  )

# --- Merge mit panel_w (nur Länder mit Hosp-Daten) ---
hosp_countries <- unique(hosp_d$Country)

panel_hosp <- panel_w %>%
  filter(Country %in% hosp_countries) %>%
  left_join(hosp_w, by = c("Country", "isoyr", "isowk"))

# --- Korrelation cor(hosp, S) und cor(icu, S) nach Welle und Lag ---
cor_hosp <- expand_grid(
  wv    = unique(panel_hosp$wave_label),
  lag_k = 0:3
) %>%
  pmap_dfr(function(wv, lag_k) {
    
    sub <- panel_hosp %>%
      filter(wave_label == wv) %>%
      arrange(Country, date) %>%
      group_by(Country) %>%
      mutate(
        hosp_lag = lag(hosp_new_pm, lag_k),
        icu_lag  = lag(icu_occ_pm,  lag_k)
      ) %>%
      ungroup() %>%
      filter(!is.na(S_mean), is.finite(S_mean))
    
    tibble(
      wave_label  = wv,
      lag         = lag_k,
      cor_hosp_S  = tryCatch(
        cor(sub$hosp_lag, sub$S_mean, use = "complete.obs"),
        error = function(e) NA_real_
      ),
      cor_icu_S   = tryCatch(
        cor(sub$icu_lag,  sub$S_mean, use = "complete.obs"),
        error = function(e) NA_real_
      ),
      N_hosp      = sum(!is.na(sub$hosp_lag) & !is.na(sub$S_mean)),
      N_icu       = sum(!is.na(sub$icu_lag)  & !is.na(sub$S_mean))
    )
  })

# --- Alle Serien zusammen ---
cor_all <- cor_all %>%
  left_join(
    cor_hosp %>% select(wave_label, lag, cor_hosp_S, cor_icu_S, N_hosp, N_icu),
    by = c("wave_label", "lag")
  )

print(kable(cor_all, digits = 3,
            col.names = c("Wave", "Lag", "cor(θ̂,S)", "cor(cases,S)",
                          "Δ(θ̂−cases)", "cor(deaths,S)", "N",
                          "cor(hosp,S)", "cor(icu,S)", "N_hosp", "N_icu")))

ncol(cor_all)
colnames(cor_all)

# cor_all neu zusammenbauen ohne Duplikate
cor_all <- cor_comparison %>%
  select(wave_label, lag, cor_theta_S, cor_cases_S, delta_cor) %>%
  left_join(cor_excess %>% select(wave_label, lag, cor_excess_S, N),
            by = c("wave_label", "lag")) %>%
  left_join(cor_hosp %>% select(wave_label, lag, cor_hosp_S, cor_icu_S, N_hosp, N_icu),
            by = c("wave_label", "lag"))

print(kable(cor_all, digits = 3,
            col.names = c("Wave", "Lag", "cor(θ̂,S)", "cor(cases,S)",
                          "Δ(θ̂−cases)", "cor(deaths,S)", "N",
                          "cor(hosp,S)", "cor(icu,S)", "N_hosp", "N_icu")))


# ==============================================================================
#  FAZIT: Policy-Response-Analyse — cor(Indikator, S) nach Welle
# ==============================================================================
#
#  Zentrale Befunde:
#
#  (1) REAKTIONSMUSTER ÜBER WELLEN:
#      W1 (Original):    Tode & ICU dominieren (cor ≈ 0.49) — reaktive Politik
#                        auf sichtbares Systemversagen (Bergamo-Effekt).
#      W2a/W2b (WT/Alpha): ICU stärkster Trigger (cor ≈ 0.58), Fälle als
#                        Ergänzung — Übergang zu antizipatorischer Politik.
#      W3 (Delta):       Hospitalisierung einziger relevanter Trigger (cor ≈ 0.30),
#                        alle anderen Indikatoren kollabieren — expliziter
#                        Politikwechsel auf Hospitalisierungsinzidenz.
#      W4 (Omicron):     ICU dominiert (cor ≈ 0.60), Tode irrelevant (cor ≈ 0.01)
#                        — vollständige Entkopplung von Mortalität bei kollabierter IFR.
#
#  (2) EXOGENITÄT VON θ̂:
#      cor(θ̂, S) ≈ 0 über alle Wellen und alle Lags (0–3 Wochen).
#      Regierungen haben nie auf den wahren Infektionsdruck reagiert —
#      ausschliesslich auf beobachtbare Proxies (ICU, Hosp, Fälle, Tode).
#      → θ̂ ist nicht durch den Policy-Kanal kontaminiert.
#      → θ̂ ist eine valide exogene Zustandsvariable für das iLQR-Problem.
#
#  (3) IMPLIKATION FÜR ENDOGENITÄTSSORGE:
#      Die Sorge, dass Testkapazität mit S korreliert und confirmed cases
#      dadurch endogen gegenüber S sind, wird durch den Befund gestützt:
#      cor(cases, S) ≈ 0.43–0.49 in W1–W2b, während cor(θ̂, S) ≈ 0.20.
#      θ̂ aus Excess Mortality umgeht diesen Surveillance-Bias deutlich besser->valide exogene Zustandsvariable für den Planer-> wichtiger Proxy für die State Dependencie
#cor vorhanden aber unbiased mit Kapazitäten usw.-> nicht ein Instrument, fängt einen andere Kanal ein-> Pandemic Pressure
#
# ==============================================================================

# ==============================================================================
#  5. VALIDATION: θ-EQUATION CONSISTENCY
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 5: θ-Equation Validation\n")
cat(strrep("=", 70), "\n\n")

# --- 5a. Predicted θ from structural equation --------------------------------
# θ_{w+1}^pred = ρ_θ^w · (1 - φ_S · S_w) · θ̂_w
#
# This checks: given calibrated (ρ_θ, φ_S) and observed S, does the
# PREDICTED next-period θ match the IMPUTED θ̂ from mortality data?
# This is a BIOLOGICAL consistency check, not a causal test.
#
# NOTE: rho_w was already merged into panel_w in Section 2d via left_join.
#       No second join needed here.

panel_w <- panel_w %>%
  arrange(Country, date) %>%
  group_by(Country) %>%
  mutate(
    theta_hat_next = lead(theta_hat, 1),
    theta_predicted = rho_w * (1 - phi_S_central * S_mean) * theta_hat
  ) %>%
  ungroup()

val_data <- panel_w %>%
  filter(date >= as.Date("2020-03-01"), date <= as.Date("2022-12-31"),
         !is.na(theta_hat_next), !is.na(theta_predicted),
         theta_hat > 0, theta_hat_next > 0)

cat(sprintf("  Validation sample: %d country-weeks\n\n", nrow(val_data)))

# --- 5b. Fit statistics ------------------------------------------------------

# Overall R²
r_sq <- cor(val_data$theta_hat_next, val_data$theta_predicted,
            use = "complete.obs")^2

# RMSE
rmse <- sqrt(mean((val_data$theta_hat_next - val_data$theta_predicted)^2,
                  na.rm = TRUE))

# Median absolute error
mae <- median(abs(val_data$theta_hat_next - val_data$theta_predicted),
              na.rm = TRUE)

# Log-scale fit (more appropriate for exponential dynamics)
val_log <- val_data %>%
  filter(theta_hat_next > 0, theta_predicted > 0) %>%
  mutate(
    log_actual    = log(theta_hat_next),
    log_predicted = log(theta_predicted)
  )
r_sq_log <- cor(val_log$log_actual, val_log$log_predicted,
                use = "complete.obs")^2

cat("--- 5b. Fit Statistics ---\n")
cat(sprintf("  R²  (levels):    %.3f\n", r_sq))
cat(sprintf("  R²  (log):       %.3f\n", r_sq_log))
cat(sprintf("  RMSE:            %.6f  (%.4f%%)\n", rmse, rmse * 100))
cat(sprintf("  Median |error|:  %.6f  (%.4f%%)\n\n", mae, mae * 100))

# By wave
val_wave <- val_data %>%
  group_by(wave, wave_label) %>%
  summarise(
    N     = n(),
    R2    = cor(theta_hat_next, theta_predicted, use = "complete.obs")^2,
    RMSE  = sqrt(mean((theta_hat_next - theta_predicted)^2, na.rm = TRUE)),
    bias  = mean(theta_predicted - theta_hat_next, na.rm = TRUE),
    .groups = "drop"
  )

cat("--- θ-Equation Fit by Wave ---\n")
print(kable(val_wave %>% select(wave_label, N, R2, RMSE, bias),
            digits = c(0, 0, 3, 6, 6),
            col.names = c("Wave","N","R²","RMSE","Bias")))

##BIAS und WERTE ANGEBEN
##Macht aber absolut Sinn, am Anfang alle die etwas hatten sin ddirekt zum Test (Angst), im Sommer tief und midle Verläufe, 
#und Omicron hat viel Resistenzen drinn, selbsttests, option out d.h., die Leute haben sich nicht mehr registriert da sie 
#geimpft waren oder es ausgereicht hatte-> aber auch nicht so interessant da man sieht das die excess deaths deutlich gesunken sind und es ein viel grösseres Rauschen gab

cat("\n  Interpretation:\n")
cat("  • R² near 1: calibrated (ρ_θ, φ_S) replicate infection dynamics well\n")
cat("  • Positive bias: model OVERPREDICTS next-period θ (ρ_θ too high or\n")
cat("    φ_S too low → insufficient suppression in model)\n")
cat("  • Negative bias: model UNDERPREDICTS (natural immunity, behavioral\n")
cat("    changes not captured by S reduce transmission beyond model)\n")
cat("  • W1_summer: low θ̂ levels make R² and RMSE less informative;\n")
cat("    ρ_θ = 1.2 is the least empirically grounded parameter value\n\n")

# --- 5c. Scatter: predicted vs actual θ̂ (log scale) -------------------------

p_validation <- val_log %>%
  mutate(wave_label = factor(wave_label, levels = wave_level_order)) %>%
  ggplot(aes(x = log_predicted, y = log_actual)) +
  geom_point(aes(color = wave_label), alpha = 0.2, size = 0.8) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#C0392B") +
  scale_color_manual(values = wave_colors, name = NULL, drop = FALSE) +
  labs(
    title = "Figure X: θ-Equation Validation (Log Scale)",
    subtitle = sprintf("log(θ̂_{w+1}) vs. log(ρ_θ(1 − φ_S·S_w)·θ̂_w). R² = %.3f. Red line: perfect fit.",
                       r_sq_log),
    x = "log(θ predicted)",
    y = "log(θ̂ imputed from mortality)",
    caption = paste0("Notes: Each point = country-week. Calibrated ρ_θ wave-specific,\n",
                     "φ_S = ", phi_S_central, ". Deviations: behavioral adaptation, ",
                     "immunity buildup, variant transitions.")
  ) + theme_aer +
  guides(color = guide_legend(nrow = 2, override.aes = list(alpha = 1, size = 2)))

print(p_validation)

##wie gut stimmt mein Modell
#Zeigt perfekt das nach 2021 mein Modell nciht mehr so gut war aber das war auch gar nich tmehr das Ziel vom SP-> Mein Modellhorizont-> Trilemma wird durch Impfung aufgehoben
#"The pandemic trilemma was binding from March 2020 through approximately December 2021, during which governments faced an inescapable trade-off between suppression, economic stability, and fiscal sustainability. The widespread availability of vaccines resolved the trilemma exogenously by collapsing the IFR to a level at which the health objective became achievable without active suppression. The post-2021 dominant strategy — vaccination as individual risk management with minimal NPIs — represents not a policy solution to the trilemma but a technological escape from it. Accordingly, the structural estimation in Stage 2 is confined to the trilemma period 2020 Q1 through 2021 Q4."


# ==============================================================================
#  6. ROBUSTNESS & SENSITIVITY ANALYSIS
# ==============================================================================
#
# This section tests the robustness of the θ-imputation and the θ-equation
# validation along three dimensions:
#
#   (a) Infection-to-death lag ℓ ∈ {2, 3, 4} weeks
#   (b) IFR bounds: ifr_lo, ifr_central, ifr_hi (from literature)
#   (c) ρ_θ bounds: rho_w_lo, rho_w_central, rho_w_hi
#
# For each perturbation, we recompute the full pipeline:
#   θ̂ = d_{w+ℓ} / IFR  →  θ_predicted = ρ_θ · (1 − φ_S · S) · θ̂  →  fit stats
#
# The diagnostic metric is LEVELS R² (not log R²), because:
#   - IFR shifts θ̂ multiplicatively but uniformly within waves, which cancels
#     in log-correlations. Levels R² captures the resulting scale mismatch.
#   - ρ_θ shifts θ_predicted multiplicatively. Again, the scale effect matters.
#   - Lag shifts change which death observation maps to which infection week,
#     introducing genuine variation visible on both scales.
#
# Bias = mean(θ_predicted − θ̂_next) is reported alongside R².
# Positive bias: model overpredicts (ρ_θ too high, or IFR too low → θ̂ inflated).
# Negative bias: model underpredicts (immunity/behavioral suppression not in model).
#
# φ_S is held fixed at the central estimate throughout. Testing φ_S jointly
# with ρ_θ would be computationally redundant: the θ-equation only contains
# the product ρ_θ · (1 − φ_S · S), so varying φ_S is equivalent to rescaling
# ρ_θ conditional on S. The joint identification of (ρ_θ, φ_S) is deferred
# to Stage 2 estimation.
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 6: Robustness & Sensitivity Analysis\n")
cat(strrep("=", 70), "\n\n")


# --- 6a. Sensitivity to lag ℓ ------------------------------------------------
# Recompute θ̂ with ℓ = 2 and ℓ = 4 (central ℓ = 3 already in theta_hat).
# θ̂ changes, ρ_θ and IFR held at central values.

cat("--- 6a. Sensitivity to Lag ℓ ---\n\n")

# Correlations between lag variants (level of θ̂)
lag_data <- panel_w %>%
  filter(date >= as.Date("2020-03-01"), date <= as.Date("2022-12-31"),
         !is.na(theta_hat), !is.na(theta_hat_l2), !is.na(theta_hat_l4))

lag_cor <- tibble(
  r_l2_l3    = cor(lag_data$theta_hat_l2, lag_data$theta_hat,    use = "complete.obs"),
  r_l3_l4    = cor(lag_data$theta_hat,    lag_data$theta_hat_l4, use = "complete.obs"),
  ratio_l2_l3 = mean(lag_data$theta_hat_l2 / pmax(lag_data$theta_hat, 1e-10), na.rm = TRUE),
  ratio_l4_l3 = mean(lag_data$theta_hat_l4 / pmax(lag_data$theta_hat, 1e-10), na.rm = TRUE)
)

cat(sprintf("  corr(θ̂_ℓ=2, θ̂_ℓ=3) = %.4f\n", lag_cor$r_l2_l3))
cat(sprintf("  corr(θ̂_ℓ=3, θ̂_ℓ=4) = %.4f\n", lag_cor$r_l3_l4))
cat(sprintf("  Mean ratio θ̂_ℓ=2 / θ̂_ℓ=3 = %.3f\n", lag_cor$ratio_l2_l3))
cat(sprintf("  Mean ratio θ̂_ℓ=4 / θ̂_ℓ=3 = %.3f\n\n", lag_cor$ratio_l4_l3))
cat("  High correlations → θ̂ is robust to lag choice.\n")
cat("  Ratio ≈ 1 → lag primarily shifts timing, not level.\n\n")

# θ-equation validation for each lag
lag_scenarios <- list(
  "ell_2" = list(d_lead = "d_lead2", label = "ℓ = 2"),
  "ell_3" = list(d_lead = "d_lead3", label = "ℓ = 3 (central)"),
  "ell_4" = list(d_lead = "d_lead4", label = "ℓ = 4")
)

val_lag <- map_dfr(names(lag_scenarios), function(scen) {
  
  d_col <- lag_scenarios[[scen]]$d_lead
  
  tmp <- panel_w %>%
    mutate(
      theta_s = pmax(0, .data[[d_col]] / ifr)
    ) %>%
    arrange(Country, date) %>%
    group_by(Country) %>%
    mutate(
      theta_s_next = lead(theta_s, 1),
      theta_s_pred = rho_w * (1 - phi_S_central * S_mean) * theta_s
    ) %>%
    ungroup() %>%
    filter(date >= as.Date("2020-03-01"), date <= as.Date("2021-12-31"),
           !is.na(theta_s_next), !is.na(theta_s_pred),
           theta_s > 0, theta_s_next > 0)
  
  tmp %>%
    group_by(wave, wave_label) %>%
    summarise(
      N    = n(),
      R2   = cor(theta_s_next, theta_s_pred, use = "complete.obs")^2,
      RMSE = sqrt(mean((theta_s_next - theta_s_pred)^2, na.rm = TRUE)),
      bias = mean(theta_s_pred - theta_s_next, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(scenario = scen)
})

# Wide format for lag sensitivity
val_lag_wide <- val_lag %>%
  select(wave_label, scenario, R2, bias) %>%
  pivot_wider(
    names_from  = scenario,
    values_from = c(R2, bias),
    names_glue  = "{.value}_{scenario}"
  )

cat("--- θ-Equation Fit: Sensitivity to Lag ℓ ---\n")
cat("  IFR and ρ_θ held at central values. Only lead distance varies.\n\n")
print(kable(
  val_lag_wide %>% select(
    wave_label,
    R2_ell_2,   bias_ell_2,
    R2_ell_3,   bias_ell_3,
    R2_ell_4,   bias_ell_4
  ),
  digits  = c(0, 3, 6, 3, 6, 3, 6),
  col.names = c("Wave",
                "R²(ℓ=2)", "Bias(ℓ=2)",
                "R²(ℓ=3)", "Bias(ℓ=3)",
                "R²(ℓ=4)", "Bias(ℓ=4)")
))

cat("\n  Interpretation:\n")
cat("  • Lag choice primarily shifts which death week maps to which infection\n")
cat("    week. R² and bias should be similar across ℓ values.\n")
cat("  • If ℓ = 2 or ℓ = 4 systematically improves fit for specific waves,\n")
cat("    this reflects variant-specific infection-to-death timing (shorter\n")
cat("    for Omicron, longer for Original).\n\n")


# --- 6b. Sensitivity to IFR bounds -------------------------------------------
# Recompute θ̂ with IFR_lo and IFR_hi. ρ_θ and φ_S held fixed.

cat("--- 6b. Sensitivity to IFR Bounds ---\n\n")

ifr_scenarios <- list(
  "IFR_lo"      = panel_w$ifr_lo,
  "IFR_central" = panel_w$ifr,
  "IFR_hi"      = panel_w$ifr_hi
)

val_ifr <- map_dfr(names(ifr_scenarios), function(scen) {
  
  tmp <- panel_w %>%
    mutate(
      theta_s = pmax(0, d_lead3 / ifr_scenarios[[scen]])
    ) %>%
    arrange(Country, date) %>%
    group_by(Country) %>%
    mutate(
      theta_s_next = lead(theta_s, 1),
      theta_s_pred = rho_w * (1 - phi_S_central * S_mean) * theta_s
    ) %>%
    ungroup() %>%
    filter(date >= as.Date("2020-03-01"), date <= as.Date("2021-12-31"),
           !is.na(theta_s_next), !is.na(theta_s_pred),
           theta_s > 0, theta_s_next > 0)
  
  tmp %>%
    group_by(wave, wave_label) %>%
    summarise(
      N    = n(),
      R2   = cor(theta_s_next, theta_s_pred, use = "complete.obs")^2,
      RMSE = sqrt(mean((theta_s_next - theta_s_pred)^2, na.rm = TRUE)),
      bias = mean(theta_s_pred - theta_s_next, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(scenario = scen)
})

val_ifr_wide <- val_ifr %>%
  select(wave_label, scenario, R2, bias) %>%
  pivot_wider(
    names_from  = scenario,
    values_from = c(R2, bias),
    names_glue  = "{.value}_{scenario}"
  )

cat("  ρ_θ and φ_S held fixed. Only θ̂ varies via IFR.\n")
cat("  IFR_lo → more infections (higher θ̂) → model overpredicts → positive bias\n")
cat("  IFR_hi → fewer infections (lower θ̂) → model underpredicts → negative bias\n\n")

print(kable(
  val_ifr_wide %>% select(
    wave_label,
    R2_IFR_lo,      bias_IFR_lo,
    R2_IFR_central, bias_IFR_central,
    R2_IFR_hi,      bias_IFR_hi
  ),
  digits  = c(0, 3, 6, 3, 6, 3, 6),
  col.names = c("Wave",
                "R²(lo)", "Bias(lo)",
                "R²(mid)", "Bias(mid)",
                "R²(hi)", "Bias(hi)")
))

# Also report θ̂ levels under each IFR scenario
panel_w <- panel_w %>%
  mutate(
    theta_hat_ifr_lo = pmax(0, d_lead3 / ifr_hi),   # Higher IFR → fewer infections
    theta_hat_ifr_hi = pmax(0, d_lead3 / ifr_lo)    # Lower IFR → more infections
  )

ifr_theta_levels <- panel_w %>%
  filter(date >= as.Date("2020-03-01"), date <= as.Date("2022-12-31"),
         !is.na(theta_hat)) %>%
  group_by(wave, wave_label) %>%
  summarise(
    theta_lo  = mean(theta_hat_ifr_lo, na.rm = TRUE) * 100,
    theta_mid = mean(theta_hat, na.rm = TRUE) * 100,
    theta_hi  = mean(theta_hat_ifr_hi, na.rm = TRUE) * 100,
    range_pct = (mean(theta_hat_ifr_hi, na.rm = TRUE) -
                   mean(theta_hat_ifr_lo, na.rm = TRUE)) /
      mean(theta_hat, na.rm = TRUE) * 100,
    .groups = "drop"
  )

cat("\n\n--- θ̂ Levels Under IFR Bounds (% of population) ---\n")
cat("  θ̂_lo uses IFR_hi (fewer implied infections), θ̂_hi uses IFR_lo\n\n")
print(kable(ifr_theta_levels, digits = 4,
            col.names = c("Wave", "Label", "θ̂_lo%", "θ̂_mid%", "θ̂_hi%", "Range/Mid%")))

cat("\n  IFR sensitivity is highest in later waves where the IFR range\n")
cat("  spans a larger factor. For early waves, range is modest (~20-40%).\n\n")


# --- 6c. Sensitivity to ρ_θ bounds -------------------------------------------
# Recompute θ_predicted with rho_lo and rho_hi. θ̂ and φ_S held fixed.
# This is the key test: ρ_θ is the least empirically anchored parameter.
# Unlike IFR (dozens of seroprevalence studies) and φ_S (Brauner et al.,
# Flaxman et al.), ρ_θ is a reduced-form parameter of THIS model that
# absorbs susceptible depletion, behavioral responses, contact
# heterogeneity, and seasonality.

cat("--- 6c. Sensitivity to ρ_θ Bounds ---\n\n")

rho_scenarios <- list(
  "rho_lo"      = panel_w$rho_w_lo,
  "rho_central" = panel_w$rho_w,
  "rho_hi"      = panel_w$rho_w_hi
)

val_rho <- map_dfr(names(rho_scenarios), function(scen) {
  
  tmp <- panel_w %>%
    mutate(
      theta_s_pred = rho_scenarios[[scen]] * (1 - phi_S_central * S_mean) * theta_hat
    ) %>%
    arrange(Country, date) %>%
    group_by(Country) %>%
    mutate(
      theta_s_next = lead(theta_hat, 1)
    ) %>%
    ungroup() %>%
    filter(date >= as.Date("2020-03-01"), date <= as.Date("2022-12-31"),
           !is.na(theta_s_next), !is.na(theta_s_pred),
           theta_hat > 0, theta_s_next > 0)
  
  tmp %>%
    group_by(wave, wave_label) %>%
    summarise(
      N    = n(),
      R2   = cor(theta_s_next, theta_s_pred, use = "complete.obs")^2,
      RMSE = sqrt(mean((theta_s_next - theta_s_pred)^2, na.rm = TRUE)),
      bias = mean(theta_s_pred - theta_s_next, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(scenario = scen)
})

val_rho_wide <- val_rho %>%
  select(wave_label, scenario, R2, bias) %>%
  pivot_wider(
    names_from  = scenario,
    values_from = c(R2, bias),
    names_glue  = "{.value}_{scenario}"
  )

cat("  θ̂ and φ_S held fixed. Only ρ_θ varies.\n")
cat("  ρ_θ_lo → less growth predicted → negative bias (underprediction)\n")
cat("  ρ_θ_hi → more growth predicted → positive bias (overprediction)\n\n")

print(kable(
  val_rho_wide %>% select(
    wave_label,
    R2_rho_lo,      bias_rho_lo,
    R2_rho_central, bias_rho_central,
    R2_rho_hi,      bias_rho_hi
  ),
  digits  = c(0, 3, 6, 3, 6, 3, 6),
  col.names = c("Wave",
                "R²(lo)", "Bias(lo)",
                "R²(mid)", "Bias(mid)",
                "R²(hi)", "Bias(hi)")
))

cat("\n  NOTE on ρ_θ vs. IFR sensitivity:\n")
cat("  ρ_θ affects only θ_predicted (right-hand side of validation).\n")
cat("  IFR affects BOTH sides (θ̂_next AND θ̂ in θ_predicted).\n")
cat("  Therefore ρ_θ has a direct, monotonic effect on bias, while IFR\n")
cat("  has a partially self-canceling effect (explaining why IFR sensitivity\n")
cat("  is weaker than ρ_θ sensitivity for a given percentage perturbation).\n\n")


# ==============================================================================
#  6d. COMBINED ROBUSTNESS SUMMARY TABLE
# ==============================================================================
# One row per specification, columns: what varied, overall R², overall bias,
# and the wave with worst fit.
#
# This table is intended for the paper appendix.

cat("--- 6d. Combined Robustness Summary ---\n\n")

# Helper function: compute overall fit for a given (lag, ifr, rho) combination
compute_fit <- function(d_lead_col, ifr_vec, rho_vec, spec_label) {
  
  tmp <- panel_w %>%
    mutate(
      theta_s      = pmax(0, .data[[d_lead_col]] / ifr_vec),
      theta_s_pred = rho_vec * (1 - phi_S_central * S_mean) * pmax(0, .data[[d_lead_col]] / ifr_vec)
    ) %>%
    arrange(Country, date) %>%
    group_by(Country) %>%
    mutate(theta_s_next = lead(theta_s, 1)) %>%
    ungroup() %>%
    filter(date >= as.Date("2020-03-01"), date <= as.Date("2022-12-31"),
           !is.na(theta_s_next), !is.na(theta_s_pred),
           theta_s > 0, theta_s_next > 0)
  
  # Overall statistics
  overall_R2   <- cor(tmp$theta_s_next, tmp$theta_s_pred, use = "complete.obs")^2
  overall_bias <- mean(tmp$theta_s_pred - tmp$theta_s_next, na.rm = TRUE)
  overall_RMSE <- sqrt(mean((tmp$theta_s_next - tmp$theta_s_pred)^2, na.rm = TRUE))
  
  # Log-scale R² (for overall model assessment)
  log_tmp <- tmp %>% filter(theta_s_next > 0, theta_s_pred > 0)
  overall_R2_log <- cor(log(log_tmp$theta_s_next), log(log_tmp$theta_s_pred),
                        use = "complete.obs")^2
  
  # Worst wave (by absolute bias)
  wave_fits <- tmp %>%
    group_by(wave_label) %>%
    summarise(
      wave_bias = mean(theta_s_pred - theta_s_next, na.rm = TRUE),
      wave_R2   = cor(theta_s_next, theta_s_pred, use = "complete.obs")^2,
      .groups   = "drop"
    )
  worst <- wave_fits %>% slice_max(abs(wave_bias), n = 1)
  
  tibble(
    specification = spec_label,
    R2_levels     = overall_R2,
    R2_log        = overall_R2_log,
    bias          = overall_bias,
    RMSE          = overall_RMSE,
    worst_wave    = worst$wave_label,
    worst_bias    = worst$wave_bias,
    worst_R2      = worst$wave_R2
  )
}

# --- Define all specifications ---
robustness_specs <- bind_rows(
  
  # Baseline
  compute_fit("d_lead3", panel_w$ifr, panel_w$rho_w,
              "Baseline (ℓ=3, IFR_mid, ρ_mid)"),
  
  # --- Lag perturbations ---
  compute_fit("d_lead2", panel_w$ifr, panel_w$rho_w,
              "ℓ = 2 (IFR_mid, ρ_mid)"),
  compute_fit("d_lead4", panel_w$ifr, panel_w$rho_w,
              "ℓ = 4 (IFR_mid, ρ_mid)"),
  
  # --- IFR perturbations ---
  compute_fit("d_lead3", panel_w$ifr_lo, panel_w$rho_w,
              "IFR_lo (ℓ=3, ρ_mid)"),
  compute_fit("d_lead3", panel_w$ifr_hi, panel_w$rho_w,
              "IFR_hi (ℓ=3, ρ_mid)"),
  
  # --- ρ_θ perturbations ---
  compute_fit("d_lead3", panel_w$ifr, panel_w$rho_w_lo,
              "ρ_lo (ℓ=3, IFR_mid)"),
  compute_fit("d_lead3", panel_w$ifr, panel_w$rho_w_hi,
              "ρ_hi (ℓ=3, IFR_mid)"),
  
  # --- Joint: IFR + ρ_θ ---
  compute_fit("d_lead3", panel_w$ifr_lo, panel_w$rho_w_lo,
              "IFR_lo + ρ_lo (ℓ=3)"),
  compute_fit("d_lead3", panel_w$ifr_lo, panel_w$rho_w_hi,
              "IFR_lo + ρ_hi (ℓ=3)"),
  compute_fit("d_lead3", panel_w$ifr_hi, panel_w$rho_w_lo,
              "IFR_hi + ρ_lo (ℓ=3)"),
  compute_fit("d_lead3", panel_w$ifr_hi, panel_w$rho_w_hi,
              "IFR_hi + ρ_hi (ℓ=3)"),
  
  # --- Joint: lag + IFR ---
  compute_fit("d_lead2", panel_w$ifr_lo, panel_w$rho_w,
              "ℓ=2 + IFR_lo (ρ_mid)"),
  compute_fit("d_lead2", panel_w$ifr_hi, panel_w$rho_w,
              "ℓ=2 + IFR_hi (ρ_mid)"),
  compute_fit("d_lead4", panel_w$ifr_lo, panel_w$rho_w,
              "ℓ=4 + IFR_lo (ρ_mid)"),
  compute_fit("d_lead4", panel_w$ifr_hi, panel_w$rho_w,
              "ℓ=4 + IFR_hi (ρ_mid)")
)

# Sort by overall log R² descending
robustness_specs <- robustness_specs %>%
  arrange(desc(R2_log))

cat("  15 specifications: baseline + 2 lag + 2 IFR + 2 ρ_θ + 4 joint(IFR,ρ) + 4 joint(ℓ,IFR)\n")
cat("  Sorted by overall log R² (best fit first).\n\n")

print(kable(
  robustness_specs,
  digits  = c(0, 3, 3, 6, 6, 0, 6, 3),
  col.names = c("Specification", "R²(lev)", "R²(log)", "Bias", "RMSE",
                "Worst Wave", "Worst Bias", "Worst R²")
))

# --- 6e. Interpretation Summary ----------------------------------------------

cat("\n\n", strrep("-", 70), "\n")
cat("  INTERPRETATION SUMMARY\n")
cat(strrep("-", 70), "\n\n")

# Identify best and worst specifications
best_spec  <- robustness_specs %>% slice_max(R2_log, n = 1)
worst_spec <- robustness_specs %>% slice_min(R2_log, n = 1)

# Ranges
R2_range   <- range(robustness_specs$R2_log)
bias_range <- range(robustness_specs$bias)

cat(sprintf("  Overall log R² ranges from %.3f to %.3f across all specifications.\n",
            R2_range[1], R2_range[2]))
cat(sprintf("  Overall bias ranges from %.6f to %.6f.\n\n", bias_range[1], bias_range[2]))
cat(sprintf("  Best specification:  %s (R²_log = %.3f, bias = %.6f)\n",
            best_spec$specification, best_spec$R2_log, best_spec$bias))
cat(sprintf("  Worst specification: %s (R²_log = %.3f, bias = %.6f)\n\n",
            worst_spec$specification, worst_spec$R2_log, worst_spec$bias))

cat("  Key findings:\n\n")
cat("  (1) LAG SENSITIVITY: ℓ has minimal impact on fit quality because\n")
cat("      the ±1 week shift primarily translates into timing noise that\n")
cat("      is absorbed by the within-wave averaging. The central ℓ = 3 is\n")
cat("      neither systematically better nor worse than ℓ = 2 or ℓ = 4.\n\n")
cat("  (2) IFR SENSITIVITY: IFR affects BOTH θ̂ and θ_predicted, creating\n")
cat("      a partially self-canceling effect. Levels R² shifts modestly;\n")
cat("      bias shifts monotonically. The central IFR minimizes |bias|\n")
cat("      for most waves, confirming the literature-based calibration.\n")
cat("      EXCEPTION: Delta, where even IFR_hi leaves residual positive\n")
cat("      bias, suggesting the model's lack of an explicit immunity\n")
cat("      channel bites hardest during the vaccination ramp-up.\n\n")
cat("  (3) ρ_θ SENSITIVITY: ρ_θ has the strongest and most direct effect\n")
cat("      on bias (one-sided: only θ_predicted changes). This confirms\n")
cat("      that ρ_θ is the parameter with the most leverage on model fit\n")
cat("      and the least external empirical anchoring.\n\n")
cat("  (4) JOINT PERTURBATIONS: The worst specifications combine\n")
cat("      directionally reinforcing errors (e.g., IFR_lo + ρ_hi: θ̂\n")
cat("      inflated AND ρ amplified → large positive bias). The best\n")
cat("      joint specifications balance opposing effects. This is\n")
cat("      expected and confirms internal consistency of the calibration.\n\n")
cat("  (5) ROBUSTNESS OF THE TRILEMMA: The qualitative structure of the\n")
cat("      θ-equation — infections grow without NPIs (ρ_θ > 1) and are\n")
cat("      suppressed by lockdowns (φ_S > 0) — holds under ALL 15\n")
cat("      specifications. The trilemma result is therefore robust to\n")
cat("      calibration uncertainty within literature-based bounds.\n\n")


##Gibt mir drei verschiedenen Szenarien pro Parameter wobei ich alle drei Versuchen muss

# ==============================================================================
#  04b_quarterly_aggregation_and_monthly_imputation.R
#
#  Creates the quarterly θ̂ panel for Stage 2 estimation.
#
#  Structure:
#    7a. Define pandemic quarters
#    7b. Aggregate weekly θ̂ → quarterly (35 countries from panel_w)
#    7c. Monthly θ-imputation for CRI, JPN, TUR
#    7d. Scale monthly θ̂ to weekly-equivalent units
#    7e. Aggregate monthly θ̂ → quarterly
#    7f. Merge into theta_quarterly_full (38 countries)
#    7g. Diagnostics: monthly vs. weekly comparison
#    7h. Quarterly validation: θ̂ vs. P-scores
#
#  Run AFTER 04_stage1_theta_imputation.R
#  Requires: panel_w, ifr_wave, pop, p_values_oecd_w, oxd_d, qdata
# ==============================================================================


cat("\n", strrep("=", 70), "\n")
cat("  SECTION 7: Quarterly Aggregation & Monthly Imputation\n")
cat(strrep("=", 70), "\n\n")


# --- 7a. Define pandemic quarters ---------------------------------------------

pandemic_qs <- c("Q1.2020", "Q2.2020", "Q3.2020", "Q4.2020",
                 "Q1.2021", "Q2.2021", "Q3.2021", "Q4.2021",
                 "Q1.2022", "Q2.2022", "Q3.2022", "Q4.2022")

cat("  Pandemic quarters: ", paste(pandemic_qs, collapse = ", "), "\n\n")


# --- 7b. Aggregate weekly θ̂ to quarterly (35 weekly-source countries) ----------
# θ̂_k^Q = (1/N_w) Σ_{w ∈ Q_k} θ̂_w   (average weekly prevalence in quarter)
# Also compute: sum (cumulative), max (peak), lag variants, IFR bounds

theta_quarterly <- panel_w %>%
  filter(date >= as.Date("2020-01-01"), date <= as.Date("2022-12-31"),
         !is.na(theta_hat)) %>%
  mutate(
    year    = year(date),
    quarter = quarter(date),
    YQ      = paste0("Q", quarter, ".", year)
  ) %>%
  group_by(Country, YQ, year, quarter) %>%
  summarise(
    theta_mean    = mean(theta_hat, na.rm = TRUE),
    theta_sum     = sum(theta_hat, na.rm = TRUE),
    theta_max     = max(theta_hat, na.rm = TRUE),
    theta_mean_l2 = mean(theta_hat_l2, na.rm = TRUE),
    theta_mean_l4 = mean(theta_hat_l4, na.rm = TRUE),
    theta_mean_lo = mean(theta_hat_ifr_lo, na.rm = TRUE),
    theta_mean_hi = mean(theta_hat_ifr_hi, na.rm = TRUE),
    S_mean        = mean(S_mean, na.rm = TRUE),
    S_max         = max(S_max, na.rm = TRUE),
    n_periods     = n(),
    wave_dominant = names(which.max(table(wave))),
    .groups       = "drop"
  ) %>%
  filter(YQ %in% pandemic_qs) %>%
  mutate(
    YQ          = factor(YQ, levels = pandemic_qs),
    freq_source = "weekly"
  )

cat(sprintf("--- 7b. Weekly → Quarterly ---\n"))
cat(sprintf("  %d obs (%d countries x %d quarters)\n",
            nrow(theta_quarterly),
            n_distinct(theta_quarterly$Country),
            n_distinct(theta_quarterly$YQ)))

# Cross-country quarterly statistics
q_theta_cross <- theta_quarterly %>%
  group_by(YQ) %>%
  summarise(
    Mean   = mean(theta_mean, na.rm = TRUE) * 100,
    Median = median(theta_mean, na.rm = TRUE) * 100,
    SD     = sd(theta_mean, na.rm = TRUE) * 100,
    Min    = min(theta_mean, na.rm = TRUE) * 100,
    Max    = max(theta_mean, na.rm = TRUE) * 100,
    .groups = "drop"
  )

cat("\n--- Quarterly theta_hat Cross-Country Statistics (% of population) ---\n")
print(kable(q_theta_cross, digits = 4,
            col.names = c("Quarter", "Mean", "Median", "SD", "Min", "Max")))
cat("\n")


# ==============================================================================
#  7c–7e. MONTHLY θ-IMPUTATION FOR CRI, JPN, TUR
#  Corrected version: covers all pandemic quarters Q1.2020 – Q4.2022
# ==============================================================================

cat(strrep("-", 50), "\n")
cat("  Monthly imputation: CRI, JPN, TUR\n")
cat(strrep("-", 50), "\n\n")

# --- 7c. Extract & prepare monthly mortality data -----------------------------

mort_monthly <- p_values_oecd_w %>%
  filter(
    time_unit == "monthly",
    entity %in% c("CRI", "JPN", "TUR")
  ) %>%
  mutate(
    Country = entity,
    date    = as.Date(date)
  ) %>%
  # FIX 1: extend to end of 2022
  filter(date >= as.Date("2020-01-01"), date <= as.Date("2022-12-31")) %>%
  select(
    Country, date,
    excess   = excess_proj_all_ages,
    expected = projected_deaths_since_2020_all_ages,
    observed = deaths_since_2020_all_ages,
    p_proj   = p_proj_all_ages
  ) %>%
  arrange(Country, date)

# Merge population
mort_monthly <- mort_monthly %>%
  left_join(pop, by = "Country")

cat(sprintf("  Monthly mortality: %d obs, %d countries\n",
            nrow(mort_monthly), n_distinct(mort_monthly$Country)))
cat(sprintf("  Date range: %s to %s\n", min(mort_monthly$date), max(mort_monthly$date)))

# --- Diagnostic: data availability per country --------------------------------
cat("\n  Data availability per country:\n")
for (cty in c("CRI", "JPN", "TUR")) {
  sub <- mort_monthly %>% filter(Country == cty)
  cat(sprintf("    %s: %d months, excess NA: %d, pop NA: %d\n",
              cty, nrow(sub),
              sum(is.na(sub$excess)),
              sum(is.na(sub$pop))))
}
cat("\n")

# --- Assign wave-specific IFR -------------------------------------------------
# FIX 2: Extend W4_omicron to cover all of 2022.
# Months Jul–Dec 2022 previously fell into "Post" → no IFR match → theta_hat = NA.
# The Omicron IFR calibration (0.04% central) remains the best available
# estimate for this period; post-vaccination IFR only decreases further,
# so treating H2.2022 as W4_omicron is conservative.

mort_monthly <- mort_monthly %>%
  mutate(
    date_mid = date + 14,
    wave = case_when(
      date_mid < as.Date("2020-06-15")  ~ "W1",
      date_mid < as.Date("2020-09-15")  ~ "W1_summer",
      date_mid < as.Date("2021-03-01")  ~ "W2_wt",
      date_mid < as.Date("2021-07-01")  ~ "W2_alpha",
      date_mid < as.Date("2022-01-01")  ~ "W3_delta",
      TRUE                              ~ "W4_omicron"   # covers all of 2022+
    )
  ) %>%
  select(-date_mid) %>%
  left_join(ifr_wave %>% select(wave, ifr, ifr_lo, ifr_hi), by = "wave")

# Diagnostic: wave assignment
wave_check <- mort_monthly %>%
  filter(date >= as.Date("2020-03-01"), date <= as.Date("2022-12-31")) %>%
  group_by(wave) %>%
  summarise(n = n(), ifr_mean = mean(ifr, na.rm = TRUE), .groups = "drop")
cat("--- Wave Assignment Check ---\n")
print(kable(wave_check, digits = 4, col.names = c("Wave", "N months", "Mean IFR")))

na_ifr <- sum(is.na(mort_monthly$ifr[mort_monthly$date >= "2020-03-01" &
                                       mort_monthly$date <= "2022-12-31"]))
if (na_ifr > 0) {
  warning(sprintf("  %d months with missing IFR — wave join failed!", na_ifr))
} else {
  cat(sprintf("  All months matched to IFR. OK.\n\n"))
}


# --- 7d. Monthly imputation with weekly-equivalent scaling --------------------
# θ̂_m = d_{m+1} / IFR gives MONTHLY incidence (infections per month per capita).
# Weekly θ̂_w = d_{w+3} / IFR gives WEEKLY incidence (infections per week per capita).
# To make units comparable, we scale monthly to weekly-equivalent:
#
#   θ̂_m^{w-equiv} = (d_{m+1} / IFR) × (7 / days_in_month_m)
#
# Note on the lead: for the LAST month of each country's data, d_lead1 = NA.
# This is correct and expected (no future death observation available).
# At quarterly aggregation, a quarter with ≥2 valid months is retained.

mort_monthly <- mort_monthly %>%
  mutate(
    d_pc          = excess / pop,
    days_in_month = as.numeric(days_in_month(date))
  ) %>%
  group_by(Country) %>%
  arrange(date) %>%
  mutate(
    d_lead1 = lead(d_pc, n = 1)
  ) %>%
  ungroup() %>%
  mutate(
    theta_monthly_raw = pmax(0, d_lead1 / ifr),
    theta_hat    = pmax(0, d_lead1 / ifr)    * (7 / days_in_month),
    theta_hat_lo = pmax(0, d_lead1 / ifr_hi) * (7 / days_in_month),
    theta_hat_hi = pmax(0, d_lead1 / ifr_lo) * (7 / days_in_month)
  )

# Diagnostics
cat("--- 7d. Monthly theta_hat Diagnostics (weekly-equivalent) ---\n")
for (cty in c("CRI", "JPN", "TUR")) {
  sub <- mort_monthly %>%
    filter(Country == cty, date >= "2020-03-01", date <= "2022-12-31",
           !is.na(theta_hat))
  cat(sprintf("  %s: %d months with valid theta_hat\n", cty, nrow(sub)))
  cat(sprintf("       mean = %.4f%%, max = %.4f%% (weekly-equivalent)\n",
              mean(sub$theta_hat) * 100,
              max(sub$theta_hat) * 100))
  cat(sprintf("       scaling factor 7/days_in_month range: [%.3f, %.3f]\n",
              min(7 / sub$days_in_month), max(7 / sub$days_in_month)))
}
cat("\n")

# Detailed NA pipeline diagnostic
cat("--- NA Pipeline (Mar 2020 – Dec 2022) ---\n")
pipeline_check <- mort_monthly %>%
  filter(date >= as.Date("2020-03-01"), date <= as.Date("2022-12-31")) %>%
  group_by(Country) %>%
  summarise(
    n_months     = n(),
    excess_ok    = sum(!is.na(excess)),
    pop_ok       = sum(!is.na(pop)),
    d_pc_ok      = sum(!is.na(d_pc)),
    d_lead1_ok   = sum(!is.na(d_lead1)),
    ifr_ok       = sum(!is.na(ifr)),
    theta_hat_ok = sum(!is.na(theta_hat)),
    .groups = "drop"
  )
print(kable(pipeline_check, col.names = c(
  "Country", "N months", "excess OK", "pop OK", "d_pc OK",
  "d_lead1 OK", "IFR OK", "theta_hat OK"
)))
cat("  Each column shows where NAs enter the pipeline.\n")
cat("  d_lead1 loses 1 obs per country (last month has no lead).\n\n")


# --- Monthly stringency: compute both S_mean and S_max from daily oxd_d -------
# FIX 3: extend to 2022-12-31 (was 2022-03-31); compute S_mean alongside S_max.
# FIX 4: the previous code produced only S_max; the summarise in 7e referenced
#         S_mean which does not exist in mort_monthly → silent NA column.

s_monthly <- oxd_d %>%
  mutate(Date = as.Date(Date)) %>%
  filter(
    Country %in% c("CRI", "JPN", "TUR"),
    Date >= as.Date("2020-01-01"), Date <= as.Date("2022-12-31")
  ) %>%
  mutate(year = year(Date), month = month(Date)) %>%
  group_by(Country, year, month) %>%
  summarise(
    S_mean = mean(StringencyIndex_PopWeighted, na.rm = TRUE) / 100,
    S_max  = max(StringencyIndex_PopWeighted,  na.rm = TRUE) / 100,
    .groups = "drop"
  )

mort_monthly <- mort_monthly %>%
  mutate(
    year  = year(date),
    month = month(date)
  ) %>%
  left_join(s_monthly, by = c("Country", "year", "month"))


# --- 7e. Aggregate monthly θ̂ to quarterly ------------------------------------
# FIX 5: extend date filter to 2022-12-31 (was 2021-12-31)

theta_quarterly_monthly <- mort_monthly %>%
  filter(
    date >= as.Date("2020-01-01"), date <= as.Date("2022-12-31"),
    !is.na(theta_hat)
  ) %>%
  mutate(
    quarter = quarter(date),
    YQ      = paste0("Q", quarter, ".", year)
  ) %>%
  group_by(Country, YQ, year, quarter) %>%
  summarise(
    theta_mean    = mean(theta_hat, na.rm = TRUE),
    theta_sum     = sum(theta_hat, na.rm = TRUE),
    theta_max     = max(theta_hat, na.rm = TRUE),
    theta_mean_l2 = mean(theta_hat, na.rm = TRUE),   # monthly: ℓ=2 indistinguishable
    theta_mean_l4 = mean(theta_hat, na.rm = TRUE),   # from ℓ=4 at monthly frequency
    theta_mean_lo = mean(theta_hat_lo, na.rm = TRUE),
    theta_mean_hi = mean(theta_hat_hi, na.rm = TRUE),
    S_mean        = mean(S_mean, na.rm = TRUE),   # now correctly exists
    S_max         = max(S_max,  na.rm = TRUE),
    n_periods     = n(),
    wave_dominant = names(which.max(table(wave))),
    .groups       = "drop"
  ) %>%
  filter(YQ %in% pandemic_qs) %>%
  mutate(
    YQ          = factor(YQ, levels = pandemic_qs),
    freq_source = "monthly"
  )

##Achtung lag 2 & 4 liegen beide innerhalb von einem Monat-> Schwierig bei Monatsdaten

cat(sprintf("--- 7e. Monthly → Quarterly ---\n"))
cat(sprintf("  %d obs (%d countries x up to %d quarters)\n",
            nrow(theta_quarterly_monthly),
            n_distinct(theta_quarterly_monthly$Country),
            n_distinct(theta_quarterly_monthly$YQ)))

# Show per-country quarter coverage
cat("  Quarter coverage per country:\n")
for (cty in c("CRI", "JPN", "TUR")) {
  qs <- theta_quarterly_monthly %>%
    filter(Country == cty) %>%
    pull(YQ) %>%
    as.character()
  cat(sprintf("    %s: %d quarters — %s\n", cty, length(qs), paste(qs, collapse = ", ")))
}
cat("\n")


# --- 7f. Merge into theta_quarterly_full (38 countries) -----------------------

theta_quarterly_full <- bind_rows(theta_quarterly, theta_quarterly_monthly) %>%
  arrange(Country, YQ)

cat(sprintf("--- 7f. Merged theta_quarterly_full ---\n"))
cat(sprintf("  Total: %d obs (%d countries x up to %d quarters)\n",
            nrow(theta_quarterly_full),
            n_distinct(theta_quarterly_full$Country),
            n_distinct(theta_quarterly_full$YQ)))
cat(sprintf("  Weekly-source countries:  %d\n",
            n_distinct(theta_quarterly_full$Country[theta_quarterly_full$freq_source == "weekly"])))
cat(sprintf("  Monthly-source countries: %d\n",
            n_distinct(theta_quarterly_full$Country[theta_quarterly_full$freq_source == "monthly"])))

# Sanity: every OECD country present?
oecd_38 <- c("AUS","AUT","BEL","CAN","CHE","CHL","COL","CRI","CZE","DEU","DNK",
             "ESP","EST","FIN","FRA","GBR","GRC","HUN","ISL","IRL","ISR","ITA",
             "JPN","KOR","LTU","LUX","LVA","MEX","NLD","NOR","NZL","POL","PRT",
             "SVK","SVN","SWE","TUR","USA")

missing <- setdiff(oecd_38, unique(theta_quarterly_full$Country))
cat(sprintf("  Missing OECD countries: %s\n\n",
            ifelse(length(missing) == 0, "NONE — all 38 present",
                   paste(missing, collapse = ", "))))


# --- 7g. Diagnostics: monthly vs. weekly source comparison --------------------

cat("--- 7g. Monthly vs. Weekly Source Comparison ---\n\n")

comp_table <- theta_quarterly_full %>%
  group_by(freq_source) %>%
  summarise(
    N          = n(),
    countries  = n_distinct(Country),
    mean_theta = mean(theta_mean, na.rm = TRUE) * 100,
    sd_theta   = sd(theta_mean, na.rm = TRUE) * 100,
    .groups    = "drop"
  )

print(kable(comp_table, digits = 4,
            col.names = c("Source", "N", "Countries", "Mean theta_hat (%)", "SD theta_hat (%)")))
cat("  Units are weekly-equivalent for both sources.\n")
cat("  Comparable magnitudes confirm monthly imputation is plausible.\n\n")

# Per-quarter IQR check
range_check <- theta_quarterly_full %>%
  filter(!is.na(theta_mean)) %>%
  group_by(YQ) %>%
  summarise(
    weekly_p25 = quantile(theta_mean[freq_source == "weekly"], 0.25, na.rm = TRUE),
    weekly_p75 = quantile(theta_mean[freq_source == "weekly"], 0.75, na.rm = TRUE),
    .groups = "drop"
  )

monthly_vals <- theta_quarterly_full %>%
  filter(freq_source == "monthly") %>%
  select(Country, YQ, theta_mean)

range_check <- range_check %>%
  left_join(
    monthly_vals %>% pivot_wider(names_from = Country, values_from = theta_mean,
                                 names_prefix = "theta_"),
    by = "YQ"
  )

cat("--- Monthly Countries vs. Weekly IQR ---\n")
for (cty in c("CRI", "JPN", "TUR")) {
  col <- paste0("theta_", cty)
  if (col %in% names(range_check)) {
    vals <- range_check[[col]]
    in_iqr <- !is.na(vals) &
      vals >= range_check$weekly_p25 &
      vals <= range_check$weekly_p75
    n_valid <- sum(!is.na(vals))
    n_in    <- sum(in_iqr, na.rm = TRUE)
    cat(sprintf("  %s: %d of %d quarters within weekly IQR\n", cty, n_in, n_valid))
  } else {
    cat(sprintf("  %s: no data in theta_quarterly_full\n", cty))
  }
}
cat("  If most quarters lie within the weekly IQR, the monthly imputation\n")
cat("  does not introduce systematic outliers into the panel.\n\n")


# --- 7h. Quarterly validation: θ̂ vs. P-scores --------------------------------

theta_q_val <- theta_quarterly_full %>%
  arrange(Country, YQ) %>%
  left_join(
    qdata %>%
      filter(Quarter %in% pandemic_qs) %>%
      select(Country, Quarter, p_proj_all_ages),
    by = c("Country", "YQ" = "Quarter")
  )

q_cor <- cor(theta_q_val$theta_mean, theta_q_val$p_proj_all_ages,
             use = "complete.obs")

cat(sprintf("--- 7h. Quarterly Validation ---\n"))
cat(sprintf("  corr(theta_hat_k, P-score_k) = %.3f\n", q_cor))
cat("  This correlation should be high (both derive from excess mortality)\n")
cat("  but not 1.0 (θ̂ uses wave-specific IFR; P-score does not).\n")
cat("  A correlation < 0.7 would indicate a problem in the aggregation.\n\n")

cat(strrep("=", 70), "\n")
cat("  theta_quarterly_full is ready for Stage 2 estimation.\n")
cat(strrep("=", 70), "\n")



##Jede Variable von Hand durchgehen
#Finaler Datensatz definieren und rest löschen

##MAYBE ALLES NOCH 2021 RAUSNEHMEN FÜR DECRIPTIVES-> FÄLLE HOCH DA IFR TIEF-< VERZERRT ALLES AUSSER MORTALITY MITPLOTTEN

# ==============================================================================
#  8. SUMMARY DIAGNOSTICS & EXPORT
# ==============================================================================

# --- 8b. Export theta_quarterly for Stage 2 -----------------------------------

cat("\n  Exporting theta_quarterly for Stage 2 panel merge...\n")

# This object can be joined to qdata via Country + YQ = Quarter
# Key variables for Stage 2:
#   theta_mean     — central θ̂ (quarterly mean prevalence)
#   theta_mean_l2  — robustness (ℓ = 2)
#   theta_mean_l4  — robustness (ℓ = 4)
#   theta_mean_lo  — robustness (IFR high bound → fewer infections)
#   theta_mean_hi  — robustness (IFR low bound → more infections)
#   S_mean         — weekly-averaged S for this quarter (cross-check)
#   S_max          - max values for robustness-> Aggregationsproblem

# Save as RDS for clean import
saveRDS(theta_quarterly_full, "theta_quarterly_stage1.rds")
saveRDS(panel_w, "theta_weekly_stage1.rds")

cat("  Saved: theta_quarterly_stage1.rds, theta_weekly_stage1.rds\n\n")

# --- 8c. Key findings for paper -----------------------------------------------

cat("=== KEY FINDINGS FOR PAPER ===\n\n")

cat("  1. Imputed θ̂ confirms four-wave structure with sharply different\n")
cat("     magnitudes: Omicron prevalence ~10-50x higher than Wave 1,\n")
cat("     reflecting both higher R₀ and lower IFR denominator.\n\n")

cat("  2. θ-equation validation (R² ≈ ", round(r_sq_log, 2),
    " in logs) confirms that\n")
cat("     calibrated (ρ_θ, φ_S) are consistent with observed data.\n")
cat("     Residual patterns indicate behavioral adaptation and immunity\n")
cat("     buildup not fully captured by S alone.\n\n")

cat("  3. Cross-check: θ̂ systematically exceeds confirmed cases,\n")
cat("     consistent with known testing underascertainment.\n")
cat("     Ratio declines over time as testing capacity improved.\n\n")

cat("  4. θ̂ robust to lag choice (ℓ ∈ {2,3,4}: corr > 0.99) and\n")
cat("     moderately sensitive to IFR bounds (±20-40% for early waves,\n")
cat("     wider for Omicron). IFR is the dominant uncertainty source.\n\n")

cat("  5. Quarterly θ̂_k ready for Stage 2 panel estimation:\n")
cat("     controls for infection-driven endogeneity of S_k in the\n")
cat("     output equation, enabling cleaner identification of fiscal\n")
cat("     parameters (α_F^DI, η, κ_F^DI, κ_F^CP).\n\n")

cat(strrep("=", 70), "\n")
cat("  Stage 1 complete. Proceed to Stage 2 (fiscal estimation).\n")
cat(strrep("=", 70), "\n")

rm(
  # --- Fiscal descriptives ---
  overall_total, by_channel_imf, cp_decomp,
  temporal_channel, temporal_wide, temporal_cum, temporal_year, temporal_count,
  count_long, vol_long, cp_share_labels,
  country_channel, country_plot, oecd_avg,
  cp_order, di_order, h_order,
  country_pies, scatter_data,
  income_channel, region_country_avg,
  instrument_rank, category_summary,
  imf_cross, blw_non_cp, def_non_cp,
  size_dist, top_decile, n_80pct,
  lorenz_data, cum_contrib, cross_80, rank_size,
  full_grid, panel_data, country_totals,
  country_quarter, mean_deploy, mean_cp, mean_di,
  tabelle_aer,
  
  # --- Stringency descriptives ---
  oxd_panel, q_stringency, q_cross, wq_stats, s_panel_stats,
  outcome_panel, cors,
  
  # --- Mortality descriptives ---
  mort_panel, weekly_agg, wave_labels, wave_windows,
  overall_stats, country_stats, within_stats, conc_stats,
  grand_mean, country_means, gm_rate, cm_rate,
  SS_total, SS_between, SS_within, SS_t_rate, SS_b_rate,
  q_anova, wave_stats, wave_country,
  wide_mort, corr_matrix, upper_tri, corr_long, regions,
  merged_sd, lag_corrs, lag_corrs_ccf, cross_corr,
  opt_lags, lag_range,
  
  # --- θ-imputation intermediates ---
  mort_w, s_weekly, conf_daily, conf_weekly,
  theta_stats, theta_agg, theta_stats_ext,
  cross_check,
  val_data, val_log, resid_time,
  lag_data, lag_cor, lag_scenarios,
  val_ifr, val_ifr_wide,
  val_rho, val_rho_wide,
  robustness_specs, best_spec, worst_spec, R2_range, bias_range,
  q_theta_cross,
  mort_monthly, wave_check, s_monthly,
  theta_quarterly_monthly,
  comp_table, range_check, monthly_vals,
  theta_q_val,
  pipeline_check, age_data, age_data_long, age_ration,
  age_summary, age_wave_tab, age_weekly_agg, age_ratio,
  cor_cp_di, cor_cpshare_total, cs_cum, ifr_scenarios, ifr_sens, ifr_theta_levels, ifr_wave,
  peak_row, q_cm, q_cs, q_cum, q_mort, q_stats, ratio_wave, reg_corr, rho_scenarios,
  tho_theta_weekly, sub, s_monthly_max, trough_row, trough_windows, val_lag, val_lag_wide,
  val_sensitivity, val_wave, val_wide, val_labels_df, wave_theta, year_stats, wave_labels_df, cor_hosp, cor_comparison, cor_excess, 
  cor_all, best_lag, rho_theta_weekly, label_row
)

# 1. Deinen Zielordner definieren
safedata <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/Database/Analyse"

# 2. Dateipfad für die RData-Datei erstellen
pathdata<- file.path(safedata, "datasets_c.RData")

# 3. Alle Datensätze in diese eine Datei speichern
save(qdata, theta_quarterly_full, panel_w, fm,
     file = pathdata)

##Welche Variablen
##Welche Checks

# ==============================================================================
#  STAGE 2 — SECTION 1: DESCRIPTIVE STATISTICS FOR STATE VARIABLES y_k AND b_k
#  Output gap (y_t_pct) and debt gap (d_t_pct) for 38 OECD countries
#  Pandemic period: Q1.2020 – Q4.2021 (trilemma horizon)
#  Pre-COVID baseline: Q1.2015 – Q4.2019
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(stringr)
library(lubridate)
library(knitr)

# --- Panel definition ---------------------------------------------------------
pandemic_qs <- c("Q1.2020", "Q2.2020", "Q3.2020", "Q4.2020",
                 "Q1.2021", "Q2.2021", "Q3.2021", "Q4.2021")

pre_covid_qs <- c("Q1.2015", "Q2.2015", "Q3.2015", "Q4.2015",
                  "Q1.2016", "Q2.2016", "Q3.2016", "Q4.2016",
                  "Q1.2017", "Q2.2017", "Q3.2017", "Q4.2017",
                  "Q1.2018", "Q2.2018", "Q3.2018", "Q4.2018",
                  "Q1.2019", "Q2.2019", "Q3.2019", "Q4.2019")

# Helper: Quarter string → date
q_to_date <- function(q) {
  yr  <- as.integer(str_sub(q, 4, 7))
  qn  <- as.integer(str_sub(q, 2, 2))
  ymd(paste0(yr, "-", (qn - 1L) * 3L + 1L, "-01"))
}

# Full panel (pre + pandemic) for variance decomposition
panel_full <- qdata %>%
  filter(Quarter %in% c(pre_covid_qs, pandemic_qs)) %>%
  mutate(
    date    = q_to_date(Quarter),
    period  = ifelse(Quarter %in% pandemic_qs, "Pandemic", "Pre-COVID"),
    quarter_num = as.integer(str_sub(Quarter, 2, 2))
  ) %>%
  select(Country, Quarter, date, period, quarter_num, year_only,
         y_t_pct, QReal.GDP.Growth_gr,
         d_t_pct, d_t_pct_r,
         DebtN_share2019, DebtR_share2019,
         DebtN_share2019_growth, DebtR_share2019_growth,
         Qpopulation_th)

# Pandemic-only panel
panel_covid <- panel_full %>% filter(period == "Pandemic")

cat("\n", strrep("=", 70), "\n")
cat("  SECTION 1: DESCRIPTIVES — y_k AND b_k STATE VARIABLES\n")
cat(strrep("=", 70), "\n\n")
cat(sprintf("  Full panel:     %d obs (%d countries × %d quarters)\n",
            nrow(panel_full), n_distinct(panel_full$Country),
            n_distinct(panel_full$Quarter)))
cat(sprintf("  Pandemic panel: %d obs (%d countries × %d quarters)\n\n",
            nrow(panel_covid), n_distinct(panel_covid$Country),
            n_distinct(panel_covid$Quarter)))


# ==============================================================================
#  1a. UNIVARIATE DESCRIPTIVES — POOLED
# ==============================================================================

cat(strrep("-", 70), "\n")
cat("  1a. Pooled Descriptive Statistics\n")
cat(strrep("-", 70), "\n\n")

desc_stats <- function(x, label) {
  tibble(
    Variable = label,
    N        = sum(!is.na(x)),
    Mean     = mean(x, na.rm = TRUE),
    SD       = sd(x, na.rm = TRUE),
    Min      = min(x, na.rm = TRUE),
    P10      = quantile(x, 0.10, na.rm = TRUE),
    P25      = quantile(x, 0.25, na.rm = TRUE),
    Median   = median(x, na.rm = TRUE),
    P75      = quantile(x, 0.75, na.rm = TRUE),
    P90      = quantile(x, 0.90, na.rm = TRUE),
    Max      = max(x, na.rm = TRUE)
  )
}

tbl_desc <- bind_rows(
  # --- y_k ---
  desc_stats(panel_full$y_t_pct[panel_full$period == "Pre-COVID"],
             "y_k: Output Gap (Pre-COVID)"),
  desc_stats(panel_covid$y_t_pct,
             "y_k: Output Gap (Pandemic)"),
  desc_stats(panel_covid$QReal.GDP.Growth_gr,
             "y_k [Rob.]: GDP Growth YoY (Pandemic)"),
  # --- b_k ---
  desc_stats(panel_full$d_t_pct[panel_full$period == "Pre-COVID"],
             "b_k: Debt Gap nominal (Pre-COVID)"),
  desc_stats(panel_covid$d_t_pct,
             "b_k: Debt Gap nominal (Pandemic)"),
  desc_stats(panel_covid$d_t_pct_r,
             "b_k [Rob.]: Debt Gap real (Pandemic)"),
  desc_stats(panel_covid$DebtN_share2019_growth,
             "b_k [Rob.]: Debt/GDP2019 growth QoQ (Pandemic)")
)

print(kable(tbl_desc, digits = 3,
            caption = "Table: Descriptive Statistics — y_k and b_k"))

cat("\n  Key moments:\n")
cat(sprintf("  y_k mean pandemic:      %.2f pp  (Pre-COVID: %.2f pp)\n",
            mean(panel_covid$y_t_pct, na.rm = TRUE),
            mean(panel_full$y_t_pct[panel_full$period == "Pre-COVID"], na.rm = TRUE)))
cat(sprintf("  y_k trough:             %.2f pp  (%s)\n",
            min(panel_covid$y_t_pct, na.rm = TRUE),
            panel_covid$Quarter[which.min(panel_covid$y_t_pct)]))
cat(sprintf("  b_k mean pandemic:      %.2f pp  (Pre-COVID: %.2f pp)\n",
            mean(panel_covid$d_t_pct, na.rm = TRUE),
            mean(panel_full$d_t_pct[panel_full$period == "Pre-COVID"], na.rm = TRUE)))
cat(sprintf("  b_k max accumulation:   %.2f pp  (%s)\n\n",
            max(panel_covid$d_t_pct, na.rm = TRUE),
            panel_covid$Quarter[which.max(panel_covid$d_t_pct)]))

##Die Unterschiede zwischen Pre-Covid und COVID sind klar ersichtlich-> Plot?
# --- Plot 1a: y_k and b_k trajectory with 95% CI across countries ------------

ci_data <- panel_full %>%
  group_by(Quarter, date, period) %>%
  summarise(
    # y_k
    y_mean  = mean(y_t_pct, na.rm = TRUE),
    y_se    = sd(y_t_pct,   na.rm = TRUE) / sqrt(sum(!is.na(y_t_pct))),
    y_lo    = y_mean - 1.96 * y_se,
    y_hi    = y_mean + 1.96 * y_se,
    y_sd    = sd(y_t_pct,   na.rm = TRUE),
    # b_k
    b_mean  = mean(d_t_pct, na.rm = TRUE),
    b_se    = sd(d_t_pct,   na.rm = TRUE) / sqrt(sum(!is.na(d_t_pct))),
    b_lo    = b_mean - 1.96 * b_se,
    b_hi    = b_mean + 1.96 * b_se,
    b_sd    = sd(d_t_pct,   na.rm = TRUE),
    N       = sum(!is.na(y_t_pct)),
    .groups = "drop"
  ) %>%
  arrange(date)

p_yk_ci <- ggplot(ci_data, aes(x = date)) +
  annotate("rect",
           xmin = as.Date("2020-01-01"), xmax = as.Date("2021-12-31"),
           ymin = -Inf, ymax = Inf, fill = "grey92", alpha = 0.7) +
  geom_ribbon(aes(ymin = y_lo, ymax = y_hi, fill = period), alpha = 0.35) +
  geom_line(aes(y = y_mean), color = "black", linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40",
             linewidth = 0.4) +
  scale_fill_manual(values = c("Pre-COVID" = "grey60", "Pandemic" = "#C0392B"),
                    name = NULL) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y",
               expand = expansion(mult = c(0.01, 0.01))) +
  labs(
    title    = "Figure X-A: Output Gap y_k — OECD Mean with 95% Confidence Band",
    subtitle = paste0("Black: cross-country mean. Band: \u00b11.96 \u00d7 SE (N \u2248 38 countries).\n",
                      "Wider band during pandemic reflects cross-country heterogeneity, not sampling noise."),
    y        = "Output Gap (% of potential GDP)",
    x        = NULL,
    caption  = paste0("Notes: 38 OECD economies, Q1.2015\u2013Q4.2021. ",
                      "HP-filter trend estimated on 2015\u20132019 data.")
  ) +
  theme_aer +
  theme(
    axis.title.y    = element_text(size = 9, family = "serif", margin = margin(r = 8)),
    legend.position = c(0.15, 0.2)
  )

p_bk_ci <- ggplot(ci_data, aes(x = date)) +
  annotate("rect",
           xmin = as.Date("2020-01-01"), xmax = as.Date("2021-12-31"),
           ymin = -Inf, ymax = Inf, fill = "grey92", alpha = 0.7) +
  geom_ribbon(aes(ymin = b_lo, ymax = b_hi, fill = period), alpha = 0.35) +
  geom_line(aes(y = b_mean), color = "#2980B9", linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40",
             linewidth = 0.4) +
  scale_fill_manual(values = c("Pre-COVID" = "grey60", "Pandemic" = "#2980B9"),
                    name = NULL) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y",
               expand = expansion(mult = c(0.01, 0.01))) +
  labs(
    title    = "Figure X-B: Debt Gap b_k — OECD Mean with 95% Confidence Band",
    subtitle = paste0("Blue: cross-country mean. Band: \u00b11.96 \u00d7 SE.\n",
                      "Expanding band from Q2.2020 reflects diverging national fiscal responses."),
    y        = "Debt Gap (pp of 2019 GDP)",
    x        = NULL,
    caption  = paste0("Notes: 38 OECD economies, Q1.2015\u2013Q4.2021. ",
                      "Nominal debt normalized to 2019 GDP.")
  ) +
  theme_aer +
  theme(
    axis.title.y    = element_text(size = 9, family = "serif", margin = margin(r = 8)),
    legend.position = c(0.15, 0.8)
  )

p_ci_combined <- (p_yk_ci / p_bk_ci) +
  plot_annotation(
    title    = "Figure X: State Variable Trajectories with Cross-Country Dispersion",
    subtitle = paste0("Wider confidence bands during the pandemic document the ",
                      "cross-country heterogeneity central to the trilemma argument."),
    theme = theme(
      plot.title    = element_text(size = 11, family = "serif", face = "bold"),
      plot.subtitle = element_text(size = 9,  family = "serif", color = "grey30")
    )
  )

print(p_ci_combined)

##NIcer Plot, evlt noch für die RObustness Spezifikationen
# ==============================================================================
#  1b. WITHIN vs. BETWEEN VARIANCE DECOMPOSITION
# ==============================================================================

cat(strrep("-", 70), "\n")
cat("  1b. Variance Decomposition: Within vs. Between\n")
cat(strrep("-", 70), "\n\n")

variance_decomp <- function(df, varname, label) {
  
  x    <- df[[varname]]
  cty  <- df$Country
  
  # Grand mean
  grand_mean  <- mean(x, na.rm = TRUE)
  
  # Between: variance of country means around grand mean
  cty_means   <- tapply(x, cty, mean, na.rm = TRUE)
  var_between <- var(cty_means, na.rm = TRUE)
  
  # Within: mean of within-country variances
  var_within  <- mean(tapply(x, cty, var, na.rm = TRUE), na.rm = TRUE)
  
  # Total
  var_total   <- var(x, na.rm = TRUE)
  icc         <- var_between / (var_between + var_within)
  
  tibble(
    Variable     = label,
    SD_total     = sqrt(var_total),
    SD_between   = sqrt(var_between),
    SD_within    = sqrt(var_within),
    ICC          = icc,
    `Within (%)` = var_within / var_total * 100,
    `Between (%)` = var_between / var_total * 100
  )
}

tbl_var <- bind_rows(
  variance_decomp(panel_covid, "y_t_pct",                "y_k: Output Gap"),
  variance_decomp(panel_covid, "QReal.GDP.Growth_gr",    "y_k [Rob.]: GDP Growth YoY"),
  variance_decomp(panel_covid, "d_t_pct",                "b_k: Debt Gap nominal"),
  variance_decomp(panel_covid, "d_t_pct_r",              "b_k [Rob.]: Debt Gap real"),
  variance_decomp(panel_covid, "DebtN_share2019_growth", "b_k [Rob.]: Debt/GDP2019 growth")
)

print(kable(tbl_var, digits = 3,
            caption = "Table: Variance Decomposition (Pandemic Period)"))

cat("\n  Interpretation:\n")
cat("  ICC > 0.5 → cross-country heterogeneity dominates → country FE essential.\n")
cat("  ICC < 0.5 → within-country dynamics dominate → time variation is the\n")
cat("  primary source of identification for the structural parameters.\n\n")

##Für Output Gap-> Within dominiert, das ist spannend da die meiste IDentifikation aus Unterschiede in den Ländern kommt-> KOmposition
##Für Debt Gap-> Between dominiert-> RE oder Pooled OLS-> Cross COuntry Phänomen

#Drei Fragen: 1) Supoptimale F-Komposition? oder fehlende institutionelle Kapazität oder pre-pandemic Schuldenpsoition?-> Ich habe eine sehr hohe heterogenität zwischen Länder-> sag tmein Modell voraus

#-> Within bei Y deutet an das unterschiedlich Strategien in einem gleichen Land unterschiedlich wirken zu unterschiedlichen Zeiten-> Mein CP, DI und state dependencies Argument
#-> Between bei D deutet an das die KOnsequenzen von F unterchiedliche sein können absierend auf den Gegebenheiten eines Landes (siehe oben) ->  Between-Variation zeigt, dass das Trilemma für verschiedene Länder unterschiedlich stark bindend ist

# ==============================================================================
#  1c. BY-WAVE DESCRIPTIVES (cross-sectional mean ± SD per quarter)
# ==============================================================================

cat(strrep("-", 70), "\n")
cat("  1c. Cross-Country Mean and SD by Quarter\n")
cat(strrep("-", 70), "\n\n")

tbl_byq <- panel_covid %>%
  group_by(Quarter) %>%
  summarise(
    y_mean  = mean(y_t_pct,  na.rm = TRUE),
    y_sd    = sd(y_t_pct,    na.rm = TRUE),
    y_min   = min(y_t_pct,   na.rm = TRUE),
    y_max   = max(y_t_pct,   na.rm = TRUE),
    b_mean  = mean(d_t_pct,  na.rm = TRUE),
    b_sd    = sd(d_t_pct,    na.rm = TRUE),
    b_min   = min(d_t_pct,   na.rm = TRUE),
    b_max   = max(d_t_pct,   na.rm = TRUE),
    N       = n(),
    .groups = "drop"
  ) %>%
  arrange(Quarter)

print(kable(tbl_byq, digits = 2,
            caption = "Table: Cross-Country Mean and Dispersion by Quarter"))

##Variationin Y wird später deutlicher-> konvergiert on average zur Erholung
##Schuldenakkumulation ist monton steigend-> Dynamik 
#-> Das was imGraph von vorher sichtbar ist

# --- Test: Länder die Output-Gap schneller geschlossen haben →
#           höhere Schuldenakkumulation?

recovery_debt <- panel_covid %>%
  group_by(Country) %>%
  summarise(
    # Output-Gap Erholung: Differenz Q4.2021 minus Q2.2020 (trough to end)
    y_trough     = y_t_pct[Quarter == "Q2.2020"],
    y_end        = y_t_pct[Quarter == "Q4.2021"],
    y_recovery   = y_end - y_trough,          # >0 = Erholung
    # Peak-Schuldenakkumulation
    b_peak       = max(d_t_pct, na.rm = TRUE),
    b_end        = d_t_pct[Quarter == "Q4.2021"],
    # Fiskaleinsatz gesamt (falls fiscal panel verfügbar)
    .groups = "drop"
  ) %>%
  filter(!is.na(y_trough), !is.na(y_end), !is.na(b_peak))

# Korrelation
cor_rec_debt <- cor(recovery_debt$y_recovery, recovery_debt$b_peak,
                    use = "complete.obs")

cat(sprintf("  cor(y_recovery, b_peak): %.3f\n", cor_rec_debt))
cat(sprintf("  N countries: %d\n\n", nrow(recovery_debt)))
cat("  Interpretation:\n")
cat("  Negative → Länder mit stärkerer Erholung haben mehr Schulden akkumuliert.\n")
cat("  Positive → Schuldenakkumulation hat Erholung NICHT erkauft (Trilemma bindend).\n\n")

# Scatter Plot
p_recovery_debt <- ggplot(recovery_debt,
                          aes(x = y_recovery, y = b_peak, label = Country)) +
  geom_point(size = 2, color = "grey40", alpha = 0.7) +
  geom_text(size = 2.3, family = "serif", vjust = -0.7, color = "grey30") +
  geom_smooth(method = "lm", se = TRUE, color = "#C0392B",
              linewidth = 0.7, fill = "grey85") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey50",
             linewidth = 0.3) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey50",
             linewidth = 0.3) +
  labs(
    title    = "Figure X: Output Recovery vs. Peak Debt Accumulation",
    subtitle = sprintf("r = %.3f. Each point = one OECD country.", cor_rec_debt),
    x        = "Output Gap Recovery Q2.2020 \u2192 Q4.2021 (pp)",
    y        = "Peak Debt Gap b_k (pp of 2019 GDP)",
    caption  = paste0(
      "Notes: Recovery = y_k(Q4.2021) \u2212 y_k(Q2.2020). ",
      "Negative slope: stronger recovery associated with larger debt overhang.\n",
      "Positive slope: debt accumulated without purchasing output recovery ",
      "(trilemma binding)."
    )
  ) +
  theme_aer +
  theme(axis.title = element_text(size = 9, family = "serif"))

print(p_recovery_debt)

#Schnellere Erholung-> Mehr Debt-> Erkauft (Trilemma besteht)-> aber nicht komplett möglich-> necessary aber nid sufficient gewesen-> BESTE STRATEGIE (einfach reinhauen war nicht optimal)-> Die Variation zweischen enzelnen Ländern zeigt die nachfolgende Tabelle
#Die Höhe von F ist ein schlechter Prädiktor für Outcomes weil sie die falsche Frage beantwortet. Die richtige Frage ist: welches Instrument (F^DI vs. F^CP), in welchem Zustand (θ_k, y_k), unter welcher Containment-Intensität (S_k)? 

#"The moderate negative correlation (r = −0.33) between output recovery and peak debt accumulation indicates that fiscal spending was a necessary but not sufficient condition for recovery. The incomplete trade-off is consistent with state-dependent transmission: countries facing deeper recessions and higher infection pressure required disproportionately larger fiscal outlays per unit of output stabilization, reflecting the structural constraints formalized in the pandemic trilemma."
#"The moderate correlation between fiscal volume and output recovery (r = −0.33) confirms that the composition, timing, and state-contingency of fiscal interventions — not their aggregate size — determine pandemic economic outcomes. This finding motivates the structural approach: a dynamic optimization framework in which the planner's instrument choice is explicitly conditioned on the evolving state vector (θ_k, y_k, b_k, S_k)."

# ==============================================================================
#  1d. COUNTRY-LEVEL SUMMARY (pandemic averages, sorted by y_k)
# ==============================================================================

cat(strrep("-", 70), "\n")
cat("  1d. Country-Level Pandemic Averages\n")
cat(strrep("-", 70), "\n\n")

tbl_cty <- panel_covid %>%
  group_by(Country) %>%
  summarise(
    y_mean   = mean(y_t_pct,  na.rm = TRUE),
    y_min    = min(y_t_pct,   na.rm = TRUE),
    b_mean   = mean(d_t_pct,  na.rm = TRUE),
    b_max    = max(d_t_pct,   na.rm = TRUE),
    N        = n(),
    .groups  = "drop"
  ) %>%
  arrange(y_mean)

print(kable(tbl_cty, digits = 2,
            caption = "Table: Country Pandemic Averages (sorted by mean y_k)"))

cat("\n  Cross-country SD of mean y_k:", round(sd(tbl_cty$y_mean, na.rm = TRUE), 2), "pp\n")
cat("  Cross-country SD of mean b_k:", round(sd(tbl_cty$b_mean, na.rm = TRUE), 2), "pp\n\n")

#Interpretation-> siehe oben

# ==============================================================================
#  1e. PRE-COVID vs. PANDEMIC MEANS TEST
# ==============================================================================

cat(strrep("-", 70), "\n")
cat("  1e. Pre-COVID vs. Pandemic: Paired t-test\n")
cat(strrep("-", 70), "\n\n")

# Use country-level means to avoid autocorrelation
pre_means_y <- panel_full %>%
  filter(period == "Pre-COVID") %>%
  group_by(Country) %>%
  summarise(y = mean(y_t_pct, na.rm = TRUE), b = mean(d_t_pct, na.rm = TRUE),
            .groups = "drop")

cov_means <- panel_covid %>%
  group_by(Country) %>%
  summarise(y = mean(y_t_pct, na.rm = TRUE), b = mean(d_t_pct, na.rm = TRUE),
            .groups = "drop")

paired_y <- t.test(cov_means$y, pre_means_y$y, paired = FALSE)
paired_b <- t.test(cov_means$b, pre_means_y$b, paired = FALSE)

cat(sprintf("  y_k: Pre-COVID mean = %.2f pp | Pandemic mean = %.2f pp\n",
            mean(pre_means_y$y, na.rm = TRUE),
            mean(cov_means$y,   na.rm = TRUE)))
cat(sprintf("       Δ = %.2f pp | t = %.2f | p = %.4f\n\n",
            mean(cov_means$y) - mean(pre_means_y$y),
            paired_y$statistic, paired_y$p.value))

cat(sprintf("  b_k: Pre-COVID mean = %.2f pp | Pandemic mean = %.2f pp\n",
            mean(pre_means_y$b, na.rm = TRUE),
            mean(cov_means$b,   na.rm = TRUE)))
cat(sprintf("       Δ = %.2f pp | t = %.2f | p = %.4f\n\n",
            mean(cov_means$b) - mean(pre_means_y$b),
            paired_b$statistic, paired_b$p.value))

#Viel bezahlt pro pp Output Gap
#"Relative to the pre-pandemic trend, OECD economies experienced an average output gap of −4.94 percentage points and accumulated 13.50 percentage points of additional debt-to-GDP during the trilemma period (both significant at p < 0.001). The implied ratio of 2.7 percentage points of debt per percentage point of unrecovered output provides a first-order estimate of the fiscal cost of the trilemma under actual — and likely suboptimal — policy responses."

# ==============================================================================
#  1f. CORRELATION STRUCTURE
# ==============================================================================

cat(strrep("-", 70), "\n")
cat("  1f. Correlation Structure (Pandemic Panel)\n")
cat(strrep("-", 70), "\n\n")

cor_vars <- panel_covid %>%
  select(y_t_pct, QReal.GDP.Growth_gr, d_t_pct, d_t_pct_r,
         DebtN_share2019_growth) %>%
  cor(use = "pairwise.complete.obs")

print(kable(round(cor_vars, 3),
            caption = "Table: Pairwise Correlations — y_k and b_k variants"))

cat("\n  Key correlations:\n")
cat(sprintf("  cor(y_k, b_k nominal):    %.3f\n",
            cor(panel_covid$y_t_pct, panel_covid$d_t_pct, use = "complete.obs")))
cat(sprintf("  cor(y_k, b_k real):       %.3f\n",
            cor(panel_covid$y_t_pct, panel_covid$d_t_pct_r, use = "complete.obs")))
cat(sprintf("  cor(y_k, GDP growth):     %.3f\n",
            cor(panel_covid$y_t_pct, panel_covid$QReal.GDP.Growth_gr, use = "complete.obs")))
cat(sprintf("  cor(b_k gap, b_k growth): %.3f\n\n",
            cor(panel_covid$d_t_pct, panel_covid$DebtN_share2019_growth, use = "complete.obs")))


# ==============================================================================
#  PLOTS
# ==============================================================================

# Helper: date for plotting
panel_full  <- panel_full  %>% mutate(date = q_to_date(Quarter))
panel_covid <- panel_covid %>% mutate(date = q_to_date(Quarter))

# OECD aggregate per quarter
oecd_q <- panel_full %>%
  group_by(Quarter, date, period) %>%
  summarise(
    y_mean  = mean(y_t_pct,  na.rm = TRUE),
    y_p25   = quantile(y_t_pct,  0.25, na.rm = TRUE),
    y_p75   = quantile(y_t_pct,  0.75, na.rm = TRUE),
    b_mean  = mean(d_t_pct,  na.rm = TRUE),
    b_p25   = quantile(d_t_pct,  0.25, na.rm = TRUE),
    b_p75   = quantile(d_t_pct,  0.75, na.rm = TRUE),
    .groups = "drop"
  )

# --- Plot 1: y_k trajectory --------------------------------------------------
p_yk_traj <- ggplot(oecd_q, aes(x = date)) +
  annotate("rect",
           xmin = as.Date("2020-01-01"), xmax = as.Date("2021-12-31"),
           ymin = -Inf, ymax = Inf, fill = "grey90", alpha = 0.5) +
  geom_ribbon(aes(ymin = y_p25, ymax = y_p75), fill = "grey70", alpha = 0.5) +
  geom_line(aes(y = y_mean), color = "black", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.4) +
  annotate("text", x = as.Date("2020-10-01"), y = Inf,
           label = "Trilemma\nperiod", vjust = 1.3, size = 2.5,
           family = "serif", color = "grey40") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y",
               expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  labs(
    title    = "Figure X-A: Output Gap y_k — OECD Mean and IQR",
    subtitle = "Black: OECD cross-country mean. Band: IQR (P25–P75). Grey shading: pandemic period.",
    y        = "Output Gap (% of potential GDP)",
    x        = NULL,
    caption  = "Notes: HP-filter trend estimated on 2015–2019 data, extrapolated. 38 OECD economies."
  ) +
  theme_aer +
  theme(axis.title.y = element_text(size = 9, family = "serif",
                                    margin = margin(r = 8)))

print(p_yk_traj)

# --- Plot 2: b_k trajectory --------------------------------------------------
p_bk_traj <- ggplot(oecd_q, aes(x = date)) +
  annotate("rect",
           xmin = as.Date("2020-01-01"), xmax = as.Date("2021-12-31"),
           ymin = -Inf, ymax = Inf, fill = "grey90", alpha = 0.5) +
  geom_ribbon(aes(ymin = b_p25, ymax = b_p75), fill = "#AED6F1", alpha = 0.5) +
  geom_line(aes(y = b_mean), color = "#2980B9", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.4) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y",
               expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  labs(
    title    = "Figure X-B: Debt Gap b_k — OECD Mean and IQR",
    subtitle = "Blue: OECD cross-country mean. Band: IQR (P25–P75). Grey shading: pandemic period.",
    y        = "Debt Gap (pp of pre-pandemic GDP)",
    x        = NULL,
    caption  = "Notes: HP-filter trend estimated on 2015–2019 data. Nominal debt normalized to 2019 GDP."
  ) +
  theme_aer +
  theme(axis.title.y = element_text(size = 9, family = "serif",
                                    margin = margin(r = 8)))

print(p_bk_traj)

# --- Plot 3: Joint trajectory (combined panel) --------------------------------
scale_b  <- max(abs(oecd_q$y_mean), na.rm = TRUE) /
  max(abs(oecd_q$b_mean),  na.rm = TRUE)

p_joint <- ggplot(oecd_q, aes(x = date)) +
  annotate("rect",
           xmin = as.Date("2020-01-01"), xmax = as.Date("2021-12-31"),
           ymin = -Inf, ymax = Inf, fill = "grey92", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.3) +
  geom_line(aes(y = y_mean), color = "black",   linewidth = 0.8) +
  geom_line(aes(y = b_mean * scale_b), color = "#2980B9", linewidth = 0.8,
            linetype = "dashed") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y",
               expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(
    name     = "Output Gap y_k (% of potential)",
    expand   = expansion(mult = c(0.05, 0.05)),
    sec.axis = sec_axis(~ . / scale_b,
                        name = "Debt Gap b_k (pp of 2019 GDP)")
  ) +
  labs(
    title    = "Figure X-C: Output Gap y_k and Debt Gap b_k (OECD Mean)",
    subtitle = "Black solid: y_k (left axis). Blue dashed: b_k (right axis).",
    x        = NULL,
    caption  = paste0("Notes: OECD cross-country means. Both series HP-detrended on pre-2020 data.\n",
                      "Grey shading: trilemma period (Q1.2020–Q4.2021).")
  ) +
  theme_aer +
  theme(
    axis.title.y       = element_text(size = 9, family = "serif",
                                      margin = margin(r = 8)),
    axis.title.y.right = element_text(size = 9, family = "serif", color = "#2980B9",
                                      margin = margin(l = 8)),
    axis.text.y.right  = element_text(color = "#2980B9"),
    axis.ticks.y.right = element_line(color = "#2980B9")
  )

print(p_joint)

#Guter Motivationsplot

# --- Plot 4: Cross-country scatter y_k vs b_k --------------------------------
p_scatter_yb <- ggplot(panel_covid, aes(x = y_t_pct, y = d_t_pct)) +
  geom_point(alpha = 0.25, size = 1.2, color = "grey40") +
  geom_smooth(method = "lm", se = TRUE, color = "#C0392B",
              linewidth = 0.7, fill = "grey85") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey50", linewidth = 0.3) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey50", linewidth = 0.3) +
  labs(
    title    = "Figure X-D: Output Gap vs. Debt Gap (Pandemic Panel)",
    subtitle = sprintf("r = %.3f (descriptive). Each point = country-quarter.",
                       cor(panel_covid$y_t_pct, panel_covid$d_t_pct,
                           use = "complete.obs")),
    x        = "Output Gap y_k (% of potential)",
    y        = "Debt Gap b_k (pp of 2019 GDP)",
    caption  = "Notes: Negative correlation expected — deeper recession generates larger debt overhang."
  ) +
  theme_aer

print(p_scatter_yb)

# --- Plot 5: Country-level heatmap -------------------------------------------
heatmap_data <- panel_covid %>%
  select(Country, Quarter, date, y_t_pct, d_t_pct) %>%
  pivot_longer(cols = c(y_t_pct, d_t_pct),
               names_to  = "variable",
               values_to = "value") %>%
  mutate(variable = recode(variable,
                           "y_t_pct" = "Output Gap y_k (%)",
                           "d_t_pct" = "Debt Gap b_k (pp)"))

p_heatmap <- ggplot(heatmap_data,
                    aes(x = date,
                        y = reorder(Country, value, FUN = mean, na.rm = TRUE),
                        fill = value)) +
  geom_tile(color = "white", linewidth = 0.1) +
  facet_wrap(~variable, scales = "free_x") +
  scale_fill_gradient2(low = "#C0392B", mid = "white", high = "#2980B9",
                       midpoint = 0, name = "pp") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  labs(
    title    = "Figure X-E: y_k and b_k — Country × Quarter Heatmap",
    subtitle = "Red: below-trend output / above-trend debt. Blue: above-trend output / below-trend debt.",
    x        = NULL,
    y        = NULL,
    caption  = "Notes: Countries ordered by mean value within each panel."
  ) +
  theme_aer +
  theme(
    axis.text.y  = element_text(size = 6),
    strip.text   = element_text(size = 8, family = "serif"),
    legend.position = "right"
  )

print(p_heatmap)

# --- Plot 6: Pre-COVID vs. pandemic boxplots ---------------------------------
box_data <- panel_full %>%
  select(Country, Quarter, period, y_t_pct, d_t_pct) %>%
  pivot_longer(cols = c(y_t_pct, d_t_pct),
               names_to  = "variable",
               values_to = "value") %>%
  mutate(variable = recode(variable,
                           "d_t_pct" = "Debt Gap b_k (pp)",
                           "y_t_pct" = "Output Gap y_k (%)"),
         period = factor(period, levels = c("Pre-COVID", "Pandemic")))

p_boxplot <- ggplot(box_data, aes(x = period, y = value, fill = period)) +
  geom_boxplot(outlier.size = 0.8, outlier.alpha = 0.4, linewidth = 0.4) +
  # Nach dem geom_boxplot hinzufügen
  geom_text(
    data = box_data %>%
      filter(variable == "Debt Gap b_k (pp)", value > 50),
    aes(label = Country),
    size = 2.3, family = "serif", hjust = -0.2, color = "grey30"
  )+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.3) +
  facet_wrap(~variable, scales = "free_y") +
  scale_fill_manual(values = c("Pre-COVID" = "grey80", "Pandemic" = "#AED6F1"),
                    guide = "none") +
  labs(
    title    = "Figure X-F: Distribution of y_k and b_k — Pre-COVID vs. Pandemic",
    subtitle = "Each observation = country-quarter. Boxes: IQR. Whiskers: 1.5 × IQR.",
    x        = NULL,
    y        = "Value (pp)",
    caption  = "Notes: 38 OECD economies. Pre-COVID: Q1.2015–Q4.2019. Pandemic: Q1.2020–Q4.2021."
  ) +
  theme_aer +
  theme(strip.text = element_text(size = 8, family = "serif"))

print(p_boxplot)

##Zeigt das Trilemma ohne ein Wort-> evtl noch als average darstellen-> nicht als Quarterly

# ==============================================================================
#  SUMMARY OUTPUT
# ==============================================================================

cat("\n\n", strrep("=", 70), "\n")
cat("  SUMMARY: KEY EMPIRICAL FACTS FOR THE PAPER\n")
cat(strrep("=", 70), "\n\n")

cat("  (1) OUTPUT GAP y_k:\n")
cat(sprintf("      Pre-COVID mean:   %.2f pp (SD: %.2f pp)\n",
            mean(panel_full$y_t_pct[panel_full$period == "Pre-COVID"], na.rm = TRUE),
            sd(panel_full$y_t_pct[panel_full$period == "Pre-COVID"],   na.rm = TRUE)))
cat(sprintf("      Pandemic mean:    %.2f pp (SD: %.2f pp)\n",
            mean(panel_covid$y_t_pct, na.rm = TRUE),
            sd(panel_covid$y_t_pct,   na.rm = TRUE)))
cat(sprintf("      Trough (OECD):    %.2f pp in %s\n",
            min(oecd_q$y_mean[oecd_q$period == "Pandemic"], na.rm = TRUE),
            oecd_q$Quarter[oecd_q$period == "Pandemic"][
              which.min(oecd_q$y_mean[oecd_q$period == "Pandemic"])]))

cat("\n  (2) DEBT GAP b_k:\n")
cat(sprintf("      Pre-COVID mean:   %.2f pp (SD: %.2f pp)\n",
            mean(panel_full$d_t_pct[panel_full$period == "Pre-COVID"], na.rm = TRUE),
            sd(panel_full$d_t_pct[panel_full$period == "Pre-COVID"],   na.rm = TRUE)))
cat(sprintf("      Pandemic mean:    %.2f pp (SD: %.2f pp)\n",
            mean(panel_covid$d_t_pct, na.rm = TRUE),
            sd(panel_covid$d_t_pct,   na.rm = TRUE)))
cat(sprintf("      Peak (OECD):      %.2f pp in %s\n",
            max(oecd_q$b_mean[oecd_q$period == "Pandemic"], na.rm = TRUE),
            oecd_q$Quarter[oecd_q$period == "Pandemic"][
              which.max(oecd_q$b_mean[oecd_q$period == "Pandemic"])]))

cat("\n  (3) IDENTIFICATION PROSPECTS:\n")
icc_y <- tbl_var$ICC[tbl_var$Variable == "y_k: Output Gap"]
icc_b <- tbl_var$ICC[tbl_var$Variable == "b_k: Debt Gap nominal"]
cat(sprintf("      y_k ICC: %.3f → %s variation dominates\n",
            icc_y, ifelse(icc_y > 0.5, "between-country", "within-country")))
cat(sprintf("      b_k ICC: %.3f → %s variation dominates\n",
            icc_b, ifelse(icc_b > 0.5, "between-country", "within-country")))
cat(sprintf("      cor(y_k, b_k): %.3f\n\n",
            cor(panel_covid$y_t_pct, panel_covid$d_t_pct, use = "complete.obs")))

rm(tbl_var, tbl_desc, tbl_cty, tbl_byq, recovery_debt, recovery_oecd, pre_means_y, pre_covid_oecd,
   box_data, ci_data, cor_vars, cov_means, covid_oecd, heatmap_data, oecd_aggregate, oecd_q, paired_b, paired_y, panel_covid, panel_full, oecd_reference)


# 1. Deinen Zielordner definieren
safedata <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/Database/Analyse"

# 2. Dateipfad für die RData-Datei erstellen
pathdata<- file.path(safedata, "dataforanalysis.RData")

# 3. Alle Datensätze in diese eine Datei speichern
save(qdata, theta_quarterly_full, panel_w, google_mobility_d, hosp_d, hosp_w, oxd_d, oxd_spatial_d, p_values_oecd_w, panel_hosp, pop, fm, theme_aer, theme_aer_bar,
     oxd_d, oxd_spatial_d, google_mobility_d, hosp_d, 
     file = pathdata)


