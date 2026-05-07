## ============================================================================
##  Two-panel plot: Average stringency vs. size of fiscal response
##  Sample: USA, CAN, GBR, DEU, CHE, AUS — sorted by DI share desc.
##  - Panel A: mean of StringencyIndex_PopWeighted, 2020Q1-2021Q4
##  - Panel B: cumulative discretionary fiscal response (% of 2019 GDP),
##             stacked into DI | CP above | CP loans | CP guarantees
##  Style:    AER / QJE journal layout (matches fiscal_composition_size_aer.pdf)
## ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readxl)
  library(patchwork)
  library(stringr)
})

load("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/processed/dataforanalysis.RData")

fm1 <- read_excel("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/raw/fiscal measures/fiscal_classified_v1_7.xlsx")
fm1 <- fm1 %>% filter(!PolicyCode %in% c(5, 6, 11, 12, 15, 16))

sel <- c("USA", "CAN", "GBR", "DEU", "CHE", "AUS")
labels_iso <- c(USA = "United States", CAN = "Canada", GBR = "United Kingdom",
                DEU = "Germany",       CHE = "Switzerland", AUS = "Australia")

pandemic_qs <- c("Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                 "Q1.2021","Q2.2021","Q3.2021","Q4.2021")

## --- Fiscal response (stacked, 4 components) ---------------------------------
agg <- fm1 %>%
  filter(broad_fiscal == 1, Country %in% sel) %>%
  group_by(Country) %>%
  summarise(
    DI       = sum(broad_fiscal_gdp[transmission_channel == "DI"], na.rm = TRUE) * 100,
    CP_above = sum(broad_fiscal_gdp[transmission_channel == "CP" & category == 1], na.rm = TRUE) * 100,
    CP_loans = sum(broad_fiscal_gdp[transmission_channel == "CP" & category == 2 & PolicyCode %in% c("40","41")], na.rm = TRUE) * 100,
    CP_guar  = sum(broad_fiscal_gdp[transmission_channel == "CP" & category == 2 & PolicyCode == "43"], na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(total = DI + CP_above + CP_loans + CP_guar,
         DI_share = DI / total)

# Country order: by DI share descending — same as fiscal_composition_size_aer
country_order <- agg %>% arrange(desc(DI_share)) %>% pull(Country)

fiscal_long <- agg %>%
  select(Country, DI, CP_above, CP_loans, CP_guar) %>%
  pivot_longer(-Country, names_to = "Component", values_to = "pct_gdp") %>%
  mutate(
    Country   = factor(labels_iso[Country],
                       levels = labels_iso[country_order]),
    Component = factor(Component,
                       levels = c("CP_guar", "CP_loans", "CP_above", "DI"),
                       labels = c("CP guarantees",
                                  "CP loans",
                                  "CP above-the-line (wages, grants)",
                                  "DI (demand injections)"))
  )

## --- Average stringency (population-weighted) --------------------------------
strg <- qdata %>%
  filter(Country %in% sel, Quarter %in% pandemic_qs) %>%
  group_by(Country) %>%
  summarise(S_mean = mean(StringencyIndex_PopWeighted, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(Country = factor(labels_iso[Country],
                          levels = labels_iso[country_order]))

## --- Shared AER theme --------------------------------------------------------
pal <- c(
  "DI (demand injections)"             = "#0072B2",
  "CP above-the-line (wages, grants)"  = "#882255",
  "CP loans"                           = "#56B4E9",
  "CP guarantees"                      = "#BBBBBB"
)

aer_theme <- theme_minimal(base_size = 11, base_family = "serif") +
  theme(
    panel.grid          = element_blank(),
    axis.line.x         = element_line(colour = "black", linewidth = 0.35),
    axis.line.y         = element_line(colour = "black", linewidth = 0.35),
    axis.ticks          = element_line(colour = "black", linewidth = 0.35),
    axis.ticks.length   = unit(3, "pt"),
    axis.text           = element_text(colour = "black", size = 9),
    axis.text.x         = element_text(colour = "black", size = 9,
                                       angle = 30, hjust = 1),
    axis.title.y        = element_text(size = 10, margin = margin(r = 6)),
    plot.title          = element_text(face = "bold", size = 11,
                                       hjust = 0, margin = margin(b = 6)),
    plot.title.position = "plot",
    plot.margin         = margin(8, 12, 6, 8),
    legend.position     = "bottom",
    legend.title        = element_blank(),
    legend.key.width    = unit(0.55, "cm"),
    legend.key.height   = unit(0.35, "cm"),
    legend.text         = element_text(size = 9),
    legend.margin       = margin(t = 2, b = 0),
    legend.box.margin   = margin(t = -4)
  )

legend_breaks <- c("DI (demand injections)",
                   "CP above-the-line (wages, grants)",
                   "CP loans",
                   "CP guarantees")

## --- Panel A: average stringency (single bar per country) --------------------
p_strg <- ggplot(strg, aes(x = Country, y = S_mean)) +
  geom_col(width = 0.66, fill = "grey35", colour = "white", linewidth = 0.25) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, 20),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(title = "Panel A.  Average containment stringency",
       x = NULL, y = "Stringency index (0\u2013100)") +
  aer_theme

## --- Panel B: stacked size of fiscal response --------------------------------
p_size <- ggplot(fiscal_long, aes(x = Country, y = pct_gdp, fill = Component)) +
  geom_col(width = 0.66, colour = "white", linewidth = 0.25) +
  scale_y_continuous(labels = function(x) sprintf("%g", x),
                     breaks = scales::pretty_breaks(6),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = pal, breaks = legend_breaks) +
  labs(title = "Panel B.  Size of fiscal response",
       x = NULL, y = "Percentage points of 2019 GDP") +
  aer_theme

## --- Combine -----------------------------------------------------------------
caption_text <- str_wrap(paste0(
  "Notes: 2020Q1\u20132021Q4. Panel A: mean Oxford Stringency Index ",
  "(population-weighted, 0\u2013100). Panel B: cumulative discretionary ",
  "fiscal measures, pp of 2019 GDP. Countries ordered by DI share. ",
  "Sources: OxCGRT; IMF Fiscal Monitor."
), width = 125)

combined <- (p_strg + p_size) +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(
    caption = caption_text,
    theme = theme(
      plot.caption = element_text(family = "serif", size = 8.5,
                                  hjust = 0, colour = "grey25",
                                  margin = margin(t = 8)),
      plot.caption.position = "plot"
    )
  ) &
  theme(legend.position = "bottom")

out_dir  <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/output/figures"
out_file <- file.path(out_dir, "stringency_vs_fiscal_aer_v2.pdf")

ggsave(out_file, combined,
       width = 10, height = 5.6, units = "in", device = cairo_pdf)

cat("Saved:", out_file, "\n\n")
cat("Average stringency, 2020Q1-2021Q4:\n")
print(strg %>% arrange(desc(S_mean)))
