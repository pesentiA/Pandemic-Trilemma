## ============================================================================
##  Two-panel plot: composition (share) vs. size (% GDP 2019) of fiscal response
##  Categories: DI | CP above-the-line | CP loans | CP guarantees
##  Sample:     USA, CAN, GBR, DEU, CHE, AUS — sorted by DI share desc.
##  Style:      AER / QJE journal layout (matches outcomes_y_cumdebt_aer.pdf)
## ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readxl)
  library(patchwork)
  library(forcats)
  library(stringr)
})

fm1 <- read_excel("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/raw/fiscal measures/fiscal_classified_v1_7.xlsx")
fm1 <- fm1 %>% filter(!PolicyCode %in% c(5, 6, 11, 12, 15, 16))

sel <- c("USA", "CAN", "GBR", "DEU", "CHE", "AUS")
labels_iso <- c(USA = "United States", CAN = "Canada", GBR = "United Kingdom",
                DEU = "Germany",       CHE = "Switzerland", AUS = "Australia")

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

# Country order: by DI share descending
country_order <- agg %>% arrange(desc(DI_share)) %>% pull(Country)

long <- agg %>%
  select(Country, DI, CP_above, CP_loans, CP_guar) %>%
  pivot_longer(-Country, names_to = "Component", values_to = "pct_gdp") %>%
  group_by(Country) %>%
  mutate(share = pct_gdp / sum(pct_gdp)) %>%
  ungroup() %>%
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

# Muted, colour-blind-safe palette — print-safe in greyscale (clear luminance steps)
pal <- c(
  "DI (demand injections)"             = "#0072B2",  # deep blue   (darkest)
  "CP above-the-line (wages, grants)"  = "#882255",  # muted wine
  "CP loans"                           = "#56B4E9",  # light blue
  "CP guarantees"                      = "#BBBBBB"   # neutral grey (lightest)
)

# Shared AER-style theme (matches outcomes_y_cumdebt_aer.pdf)
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

p_share <- ggplot(long, aes(x = Country, y = share, fill = Component)) +
  geom_col(width = 0.66, colour = "white", linewidth = 0.25) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, 1, 0.2),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = pal, breaks = legend_breaks) +
  labs(title = "Panel A.  Composition of fiscal response",
       x = NULL, y = "Share of total response") +
  aer_theme

p_size <- ggplot(long, aes(x = Country, y = pct_gdp, fill = Component)) +
  geom_col(width = 0.66, colour = "white", linewidth = 0.25) +
  scale_y_continuous(labels = function(x) sprintf("%g", x),
                     breaks = scales::pretty_breaks(6),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = pal, breaks = legend_breaks) +
  labs(title = "Panel B.  Size of fiscal response",
       x = NULL, y = "Percentage points of 2019 GDP") +
  aer_theme

caption_text <- str_wrap(paste0(
  "Notes: Cumulative discretionary fiscal measures, 2020Q1\u20132021Q4. ",
  "DI = demand injections; CP = corporate protection (wage subsidies and ",
  "grants; loans; guarantees at face value). Countries ordered by DI share. ",
  "Source: IMF Fiscal Monitor."
), width = 125)

combined <- (p_share + p_size) +
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
out_file <- file.path(out_dir, "fiscal_composition_size_aer_v2.pdf")

ggsave(out_file, combined,
       width = 10, height = 5.6, units = "in", device = cairo_pdf)

cat("Saved:", out_file, "\n")
cat("\nCountry order (by DI share desc):\n")
print(agg %>% arrange(desc(DI_share)) %>%
        mutate(DI_share = round(DI_share * 100, 1)) %>%
        select(Country, DI, CP_above, CP_loans, CP_guar, total, DI_share))
