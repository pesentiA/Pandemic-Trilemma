## ============================================================================
##  Two-panel plot: Output Gap vs Cumulative New Debt since Q4.2019
##  Sample: USA, CAN, GBR, DEU, CHE, AUS, Q1.2019 – Q4.2022
##  - Panel A: y_t_pct (output gap, % of HP trend)
##  - Panel B: DebtN_share2019 - DebtN_share2019[Q4.2019]
##             = cumulative additional debt since end-2019, in % of 2019 GDP
##  Style:    AER / QJE journal layout (serif, muted palette, COVID shading)
## ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(stringr)
})

load("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/processed/dataforanalysis.RData")

sel <- c("USA", "CAN", "GBR", "DEU", "CHE", "AUS")
labels_iso <- c(USA = "United States", CAN = "Canada", GBR = "United Kingdom",
                DEU = "Germany",       CHE = "Switzerland", AUS = "Australia")

qs <- c("Q1.2019","Q2.2019","Q3.2019","Q4.2019",
        "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
        "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
        "Q1.2022","Q2.2022","Q3.2022","Q4.2022")

dat <- qdata %>%
  filter(Country %in% sel, Quarter %in% qs) %>%
  mutate(
    qnum  = as.integer(substr(Quarter, 2, 2)),
    yr    = as.integer(substr(Quarter, 4, 7)),
    qdate = as.Date(sprintf("%d-%02d-01", yr, (qnum - 1) * 3 + 1))
  ) %>%
  arrange(Country, qdate) %>%
  group_by(Country) %>%
  mutate(
    base_debt = DebtN_share2019[Quarter == "Q4.2019"][1],
    cum_new_debt = DebtN_share2019 - base_debt
  ) %>%
  ungroup() %>%
  mutate(Country = factor(labels_iso[Country], levels = labels_iso[sel])) %>%
  select(Country, qdate, y_t_pct, cum_new_debt)

# Muted, Okabe-Ito-derived palette (six countries, colour-blind safe)
pal <- c(
  "United States" = "#0072B2",
  "Canada"        = "#D55E00",
  "United Kingdom"= "#117733",
  "Germany"       = "#882255",
  "Switzerland"   = "#56B4E9",
  "Australia"     = "#E69F00"
)

# Linetypes alternate so the figure remains legible in greyscale reprints
ltys <- c(
  "United States" = "solid",
  "Canada"        = "solid",
  "United Kingdom"= "longdash",
  "Germany"       = "longdash",
  "Switzerland"   = "dotdash",
  "Australia"     = "dotdash"
)

# COVID shading: Q1.2020 – Q4.2021 (the trilemma window)
covid_band <- data.frame(
  xmin = as.Date("2020-01-01"),
  xmax = as.Date("2021-12-31")
)

aer_theme <- theme_minimal(base_size = 11, base_family = "serif") +
  theme(
    panel.grid          = element_blank(),
    axis.line           = element_line(colour = "black", linewidth = 0.35),
    axis.ticks          = element_line(colour = "black", linewidth = 0.35),
    axis.ticks.length   = unit(3, "pt"),
    axis.text           = element_text(colour = "black", size = 9),
    axis.title.y        = element_text(size = 10, margin = margin(r = 6)),
    plot.title          = element_text(face = "bold", size = 11,
                                       hjust = 0, margin = margin(b = 6)),
    plot.title.position = "plot",
    plot.margin         = margin(8, 12, 6, 8),
    legend.position     = "bottom",
    legend.title        = element_blank(),
    legend.key.width    = unit(1.1, "cm"),
    legend.key.height   = unit(0.35, "cm"),
    legend.text         = element_text(size = 9),
    legend.margin       = margin(t = 2, b = 0),
    legend.box.margin   = margin(t = -4)
  )

date_breaks <- as.Date(c("2019-01-01","2020-01-01","2021-01-01",
                         "2022-01-01","2023-01-01"))
date_labels <- c("2019","2020","2021","2022","2023")

make_panel <- function(df, yvar, ylab, title) {
  ggplot(df, aes(qdate, .data[[yvar]],
                 colour = Country, linetype = Country)) +
    geom_rect(data = covid_band,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE,
              fill = "grey92", alpha = 0.6) +
    geom_hline(yintercept = 0, colour = "grey50",
               linewidth = 0.3, linetype = "dotted") +
    geom_line(linewidth = 0.65) +
    geom_point(size = 1.2, stroke = 0) +
    scale_colour_manual(values = pal) +
    scale_linetype_manual(values = ltys) +
    scale_x_date(breaks = date_breaks, labels = date_labels,
                 expand = expansion(mult = c(0.01, 0.01))) +
    labs(title = title, x = NULL, y = ylab) +
    aer_theme
}

p_y <- make_panel(dat, "y_t_pct",
                  ylab  = "Percent of HP trend",
                  title = "Panel A.  Output gap") +
  scale_y_continuous(labels = function(x) sprintf("%g", x),
                     breaks = scales::pretty_breaks(6))

p_d <- make_panel(dat, "cum_new_debt",
                  ylab  = "Percentage points of 2019 GDP",
                  title = "Panel B.  Cumulative new debt since 2019Q4") +
  scale_y_continuous(labels = function(x) sprintf("%g", x),
                     breaks = scales::pretty_breaks(6))

caption_text <- str_wrap(paste0(
  "Notes: 2019Q1\u20132022Q4. Shaded area: pandemic window. ",
  "Panel A: HP-filtered output gap (2015\u20132019 trend). ",
  "Panel B: cumulative new gross debt since 2019Q4, pp of 2019 GDP."
), width = 125)

combined <- (p_y + p_d) +
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
out_file <- file.path(out_dir, "outcomes_y_cumdebt_aer_v2.pdf")

ggsave(out_file, combined,
       width = 10, height = 5.4, units = "in", device = cairo_pdf)

cat("Saved:", out_file, "\n")
