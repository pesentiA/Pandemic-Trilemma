rm(list=ls())

install.packages("rnaturalearthdata")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("rnaturalearth")) install.packages("rnaturalearth")
if (!require("dplyr")) install.packages("dplyr")

library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)


library(ggplot2)
library(rnaturalearth)
library(dplyr)

# Länder in den jeweiligen Gruppen (Verwenden Sie die korrekten ISO-Codes)
treatment_group <- c("AUS", "BEL", "CAN", "CHL", "COL",
                     "CRI", "DEU", "ESP", "FRA", "GBR",
                     "GRC", "IRL", "ISR", "ITA", "MEX",
                     "NLD", "PRT", "TUR", "USA")

control_group <- c("AUT", "CHE", "CZE", "DNK", "EST",
                   "FIN", "HUN", "ISL", "JPN", "KOR",
                   "LTU", "LUX", "LVA", "NOR", "NZL",
                   "POL", "SVK", "SVN", "SWE")

# Holen Sie sich die Weltkarte
world <- ne_countries(scale = "medium", returnclass = "sf")

# Entfernen Sie die Antarktis
world <- world[world$admin != "Antarctica",]

# Erstellen Sie eine neue Spalte für die Gruppenzugehörigkeit
world$group <- ifelse(world$iso_a3 %in% treatment_group, 'Treatment Group',
                      ifelse(world$iso_a3 %in% control_group, 'Control Group', 'Other'))



ggplot(data = world) +
  geom_sf(aes(fill = group), color = "white") +
  scale_fill_manual(values = c("Treatment Group" = "red", "Control Group" = "blue"),
                    labels = c("Control", "Treatment")) +
  theme_void() +
  theme(legend.position = "bottom") +  # Legende unten platzieren
  labs(fill = "")  # Entfernt den Titel der Legende
