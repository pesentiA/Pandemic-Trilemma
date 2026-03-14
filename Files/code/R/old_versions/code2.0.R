
# Libraries
library(tidyverse)
library(stats)

# Load the data
data <- read.csv("final_oecd_covid_data.csv")

# Define the treatment group based on stringency index and year
median_value <- median(subset(data, Year > 2019)$stringency_index, na.rm = TRUE)
data$strict <- ifelse(data$stringency_index > median_value & data$Year > 2019, 1, 0)

# Define the treatment period
data$covid <- ifelse(data$Year >= 2020 & data$Year <= 2022, 1, 0)

# Define the interaction term for treatment
data$treatment <- data$strict * data$covid

# Fit the DiD model
model <- lm(`GDP per Capita` ~ strict + covid + treatment, data=data)
summary(model)
