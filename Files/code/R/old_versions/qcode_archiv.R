
rm(list=ls())

packages_vector <- c( "did2s","haven", "dplyr",  "sandwich",  "jtools", "data.table",
                      "fBasics","gtools","rnaturalearth", "rnaturalearthdata", "foreign","gt", "Synth","gridExtra", "fixest","huxtable", "xtable", "foreign", "stargazer", "AER", "causalweight", "tidyr","expss","stringr","pscore","AER","ggplot2","haven","lubridate" ,"knitr",
                      "kableExtra", "psych", "pastecs","purrr","magrittr","did","pte","remote", "did2s")


#install.packages(packages_vector)
lapply(packages_vector, require, character.only = TRUE) 


#library(dplyr)
# List loaded packages 
(.packages())

# Set options
options(max.print = 9999, scipen = 999, na.print = "")

dir <- "C:/Users/aulis/OneDrive/Studium/Wirtschaftswissenschaften/Master-Thesis/New"

setwd(dir)

set.seed(123456)
###Daten einlesen
data<-read.csv("C:/Users/aulis/OneDrive/Studium/Wirtschaftswissenschaften/Master-Thesis/New/Working/milk_rethinking.csv", header=TRUE, sep=",")

attach(data)

lm1<-lm(kcal.per.g~neocortex.perc+mass)
summary(lm1)  

plot(lm1)  

install.packages("ggfortify")
library(ggfortify)

autoplot(lm1)



###Ausreiser sind wegen IRL-> Speak about it

#qdata <- filter(qdata, Country != "IRL")




##Plot economic support
table(subset(qdata, TimeIndex >= 12 & TimeIndex <= 24)$qstrict)

common_trends <- qdata %>% 
  filter(TimeIndex >= 12 & TimeIndex <= 24) %>%
  group_by(TimeIndex, qstrict) %>% 
  summarise(mean = mean(E3_Fiscal.measures, na.rm = TRUE))


# Stellen Sie sicher, dass qstrict als Faktor vor der Plot-Erstellung definiert ist
common_trends$qstrict <- as.factor(common_trends$qstrict)

# Korrekter Aufruf von ggplot mit TimeIndex als x-Achse
ggplot(data = common_trends, 
       aes(x = TimeIndex, y = mean, 
           group = qstrict, color = qstrict)) + 
  geom_line() +
  scale_color_manual(values = c("0" = "blue", "1" = "red")) +
  geom_vline(xintercept = 13, linetype="dashed") +
  scale_x_continuous(breaks = seq(12, 24, by = 1)) +
  theme_bw(base_size = 20)

