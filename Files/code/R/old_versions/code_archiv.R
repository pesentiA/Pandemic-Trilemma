##############heck for over performance in 2021 and 2022#######################


summary(data_2021$Real.GDP.Growth)
summary(data_2022$Real.GDP.Growth)

describe(data_2021$Real.GDP.Growth)
describe(data_2022$Real.GDP.Growth)


#data21 <- data_2021 %>%
#mutate(hrec = ifelse(Real.GDP.Growth > 5.498, 1, 0))



data21 <- data %>%
  group_by(Country) %>%
  mutate(alleKriterienErfuellt = ifelse(any(Real.GDP.Growth > 7.498 & Year == 2021) & 
                                          any(Real.GDP.Growth > 5.268 & Year == 2022), 1, 0)) %>%
  ungroup()

# Verwenden Sie alleKriterienErfuellt anstelle von hrec
hrec1 <- data21 %>%
  filter(alleKriterienErfuellt == 1) %>%
  select(Country)

hrec1out <- unique(hrec1$Country)

print(hrec1out)



# Angenommen hrecOut und treatment_countries sind Ihre beiden Vektoren
gemeinsame_laender = intersect(hrec1out, treatment_countries)

# Ausgabe der gemeinsamen Ländercodes
print(hrec1out)
print(treatment_countries)
print(gemeinsame_laender)


dependent_vars<-c("GDP.per.Capita","Real.GDP.Growth", "Private.Consumption", 
                  "Net.National.Income", "Gross.National.Income","GNI.Growth", 
                  "Net.Lending.Borrowing.Government", "Gov.Cons",
                  "Government.Debt.Ratio", "X", "M","Exports", "Imports", "Integration", "Trade.in.Goods.and.Services", "Trade.Balance",
                  "BOP","GB", "X68_Gross_capital_formation", "GDP.per.Hour.Worked",  "Inflation_Rate", "CPI", "Unemployment.Rate", "Youth.Unemployment.Rate",
                  "Older.Persons.Unemployment.Rate","X45_RnD","X235_Global_Innovation_Index",  "X48_Gross_exp_R.D_GERD","X117_Innovation_linkages", "X118_University_industry_R.D", "X168_Software_spending_GDP",
                  "X58_ICT_access", "X59_ICT_use", "X57_ICT_tech","X185_High_t_exports_t_trade",
                  "X91_Trade._diversification_market_scale", "X2_Human_capital_research_index", "X76_Credit","X82_Investment",
                  "Short.Term.Interest.Rate", "Long.Term.Interest.Rate", "X4_Market_sophistication_index", "X5_Business_sophistication_index",
                  "X24_Education","X71_GDP_unit_energy_use", "X63_General_infrastructure", "X3_Infrastructure_index",  
                  "X9_Political_a_operational_stability","X14_Regulatory_environment", "X15_Regulatory_quality", "X16_Rule_of_law", "X12_Government_effectiveness", "X9_Political_a_operational_stability",
                  "X8_Political_environment", "X1_Institutions_index", "democracy_eiu", "X14_Regulatory_environment","X15_Regulatory_quality", "X65.and.over","X85.and.over")






#####OX Annual Data erstellen----
# Datumskonvertierung (Annahme: Date ist im Format yyyymmdd)
#ox2$Date <- as.Date(as.character(ox2$Date), format = "%Y%m%d")



#ox2an <- ox2 %>%
#mutate(Year = year(Date)) %>%
#group_by(CountryCode, Year) %>%
#summarize(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = 'drop')

#write.csv(combined_data, ("C:/Users/aulis/OneDrive/Studium/Wirtschaftswissenschaften/Master-Thesis/New/Working/combined_data.csv"), row.names = FALSE)

#colnames(ox2an)[colnames(ox2an) == "CountryCode"] <- "Country"


#combined_data <- full_join(data, ox2an, by = c("Country", "Year"))
##########################################################################################################################

####Callaway and Sant'Anna
##with variation in treatment timing->usefull for policies!!!
##https://bcallaway11.github.io/did/
##gives us group-time-average treatment effects
#install.packages("devtools")
#devtools::install_github("bcallaway11/did")


library(did)
library(remotes)




out2 <- att_gt(yname = "Real.GDP.Growth",
               gname = "g",
               idname = "CountryID",
               tname = "Year",
               xformla = ~1,
               data = data,
               est_method = "reg"
)


summary(out2)


ggdid(out2, ylim = c(-.5,.3))



##event-study reports the overall effect of participating in the treatment
es2 <- aggte(out2, type = "dynamic", balance_e = 0)
summary(es2)



##Aggregating group-time average treatment effects->option "aggregated"simple"

ggdid(es2)


group_effects2 <- aggte(out2, type = "simple")
summary(group_effects2)


##with not yet treated-> diff. controlgroup

data$notyettreated<-ifelse(data$strict==0,1,0)

example_attgt_altcontrol <- att_gt(yname = "Real.GDP.Growth",
                                   tname = "Year",
                                   idname = "CountryID",
                                   gname = "g",
                                   xformla = ~1,
                                   data = data,
                                   control_group = "notyettreated"          
)

summary(example_attgt_altcontrol)

es3 <- aggte(example_attgt_altcontrol, type = "dynamic")
summary(es3)


##w##ith alternative estimation method (reg or ipw)

example_attgt_reg <- att_gt(yname = "Real.GDP.Growth",
                            tname = "Year",
                            idname = "CountryID",
                            gname = "g",
                            xformla = ~X,
                            data = data,
                            est_method = "ipw"          
)


summary(example_attgt_reg)

es3 <- aggte(example_attgt_altcontrol, type = "dynamic", balance_e = 0)
summary(es3)


###################################
###Roth and Sant'Anna staggered


library(staggered)

df <- staggered::pj_officer_level_balanced


data$g2 <- ifelse(data$strict == 0, 0, Inf)
#Calculate efficient estimator for the simple weighted average
staggered(df = df, 
          i = "uid",
          t = "period",
          g = "first_trained",
          y = "complaints", 
          estimand = "simple")

eventPlotResults <- staggered(df = df,         
                              i = "uid",
                              t = "period",
                              g = "first_trained",
                              y = "complaints", 
                              estimand = "eventstudy", 
                              eventTime = 0:23)
head(eventPlotResults)

eventPlotResults$ymin_ptwise <- with(eventPlotResults, estimate + 1.96 * se)
eventPlotResults$ymax_ptwise <- with(eventPlotResults, estimate - 1.96 * se)
ggplot(eventPlotResults, aes(x=eventTime, y =estimate)) +
  geom_pointrange(aes(ymin = ymin_ptwise, ymax = ymax_ptwise))+ 
  geom_hline(yintercept =0) +
  xlab("Event Time") + ylab("Estimate") +
  ggtitle("Effect of Procedural Justice Training on Officer Complaints")


library(ggplot2)



staggered(df = data, 
          i = "CountryID",
          t = "time",
          g = "g",
          y = "Real.GDP.Growth", 
          estimand = "simple")

eventPlotResults2 <- staggered(df = data,         
                               i = "CountryID",
                               t = "time",
                               g = "g",
                               y = "Real.GDP.Growth", 
                               estimand = "eventstudy", 
                               eventTime = 0:3)
head(eventPlotResults2)

eventPlotResults2$ymin_ptwise <- with(eventPlotResults2, estimate + 1.96 * se)
eventPlotResults2$ymax_ptwise <- with(eventPlotResults2, estimate - 1.96 * se)
ggplot(eventPlotResults2, aes(x=eventTime, y =estimate)) +
  geom_pointrange(aes(ymin = ymin_ptwise, ymax = ymax_ptwise))+ 
  geom_hline(yintercept =0) +
  xlab("Event Time") + ylab("Estimate") +
  ggtitle("Effect of Strictivness on Real GDP Growth")


###same but other grouping
#Calculate efficient estimator for the cohort weighted average
staggered(df = df, 
          i = "uid",
          t = "period",
          g = "first_trained",
          y = "complaints", 
          estimand = "cohort") 
#>       estimate          se   se_neyman
#> 1 -0.001084689 0.002261011 0.002264876

#Calculate efficient estimator for the calendar weighted average
staggered(df = df, 
          i = "uid",
          t = "period",
          g = "first_trained",
          y = "complaints", 
          estimand = "calendar")
#>      estimate         se   se_neyman
#> 1 -0.00187198 0.00255863 0.002561472







####################################################################################################################
##################################################Synthetic-Control################################################
###################################################################################################################


# Wähle die USA und den Behandlungszeitraum (ab 2020)
##ANPASSEN##
data$treated <- ifelse(data$Country == "USA", 1, 0)

# Definiere die Identifikationsvariablen (Land und Jahr)
id_var <- "Country"
time_var <- "Year"
treatment_var <- "treated"

##ANPASSEN## AB HIEEER
# Definiere die prädiktiven Variablen, die zur Erstellung der synthetischen Kontrolle verwendet werden sollen
predictor_vars <- c( "Integration","Unemployment.Rate","Government.Debt.Ratio", "Government.Spending.per.Capita", "Inflation_Rate","Human.Development.Index","X15.to.64", "X65.and.over", "Imports", "Exports" , "Private.Consumption", "Household.Spending", "Nordamerika")

#GOV.Spending raus evtl.

# Erstelle das dataprep-Objekt
# Prüfe die Namen der Variablen im Dataframe
print(names(data))

data$CountryID <- as.numeric(factor(data$Country))

# Wenn "Country" eine Zeichenfolge ist, erstelle eine numerische ID
#usa_id <- as.numeric(factor("USA", levels = unique(data$Country)))

##ANPASSEN##
# Dann verwende "CountryID" als unit.variable
dataprep_out <- dataprep(
  foo = data,
  predictors = predictor_vars,
  predictors.op = "mean",
  dependent = "Real.GDP.Growth",
  unit.variable = "CountryID",  # Achte darauf, dass diese Spalte im Datensatz vorhanden und numerisch ist
  time.variable = "Year",
  treatment.identifier = 38,  # Numerische ID 
  controls.identifier = setdiff(unique(data$CountryID), 38),  # Alle anderen Länder als Kontrollländer
  time.predictors.prior = c(2017, 2018, 2019),
  special.predictors = NULL,
  time.optimize.ssr = c(2020, 2021, 2022),
  time.plot = c(2017:2022)
)

# Durchführung der synt


# Erstelle das synthetische Kontrollmodell
synth_out <- synth(data.prep.obj = dataprep_out, method = "BFGS")

# Plotte die Ergebnisse
path.plot(synth_out, dataprep_out)



###Information
#Die Ergebnisse aus der Synthetischen Kontrollmethode können manchmal komplex sein, aber hier ist eine grobe Erklärung:

#MSPE (Mean Squared Prediction Error)
#MSPE (LOSS V): 238930816: Dies ist der durchschnittliche quadratische Fehler zwischen der behandelten Einheit (in Ihrem Fall die USA) und der synthetischen Kontrolleinheit. Ein niedrigerer Wert ist besser und zeigt, dass die synthetische Kontrolleinheit eine gute Schätzung der "Gegenfaktischen" Realität der behandelten Einheit ist.
#Gewichtung der Prädiktoren (solution.v)

#solution.v: Diese Zahlen repräsentieren die Gewichtung der Prädiktoren, die Sie in predictor_vars definiert haben. Sie zeigen, wie wichtig jeder Prädiktor ist, um die behandelte Einheit und die Kontrolleinheit zu vergleichen. Zum Beispiel ist der Wert 0.7897401 relativ hoch, was darauf hindeutet, dass dieser Prädiktor besonders wichtig ist.
#Gewichtung der Kontrolleinheiten (solution.w)

#solution.w: Diese sind die Gewichtungen für jede Kontrolleinheit (andere Länder in Ihrem Datensatz). Die Gewichtungen sagen Ihnen, wie viel jede Kontrolleinheit zur Schätzung der synthetischen Kontrolleinheit beiträgt. Zum Beispiel trägt die Kontrolleinheit mit dem Gewicht 0.3884802 stark zur Bildung der synthetischen Kontrolleinheit bei.
#Jetzt sollten Sie im Allgemeinen die Ergebnisse plotten, um visuell zu beurteilen, wie gut die synthetische Kontrolleinheit die behandelte Einheit nachbildet. Idealerweise sollten die Zeitreihen der behandelten Einheit und der synthetischen Kontrolleinheit vor der Behandlung sehr ähnlich sein und nach der Behandlung auseinander gehen, wenn die Behandlung einen Effekt hatte.






################################################################################
#####################SEMI-PARAMETRIC-ESTIMATION->Real.GDP.Growth#################
################################################################################

data_ipw<-data
attach(data_ipw)

#X10.to.14, X15.to.19, X20.to.24, X25.to.29, X30.to.34, X35.to.39, X40.to.44, X45.to.49, X5.to.9, X50.to.54, X55.to.59, 
#controls_ipw<-cbind(Private.Consumption,Nordamerika, Asien_Pazifik, Suedamerika, Unemployment.Rate, Youth.Unemployment.Rate, Older.Persons.Unemployment.Rate, Trade.in.Goods.and.Services, Trade.Balance, Population.Density, Median.Age, Life.Expectancy, Human.Development.Index, Population, total_deaths_per_million, total_deaths, total_cases, total_tests, hosp_patients, hosp_patients_per_million,  Inflation_Rate, EconomicSupportIndex)

controls_ipw<-cbind(Suedamerika,X65.and.over, Unemployment.Rate, Life.Expectancy, Household.Spending, Government.Spending.per.Capita)

pscore.model.1 <- glm(treatment ~ controls_ipw, data = data_ipw, family = binomial(link = "probit"))
summ(pscore.model.1, robust = "HC1")

tidy_pscore <- tidy(pscore.model.1)
tidy_pscore


## generate p-score for each observation --> propensity for being treated
data_ipw$pscore <- pscore.model.1$fitted.values
summary(data_ipw$pscore)

##likelihood, of being treated in the treatment period

## generate p-score for each observation --> propensity for being treated
data_ipw$pscore <- pscore.model.1$fitted.values
summary(data_ipw$pscore)

##likelihood, of being treated in the treatment period

data_ipw$treated <- factor(treatment,
                           levels = c(0,1),
                           label = c("D=0", "D=1"))

#Density plot for the propensity score by treatment status
ggplot(data_ipw, aes(x = pscore, fill = treated)) +
  geom_density(alpha=0.4) +
  scale_fill_grey()+
  theme_grey(base_size = 20) +
  ggtitle("common support assumption")+
  xlim(0, 1)

print("Sample size before imposing common support ")
nrow(data_ipw)

pscore_max0 <-max(data_ipw$pscore[treatment==0])
print("Maximum p-score in control group")
pscore_max0

insample<- ifelse(data_ipw$pscore <= pscore_max0 # trimming for ATET
                  & data_ipw$pscore != 0 # non-treatment not perfectly predicted
                  & data_ipw$pscore != 1 # treatment not perfectly predicted
                  & !is.na(data_ipw$pscore) # p-score non-missing 
                  , 1, 0) # if all conditions true = 1 

print("Sample size after imposing common support ")
sum(insample)

data_ipw <- dplyr::filter(data_ipw, insample ==1)


y1_ipw <- Real.GDP.Growth[insample==1]
treat_ipw <- treatment[insample==1]
x1_ipw <- controls_ipw[insample==1,]

data_ipw$weight[treat_ipw==1] <- 1
data_ipw$weight[treat_ipw==0] <- data_ipw$pscore[treat_ipw==0]/
  (1-data_ipw$pscore)[treat_ipw==0]
weight <- data_ipw$weight

Y1<-sum(y1_ipw*treat_ipw)/sum(treat_ipw)
Y0<-sum(y1_ipw*(1-treat_ipw)*weight)/sum((1-treat_ipw)*weight)
ipw_atet_manual <- Y1-Y0

print("Effect on Real GDP Growth (ATET)")
ipw_atet_manual

###############################################################################
###############################################################################
#BOOTYTRAP for GDP PER CAPITA
###############################################################################
###############################################################################
length(insample)
nrow(controls)

attach(data)
set.seed(1234)

#controls<-cbind(Nordamerika, Asien_Pazifik,  Population, Government.Debt.Ratio, Human.Development.Index, Avg_CCI,  hosp_patients, Inflation_Rate)
#controls<-cbind(Nordamerika, Asien_Pazifik, Suedamerika, Real.GDP.Growth, Real.GDP.PC, GDP.per.Hour.Worked, Government.Debt.Ratio, Government.Spending.per.Capita, Gross.National.Income, Net.National.Income, Household.Spending, Household.Finance, Household.Income, Household.Net.Wealth, Net.Lending.Borrowing.Corporates, Net.Lending.Borrowing.Government, Net.Lending.Borrowing.Households, Unemployment.Rate, Youth.Unemployment.Rate, Older.Persons.Unemployment.Rate, Trade.in.Goods.and.Services, Trade.Balance, Population.Density, Median.Age, Life.Expectancy, Human.Development.Index, Population, total_deaths_per_million, total_deaths, total_cases, total_tests, hosp_patients, hosp_patients_per_million,  Inflation_Rate, EconomicSupportIndex)
controls<-controls_ipw


#controls<-cbind(Nordamerika, Asien_Pazifik, Suedamerika, Unemployment.Rate, total_cases, Household.Spending, Population.Density, Trade.in.Goods.and.Services,  Government.Debt.Ratio, Human.Development.Index, Avg_CCI, Inflation_Rate, Net.National.Income)



ipw_atet <- treatweight(y = Real.GDP.Growth, # take initial data 
                        d = treatment,
                        x = controls,
                        ATET = TRUE, # if = FALSE, estimates ATE (default)
                        trim = (1-pscore_max0), # depends on estimated effec
                        boot = 2600)
ipw_atet




















########################################################################################





#########################################################################################

high_stringency_data_2020 <- data %>%
  filter(Year == 2020 & stringency_index > 45)

# Filter data for the year 2021 with a stringency_index > 48.92
high_stringency_data_2021 <- data %>%
  filter(Year == 2021 & stringency_index > 49)

# Filter data for the year 2022 with a stringency_index > 50 (oder Ihren gewünschten Grenzwert)
high_stringency_data_2022 <- data %>%
  filter(Year == 2022 & stringency_index > 15)

# Combine the data for all three years
high_stringency_data_combined <- rbind(high_stringency_data_2020, high_stringency_data_2021, high_stringency_data_2022)

# Identify countries that meet the criteria for all three years
treatment_countries <- high_stringency_data_combined %>%
  group_by(Country) %>%
  summarise(count_years = n_distinct(Year)) %>%
  filter(count_years == 3) %>%
  pull(Country)

# Create a new column in the original data to indicate if a country is in the treatment group
data <- data %>%
  mutate(TreatmentGroup = ifelse(Country %in% treatment_countries, 1, 0))

# View the first few rows to verify
head(data)


# Definieren der Nutzenfunktion
utility_function <- function(c) {
  return(c^(1/2))
}

# Erstellen eines Datensatzes von c-Werten
c_values <- seq(0, 100, by=1)
U_values <- sapply(c_values, utility_function)

# Erstellen des Plots
plot_data <- data.frame(c_values, U_values)
ggplot(plot_data, aes(x=c_values, y=U_values)) +
  geom_line() +
  labs(title="Nutzengraph", x="Konsum (c)", y="Nutzenniveau (U(c))") +
  theme_minimal()







