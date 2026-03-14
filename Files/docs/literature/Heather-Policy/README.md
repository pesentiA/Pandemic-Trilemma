
# Replication Package for: Which crisis support fiscal measures worked during the COVID-19 shock in Europe?


This replication package accompanies Pappa, Ramos and Vella. (forthcoming). "Which crisis support fiscal measures worked during the COVID-19 shock in Europe?". SERIEs.


## Authors

- Evi Pappa
- Andrey Ramos
- Eugenia Vella


# Data availability and provenance statements
### Statement about rights

The author(s) of the manuscript have legitimate access to and permission to use the data used in this manuscript.

### Summary of availability

- All data **are** publicly available.


### Details on each data source

- The paper constructs an original dataset that categorizes COVID-19 fiscal measures announcements in 12 European Union (EU) countries into 7 distinct spending categories. This dataset can be obtained from the file data.csv and redistribution for the purpose of replication is allowed.
- Data on fatalities and the stringency index comes from Ritchie et al. (2020). 
- Data on the consumer confidence and ESI indices, seasonally adjusted, are taken from the consumer surveys conducted by the Directorate General for Economic and Financial Affairs (Available at: https://ec.europa.eu/info/business-economy-euro/indicators-statistics/economic-databases/business-and-consumer-surveys).
- Data on Inflation, Employment, and GDP growth are from Eurostat. 
- The Europe Brent Spot Price is from Thomson Reuters. 
- Long-run interest rates are from the Monetary and Financial Statistics (MEI) by the OECD.

# Computational requirements

### Software requirements


- 	Stata (code was last run with version 16).
### Memory and runtime requirements

#### Summary

- Approximate time needed to reproduce the analyses on a standard desktop machine: 10-60 minutes.

# Instructions to replicators

- The file data.csv contains the data used in the paper.
- The program replication_file.do reproduces the estimations of Equations 1 and 2 in the paper. In particular, it reproduces the values reported in Tables 1, 2, 3, and 4.


### Variables description

Data are quarterly from 2020Q2 to 2021Q2.

- country_id: Country Identification
- country: Country name
- period: Quarter	
- esi: ESI index	
- gdp_growth: Growth rate or real GDP	
- stringency: Stringency index
- fatalities: Number of fatalities due to COVID	
- icu: ICU occupation	
- assistance_sme: Spending in Assistance to SMEs	
- assistance_sme_diff: First differences of spending in Assistance to SMEs	
- transform: Spending in measures to transform the economy	
- transform_diff: First differences of spending in measures to transform the economy
- other: Other spending	
- spending_pandemic: Spending due to pandemic	
- spending_pandemic_diff: First differences of spending due to pandemic	
- transfers: Spending in transfers to households
- transfers_diff: First differences of spending in transfers to households
- unemployment_transfer: Spending in unemployment transfers
- unemployment_transfer_diff: First differences in spending in unemployment transfers
- universal_help: Spending in universal help
- universal_help_diff: First differences in spending in universal help	
- total_spending: Total spending
- total_spending_diff: First differences in total spending	
- employment: Employment rate	
- employment_growth: Growth rate of unemployment	
- confidence_average: Quarterly average in confidence index
- confidence_average_diff: First differences of conficende index	
- cpi: Consumer Price Index	
- cpi_diff: First differentes of consumer price index													

