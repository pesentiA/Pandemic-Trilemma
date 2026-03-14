
** Import dataset

import excel "C:\Users\anramosr\Desktop\COVID\data_covid_final.xlsx", sheet("Sheet1") firstrow

* Generate time fixed effects

egen  periodnum = group(period)
generate Q1 = (periodnum == 3)
generate Q2 = (periodnum == 4)
generate Q3 = (periodnum == 5)
generate Q4 = (periodnum == 6)
generate Q5 = (periodnum == 7)
generate Q6 = (periodnum == 8)
generate Q7 = (periodnum == 9)
generate Q8 = (periodnum == 10)

*Panel regressions

xtset country_id periodnum

* Prepare the data and generate lags

replace employment_growth = employment_growth*100
replace employment = employment*100
replace unemployment_growth = unemployment_growth*100
by country_id: gen lag_esi_1 = esi[_n-1]
by country_id: gen esi_change = esi - esi[_n-1]


by country_id: gen lag_gdp_growth_1 = gdp_growth[_n-1]
by country_id: gen lag_gdp_growth_2 = lag_gdp_growth_1[_n-1]

by country_id: gen lag_confidence_average_diff_1 = confidence_average_diff[_n-1]
by country_id: gen lag_confidence_average_diff_2 = lag_confidence_average_diff_1[_n-1]

by country_id: gen lag_cpi_diff_1 = cpi_diff[_n-1]
by country_id: gen lag_cpi_diff_2 = lag_cpi_diff_1[_n-1]

by country_id: gen lag_employment_growth_1 = employment_growth[_n-1]
by country_id: gen lag_employment_growth_2 = lag_employment_growth_1[_n-1]

by country_id: gen lag_esi_change_1 = esi_change[_n-1]
by country_id: gen lag_esi_change_2 = lag_esi_change_1[_n-1]

by country_id: gen lag_employment_1 = employment[_n-1]
by country_id: gen lag_employment_2 = lag_employment_1[_n-1]

by country_id: gen lag_total_spending_diff_1= total_spending_diff[_n-1]
by country_id: gen lag_total_spending_diff_2= lag_total_spending_diff_1[_n-1]
by country_id: gen lag_total_spending_diff_3= lag_total_spending_diff_2[_n-1]

by country_id: gen lag_assistance_sme_diff_1= assistance_sme_diff[_n-1]
by country_id: gen lag_assistance_sme_diff_2= lag_assistance_sme_diff_1[_n-1]
by country_id: gen lag_assistance_sme_diff_3= lag_assistance_sme_diff_2[_n-1]

by country_id: gen lag_transform_diff_1= transform_diff[_n-1]
by country_id: gen lag_transform_diff_2= lag_transform_diff_1[_n-1]
by country_id: gen lag_transform_diff_3= lag_transform_diff_2[_n-1]

by country_id: gen lag_spending_pandemic_diff_1= spending_pandemic_diff[_n-1]
by country_id: gen lag_spending_pandemic_diff_2= lag_spending_pandemic_diff_1[_n-1]
by country_id: gen lag_spending_pandemic_diff_3= lag_spending_pandemic_diff_2[_n-1]

by country_id: gen lag_transfers_diff_1= transfers_diff[_n-1]
by country_id: gen lag_transfers_diff_2= lag_transfers_diff_1[_n-1]
by country_id: gen lag_transfers_diff_3= lag_transfers_diff_2[_n-1]

by country_id: gen lag_unemployment_transfer_diff_1= unemployment_transfer_diff[_n-1]
by country_id: gen lag_unemployment_transfer_diff_2= lag_unemployment_transfer_diff_1[_n-1]
by country_id: gen lag_unemployment_transfer_diff_3= lag_unemployment_transfer_diff_2[_n-1]

by country_id: gen lag_universal_help_diff_1= universal_help_diff[_n-1]
by country_id: gen lag_universal_help_diff_2= lag_universal_help_diff_1[_n-1]
by country_id: gen lag_universal_help_diff_3= lag_universal_help_diff_2[_n-1]

* Generate cumulative variables

gen gdp_growth_cum_1 = gdp_growth + lag_gdp_growth_1
gen gdp_growth_cum_2 = gdp_growth + lag_gdp_growth_1 + lag_gdp_growth_2

gen confidence_average_diff_cum_1 = confidence_average_diff + lag_confidence_average_diff_1
gen confidence_average_diff_cum_2 = confidence_average_diff + lag_confidence_average_diff_1 + lag_confidence_average_diff_2

gen cpi_diff_cum_1 = cpi_diff + lag_cpi_diff_1
gen cpi_diff_cum_2 = cpi_diff + lag_cpi_diff_1 + lag_cpi_diff_2

gen employment_cum_1 = employment + lag_employment_1
gen employment_cum_2 = employment + lag_employment_1 + lag_employment_2

gen esi_change_cum_1 = esi_change + lag_esi_change_1
gen esi_change_cum_2 = esi_change + lag_esi_change_1 + lag_esi_change_2

gen employment_growth_cum_1 = employment_growth + lag_employment_growth_1
gen employment_growth_cum_2 = employment_growth + lag_employment_growth_1 + lag_employment_growth_2

gen total_spending_diff_cum_1    = lag_total_spending_diff_1 + lag_total_spending_diff_2 
gen assistance_sme_diff_cum_1    = lag_assistance_sme_diff_1 + lag_assistance_sme_diff_2
gen transform_diff_cum_1         = lag_transform_diff_1 + lag_transform_diff_1
gen spending_pandemic_diff_cum_1 = lag_spending_pandemic_diff_1 + lag_spending_pandemic_diff_2
gen transfers_diff_cum_1         = lag_transfers_diff_1 + lag_transfers_diff_2
gen unemployment_transfer_diff_cum_1 = lag_unemployment_transfer_diff_1 + lag_unemployment_transfer_diff_2
gen universal_help_diff_cum_1    =  lag_universal_help_diff_1 + lag_universal_help_diff_2


gen total_spending_diff_cum_2    = lag_total_spending_diff_1 + lag_total_spending_diff_2 +  lag_total_spending_diff_3
gen assistance_sme_diff_cum_2    = lag_assistance_sme_diff_1 + lag_assistance_sme_diff_2 + lag_assistance_sme_diff_3
gen transform_diff_cum_2         = lag_transform_diff_1 + lag_transform_diff_1 + lag_transform_diff_3
gen spending_pandemic_diff_cum_2 = lag_spending_pandemic_diff_1 + lag_spending_pandemic_diff_2 +  lag_spending_pandemic_diff_3
gen transfers_diff_cum_2         = lag_transfers_diff_1 + lag_transfers_diff_2 + lag_transfers_diff_3
gen unemployment_transfer_diff_cum_2 = lag_unemployment_transfer_diff_1 + lag_unemployment_transfer_diff_2 + lag_unemployment_transfer_diff_3
gen universal_help_diff_cum_2    =  lag_universal_help_diff_1 + lag_universal_help_diff_2 + lag_universal_help_diff_3

by country_id: gen lag_total_spending_1 = total_spending[_n-1]
gen lm1 = log(m1)
gen lbrent = log(brent)


* Drop

drop if period == "2019Q1"
drop if period == "2019Q2"

*** Replication of Equation 1 estimates

** Dependent variable: GDP growth

* Independent variable: total_spending

xtabond gdp_growth lag_total_spending_diff_1 Q4, lags(2) vce(robust)
estat abond

xtabond gdp_growth_cum_1 total_spending_diff_cum_1 Q4, lags(2) vce(robust)
estat abond

xtabond gdp_growth_cum_2 total_spending_diff_cum_2 Q4, lags(2) vce(robust)
estat abond

* Independent variable: assistance_sme

xtabond gdp_growth lag_assistance_sme_diff_1 lag_total_spending_1 Q4, lags(2) vce(robust)
estat abond

xtabond gdp_growth_cum_1 assistance_sme_diff_cum_1 lag_total_spending_1 Q4, lags(2) vce(robust)
estat abond

xtabond gdp_growth_cum_2 assistance_sme_diff_cum_2 lag_total_spending_1 Q4, lags(2) vce(robust)
estat abond

* Independent variable: transform

xtabond gdp_growth lag_transform_diff_1 lag_total_spending_1 Q4, lags(2) vce(robust)
estat abond

xtabond gdp_growth_cum_1 transform_diff_cum_1 lag_total_spending_1 Q4, lags(2) vce(robust)
estat abond

xtabond gdp_growth_cum_2 transform_diff_cum_2 lag_total_spending_1 Q4, lags(2) vce(robust)
estat abond

* Independent variable: spending_pandemic

xtabond gdp_growth lag_spending_pandemic_diff_1 lag_total_spending_1 Q4, lags(2) vce(robust)
estat abond

xtabond gdp_growth_cum_1 spending_pandemic_diff_cum_1 lag_total_spending_1 Q4, lags(2) vce(robust)
estat abond

xtabond gdp_growth_cum_2 spending_pandemic_diff_cum_2 lag_total_spending_1 Q4, lags(2) vce(robust)
estat abond

* Independent variable: unemployment_transfer

xtabond gdp_growth lag_unemployment_transfer_diff_1 lag_total_spending_1 Q4, lags(2) vce(robust)
estat abond

xtabond gdp_growth_cum_1 unemployment_transfer_diff_cum_1 lag_total_spending_1 Q4, lags(2) vce(robust)
estat abond

xtabond gdp_growth_cum_2 unemployment_transfer_diff_cum_2 lag_total_spending_1 Q4, lags(2) vce(robust)
estat abond

* Independent variable: universal_help

xtabond gdp_growth lag_universal_help_diff_1 lag_total_spending_1 Q4, lags(2) vce(robust)
estat abond

xtabond gdp_growth_cum_1 universal_help_diff_cum_1 lag_total_spending_1  Q4, lags(2) vce(robust)
estat abond

xtabond gdp_growth_cum_2 universal_help_diff_cum_2 lag_total_spending_1 Q4, lags(2) vce(robust)
estat abond

** Dependent variable: Consumer confidence change

* Independent variable: total_spending

xtabond confidence_average_diff lag_total_spending_diff_1 Q4 fatalities, lags(2) vce(robust)
estat abond

xtabond confidence_average_diff_cum_1 total_spending_diff_cum_1 Q4 fatalities, lags(2) vce(robust)
estat abond

xtabond confidence_average_diff_cum_2 total_spending_diff_cum_2 Q4 fatalities, lags(2) vce(robust)
estat abond

* Independent variable: assistance_sme

xtabond confidence_average_diff lag_assistance_sme_diff_1 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

xtabond confidence_average_diff_cum_1 assistance_sme_diff_cum_1 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

xtabond confidence_average_diff_cum_2 assistance_sme_diff_cum_2 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

* Independent variable: transform

xtabond confidence_average_diff lag_transform_diff_1 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

xtabond confidence_average_diff_cum_1 transform_diff_cum_1 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

xtabond confidence_average_diff_cum_2 transform_diff_cum_2 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

* Independent variable: spending_pandemic

xtabond confidence_average_diff lag_spending_pandemic_diff_1 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

xtabond confidence_average_diff_cum_1 spending_pandemic_diff_cum_1 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

xtabond confidence_average_diff_cum_2 spending_pandemic_diff_cum_2 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

* Independent variable: transfers

xtabond confidence_average_diff lag_transfers_diff_1 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

xtabond confidence_average_diff_cum_1 transfers_diff_cum_1 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

xtabond confidence_average_diff_cum_2 transfers_diff_cum_2 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

*Independent variable: unemployment_transfer

xtabond confidence_average_diff lag_unemployment_transfer_diff_1 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

xtabond confidence_average_diff_cum_1 unemployment_transfer_diff_cum_1 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

xtabond confidence_average_diff_cum_2 unemployment_transfer_diff_cum_2 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

* Independent variable: universal_help

xtabond confidence_average_diff lag_universal_help_diff_1 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

xtabond confidence_average_diff_cum_1 universal_help_diff_cum_1 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

xtabond confidence_average_diff_cum_2 universal_help_diff_cum_2 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

** Dependent variable: CPI (Arellano-Bond)

* Independent variable: total_spending

xtabond cpi_diff lag_total_spending_diff_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

xtabond cpi_diff_cum_1 total_spending_diff_cum_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

xtabond cpi_diff_cum_2 total_spending_diff_cum_2 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

* Independent variable: assistance_sme

xtabond cpi_diff lag_assistance_sme_diff_1 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

xtabond cpi_diff_cum_1 assistance_sme_diff_cum_1 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

xtabond cpi_diff_cum_2 assistance_sme_diff_cum_2 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond


* Independent variable: transform

xtabond cpi_diff lag_transform_diff_1 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

xtabond cpi_diff_cum_1 transform_diff_cum_1 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

xtabond cpi_diff_cum_2 transform_diff_cum_2 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond


* Independent variable: spending_pandemic

xtabond cpi_diff lag_spending_pandemic_diff_1 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

xtabond cpi_diff_cum_1 spending_pandemic_diff_cum_1 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

xtabond cpi_diff_cum_2 spending_pandemic_diff_cum_2 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond


* Independent variable: transfers

xtabond cpi_diff lag_transfers_diff_1 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

xtabond cpi_diff_cum_1 transfers_diff_cum_1 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

xtabond cpi_diff_cum_2 transfers_diff_cum_2 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond
xtabond cpi_diff_cum_2 transfers_diff_cum_2 lag_total_spending_1 li_ir lbrent Q4, lags(2)
estat sargan


* Independent variable: unemployment_transfer

xtabond cpi_diff lag_unemployment_transfer_diff_1 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

xtabond cpi_diff_cum_1 unemployment_transfer_diff_cum_1 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

xtabond cpi_diff_cum_2 unemployment_transfer_diff_cum_2 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond


* Independent variable: universal_help

xtabond cpi_diff lag_universal_help_diff_1 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

xtabond cpi_diff_cum_1 universal_help_diff_cum_1 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

xtabond cpi_diff_cum_2 universal_help_diff_cum_2 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

** Dependent variable: Employment

* Independent variable: total_spending

xtabond employment lag_total_spending_diff_1 stringency Q4, lags(2) vce(robust)
estat abond

xtabond employment_cum_1 total_spending_diff_cum_1 stringency Q4, lags(2) vce(robust)
estat abond

xtabond employment_cum_2 total_spending_diff_cum_2 stringency Q4, lags(2) vce(robust)
estat abond

* Independent variable: assistance_sme

xtabond employment lag_assistance_sme_diff_1 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

xtabond employment_cum_1 assistance_sme_diff_cum_1 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

xtabond employment_cum_2 assistance_sme_diff_cum_2 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

* Independent variable: transform

xtabond employment lag_transform_diff_1 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

xtabond employment_cum_1 transform_diff_cum_1 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

xtabond employment_cum_2 transform_diff_cum_2 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

** Independent variable: spending_pandemic

xtabond employment lag_spending_pandemic_diff_1 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

xtabond employment_cum_1 spending_pandemic_diff_cum_1 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

xtabond employment_cum_2 spending_pandemic_diff_cum_2 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

* Independent variable: transfers

xtabond employment lag_transfers_diff_1 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

xtabond employment_cum_1 transfers_diff_cum_1 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

xtabond employment_cum_2 transfers_diff_cum_2 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

* Independent variable: unemployment_transfer

xtabond employment lag_unemployment_transfer_diff_1 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

xtabond employment_cum_1 unemployment_transfer_diff_cum_1 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

xtabond employment_cum_2 unemployment_transfer_diff_cum_2 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

* Independent variable: universal_help

xtabond employment lag_universal_help_diff_1 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

xtabond employment_cum_1 universal_help_diff_cum_1 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

xtabond employment_cum_2 universal_help_diff_cum_2 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

** Dependent variable: esi_change (Arellano-Bond)

* Independent variable: total_spending

xtabond esi_change lag_total_spending_diff_1 stringency, lags(2) vce(robust)
estat abond

xtabond esi_change_cum_1 total_spending_diff_cum_1 stringency, lags(2) vce(robust)
estat abond

xtabond esi_change_cum_2 total_spending_diff_cum_2 stringency, lags(2) vce(robust)
estat abond

* Independent variable: assistance_sme

xtabond esi_change lag_assistance_sme_diff_1 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond

xtabond esi_change_cum_1 assistance_sme_diff_cum_1 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond

xtabond esi_change_cum_2 assistance_sme_diff_cum_2 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond

* Independent variable: transform

xtabond esi_change lag_transform_diff_1 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond

xtabond esi_change_cum_1 transform_diff_cum_1 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond

xtabond esi_change_cum_2 transform_diff_cum_2 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond

* Independent variable: spending_pandemic

xtabond esi_change lag_spending_pandemic_diff_1 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond

xtabond esi_change_cum_1 spending_pandemic_diff_cum_1 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond

xtabond esi_change_cum_2 spending_pandemic_diff_cum_2 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond

** Independent variable: transfers

xtabond esi_change lag_transfers_diff_1 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond

xtabond esi_change_cum_1 transfers_diff_cum_1 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond

xtabond esi_change_cum_2 transfers_diff_cum_2 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond

* Independent variable: unemployment_transfer

xtabond esi_change lag_unemployment_transfer_diff_1 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond

xtabond esi_change_cum_1 unemployment_transfer_diff_cum_1 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond

xtabond esi_change_cum_2 unemployment_transfer_diff_cum_2 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond

* Independent variable: universal_help

xtabond esi_change lag_universal_help_diff_1 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

xtabond esi_change_cum_1 universal_help_diff_cum_1 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond

xtabond esi_change_cum_2 universal_help_diff_cum_2 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond


*** Replication of Equation 2 estimates

** Dependent variable: GDP growth

* Independent variable: total_spending

xtabond gdp_growth lag_total_spending_diff_1 Q4, lags(2) vce(robust)
estat abond

xtabond gdp_growth_cum_1 total_spending_diff_cum_1 Q4, lags(2) vce(robust)
estat abond

xtabond gdp_growth_cum_2 total_spending_diff_cum_2 Q4, lags(2) vce(robust)
estat abond

* Independent variable: assistance_sme

xtabond gdp_growth lag_assistance_sme_diff_1 lag_total_spending_1 Q4, lags(2) vce(robust)
estat abond

xtabond gdp_growth_cum_1 lag_assistance_sme_diff_2 lag_total_spending_1 Q4, lags(2) vce(robust)
estat abond

xtabond gdp_growth_cum_2 lag_assistance_sme_diff_3 lag_total_spending_1 Q4, lags(2) vce(robust)
estat abond

* Independent variable: transform

xtabond gdp_growth lag_transform_diff_1 lag_total_spending_1 Q4, lags(2) vce(robust)
estat abond

xtabond gdp_growth_cum_1 lag_transform_diff_2 lag_total_spending_1 Q4, lags(2) vce(robust)
estat abond

xtabond gdp_growth_cum_2 lag_transform_diff_3 lag_total_spending_1 Q4, lags(2) vce(robust)
estat abond

* Independent variable: spending_pandemic

xtabond gdp_growth lag_spending_pandemic_diff_1 lag_total_spending_1 Q4, lags(2) vce(robust)
estat abond

xtabond gdp_growth_cum_1 lag_spending_pandemic_diff_2 lag_total_spending_1 Q4, lags(2) vce(robust)
estat abond

xtabond gdp_growth_cum_2 lag_spending_pandemic_diff_3 lag_total_spending_1 Q4, lags(2) vce(robust)
estat abond

* Independent variable: unemployment_transfer

xtabond gdp_growth lag_unemployment_transfer_diff_1 lag_total_spending_1 Q4, lags(2) vce(robust)
estat abond

xtabond gdp_growth_cum_1 lag_unemployment_transfer_diff_2 lag_total_spending_1 Q4, lags(2) vce(robust)
estat abond

xtabond gdp_growth_cum_2 lag_unemployment_transfer_diff_3 lag_total_spending_1 Q4, lags(2) vce(robust)
estat abond

* Independent variable: universal_help

xtabond gdp_growth lag_universal_help_diff_1 lag_total_spending_1 Q4, lags(2) vce(robust)
estat abond

xtabond gdp_growth_cum_1  lag_universal_help_diff_2 lag_total_spending_1  Q4, lags(2) vce(robust)
estat abond

xtabond gdp_growth_cum_2 lag_universal_help_diff_3 lag_total_spending_1 Q4, lags(2) vce(robust)
estat abond

** Dependent variable: Consumer confidence change

* Independent variable: total_spending

xtabond confidence_average_diff lag_total_spending_diff_1 Q4 fatalities, lags(2) vce(robust)
estat abond

xtabond confidence_average_diff_cum_1 lag_total_spending_diff_2 Q4 fatalities, lags(2) vce(robust)
estat abond

xtabond confidence_average_diff_cum_2 lag_total_spending_diff_3 Q4 fatalities, lags(2) vce(robust)
estat abond

* Independent variable: assistance_sme

xtabond confidence_average_diff lag_assistance_sme_diff_1 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

xtabond confidence_average_diff_cum_1 lag_assistance_sme_diff_2 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

xtabond confidence_average_diff_cum_2 lag_assistance_sme_diff_3 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

* Independent variable: transform

xtabond confidence_average_diff lag_transform_diff_1 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

xtabond confidence_average_diff_cum_1 lag_transform_diff_2 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

xtabond confidence_average_diff_cum_2 lag_transform_diff_3 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

* Independent variable: spending_pandemic

xtabond confidence_average_diff lag_spending_pandemic_diff_1 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

xtabond confidence_average_diff_cum_1 lag_spending_pandemic_diff_2 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

xtabond confidence_average_diff_cum_2 lag_spending_pandemic_diff_3 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

* Independent variable: transfers

xtabond confidence_average_diff lag_transfers_diff_1 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

xtabond confidence_average_diff_cum_1 lag_transfers_diff_2 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

xtabond confidence_average_diff_cum_2 lag_transfers_diff_3 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

*Independent variable: unemployment_transfer

xtabond confidence_average_diff lag_unemployment_transfer_diff_1 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

xtabond confidence_average_diff_cum_1 lag_unemployment_transfer_diff_2 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

xtabond confidence_average_diff_cum_2 lag_unemployment_transfer_diff_3 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

* Independent variable: universal_help

xtabond confidence_average_diff lag_universal_help_diff_1 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

xtabond confidence_average_diff_cum_1 lag_universal_help_diff_2 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

xtabond confidence_average_diff_cum_2 lag_universal_help_diff_3 lag_total_spending_1 Q4 fatalities, lags(2) vce(robust)
estat abond

** Dependent variable: CPI (Arellano-Bond)

* Independent variable: total_spending

xtabond cpi_diff lag_total_spending_diff_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

xtabond cpi_diff_cum_1 lag_total_spending_diff_2 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

xtabond cpi_diff_cum_2 lag_total_spending_diff_3 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

* Independent variable: assistance_sme

xtabond cpi_diff lag_assistance_sme_diff_1 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

xtabond cpi_diff_cum_1 lag_assistance_sme_diff_2 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

xtabond cpi_diff_cum_2 lag_assistance_sme_diff_3 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond


* Independent variable: transform

xtabond cpi_diff lag_transform_diff_1 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

xtabond cpi_diff_cum_1 lag_transform_diff_2 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

xtabond cpi_diff_cum_2 lag_transform_diff_3 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond


* Independent variable: spending_pandemic

xtabond cpi_diff lag_spending_pandemic_diff_1 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

xtabond cpi_diff_cum_1 lag_spending_pandemic_diff_2 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

xtabond cpi_diff_cum_2 lag_spending_pandemic_diff_3 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond


* Independent variable: transfers

xtabond cpi_diff lag_transfers_diff_1 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

xtabond cpi_diff_cum_1 lag_transfers_diff_2 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

xtabond cpi_diff_cum_2 lag_transfers_diff_3 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

* Independent variable: unemployment_transfer

xtabond cpi_diff lag_unemployment_transfer_diff_1 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

xtabond cpi_diff_cum_1 lag_unemployment_transfer_diff_2 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

xtabond cpi_diff_cum_2 lag_unemployment_transfer_diff_3 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

* Independent variable: universal_help

xtabond cpi_diff lag_universal_help_diff_1 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

xtabond cpi_diff_cum_1 lag_universal_help_diff_2 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

xtabond cpi_diff_cum_2 lag_universal_help_diff_3 lag_total_spending_1 li_ir lbrent Q4, lags(2) vce(robust)
estat abond

** Dependent variable: Employment

* Independent variable: total_spending

xtabond employment lag_total_spending_diff_1 stringency Q4, lags(2) vce(robust)
estat abond

xtabond employment_growth_cum_1 lag_total_spending_diff_2 stringency Q4, lags(2) vce(robust)
estat abond

xtabond employment_growth_cum_2 lag_total_spending_diff_3 stringency Q4, lags(2) vce(robust)
estat abond

* Independent variable: assistance_sme

xtabond employment lag_assistance_sme_diff_1 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

xtabond employment_growth_cum_1 lag_assistance_sme_diff_2 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

xtabond employment_growth_cum_2 lag_assistance_sme_diff_3 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

* Independent variable: transform

xtabond employment lag_transform_diff_1 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

xtabond employment_growth_cum_1 lag_transform_diff_1 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

xtabond employment_growth_cum_2 lag_transform_diff_1 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

** Independent variable: spending_pandemic

xtabond employment lag_spending_pandemic_diff_1 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

xtabond employment_growth_cum_1 lag_spending_pandemic_diff_2 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

xtabond employment_growth_cum_2 lag_spending_pandemic_diff_3 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

* Independent variable: transfers

xtabond employment lag_transfers_diff_1 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

xtabond employment_growth_cum_1 lag_transfers_diff_2 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

xtabond employment_growth_cum_2 lag_transfers_diff_3 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

* Independent variable: unemployment_transfer

xtabond employment lag_unemployment_transfer_diff_1 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

xtabond employment_growth_cum_1 lag_unemployment_transfer_diff_2 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

xtabond employment_growth_cum_2 lag_unemployment_transfer_diff_3 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

* Independent variable: universal_help

xtabond employment lag_universal_help_diff_1 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

xtabond employment_growth_cum_1  lag_universal_help_diff_2 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

xtabond employment_growth_cum_1  lag_universal_help_diff_3 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

** Dependent variable: esi_change (Arellano-Bond)

* Independent variable: total_spending

xtabond esi_change lag_total_spending_diff_1 stringency, lags(2) vce(robust)
estat abond

xtabond esi_change_cum_1 lag_total_spending_diff_2 stringency, lags(2) vce(robust)
estat abond

xtabond esi_change_cum_2 lag_total_spending_diff_3 stringency, lags(2) vce(robust)
estat abond

* Independent variable: assistance_sme

xtabond esi_change lag_assistance_sme_diff_1 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond

xtabond esi_change_cum_1 lag_assistance_sme_diff_2 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond

xtabond esi_change_cum_2 lag_assistance_sme_diff_3 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond

* Independent variable: transform

xtabond esi_change lag_transform_diff_1 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond

xtabond esi_change_cum_1 lag_transform_diff_2 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond

xtabond esi_change_cum_2 lag_transform_diff_3 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond

* Independent variable: spending_pandemic

xtabond esi_change lag_spending_pandemic_diff_1 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond

xtabond esi_change_cum_1 lag_spending_pandemic_diff_2 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond

xtabond esi_change_cum_2 lag_spending_pandemic_diff_3 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond

** Independent variable: transfers

xtabond esi_change lag_transfers_diff_1 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond

xtabond esi_change_cum_1 lag_transfers_diff_2 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond

xtabond esi_change_cum_2 lag_transfers_diff_3 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond

* Independent variable: unemployment_transfer

xtabond esi_change lag_unemployment_transfer_diff_1 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond

xtabond esi_change_cum_1 lag_unemployment_transfer_diff_2 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond

xtabond esi_change_cum_2 lag_unemployment_transfer_diff_3 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond

* Independent variable: universal_help

xtabond esi_change lag_universal_help_diff_1 lag_total_spending_1 stringency Q4, lags(2) vce(robust)
estat abond

xtabond esi_change_cum_1 lag_universal_help_diff_2 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond

xtabond esi_change_cum_2 lag_universal_help_diff_3 lag_total_spending_1 stringency, lags(2) vce(robust)
estat abond