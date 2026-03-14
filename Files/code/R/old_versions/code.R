####################################################################
#   PHASE 1: LOADING DATA PACKAGES AND CLEANING
####################################################################

###Install Packages and load Data ----

rm(list=ls())

packages_vector <- c( "did2s","haven", "dplyr",  "sandwich",  "jtools", "data.table",
                      "fBasics","gtools","rnaturalearth", "rnaturalearthdata", "foreign","gt", "Synth","gridExtra", "fixest","huxtable", "xtable", "foreign", "stargazer", 
                      "AER", "causalweight", "tidyr","expss","stringr","pscore","AER","ggplot2","haven","lubridate" ,"knitr",
                      "kableExtra", "psych", "ggrepel", "pastecs","sf","fmsb", "scales", "treemapify", "treemap", 
                      "viridis", "purrr","magrittr","did","pte","remote", "did2s", "gridExtra", "jsonlite", "conflicted")

#install.packages("pte")

#install.packages(packages_vector)
lapply(packages_vector, require, character.only = TRUE) 

# List loaded packages 
(.packages())

# Set options
options(max.print = 9999, scipen = 999, na.print = "")

dir <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working"
setwd(dir)

##setseeds for bootsrtap
set.seed(123456)

# oder explizit Präferenz setzen:
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")


###read data
data<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/ydata.csv", header=TRUE, sep=",")

cadata<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/cadata.csv", header=TRUE, sep=",")

hhdata<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/hh.indicators2.csv", header=TRUE, sep=",")
fxdata<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/fxrate2.csv", header=TRUE, sep=",")

fxdata <- fxdata %>% dplyr::select(-Variable)
##fxdata f?r costa rica nicht vorhanden

tradedata<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/trade4.csv", header=TRUE, sep=",")
NAAGdata<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/NAAG.csv", header=TRUE, sep=",")
govdata<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/gov3_wide.csv", header=TRUE, sep=",")
govdata <- govdata[govdata$Country != "CHN", ]
prdata<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/prices2.csv", header=TRUE, sep=",")
edata<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/external.csv", header=TRUE, sep=",")
irdata<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/ir2.csv", header=TRUE, sep=",")
ladata<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/labour2.csv", header=TRUE, sep=",")
hhprdata<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/hh_prices2.csv", header=TRUE, sep=",")



##define all OECD countries

oecd_laender <- c(
  "Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia", 
  "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", 
  "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan", 
  "Korea", "Latvia", "Lithuania", "Luxembourg", "Mexico", "Netherlands", 
  "New Zealand", "Norway", "Poland", "Portugal", "Slovakia", "Slovenia", 
  "Spain", "Sweden", "Switzerland", "Turkey","Costa Rica", "United Kingdom", "United States"
)


#in ISO-3 Code

oecd_countries <- c("AUS", "AUT", "BEL", "CAN", "CHL","CRI", "COL", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN", "KOR", "LVA", "LTU", "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA")


##CHL,CRI,COL,TUR,MEX fehlen fast alle Daten
#Welche Variablen kann ich brauchen??

keys <- c("Country","Year")

df_combined <- cadata %>%
  left_join(data, by = keys) %>%
  arrange(across(all_of(keys)))


zu_entfernen <- c("Euro area (20 countries)","European Union (27 countries from 01/02/2020)","G7", "OECD", "OECD Europe", "United States - Mexico - Canada Agreement (USMCA)")

df_combined <- df_combined %>%
  filter(! Country %in% zu_entfernen)

data<-df_combined


data <- NAAGdata %>%
  right_join(data, by = keys) %>%
  arrange(across(all_of(keys)))

data <- fxdata %>%
  right_join(data, by = keys) %>%
  arrange(across(all_of(keys)))

data <- hhdata %>%
  right_join(data, by = keys) %>%
  arrange(across(all_of(keys)))

data <- tradedata %>%
  right_join(data, by = keys) %>%
  arrange(across(all_of(keys)))

data <- prdata %>%
  right_join(data, by = keys) %>%
  arrange(across(all_of(keys)))

data <- edata %>%
  right_join(data, by = keys) %>%
  arrange(across(all_of(keys)))

data <- irdata %>%
  right_join(data, by = keys) %>%
  arrange(across(all_of(keys)))

data <- ladata %>%
  right_join(data, by = keys) %>%
  arrange(across(all_of(keys)))

data <- hhprdata %>%
  right_join(data, by = keys) %>%
  arrange(across(all_of(keys)))
######
#Map Time Variable
jahr_werte <- 2015:2024
zeit_werte <- c(-4, -3, -2, -1, 0, 1, 2, 3, 4, 5)
mapping <- setNames(zeit_werte, jahr_werte)

data <- data %>%
 mutate(
   Year = as.character(Year),
    time = mapping[Year]
  ) %>%
  arrange(Country, Year)

jahr_werte <- 2015:2024
zeit_werte <- c(-4, -3, -2, -1, 0, 1, 2, 3, 4, 5)
mapping <- setNames(zeit_werte, jahr_werte)

data <- data %>%
  mutate(
    Year = as.character(Year),
    rel_year = mapping[Year]
  ) %>%
  arrange(Country, Year)


cro(data$time)

##add democracy zu den neuen Jahren
# Fetch the data
df <- read.csv("https://ourworldindata.org/grapher/democracy-index-eiu.csv?v=1&csvType=full&useColumnShortNames=true")

df$Entity<-NULL
df$owid_region<-NULL
data$democracy_eiu<-NULL

colnames(df) <- c("Country","Year","democracy_eiu")

oecd_countries <- c("AUS", "AUT", "BEL", "CAN", "CHL","CRI", "COL", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN", "KOR", "LVA", "LTU", "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA")

df <- df %>%
  filter(Country %in% oecd_countries)

df <- subset(df, Year >= 2015)

data <- merge(df, data, by = c("Country", "Year"))


df2<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/wwd2.csv", header=TRUE, sep=",")

laender <- unique(df2$Country)

# Leere Kopie von df2 (nur Struktur, keine Daten)
leere_zeile <- df2[0, ]

# Neue Zeilen erzeugen: 1 Zeile pro Land, Jahr = 2024, Rest = NA
neue_zeilen <- leere_zeile[rep(1, length(laender)), ]
neue_zeilen$Country <- laender
neue_zeilen$Year <- 2024

# Anh?ngen und optional sortieren
df2 <- rbind(df2, neue_zeilen)
df2 <- df2[order(df2$Country, df2$Year), ]


data <- merge(df2, data, by = c("Country", "Year"))

rm(leere_zeile, neue_zeilen, df, df2)


write.csv(data, ("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/output r/nadata.csv"), row.names = FALSE)

data<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/output r/nadata.csv", header=TRUE, sep=",")


###OECD Control Data

oecddata<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/oecddata.csv", header=TRUE, sep=",")

unique(oecddata$ccodealp)

oecddata <- oecddata %>%
  filter(ccodealp %in% oecd_countries)

oecddata <- subset(oecddata, year >= 2015)

oecddata <- oecddata %>% select(where(~ any(!is.na(.))))


oecddata <- oecddata %>% select(where(~ length(unique(na.omit(.))) > 1))
oecddata <- oecddata %>% select(where(~ length(unique(na.omit(.))) > 1))
oecddata <- oecddata %>% 
  select(where(~ sum(!is.na(.[oecddata$year >= 2022])) > 0))



oecddata <- oecddata %>% select(
  -cname_qog, -cname, -ccodecow, -ccodealp_year, -cname_year, -ccode, -ccode_qog, 
  -dev_altv2, -dev_othv2, -dev_regv2, -dev_tv2, -fao_luagr, -fao_luagrara, -fao_luagrcrop, 
  -fao_luagrirreqcrop, -fao_luagrorg, -fao_lucrop, -fao_luforest, -fao_luforplant, 
  -fao_luforreg, -ictd_taxnresinsc, -ictd_taxres, -ideaesd_esf, -ideaesd_esnl, -ideaesd_lsde, 
  -ideaesd_lsvm, -ideaesd_tiers, -ideavt_legcv, -ideavt_legvt, -idf_dia, -idf_dia2030, 
  -idf_dia2045, -idf_ifg2030, -idf_ifg2045, -idf_igt, -idf_igt2030, -idf_igt2045, 
  -ipu_l_s, -ipu_l_sw, -ipu_l_w, -nrmi_chi, -opri_oaeece, -opri_oaepe,
  -opri_oeals, -opri_oeace, -opri_oeapsnt, -opri_oeaus, -opri_tdurce, -opri_tdurls,
  -opri_tdurpsnt, -opri_tilsef, -opri_tilset, -opri_tipef, -opri_tipet, -opri_tiprepef,
  -opri_tiprepet, -opri_tisef, -opri_tiset, -opri_tiusef, -opri_tiuset, -opri_yearschoolf, 
  -opri_yearschoolm, -opri_yearschoolt, -opri_tdurece, -opri_tdurused, 
  -ucdp_type2, -ucdp_type3, -ucdp_type4,
  -une_girlglsf, -une_girlglsm, -une_girlglst, -warc_acov, -warc_agi35, -warc_agi40,
  -warc_agi4160, -warc_agi61, -warc_fem35, -warc_fem40, -warc_fem61, -warc_leadage,
  -warc_leadgen, -warc_meanage, -warc_medianage, -warc_min35, -warc_min40, -warc_min4160,
  -warc_min61, -warc_wmin, -wdi_incsh10h, -wdi_incsh10l, -wdi_incsh202, -wdi_incsh203, -wdi_incsh204,
  -wdi_incsh20h, -wdi_incsh20l, -wdi_povgap215, -wdi_povgap365, -yri_agi30, -yri_agi35,
  -yri_agi40, -yri_agi4160, -yri_agi61, -yri_fem30, -yri_fem35, -yri_fem40, -yri_fem4160, -yri_fem61,
  -yri_meanage, -yri_medianage, -yri_mp30, -yri_mp35, -yri_mp40, -yri_mp4160, -yri_mp61, 
  -bicc_gmi, -bicc_hw, -bicc_milexp, -bicc_milper,
  -gti_gti, -ht_colonial, -ht_region, -egov_egov, -egov_epar, -egov_hci, -egov_osi, -egov_tii, 
  -wdi_armexp, -wdi_armimp, -sai_statehiste0, -sai_statehiste01, -sai_statehiste1, -sai_statehisten0, 
  -sai_statehisten01, -sai_statehisten1,
  -wdi_pop, -wpp_fertrate, -wpp_fertrate_2030, -wpp_fertrate_2050, -wpp_medianage,
  -wpp_medianage_2030, -wpp_medianage_2050, -wpp_pop_2030, -wpp_pop_2050, -wpp_popden_2030, -wpp_popden_2050,
  -wpp_sexratio_2030, -wpp_sexratio_2050, -br_chpar, -br_cw, -br_coup, -br_dem, -br_elect, 
  -br_elecyear, -br_fcoup,
  -br_mon, -br_pres, -br_pvote, -cbi_dec, -cbi_dir, -cbi_inc, -cbi_ref, -fe_cultdiv, -fe_etfra, -fe_plural, 
  -fh_pr, -gol_enppo, -gol_inst, -gol_mt, -gol_nos, -gol_pr, -gol_preel, -gol_upseat, -gol_uptier, 
  -wpp_pop, -cbi_cbiu, -cbi_cbiw, -cbi_cceo, -cbi_cll, -cbi_cobj, -cbi_cpol, -cbi_reg, 
  -cbie_board, -cbie_boardref, -cbie_cbiconstitution, -cbie_cwne, -cbie_finances, -cbie_financesref,
  -cbie_indexref, -cbie_lending, -cbie_lendingref, -cbie_lvau, -cbie_obj, -cbie_objref, -cbie_policyref,
  -cbie_report, -fh_aor, -fh_cl, -fh_ep, -fh_feb, -fh_fog, -fh_pair, -fh_ppp, -fh_rol, -fh_status, 
  -gol_est, -gol_est_spec, -ictd_revres, -wdi_belmedinc, -wdi_debt, -wgov_minmil, -wgov_totmil,
  -cbie_gmt, -ef_crop, -ef_fg, -ef_for, -ef_gl, -fi_ftradeint, -fi_ftradeint_pd, 
  -fi_legprop, -fi_legprop_pd, -fi_reg, -fi_sog, -fi_reg_pd, -fi_sm, -fi_sm_pd, -fi_sog_pd,
  -gain_cap, -gain_econ, -gain_ecos, -gain_exp, -gain_food, -gain_hab, -gain_inf, -gain_sens, -gain_vuln, 
  -gain_vulngdp, -gain_wat, -gggi_eas, -gggi_hss, -gggi_pos, -gggi_pes, -gol_adm, -gol_dist, 
  -gol_enep1, -gol_enepo, -gol_enpp, -gol_enpp1,
  -gpi_mil, -ictd_grants, -ictd_nontax, -ictd_revexsc, -ictd_revinsc, -ictd_taxinsc, -mad_gdppc,
  -spi_bn, -spi_fob, -spi_opp, -ti_cpi_max, -ti_cpi_min, -ti_se, -vdem_academ, -vdem_gender, 
  -wbgi_cce, -wbgi_ccn, -wbgi_ccs, -wbgi_gee,
  -wbgi_gen, -wbgi_ges, -wbgi_pve, -wbgi_pvn, -wbgi_pvs, -wbgi_rle, -wbgi_rln, -wbgi_rls, 
  -wbgi_rqe, -wbgi_rqn, -wbgi_rqs, -wbgi_vae, -wbgi_van, -wbgi_vas, 
  -wdi_acel, -wdi_acelr, -wdi_acelu, -wdi_birth, -wdi_co2, -wdi_eduprp, -wdi_eduprs,
  -wdi_empagrf, -wdi_empagrm, -wdi_empf, -wdi_empindf, -wdi_empindm, -wdi_empprfilo, -wdi_empprfne, 
  -wdi_empprmilo, -wdi_empprmne, -wdi_empprne, -wdi_emppryfilo, -wdi_emppryfne, -wdi_empprymne, 
  -wdi_empserf, -wdi_empserm, -wdi_expmil, -wdi_expmilge, -wdi_export,
  -wdi_fertility, -wdi_gdpcapcur, -wdi_gdppppcur, -wdi_gerpf, -wdi_gerp, -wdi_gerpm, -wdi_gers, 
  -wdi_gersf, -wdi_gersm, -wdi_gert, -wdi_gertf,
  -wdi_gertm, -wdi_lfpeduaf, -wdi_lfpeduam, -wdi_lfpedubf, -wdi_lfpedubm, -wdi_lfpeduif, -wdi_lfpeduim,
  -wdi_lfpf, -wdi_lfpfilo15, -wdi_lfpyfilo, -wdi_lfpfne15, -wdi_lfpyfne, -wdi_lfpmilo15, -wdi_lfpmne15,
  -wdi_lfpne15, -wdi_lfpedua, -wdi_lfpedub, -wdi_lfpedui, -wdi_lfpilo15,
  -wdi_lfpr, -wdi_lfprf, -wdi_lfprm, -wdi_lfpyilo, -wdi_lfpymilo, -wdi_lfpymne, -wdi_lfpyne, 
  -wdi_lifexp, -wdi_lifexpf, -wdi_lifexpm,
  -wdi_mortinf, -wdi_mortinff, -wdi_mortinfm, -wdi_mortnn, -wdi_mortu5, -wdi_mortu5f, -wdi_mortu5m, 
  -wdi_refori, -wdi_sempf, -wdi_semp, -wdi_sva2015, 
  -wdi_unempeduaf, -wdi_unempeduam, -wdi_unempedubf, -wdi_unempedubm, -wdi_unempeduif, -wdi_unempeduim, 
  -wdi_unempfilo, -wdi_unempfne, -wdi_unempilo, -wdi_unempyfilo, -wdi_unempyfne, -wdi_unempyne, 
  -wdi_wip, -wdi_wombuslawi, -wgov_leadexp, -wgov_minage, -wgov_minfem, -wgov_min, -wgov_minten,
  -wgov_mret, -wgov_totfem, -wgov_tot, -wgov_totage, -wgov_totten, -wgov_tret, -who_dwtot, 
  -who_infmortf, -who_infmortm, -who_infmortt,
  -who_sanittot, -vdem_corr, -vdem_delibdem, -vdem_dl_delib, -vdem_edcomp_thick, -vdem_egal, -vdem_egaldem, 
  -vdem_elvotbuy, -vdem_exbribe, -vdem_excrptps, -vdem_execorr,
  -vdem_exembez, -vdem_exthftps, -vdem_gcrrpt, -vdem_jucorrdc, -vdem_libdem, -vdem_liberal, 
  -vdem_mecorrpt, -vdem_partip, -vdem_partipdem, -vdem_polyarchy, -vdem_pubcorr,
  -rd_outw, -wdi_popf, -wdi_gdpcappppcon2021, -wdi_gdppppcon2021, -wdi_gnicon2015, -wdi_gdpcappppcur, 
  -wdi_gnipppcon2021, -cbie_policy, -fi_index_pd,
  -wdi_gdpcapcon2015, -wdi_gniatlcur, -wdi_gnicapatlcur, -wdi_gnicapcon2015, -wdi_gnicappppcon2021, 
  -wdi_gnicappppcur, -wdi_gnicur, -wdi_migration, -wdi_refasy,
  -wpp_netmig, -wpp_sexratio, -gted_rfusd, -rd_inw)

                                
  
oecddata<-oecddata%>%rename(Year=year)
oecddata<-oecddata%>%rename(Country=ccodealp)
names(oecddata)

oecddata <- oecddata %>%
  distinct(Country, Year, .keep_all = TRUE)

cat("c(", paste0('"', names(oecddata), '"', collapse = ", "), ")")

data <- merge(oecddata, data, by = c("Country", "Year"))



###World Bank Data
wdi<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/wdi.csv", header=TRUE, sep=",")

wdi <- wdi %>%
  filter(Country %in% oecd_countries)


wdi <- select(wdi, -c(bank_nonperforming_loans_to_total_gross_loans_percent, bank_capital_to_assets_ratio_percent))

data <- merge(wdi, data, by = c("Country", "Year"))


############INTERNAL CLEANING#############################################################
##get ride of duplicate entries
data$Real.GDP.Growth <- data$RGDP
data$Private.Consumption <- data$PRC
data$Gov.Cons <- data$GOC
data$Gross.Cap.form <- data$GFCF
data$X <- data$RX
data$M <- data$RM

population_2019 <- data %>%
  filter(Year == 2019) %>%
  select(Country, Total.population)

data %>%
  filter(Year == 2019) %>%
  group_by(TreatmentGroup) %>%
  summarise(
    durchschnittliche_bevölkerung = mean(Total.population, na.rm = TRUE),
    summe_bevölkerung = sum(Total.population, na.rm = TRUE),
    anzahl_länder = n_distinct(Country)
  )



##get rid of unused variables


drop_cols <- c(
  "GINI","Nurses","Doctors","Median.Age",
  "Life.Expectancy","stringency_index","Nordamerika",
  "Asien_Pazifik","Suedamerika","Measurement.Unit","X40.to.44","X45.to.49",
  "X0.to.4","X5.to.9","X10.to.14","X15.to.19","X20.to.24","X25.to.29","X30.to.34",
  "X35.to.39","X50.and.over","X50.to.54","X55.to.59","X60.to.64","X65.to.69",
  "Old.age.dependency.ratio..65.and.over.20.64.","Young.age.dependency.ratio...20.20.64.",
  "X15.to.64","X20.to.64","X70.to.74","X75.to.79","X80.to.84",
  "Share.of.15.to.24...youth","Share.of.under.15...children",
  "Share.of.65.and.over...elderly","Share.of.15.to.64...working.age","Under.20",
  "Total.dependency.ratio....20...65.....20.64.","X95_Applied.tariff.rate","Net.NI",
  "Household.Income","Household.Net.Wealth","Household.Finance","Immediate.Interest.Rate",
  "Narrow.Money.M1","Broad.Money.M3","Exchange.Rate..in.USD.","PH_Spending",
  "X83_Ease_protecting_minority_investors","X22_Ease_resolving_insolvency",
  "X77_Ease_of_getting_credit","X21_Ease_starting_business",
  "X39_Graduates_in_science_engineering","X135_High_t_imports_t_trade",
  "X27_Government_funding_pupil","X191_ICT_s_exports_t_trade","X103_Knowledge_intensive_empl",
  "X25_Expenditure_education","X115_Females_employed_advanced_degrees","X61_Online_e_participation",
  "X166_New_businesses","X36_Tertiary_education","X37_Tertiary_enrolment_gross",
  "X41_Tertiary_inbound_mobility","X145_Research_talent","X46_Researchers_FTE",
  "X60_Government_online_service","X140_ICT_s_imports_t_trade","X107_GERD_performed_business",
  "X110_GERD_financed_business","X50_QS_top_3_universities","HH.Cons.",
  "Net.Lending.Borrowing.Households","Net.Lending.Borrowing.Corporates","HH.Spending","Act.Cons.",
  "GTR_GDP","GTE_GDP","GTE_POP","Household.Spending","X20_Business_environment",
  "RGDP","PRC","GOC","GFCF","RX","RM","Unnamed..29","Compensation.of.employees.received.by.households",
  "Consumer.price.index","Consumer.price.index.with.fixed.interest.rates",
  "Consumer.price.index..harmonised","Core.inflation.index","Core.inflation.index..harmonised",
  "Adjustment.for.the.change.in.net.equity.of.households.in.pension.fund.reserves",
  "Current.disbursements.of.households.and.non.profit.institutions.serving.households",
  "Employees.and.self.employed.contributions.to.social.security",
  "General.government.fixed.capital.formation..deflator..based.on.appropriation.account",
  "Direct.taxes.on.households",
  "Gross.current.receipts.of.households.and.non.profit.institutions.serving.households",
  "Gross.disposable.income.of.household.and.non.profit.institutions.serving.households",
  "Gross.national.product..market.prices..deflator",
  "Gross.saving.of.households.and.non.profit.institutions.serving.households",
  "Gross.saving.ratio.of.households.and.non.profit.institutions.serving.households",
  "Gross.self.employment.income.received.by.households",
  "Mainland.gross.domestic.product..market.prices..deflator",
  "Real.gross.disposable.income.of.households.and.non.profit.institutions.serving.households",
  "Net.disposable.income.of.households.and.non.profit.institutions.serving.households",
  "Net.saving.of.households.and.non.profit.institutions.serving.households",
  "Net.saving.ratio.of.households.and.non.profit.institutions.serving.households",
  "Offshore.gross.domestic.product..market.prices..deflator",
  "Net.self.employment.income.received.by.households",
  "Employment.country.specific","General.government.employment","Core.inflation","Headline.inflation",
  "Harmonised.core.inflation","Harmonised.headline.inflation",
  "Total.employers.social.contributions",
  "Net.current.receipts.of.households.and.non.profit.institutions.serving.households",
  "Real.net.disposable.income.of.households.and.non.profit.institutions.serving.households",
  "Other.current.income.payable.by.households.and.non.profit.institutions.serving.households",
  "Other.current.income.received.by.households.and.non.profit.institutions.serving.households",
  "Total.employment..national.accounts.basis.",
  "Cyclically.adjusted.total.direct.taxes.received.by.general.government",
  "Cyclically.adjusted.current.disbursements.of.general.government.as.a.percentage.of.potential.GDP",
  "Private.final.consumption.expenditure..nominal.value..appropriation.account",
  "Wage.rate.total.economy","Unit.labour.cost.in.total.economy",
  "Cyclically.adjusted.current.disbursements.of.general.government.excluding.gross.interest.payments..as.a.percentage.of.potential.GDP",
  "Net.one.offs.of.general.government","Net.one.offs.of.general.government.as.a.percentage.of.potential.GDP",
  "Productive.capital.stock.scrapping.rate","Productive.capital.stock..volume",
  "Underlying.general.government.net.lending",
  "Underlying.general.government.net.lending.as.a.percentage.of.potential.GDP",
  "GR.real.DI.pc.hh","I.real.DI.pc.hh","S.CT.hh","S.Debt.hh","S.LUN","S.NFW.hh","S.SR.hh", "Debt","Dependent.employment.total.economy",
  "Total.employment..labour.force.survey.basis.","Hours.worked.per.worker.total.economy",
  "Total.self.employed","Working.age.population.age.15.74",
  "Cyclically.adjusted.direct.taxes.on.households",
  "Cyclically.adjusted.direct.taxes.on.business",
  "Cyclically.adjusted.general.government.net.lending.as.a.percentage.of.potential.GDP",
  "Cyclically.adjusted.general.government.net.lending",
  "Trend.employment.coefficient",
  "Underlying.general.government.primary.balance.as.a.percentage.of.potential.GDP",
  "Underlying.general.government.primary.balance",
  "Private.final.consumption.expenditure.deflator.growth",
  "Export.market.for.goods.and.services.volume.in.USD",
  "Goods.and.services.trade.volume.in.USD",
  "I.CC","I.real.FC.pc.hh","Gov.cons.ca1","Gross.fc.ca1","HH.cons.ca1","m.ca1","x.ca1","real.GDP.ca",
  "X71_GDP_unit_energy_use","X73_Environmental_performance","X235_Global_Innovation_Index",
  "X12_Government_effectiveness","X68_Gross_capital_formation","X2_Human_capital_research_index",
  "X3_Infrastructure_index","X1_Institutions_index","X192_Intangible_assets","X82_Investment",
  "X129_Knowledge_absorption","X6_Knowledge_technology_outputs_index","X102_Knowledge_workers",
  "X224_Online_creativity","X9_Political_a_operational_stability","X8_Political_environment",
  "X14_Regulatory_environment","X15_Regulatory_quality","X45_RnD",
  "X16_Rule_of_law","X168_Software_spending_GDP","X118_University_industry_R.D",
  "Total.population","I.real.GDP.pc","Compensation.rate.total.economy", "GNI.pc.growth", "gpi_conf", "gpi_gpi", "gpi_ss", "wdi_busden",
  "wdi_chexppgdp", "wdi_dgovhexp", "gted_rfgdp", "gted_rftax", "gpcr_growth", "wdi_dprivhexp", "wdi_expedu", "wdi_expeduge", "wdi_gini", "wdi_gnicapr",
  "wdi_mortf", "wdi_mortm", "wdi_ophexp", "wdi_ptef", "wdi_ptem", "wdi_svapg", "Compensation.of.employees..total.economy", "Wages..total.economy",
  "Labour.force", "Cyclically.adjusted.current.receipts.of.general.government.as.a.percentage.of.potential.GDP", "Potential.employment.of.total.economy",
  "Productive.capital.stock..volume..annual.average", "Ratio.of.potential.to.actual.real.GDP.of.the.total.economy","Trend.EmpRate.15.74", 
  "Trend.working.age.population..age.15.74", "Trend.labour.efficiency","Productive.capital.stock.volume.annual.average.growth", "Productive.capital.stock.volume.growth",
  "Price.of.commodity.exports", "Price.of.commodity.imports", "nan", "Long.Term.Interest.Rate", "Short.Term.Interest.Rate", "avg_stringency_index_x", "avg_stringency_index_y", "wert",
  "avg_stringency_index.y.y", "bashare", "ashare",   "above.usdbn", "spending.health.usbn", "other.spending.usbn", "acc.deff.gdp", 
  "off.budget.usdbn", "below.usbn", "guarantees.usbn", "quasi.fiscal.usbn", 
  "above.gdp", "spending.health.gdp", "other.spending.gdp", "acc.deff.gdp.1", 
  "off.budget.gdp", "below.gdp", "guarantees.gdp", "quasi.fiscal.gdp", 
  "ratio.ab.gdp", "ratio.ab.usbn", "ratio.ao.gdp", "ratio.ao.usbn", 
  "total.spending.usbn", "total.spending.gdp", "bshare", "ashare", 
  "POLCONV_VDEM", "POLCONIII_VDEM", "legfralower", "legfraupper", 
  "ab_us", "on_us", "health_us", "nhealth_us", "acc_us", "of_us", 
  "be_us", "gu_us", "qf_us", "ab_per", "on_per", "ab_ppp", "on_ppp", 
  "health_per", "health_ppp", "nhealth_per", "nhealth_ppp", "acc_per", 
  "acc_ppp", "of_per", "of_ppp", "be_per", "be_ppp", "gu_per", "gu_ppp", 
  "qf_per", "qf_ppp", "real_gdp_per", "total_netspend_usd", "total_moving_usd", 
  "share_onw0_mo", "share_on_mo", "share_onw0_t", "share_on_t", 
  "share_nhealth_mo", "share_nhealth_t", "share_health_mo", "share_health_t", 
  "share_acc_mo", "share_acc_t", "share_nhealth_on", "share_health_on", 
  "share_of_mo", "share_of_t", "share_be_mo", "share_be_t", "share_gu_mo", 
  "share_gu_t", "share_be_of", "share_gu_of", "total_mo_per", "avg_stringency_index.x.x", "avg_stringency_index.y", "gpcr_eci", "Unemployment.Rate",
  "Youth.Unemployment.Rate", "Older.Persons.Unemployment.Rate", "wdi_emp", "wdi_empagr", "wdi_empind", "wdi_empm", "wdi_empprilo", "wdi_emppryilo", 
  "wdi_empprymilo", "wdi_emppryne", "wdi_empser", "wdi_sempm", "Labour.part.15_74", "Unemployment.level", "whr_hap", "cbi_difreg", "total_deaths",
  "wdi_gdpagr", "wdi_gdpcapgr", "wdi_gdpgr", "wdi_gdpgr", "wdi_gdpind", "wdi_sva", "ef_ef", "cbie_index", 
  "Long.term.interest.rate.on.government.bonds", "gol_enep", "wdi_gnicapgr", "wdi_gnipppcur", "wdi_import",
  "wdi_trade", "wdi_tradeserv", "wdi_unempyilo", "wdi_unempymilo", "dr_eg",
  "dr_pg", "dr_sg", "gain_gaingdp", "gain_read", "wdi_death", "Gross.capital.formation..deflator", "Trend.EmpRate.15.74", "Real.GDP.pc.growth", 
  "Private.consumption.real.growth", "wdi_pte")


# robustes Droppen
data <- dplyr::select(data, -any_of(drop_cols))

##Ab GDP.per.Capita neu gel?scht 05.08.2025

cat("c(", paste0('"', names(data), '"', collapse = ", "), ")")
##clean out loaded and merged datasets

###all OECD Data
data <- data %>%
  rename(
    Deflator.GDP=Gross.domestic.product..market.prices..deflator,
    Deflator.tgross.cf=Gross.total.fixed.capital.formation..deflator,
    Deflator.M=Imports.of.goods.and.services..deflator..national.accounts.basis.,
    Deflator.pfc=Private.final.consumption.expenditure..deflator,
    PPP.nat.c=Purchasing.power.parity..national.currency.per.USD,
    Deflator.tdc=Total.domestic.expenditure..deflator,
    Labour.part.15_74=Labour.force.participation.rate.as.a.percentage.of.population.aged.15.74,
    Emprat.gap=Employment.rate.gap..percentage.points,
    Emprate.supply.15_74=Employment.rate..supply.definition..as.a.percentage.of.population.aged.15.74,
    Trend.EmpRate.15.74=Trend.employment.rate..as.a.percentage.of.population.aged.15.74,
    Export.performance.growth=Export.performance.for.goods.and.services.volume.growth,
    Export.volume.growth.NAB=Exports.of.goods.and.services.volume.growth.nab,
    Import.volume.growth.NAB=Imports.of.goods.and.services.volume.growth.nab,
    Private.consumption.growth=Private.final.consumption.expenditure.volume.growth,
    Prim.Income.Balance.pct.GDP=Balance.of.primary.income.as.a.percent.of.GDP,
    Sec.Income.Balance.pct.GDP=Balance.of.secondary.income.as.a.percent.of.GDP,
    Comp.Export.Prices=Competitors.price.of.goods.and.services.exports,
    Trade.Contribution.USD.Real=Contribution.to.world.trade.growth.goods.and.services.in.USD.OECD.reference.year.prices,
    Comp.Index.CPI=Indicator.of.competitiveness.based.on.relative.consumer.prices,
    Comp.Index.ULC=Indicator.of.competitiveness.based.on.relative.unit.labour.costs.in.total.economy,
    Trade.Share.World.USD=Share.of.country.s.trade.in.world.trade.volume.in.USD.at.OECD.reference.year.prices,
    Real.GDP.Growth.NAAG=real.GDP.NAAG,
    Gov.Con2=Gov.cons.ca,
    Gross.fc=Gross.fc.ca,
    Real.GDP.Growth2=real.GDP.a)

##moving variables
## moving variables
data <- data %>%
  select(
    # Metadaten
    Country, Year, time,
    
    # BIP, Wachstum, Output-Gap
    Real.GDP.Growth.NAAG, Real.GDP.Growth2,
    GNI.2010,
    Output.gap.as.a.percentage.of.potential.GDP,
    Potential.output.volume.growth,
    
    # Konsum, Investitionen, Sparen
    HH.cons.ca, Gov.Con2, Gov.cons.growth, Gov.cons.share,
    Gross.capital.formation.growth, Gross.capital.formation.share,
    Gross.fixed.capital.growth, Gross.fixed.capital.share,
    Gross.domestic.savings.share, Gross.savings.share, Gross.savings.gni,
    Private.consumption.growth,
    Total.domestic.expenditure.volume.growth,
    Gross.national.expenditure.share, Net.lending.borrowing.share, PPP.nat.c,
    
    # Handel
    x.ca, m.ca,
    Export.performance.growth, Export.volume.growth.NAB, Import.volume.growth.NAB,
    Net.exports.of.goods.and.services.as.a.percent.of.GDP,
    Terms.of.trade.goods.and.services, Trade.Share.World.USD,
    Comp.Export.Prices, Trade.Contribution.USD.Real,
    Current.account.balance.as.a.percentage.of.GDP,
    
    # Arbeitsmarkt und Erwerbst?tigkeit
    Emprat.gap, Emprate.supply.15_74,
    Total.employment.growth, Labour.part.15_74, Labour.productivity.total.economy,
    Unemployment.rate, S.UR, 
    
    # Preise, Deflatoren, Wettbewerbsf?higkeit
    Deflator.GDP,  Deflator.M, Deflator.pfc,
    Deflator.tgross.cf,
    Deflator.tdc,
    Comp.Index.CPI, Comp.Index.ULC,
    FX.usd,
    
    # Verteilungsindikatoren und Wohlfahrt
    top_top10_income_share, top_top1_income_share, undp_hdi,
    gain_gov, gain_soc, gain_heal, gain_readgdp,
    
    # Governance, Institutionen, Demokratie
    control.of.corruption, gov.effect., rule.of.law, pol.stab.viole, regulatory.quality,
    voice.accountability, democracy_eiu, ti_cpi,
    gggi_ggi,icrg_qog, spi_ospi,
    
    # Bildung, Gesundheit, Bev?lkerung
    PopulationVaccinated,
    wdi_pop14, wdi_pop1564, wdi_pop65, wdi_popgr, wdi_popurb, wdi_popurbagr,
    wdi_poprul, wdi_poprulgr, wpp_popden, wdi_agedr,
    
    # Arbeitsmarktdetails (WDI)
    wdi_unempedua, wdi_unempedub, wdi_unempedui, wdi_unempmilo, wdi_unempmne,
    wdi_unempne, wdi_unempymne,
    
    # Digitalisierung & Infrastruktur
    wdi_broadb, wdi_internet, wdi_mobile, wdi_tele,
    wdi_interrev, wdi_interexp,
    
    # Zentralbank, Zinsen & Fiskalindikatoren
    Central.bank.key.interest.rate, Short.term.interest.rate,
    Prim.Income.Balance.pct.GDP, Sec.Income.Balance.pct.GDP,
    
    # Kapitalstock & Produktivit?t
    Gross.fc,
    
    # COVID-19 & Pandemieindikatoren
    total_cases,total_deaths_per_million,
    hosp_patients, hosp_patients_per_million, total_tests, reproduction_rate,
    StringencyIndex_Average, ContainmentHealthIndex_Average,
    EconomicSupportIndex, GovernmentResponseIndex_Average,
    
    # Alle restlichen Variablen
    everything()
  )

##checking for N/A
names(which(sapply(data, anyNA)))

anyNA(data$wdi_gdpcapgr)

##dropping last variables
drop_cols <- c("Labour.part.15_74", "Trend.EmpRate.15.74", "Deflator.tgross.cf", "gggi_ggi", "wdi_unempedua", "wdi_unempedub", "wdi_unempedui", "wdi_unempmilo")
  
data <- dplyr::select(data, -any_of(drop_cols))


##possibility to write a new csv of merged dataset
#write.csv(data, ("C:/Users/aulis/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/output r/nadata.csv"), row.names = FALSE)


##Loading oxford data
ox<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Master-Thesis/New/Working/Oxford/Oxcnat.csv", header=TRUE, sep=",")


##filter out all non-OECD countries
unique_countries_in_data <- unique(ox$CountryCode)


oecd_countries_in_data <- unique_countries_in_data[unique_countries_in_data %in% oecd_countries]


num_oecd_countries_in_data <- length(oecd_countries_in_data)


num_unique_countries_in_data <- length(unique_countries_in_data)


all_countries_are_oecd <- num_oecd_countries_in_data == num_unique_countries_in_data


num_oecd_countries_in_data
num_unique_countries_in_data
all_countries_are_oecd


ox2 <- subset(ox, CountryCode %in% oecd_countries)

##Check for varianbles with only N/A in it
na_counts_per_column <- colSums(is.na(ox2))

total_rows <- nrow(ox2)

columns_with_only_na <- names(na_counts_per_column[na_counts_per_column == total_rows])

columns_with_only_na

ox$RegionName<-NULL
ox$RegionCode<-NULL


###Loading oxford data with notes
oxnotes<-read.csv("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Master-Thesis/New/Working/Oxford/Oxcnotes.csv", header=TRUE, sep=",")


##filter out all non-OECD countries (same as before)
unique_countries_in_data <- unique(oxnotes$CountryCode)

oecd_countries_in_data <- unique_countries_in_data[unique_countries_in_data %in% oecd_countries]

num_oecd_countries_in_data <- length(oecd_countries_in_data)

num_unique_countries_in_data <- length(unique_countries_in_data)

all_countries_are_oecd <- num_oecd_countries_in_data == num_unique_countries_in_data

num_oecd_countries_in_data
num_unique_countries_in_data
all_countries_are_oecd

oxnotes2 <- subset(oxnotes, CountryCode %in% oecd_countries)

na_counts_per_column <- colSums(is.na(oxnotes2))

total_rows <- nrow(oxnotes2)

columns_with_only_na <- names(na_counts_per_column[na_counts_per_column == total_rows])

columns_with_only_na


oxnotes2$RegionName<-NULL
oxnotes2$RegionCode<-NULL

##Switzerland
che_data <- subset(data, Country == "CHE")


summary_2017_2019 <- summary(subset(che_data, Year >= 2017 & Year <= 2019)$Government.Debt.Ratio)
print("Summary for 2017-2019:")
print(summary_2017_2019)

# Erstelle Summarys f?r die Jahre 2020-2022
summary_2020_2022 <- summary(subset(che_data, Year >= 2020 & Year <= 2022)$Government.Debt.Ratio)
print("Summary for 2020-2022:")
print(summary_2020_2022)


##USA
usa_data <- subset(data, Country == "USA")

# Erstelle Summarys f?r die Jahre 2017-2019
summary_2017_2019 <- summary(subset(usa_data, Year >= 2017 & Year <= 2019)$Government.Debt.Ratio)
print("Summary for 2017-2019:")
print(summary_2017_2019)

# Erstelle Summarys f?r die Jahre 2020-2022
summary_2020_2022 <- summary(subset(usa_data, Year >= 2020 & Year <= 2022)$Government.Debt.Ratio)
print("Summary for 2020-2022:")
print(summary_2020_2022)



##Cleaning of the main data----

attach(data)

data$Household.Spending<-data$HH.Spending

##Add values
data <- data %>% 
  mutate(Government.Debt.Ratio = if_else(Country == "CRI" & Year == 2017, 61.8, Government.Debt.Ratio))
data <- data %>% 
  mutate(Government.Debt.Ratio = if_else(Country == "CRI" & Year == 2018, 63.83, Government.Debt.Ratio))
data <- data %>% 
  mutate(Government.Debt.Ratio = if_else(Country == "CRI" & Year == 2019, 72.88, Government.Debt.Ratio))
data <- data %>% 
  mutate(Government.Debt.Ratio = if_else(Country == "CRI" & Year == 2020, 78.37, Government.Debt.Ratio))
data <- data %>% 
  mutate(Government.Debt.Ratio = if_else(Country == "CRI" & Year == 2021, 78.64, Government.Debt.Ratio))
data <- data %>% 
  mutate(Government.Debt.Ratio = if_else(Country == "CRI" & Year == 2022, 82.36, Government.Debt.Ratio))

years_to_change <- c(2017, 2018, 2019, 2020, 2021, 2022)
new_values <- c(76.6, 75.2, 69.5, 71.7, 65.4, 45.19)  

# ?ndern der Werte im Dataframe
years_to_change <- c(2017, 2018, 2019, 2020, 2021, 2022)

data <- data %>% 
  mutate(Government.Debt.Ratio = case_when(
    Country == "IRL" & Year == 2017 ~ 76.6,
    Country == "IRL" & Year == 2018 ~ 75.2,
    Country == "IRL" & Year == 2019 ~ 69.5,
    Country == "IRL" & Year == 2020 ~ 71.7,
    Country == "IRL" & Year == 2021 ~ 65.4,
    Country == "IRL" & Year == 2022 ~ 45.19,
    TRUE ~ Government.Debt.Ratio  
  ))


data <- data %>% 
  mutate(Government.Debt.Ratio = case_when(
    Country == "ISL" & Year == 2017 ~ 71.742,
    Country == "ISL" & Year == 2018 ~ 63.247,
    Country == "ISL" & Year == 2019 ~ 66.576,
    Country == "ISL" & Year == 2020 ~ 77.788,
    Country == "ISL" & Year == 2021 ~ 75.575,
    Country == "ISL" & Year == 2022 ~ 68.726,
    TRUE ~ Government.Debt.Ratio 
  ))


data <- data %>% 
  mutate(Gross.National.Income = case_when(
    Country == "ISL" & Year == 2017 ~ 60230,
    Country == "ISL" & Year == 2018 ~ 67770,
    Country == "ISL" & Year == 2019 ~ 72980,
    Country == "ISL" & Year == 2020 ~ 62110,
    Country == "ISL" & Year == 2021 ~ 63140,
    Country == "ISL" & Year == 2022 ~ 68220,
    TRUE ~ Gross.National.Income 
  ))

data <- data %>% 
  mutate(Net.National.Income = case_when(
    Country == "ISL" & Year == 2017 ~ 58170.19205,
    Country == "ISL" & Year == 2018 ~ 58338.79314,
    Country == "ISL" & Year == 2019 ~ 57512.779,
    Country == "ISL" & Year == 2020 ~ 48047.37832,
    Country == "ISL" & Year == 2021 ~ 54204.42,
    Country == "ISL" & Year == 2022 ~ 58560,
    TRUE ~ Net.National.Income  
  ))

data <- data %>% 
  mutate(Net.National.Income = case_when(
    Country == "COL" & Year == 2017 ~ 5365.906,
    Country == "COL" & Year == 2018 ~ 5561.63896,
    Country == "COL" & Year == 2019 ~ 5306.77,
    Country == "COL" & Year == 2020 ~ 4464.96341,
    Country == "COL" & Year == 2021 ~ 5094.625,
    Country == "COL" & Year == 2022 ~ 5960,
    TRUE ~ Net.National.Income  
  ))


data <- data %>% 
  mutate(Inflation_Rate = case_when(
    Country == "CRI" & Year == 2022 ~ 8.27,
    TRUE ~ Inflation_Rate  
  ))
data <- data %>% 
  mutate(RGDPPC = case_when(
    Country == "CRI" & Year == 2017 ~ 12389,
    Country == "CRI" & Year == 2018 ~ 12570,
    Country == "CRI" & Year == 2019 ~ 12736,
    Country == "CRI" & Year == 2020 ~ 12064,
    Country == "CRI" & Year == 2021 ~ 12871,
    Country == "CRI" & Year == 2022 ~ 13295,
    TRUE ~ RGDPPC  
  ))

data <- data %>% 
  mutate(RGDPPPP = case_when(
    Country == "CRI" & Year == 2017 ~ 18710,
    Country == "CRI" & Year == 2018 ~ 18984,
    Country == "CRI" & Year == 2019 ~ 19234,
    Country == "CRI" & Year == 2020 ~ 18220,
    Country == "CRI" & Year == 2021 ~ 19438,
    Country == "CRI" & Year == 2022 ~ 20079,
    TRUE ~ RGDPPPP  
  ))

data <- data %>% 
  mutate(RGDPPPP = case_when(
    Country == "LTU" & Year == 2017 ~ 31661,
    Country == "LTU" & Year == 2018 ~ 33241,
    Country == "LTU" & Year == 2019 ~ 34870,
    Country == "LTU" & Year == 2020 ~ 34853,
    Country == "LTU" & Year == 2021 ~ 36760,
    Country == "LTU" & Year == 2022 ~ 37129,
    TRUE ~ RGDPPPP  
  ))


data <- data %>% 
  mutate(RGDPPC = case_when(
    Country == "LTU" & Year == 2017 ~ 15662,
    Country == "LTU" & Year == 2018 ~ 16444,
    Country == "LTU" & Year == 2019 ~ 17250,
    Country == "LTU" & Year == 2020 ~ 17241,
    Country == "LTU" & Year == 2021 ~ 18185,
    Country == "LTU" & Year == 2022 ~ 18367,
    TRUE ~ RGDPPC  
  ))

data <- data %>% 
  mutate(RGDPPC = case_when(
    Country == "LVA" & Year == 2017 ~ 14854,
    Country == "LVA" & Year == 2018 ~ 15566,
    Country == "LVA" & Year == 2019 ~ 16075,
    Country == "LVA" & Year == 2020 ~ 15823,
    Country == "LVA" & Year == 2021 ~ 16622,
    Country == "LVA" & Year == 2022 ~ 16992,
    TRUE ~ RGDPPC  
  ))

data <- data %>% 
  mutate(RGDPPPP = case_when(
    Country == "LVA" & Year == 2017 ~ 26906,
    Country == "LVA" & Year == 2018 ~ 28197,
    Country == "LVA" & Year == 2019 ~ 29199,
    Country == "LVA" & Year == 2020 ~ 28662,
    Country == "LVA" & Year == 2021 ~ 30109,
    Country == "LVA" & Year == 2022 ~ 30652,
    TRUE ~ RGDPPPP  
  ))


data <- data %>% 
  mutate(StringencyIndex_Average = case_when(
    Country == "COL" & Year == 2017 ~ 0,
    Country == "COL" & Year == 2018 ~ 0,
    Country == "COL" & Year == 2019 ~ 0,
    Country == "COL" & Year == 2020 ~ 63.54192,
    Country == "COL" & Year == 2021 ~ 61.0092,
    Country == "COL" & Year == 2022 ~ 27.75649,
    TRUE ~ StringencyIndex_Average  
  ))

specific_country <- "CRI"
specific_year <- 2022
new_value <- 8.3
data$CPI[data$Country == specific_country & data$Year == specific_year] <- new_value

specific_country <- "JPN"
specific_year <- 2021
new_value <- -0.3
data$CPI[data$Country == specific_country & data$Year == specific_year] <- new_value

specific_country <- "JPN"
specific_year <- 2022
new_value <- 2.5
data$CPI[data$Country == specific_country & data$Year == specific_year] <- new_value



################################################################################
#  PHASE 2: CREATE TREATMENT GROUP AND PERIOD
################################################################################

##Correlation matrix
##correlation average by country----
ergebnisse <- data %>%
  group_by(Country) %>%
  summarize(
    korrelation = cor(StringencyIndex_Average, Real.GDP.Growth, use = "complete.obs"),
    .groups = 'drop'
  )

## correlationmatrix average berechnen
cor_matrix <- cor(data[, c("Real.GDP.Growth", "Private.Consumption", "StringencyIndex_Average", "GovernmentResponseIndex_Average", "ContainmentHealthIndex_Average","EconomicSupportIndex")], use = "complete.obs")

# correlationmatrix ausgeben
print(cor_matrix)

#Covariance
cov_matrix <- cov(data[, c("Real.GDP.Growth", "Private.Consumption", "StringencyIndex_Average", "GovernmentResponseIndex_Average", "ContainmentHealthIndex_Average","EconomicSupportIndex")], use = "complete.obs")

# correlationmatrix ausgeben
print(cov_matrix)

##output to latex

cor_matrix_table <- xtable(cor_matrix, caption = "Korrelationsmatrix")

# LaTeX-Code erzeugen

print.xtable(cor_matrix_table, type = "latex", comment = FALSE)


#2020

data_2020 <- data %>% 
  filter(Year == 2020)


cor_matrix_2020 <- cor(data_2020[, c("Real.GDP.Growth", "Private.Consumption", "StringencyIndex_Average", "GovernmentResponseIndex_Average", "ContainmentHealthIndex_Average", "EconomicSupportIndex")], use = "complete.obs")

print(cor_matrix_2020)

cor_matrix_table20 <- xtable(cor_matrix_2020, caption = "Korrelationsmatrix")

print.xtable(cor_matrix_table20, type = "latex", comment = FALSE)

##2021

data_2021 <- data %>% 
  filter(Year == 2021)

cor_matrix_2021 <- cor(data_2021[, c("Real.GDP.Growth", "Private.Consumption", "StringencyIndex_Average", "GovernmentResponseIndex_Average", "ContainmentHealthIndex_Average", "EconomicSupportIndex")], use = "complete.obs")

print(cor_matrix_2021)

cor_matrix_table21 <- xtable(cor_matrix_2021, caption = "Korrelationsmatrix")

print.xtable(cor_matrix_table21, type = "latex", comment = FALSE)

#2022

data_2022 <- data %>% 
  filter(Year == 2022)

cor_matrix_2022 <- cor(data_2022[, c("Real.GDP.Growth", "Private.Consumption", "StringencyIndex_Average", "GovernmentResponseIndex_Average", "ContainmentHealthIndex_Average", "EconomicSupportIndex")], use = "complete.obs")

print(cor_matrix_2022)

cor_matrix_table22 <- xtable(cor_matrix_2022, caption = "Korrelationsmatrix")

print.xtable(cor_matrix_table22, type = "latex", comment = FALSE)
#----

##Assignment to treatment group

##some descriptives of treatment indicators
summary_2020<-summary(subset(data, Year == 2020)$StringencyIndex_Average)
summary_2021<-summary(subset(data, Year == 2021)$StringencyIndex_Average)
summary_2022<-summary(subset(data, Year == 2022)$StringencyIndex_Average)

print(summary_2020)
print(summary_2021)


summary(subset(data, Year == 2020)$GovernmentResponseIndex_Average)
summary(subset(data, Year == 2021)$GovernmentResponseIndex_Average)

summary(subset(data, Year == 2020)$EconomicSupportIndex)
summary(subset(data, Year == 2021)$EconomicSupportIndex)

summary(subset(data, Year == 2020)$ContainmentHealthIndex_Average)
summary(subset(data, Year == 2021)$ContainmentHealthIndex_Average)


##distribution across countries

filtered_data <- data %>%
  filter(Year %in% c(2020, 2021))

# Erstelle eine Liste der OECD-L?nder, z.B.:
oecd_countries <- c("AUS", "AUT", "BEL", "CAN", "CHL", "COL", "CZE", "DNK", 
                    "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", 
                    "ISR", "ITA", "JPN", "KOR", "LVA", "LTU", "LUX", "MEX", 
                    "NLD", "NZL", "NOR", "POL", "PRT", "SVK", "SVN", "ESP", 
                    "SWE", "CHE", "TUR", "GBR", "USA", "CRI")


# Filtere die Daten f?r die OECD-L?nder
oecd_data <- filtered_data %>%
  filter(Country %in% oecd_countries)

# W?hle nur die relevanten Spalten aus
oecd_data <- oecd_data %>%
  select(Country, Year, StringencyIndex_Average)

# Pivotiere die Daten f?r eine breitere Darstellung
oecd_table <- oecd_data %>%
  pivot_wider(names_from = Year, values_from = StringencyIndex_Average, names_prefix = "Year_")

# Berechne den Durchschnitt der beiden Jahre und f?ge ihn als neue Spalte hinzu
oecd_table <- oecd_table %>%
  mutate(Average_2020_2021 = rowMeans(select(., starts_with("Year_")), na.rm = TRUE))

# Sortiere die Tabelle basierend auf den Werten der Average-Spalte von hoch bis tief
oecd_table <- oecd_table %>%
  arrange(desc(Average_2020_2021))

# Falls du das Ergebnis als CSV speichern m?chtest
write.csv(oecd_table, "oecd_stringency_index_sorted.csv", row.names = FALSE)

summary(oecd_table$Average_2020_2021)

med <- mean(oecd_table$Year_2020, na.rm = TRUE)

# L?nder ?ber dem Median ausw?hlen
above_median <- oecd_table %>%
  filter(Year_2020 > med) %>%
  select(Country, Year_2020)

above_median


# Zeige die resultierende Tabelle
print(n=38,oecd_table)


##
#Alternativ: Groups (geographical)----


#Get rid of different
#data <- data %>%
  #filter(!Country %in% c("AUT", "BEL","CRI", "CZE", "DNK", "FRA", "DEU", "GRC", "HUN", "KOR","LTU", "NLD", "POL", "PRT", "SVK", "SVN", "SWE", "TUR"))

# Liste der ISO-Codes f?r OECD-L?nder in Europa (ISO 3166-1 alpha-3)
#oecd_europa <- c("AUT", "BEL", "CZE", "EST", "FIN", "FRA", "DEU", "GRC", "HUN",
                     #"IRL", "ITA", "LVA", "LTU", "LUX", "NLD", "POL", "PRT",
                      #"SVK", "SVN", "ESP", "GBR", "CHE", "TUR")


#high_stringency_data_2020 <- data %>%
  #filter(Year == 2020 & Country %in% c("AUS" ,"CAN", "CHL", "COL", "ESP", "GBR", "IRL", "ISR", "ITA", "MEX", "USA"))


#high_stringency_data_2021 <- data %>%
  #filter(Year == 2021 & Country %in% c("AUS" ,"CAN", "CHL", "COL", "ESP", "GBR", "IRL", "ISR", "ITA", "MEX", "USA"))


#high_stringency_data_2022 <- data %>%
  #filter(Year == 2022 & Country %in% c("COL", "GRC", "IRL", "ISR", "SVN", "TUR"))


##----

##filter out specific countries
#data <- filter(data, Country != "EST")


##filter out all between 1. and 3. quantil

#included_countries <- c("EST", "FIN", "ISL", "JPN", "LUX", "NZL", "AUS", "CHL", "COL", "IRL", "ITA")

# Filtere die Daten, um nur die spezifischen OECD-L?nder zu behalten
#data <- data %>%
  #filter(Country %in% included_countries)

# Zeige die resultierenden gefilterten Daten
#print(data)



##Assignment to group based on stringency intensity

#50.07
high_stringency_data_2020 <- data %>%
  filter(Year == 2020 & StringencyIndex_Average >= 49.98)

# Filter data for the year 2021 with a stringency_index 
high_stringency_data_2021 <- data %>%
  filter(Year == 2021 & StringencyIndex_Average >= 53.75)

# Filter data for the year 2022 with a stringency_index 
#high_stringency_data_2022 <- data %>%
  #filter(Year == 2022 & StringencyIndex_Average >0)

# Combine the data for all three years
high_stringency_data_combined <- rbind(high_stringency_data_2020, high_stringency_data_2021)

# Identify countries that meet the criteria for all three years
treatment_countries <- high_stringency_data_combined %>%
  group_by(Country) %>%
  summarise(count_years = n_distinct(Year)) %>%
  filter(count_years == 2) %>%
  pull(Country)

# Create a new column in the original data to indicate if a country is in the treatment group
data <- data %>%
  mutate(TreatmentGroup = ifelse(Country %in% treatment_countries, 1, 0))



#Alternativ: ASSIGNMENT TO GROUP WITH FISCAL MEASURES INTENSITY----


#summary(subset(data, Year == 2020)$share_on_mo)
#summary(subset(data, Year == 2021)$share_on_mo)


  
#high_stringency_data_2020 <- data %>%
  #filter(Year == 2020 & share_on_mo > 0.70)

#Filter data for the year 2021 with a stringency_index 
#high_stringency_data_2021 <- data %>%
  #filter(Year == 2021 & share_on_mo > 0)

colnames(data)

# Combine the data for all three years
#high_stringency_data_combined <- rbind(high_stringency_data_2020, high_stringency_data_2021)

# Identify countries that meet the criteria for all three years
#treatment_countries <- high_stringency_data_combined %>%
  #group_by(Country) %>%
  #summarise(count_years = n_distinct(Year)) %>%
  #filter(count_years == 2) %>%
  #pull(Country)

# Create a new column in the original data to indicate if a country is in the treatment group
#data <- data %>%
  #mutate(TreatmentGroup = ifelse(Country %in% treatment_countries, 1, 0))



##Define treatment period and groups----

# Treatment period variable (after 2019)
data$covid <- ifelse(data$Year >= 2020 & data$Year <= 2024, 1, 0)
cro(data$TreatmentGroup)

data$strict<-data$TreatmentGroup
# Treatment variable (treated x post)
data$treatment <- (data$strict*data$covid)
cro(data$treatment)


###NORMAL
control_countries <- data %>%
  filter(!Country %in% treatment_countries) %>%
  distinct(Country) %>%
  pull(Country)


print(treatment_countries)
print(control_countries)

##Create worldmap

world <- ne_countries(scale = "medium", returnclass = "sf")

# Entferne Antarktis
world <- world[world$admin != "Antarctica",]

# Erstellen Sie eine neue Spalte f?r die Gruppenzugeh?rigkeit
world$group <- ifelse(world$iso_a3 %in% treatment_countries, 'Treatment Group',
                      ifelse(world$iso_a3_ %in% control_countries, 'Control Group', 'Other'))


ggplot(data = world) +
  geom_sf(aes(fill = group), color = "white") +
  scale_fill_manual(values = c("Treatment Group" = "red", "Control Group" = "blue"),
                    labels = c("Control", "Treatment")) +
  theme_void() +
  theme(legend.position = "bottom") +  # Legende unten platzieren
  labs(fill = "")  # Entfernt den Titel der Legende

ggsave("worldmap.pdf", width = 10, height = 6, device = 'pdf')

#------------------------------------------------------------------------------
  
library(dplyr)
library(cobalt)

# 1) Pre-Daten und Baseline-Kovariaten bilden (Beispiel: Mittel 2015–2019 je Land)
pre <- data %>% filter(Year >= 2015, Year <= 2019)

unem

wdi

pre_cov <- pre %>%
  group_by(Country) %>%
  summarise(
    gdp_pc_mean   = mean(Real.GDP.Growth2, na.rm = TRUE),
    unemp_mean    = mean(wdi_unempmne, na.rm = TRUE),
    debt_ratio_19 = dplyr::last(na.omit(Government.Debt.Ratio[order(Year)])),
    median_age    = mean(Median.Age, na.rm = TRUE),
    life_exp      = mean(Life.Expectancy, na.rm = TRUE),
    pop_density   = mean(Population.Density, na.rm = TRUE),
    gov_eff       = mean(wdi_agedr, na.rm = TRUE),
    rule_law      = mean(rule.of.law, na.rm = TRUE),
    dem_index     = mean(democracy_eiu, na.rm = TRUE),
    rgdp_level    = mean(Real.GDP.Growth2, na.rm = TRUE),   # Level
    rgdp_slope    = { b <- try(coef(lm(Real.GDP.Growth2 ~ Year)), silent = TRUE);
    if(inherits(b, "try-error")) NA_real_ else unname(b["Year"]) },
    .groups = "drop"
  )

# 2) Treatment-Zuteilung (deine bereits definierte treat_df mit Country,treat)
base_pre <- pre_cov %>% left_join(treat_df, by = "Country") %>% tidyr::drop_na(treat)

# 3) SMDs ungewichtet (ohne PS/Matching)
xvars <- c("gdp_pc_mean","unemp_mean","debt_ratio_19","median_age","life_exp",
           "pop_density","gov_eff","rule_law","dem_index","rgdp_level","rgdp_slope")

bal.tab(x = base_pre[, xvars],
        treat = base_pre$treat,
        un = TRUE, s.d.denom = "pooled",
        thresholds = c(m = .1))  # zeigt SMDs und markiert >0.1

# Optional: wenn du schon Gewichte (z. B. aus Entropy Balancing) hast:
# bal.tab(x = base_pre[, xvars], treat = base_pre$treat,
#         weights = deine_gewichte_je_Land, estimand = "ATE",
#         un = TRUE, s.d.denom = "pooled", thresholds = c(m=.1))






##define Outcomes

quoted_names <- shQuote(names(data), type = "cmd")
cat(quoted_names, sep = ", ")

##Teile die Variablen in Gruppen ein, je nach Verf?gbarkeit der Daten
#Gruppe 1: bis 2024, gruppe 2: bis 2023, gruppe 3: bis 2022, gruppe 4: ganze bl?cke fehlen, gruppe 5: rest

names(data)

mapping <- list(
  "Gruppe1" = c(
    "Real.GDP.Growth.NAAG","Real.GDP.Growth2","Private.consumption.growth",
    "HH.cons.ca","Prim.Income.Balance.pct.GDP","Sec.Income.Balance.pct.GDP",
    "Gov.Con2","m.ca","x.ca","Unemployment.rate","Total.employment.growth",
    "Emprate.supply.15_74","Central.bank.key.interest.rate","Short.term.interest.rate",
    "Export.performance.growth","Export.volume.growth.NAB","Import.volume.growth.NAB",
    "Trade.Contribution.USD.Real","Current.account.balance.as.a.percentage.of.GDP",
    "Total.domestic.expenditure.volume.growth","Gross.fc","GNI.2010",
    "Exports.of.goods.and.services..deflator..national.accounts.basis.",
    "Final.domestic.expenditure..deflator","Government.final.consumption.expenditure..deflator",
    "Deflator.GDP","Deflator.M","Deflator.pfc","PPP.nat.c","Deflator.tdc","icrg_qog"),
  "Gruppe2" = c(
    "Net.lending.borrowing.share","rd_inw_gdp",
    "ti_cpi","top_top10_income_share","top_top1_income_share",
    "wdi_agedr","wdi_pop14",
    "wdi_pop1564","wdi_pop65",
    "wdi_broadb","wdi_fdiin",
    "wdi_fdiout","wdi_inflation",
    "wdi_internet","wdi_popgr",
    "wdi_poprul","wdi_poprulgr",
    "wdi_popurb","wdi_popurbagr",
    "wdi_svapgdp",
    "wdi_tele","wpp_popden",
    "control.of.corruption","gov.effect.",
    "pol.stab.viole","regulatory.quality",
    "rule.of.law","voice.accountability"),
  "Gruppe3" = c(
    "dr_ig","ef_bul",
    "ef_carb","fi_index",
    "gain_gain","gain_gov",
    "gain_heal","gain_readgdp",
    "gain_soc","spi_ospi",
    "undp_hdi","wdi_interexp",
    "wdi_interrev","wdi_mobile",
    "wdi_taxrev"),
  "Gruppe4" = c(
    "Gross.capital.formation.growth",
    "wdi_interexp","wdi_interrev",
    "wdi_taxrev",
    "Labour.productivity.total.economy",
    "Emprat.gap", "Output.gap.as.a.percentage.of.potential.GDP",
    "Potential.output.volume.growth", "Comp.Export.Prices",
    "Comp.Index.CPI", "Comp.Index.ULC",
    "Trade.Share.World.USD","S.UR",
    "FX.usd","Trade.Balance"),
  "Gruppe5" = c(
    "Gov.cons.growth","Gov.cons.share","Gross.capital.formation.share",
    "Gross.fixed.capital.growth","Gross.fixed.capital.share",
    "Gross.domestic.savings.share","Gross.savings.share","Gross.savings.gni",
    "Gross.national.expenditure.share","Net.exports.of.goods.and.services.as.a.percent.of.GDP",
    "Terms.of.trade.goods.and.services","democracy_eiu","PopulationVaccinated",
    "wdi_unempmne","wdi_unempne","wdi_unempymne","total_cases",
    "total_deaths_per_million","hosp_patients","hosp_patients_per_million",
    "total_tests","reproduction_rate","StringencyIndex_Average",
    "ContainmentHealthIndex_Average","EconomicSupportIndex",
    "GovernmentResponseIndex_Average","Real.GDP.Growth","GDPGRO","RGDPPC",
    "RGDPPPP","BOP","GB","GDP.per.Hour.Worked","Private.Consumption",
    "Privat.Cons","Avg_CCI","Gross.National.Income","Net.National.Income",
    "Government.Debt.Ratio","Government.Spending.per.Capita","Gov.Cons",
    "Net.Lending.Borrowing.Government","Exports","Imports","Integration",
    "X","M","Trade.in.Goods.and.Services","Inflation_Rate","CPI","PLI",
    "Gross.Cap.form","Health_Spending","X65.and.over","X85.and.over",
    "Human.Development.Index","X5_Business_sophistication_index",
    "X7_Creative_outputs_index","X76_Credit","X11_Domestic_market_scale_bn_PPP.",
    "X24_Education","X64_Electricity_output","X63_General_infrastructure",
    "X48_Gross_exp_R.D_GERD","X185_High_t_exports_t_trade","X58_ICT_access",
    "X59_ICT_use","X57_ICT_tech","X117_Innovation_linkages",
    "X4_Market_sophistication_index","X91_Trade._diversification_market_scale",
    "GNI.Growth"))

variable_mapping <- bind_rows(lapply(names(mapping), function(gruppe) {
  data.frame(
    Variable = mapping[[gruppe]],
    Kategorie = gruppe
  )
}))

print(variable_mapping)

outcomes <- mapping[["Gruppe1"]]
outcomes2 <- mapping[["Gruppe2"]]
outcomes3 <- mapping[["Gruppe3"]]
outcomes4 <- mapping[["Gruppe4"]]
outcomes5 <- mapping[["Gruppe5"]]


##Reihenfolge Outcomes wie im Datensatz
#outcomes <- intersect(names(data), outcomes)

setdiff(outcomes, names(data))

present  <- intersect(outcomes, names(data))
missing  <- setdiff(outcomes, names(data))
length(missing); head(missing)

df_out <- dplyr::select(data, dplyr::any_of(outcomes))

# was von outcomes wurde gedroppt?
intersect(outcomes, drop_cols)

# welche outcomes fehlen im Datensatz nach dem Droppen/Umbenennen?
setdiff(outcomes, names(data))


##outcomes=dependent_vars
dependent_vars<-outcomes



# Nicht-numerische Spaltennamen
non_num <- names(df_out)[!sapply(df_out, is.numeric)]
non_num

rm(fxdata,NAAGdata,df_combined,tradedata,hhdata,cadata, govdata, prdata, edata, irdata, ladata, hhprdata, oecddata, wdi, population_2019, oxnotes2, oxnotes, ox2, ox, data_2020, data_2021, 
   data_2022, ergebnisse, che_data, cor_matrix_table, cor_matrix_table20, cor_matrix_table21, cor_matrix_table22, cor_matrix, cor_matrix_2020, cor_matrix_2021, cor_matrix_2022, cov_matrix, high_stringency_data_2020, 
   high_stringency_data_2021, high_stringency_data_combined, oecd_data, oecd_table, usa_data, world, above_median)

####################################################################
#   PHASE 3: DESCRIPTIVE STATISTICS AND VALIDITY CHECKS
####################################################################

#write.csv(data, ("C:/Users/aulis/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/output r/nadata.csv"), row.names = FALSE)

##HISTOGRAM

# Filter data for the years 2020-2022
data_2020_2022 <- subset(data, Year %in% c(2020))

# Plot the distribution of the stringency index for the years 2020-2022
ggplot(data_2020_2022, aes(x = StringencyIndex_Average)) +
  geom_histogram(binwidth = 3, fill = "blue", color = "black", alpha = 0.7) +
  geom_density(aes(y = ..count..), color = "red") + 
  labs(title = "Distribution of Stringency Index (2020)",
       x = "Stringency Index",
       y = "Frequency") +
  scale_x_continuous(breaks = seq(0, 100, 5), limits = c(25, 75)) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.title.position = "plot"
  )


#Descriptives

summary(subset(data, Year > 2019)$StringencyIndex_Average)

#Berechnen des Durchschnitts des stringency_index f?r die Jahre 2020-2022 pro Land
avg_stringency <- data %>%
  filter(Year %in% c(2020, 2021,2022)) %>%
  group_by(Country) %>%
  summarise(avg_stringency_index = mean(StringencyIndex_Average, na.rm = TRUE))

# F?ge den berechneten Durchschnitt zum urspr?nglichen Datensatz hinzu
data <- left_join(data, avg_stringency, by = "Country")
summary(data$avg_stringency_index)

# Plot the distribution of avg_stringency_index
p<-ggplot(data, aes(x = avg_stringency_index)) +
  geom_histogram(binwidth = 3, fill = "blue", color = "black", alpha = 0.7) +
  geom_density(aes(y = ..count..), color = "red") + 
  labs(title = "Distribution of Average Stringency Index (2020-2022)",
       x = "Average Stringency Index",
       y = "Frequency") +
  theme_minimal()


ggsave("stringency_distribution.pdf", plot = p, width = 10, height = 6)

##only for 2020 and 2021
# Filtern der Daten f?r die Jahre 2020 und 2021
filtered_data <- data %>%
  filter(Year %in% c(2020, 2021))

# Berechnen des Durchschnitts des stringency_index f?r die Jahre 2020-2021 pro Land
avg_stringency <- filtered_data %>%
  group_by(Country) %>%
  summarise(avg_stringency_index = mean(StringencyIndex_Average, na.rm = TRUE))

# F?ge den berechneten Durchschnitt zum urspr?nglichen Datensatz hinzu
filtered_data <- left_join(filtered_data, avg_stringency, by = "Country")

# Zusammenfassung der avg_stringency_index
summary(filtered_data$avg_stringency_index)

# Plot the distribution of avg_stringency_index
p <- ggplot(filtered_data, aes(x = avg_stringency_index)) +
  geom_histogram(binwidth = 3, fill = "blue", color = "black", alpha = 0.7) +
  geom_density(aes(y = ..count..), color = "red") + 
  labs(title = "Distribution of Average Stringency Index (2020-2021)",
       x = "Average Stringency Index",
       y = "Frequency") +
  theme_minimal()

# Zeige den Plot
print(p)
ggsave("histo.pdf", plot = p, width = 10, height = 6)

 head(qdata)
##validity checks of distribution of the values across the sample

did_vars <- c("strict", "covid", "treatment")
#did_vars <- c("connected", "submarines", "treatment")
did_var<-as.numeric(did_vars)
# DiD variables, before (pre)treatment is introduced
desc_before <- dplyr::filter(data, data$covid==0) 
fBasics::basicStats(desc_before[did_vars]) %>% 
  t() %>% 
  as.data.frame() %>% 
  dplyr::select(Mean, Stdev, Minimum, Maximum, nobs)


# DiD variables, after treatment is introduced
desc_after <- dplyr::filter(data, data$covid==1) 
fBasics::basicStats(desc_after[did_vars]) %>% 
  t() %>% 
  as.data.frame() %>% 
  dplyr::select(Mean, Stdev, Minimum, Maximum, nobs)


# What periods we have - calendar
cro(Year)

# Normalized time periods/binning (to use in regressions)
cro(time)


####Summary statistics for outcomes by treatment group, before and after treatment
##ACHTUNG, momentan nur outcomes von gruppe 1
##replace N/A with 0
data[is.na(data)] <- 0

outcomes <- dplyr::select(data, outcomes)
outcomes_names <- colnames(outcomes) 


# Define a function estimating the differences in variables across D and T
balance_check.model <- function(x, data){
  
  # Observations
  nobs<-nrow(x)
  
  # Conditional means
  mean_d0_before <- mean(x[data$strict==0 & data$covid==0], na.rm=TRUE) # filters out missings 
  mean_d1_before <- mean(x[data$strict==1 & data$covid==0], na.rm=TRUE)
  mean_d0_after <- mean(x[data$strict==0 & data$covid==1], na.rm=TRUE) 
  mean_d1_after <- mean(x[data$strict==1 & data$covid==1], na.rm=TRUE)
  
  # Difference in means before treatment
  diff_before <- lm(x[covid==0] ~ data$strict[covid==0])
  cov <- vcovHC(diff_before, type = "HC")
  robust.se_before <- sqrt(diag(cov))
  
  # Difference in means after treatment
  diff_after <- lm(x[covid==1] ~ data$strict[covid==1])
  cov <- vcovHC(diff_after, type = "HC")
  robust.se_after <- sqrt(diag(cov))
  
  list(mean_d0_before = mean_d0_before, 
       mean_d1_before = mean_d1_before,
       diff_before = diff_before$coefficients[2], 
       robust.se_before = robust.se_before[2], 
       pval_before = 2*pnorm(-abs(diff_before$coefficients[2]/robust.se_before[2])), 
       mean_d0_after = mean_d0_after, 
       mean_d1_after = mean_d1_after,
       diff_after = diff_after$coefficients[2], 
       robust.se_after = robust.se_after[2], 
       pval_after = 2*pnorm(-abs(diff_after$coefficients[2]/robust.se_after[2]))
  )             
}

diff_output <- apply(outcomes, 2,function(x) balance_check.model(x, data))

# convert list to table
diff_output<-rbindlist(diff_output)


# add a row with number of observations
n_d0_before <- nrow(data[data$strict==0 & data$covid==0,])
n_d1_before <- nrow(data[data$strict==1 & data$covid==0,])
n_d0_after <- nrow(data[data$strict==0 & data$covid==1,])
n_d1_after <- nrow(data[data$strict==1 & data$covid==1,])
obs <-c(n_d0_before, n_d1_before, NA, NA, NA, n_d0_after, n_d1_after, NA, NA, NA)

diff_output <- rbind(as.matrix(diff_output), obs)

rownames(diff_output)<- c(outcomes_names, "Observations")
colnames(diff_output)<- c("E(Y|D=0, T=0)", "E(Y|D=1, T=0)", 
                          "Difference", "s.e.", "p-value", 
                          "E(Y|D=0, T=1)", "E(Y|D=1, T=1)", 
                          "Difference", "s.e.", "p-value")

print("Average Outcomes before and after Treatment")
xtable(diff_output, digits=2)
print(diff_output)


latex_code <- xtable(diff_output, digits=2)
print(latex_code, type = "latex")  # f?r LaTeX-Code
sink("meine_tabelle.tex")
print(latex_code, type = "latex")
sink()



##CHECK for CT Assumption - Common trends graph (unadjusted means of outcome by year)

# Create a group-means data set

data$Year<-as.numeric(data$Year)

table(subset(data, Year >= 2015 & Year <= 2024)$strict)

common_trends <- data %>% 
  filter(Year >= 2015 & Year <= 2024) %>%
  group_by(Year, strict) %>% 
  summarise(mean = mean(Real.GDP.Growth2, na.rm = TRUE))

#CHECK
head(common_trends)
common_trends$strict <- as.factor(common_trends$strict)

##Plot for real gdp growth
ggplot(data = common_trends, 
       aes(x = Year, y = mean, 
           group = strict, color = strict)) + 
  geom_line() +
  scale_color_manual(values = c("0" = "blue", "1" = "red")) +
  geom_vline(xintercept = 2019, linetype="dashed") +
  scale_x_continuous(breaks = seq(2015, 2024, by = 1)) +
  theme_bw(base_size = 20)



realgdp<-ggplot(data = common_trends, 
       aes(x = Year, y = mean, 
           group = strict, color = strict)) + 
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = 2019, linetype = "solid", color = "black", size=1) +
  annotate("text", x = 2019, y = Inf, label = "Covid", vjust = 2, hjust=-0.1) +
  scale_x_continuous(breaks = seq(2015, 2024, by = 1)) +
  scale_color_manual(values = c("0" = "blue", "1" = "red"),
                     name = "",  # Leerer Name f?r die Legende
                     labels = c("Controlgroup", "Treatmentgroup")) +
  labs(title = NULL,
       x = "Year",
       y = "Mean Real GDP Growth",
       caption = NULL) +
  theme_bw(base_size = 16) +
  theme(panel.grid.major = element_line(colour = "#808080", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        legend.position = "top")

realgdp

ggsave("realgdp.png", plot = realgdp, width = 6, height = 4)



##Overview Plot from Section Background and Literature 

library(patchwork)


# 1) Year sauber als Integer und Daten filtern
data <- data %>% mutate(Year = as.integer(Year))

filtered_data <- data %>%
  filter(Year >= 2020, Year <= 2021)


# Berechnen des Durchschnitts von StringencyIndex_Average f?r jedes Land
average_stringency <- filtered_data %>%
  group_by(Country) %>%
  summarise(mean_stringency = mean(StringencyIndex_Average, na.rm = TRUE))

# Berechnen des Durchschnitts von total_intensity_per f?r jedes Land und Umwandlung in Prozent
average_intensity <- filtered_data %>%
  group_by(Country) %>%
  summarise(mean_total_intensity_per = mean(total_intensity_per, na.rm = TRUE) * 100)

# Berechnen des Durchschnitts von Real.GDP.Growth f?r jedes Land
average_gdp_growth <- filtered_data %>%
  group_by(Country) %>%
  summarise(mean_gdp_growth = mean(Real.GDP.Growth, na.rm = TRUE))

# Zusammenf?hren der Daten f?r die drei Plots mit full_join
combined_data <- full_join(average_stringency, average_intensity, by = "Country")
combined_data <- full_join(combined_data, average_gdp_growth, by = "Country")

# Erstellen des kombinierten Plots
combined_plot <- ggplot(combined_data, aes(x = reorder(Country, -mean_stringency))) +
  geom_bar(aes(y = mean_stringency, fill = "Mean Stringency Index"), stat = "identity", alpha = 0.7, width = 0.6) +
  geom_text(aes(y = mean_stringency, label = round(mean_stringency, 1)), vjust = -0.3, color = "black", size = 5.5) +  # Werte ?ber den Balken anzeigen
  geom_bar(aes(y = mean_total_intensity_per, fill = "Mean Total Intensity Per (%)"), stat = "identity", alpha = 1, width = 0.3, position = position_nudge(x = -0.15)) +
  geom_bar(aes(y = mean_gdp_growth, fill = "Mean Real GDP Growth (%)"), stat = "identity", alpha = 1, width = 0.3, position = position_nudge(x = 0.15)) +
  geom_text(aes(y = mean_gdp_growth, label = round(mean_gdp_growth, 1)), vjust = 1.5, color = "black", size = 5.5, position = position_nudge(x = 0.30)) +  # Werte ?ber den Balken anzeigen
  scale_y_continuous(
    name = "Index Points",
    sec.axis = sec_axis(~., name = "Percentage", breaks = seq(-20, 100, by = 20))
  ) +
  scale_fill_manual(name = "", values = c("Mean Stringency Index" = "darkgray", "Mean Total Intensity Per (%)" = "#00008B", "Mean Real GDP Growth (%)" = "orange")) +
  labs(x = "Country") +
  theme_minimal(base_size = 16) +
  theme(panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),  # Dreht die L?ndercodes f?r bessere Lesbarkeit
        legend.position = "bottom")

# Plot anzeigen
print(combined_plot)

# Plot als PDF speichern
ggsave("combined_plot.pdf", plot = combined_plot, width = 20, height = 12)


##All plots for CT-Assumption
# Data preprocessing



data$strict <- as.factor(data$strict)

###NEu

# saubere Typen
data <- data %>%
  mutate(
    Year   = as.integer(Year),
    strict = as.factor(strict)
  )

# ---- dependent_vars als Character-Vektor mit SPALTENNAMEN ----
# Falls 'outcomes' ein data.frame/tibble mit Zielspalten ist:
if (is.data.frame(outcomes)) {
  dependent_vars <- intersect(names(outcomes), names(data))
} else if (is.character(outcomes)) {
  dependent_vars <- intersect(outcomes, names(data))
} else if (is.numeric(outcomes)) {
  dependent_vars <- names(data)[outcomes]
} else {
  stop("outcomes muss entweder ein data.frame, character- oder numeric-Vektor sein.")
}

# Sicherheitscheck
stopifnot(length(dependent_vars) > 0)
fehlend <- setdiff(dependent_vars, names(data))
if (length(fehlend) > 0) stop("Fehlende Spalten: ", paste(fehlend, collapse = ", "))

# ---- Funktionen ohne sym/!!, Zugriff ueber .data[[...]] ----
calculate_means <- function(var_name, data) {
  data %>%
    group_by(Year, strict) %>%
    summarise(mean_value = mean(.data[[var_name]], na.rm = TRUE), .groups = "drop")
}

create_plot <- function(data_means, var_name) {
  ggplot(data_means, aes(x = Year, y = mean_value, group = strict, color = strict)) +
    geom_line(linewidth = 1) +
    geom_vline(xintercept = 2019, linetype = "dashed") +
    scale_x_continuous(breaks = 2015:2024) +
    scale_color_manual(values = c("0" = "blue", "1" = "red")) +
    theme_bw(base_size = 15) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(angle = 45, hjust = 1)) +
    labs(title = paste("Plot for", var_name), x = "Year", y = "Mean")
}

# ---- WICHTIG: ueber die NAMEN iterieren ----
plots_list <- lapply(dependent_vars, function(var_name) {
  means_data <- calculate_means(var_name, data)
  create_plot(means_data, var_name)
})
names(plots_list) <- dependent_vars


# Save the plots to a PDF, arranging four plots per page
pdf("output_plots.pdf", width = 12, height = 10)  # Larger size for better visibility

# Handling to print four plots per page
plot_count <- length(plots_list)
num_pages <- ceiling(plot_count / 4)

for (i in 1:num_pages) {
  # Subset four plots or remaining plots
  plots_subset <- plots_list[((i-1)*4 + 1):min(i*4, plot_count)]
  grid.arrange(grobs = plots_subset, ncol = 2, nrow = 2)
}

dev.off()

# Display all plots in RStudio viewer or graphics window
for (i in 1:num_pages) {
  plots_subset <- plots_list[((i-1)*4 + 1):min(i*4, plot_count)]
  grid.arrange(grobs = plots_subset, ncol = 2, nrow = 2)
}



####################################################################
#   PHASE 4: DiD effect estimation 
####################################################################
###Possibility to cut out specific observations or time periods
#data <- subset(data, Country != "FIN")
#data <- data %>% filter(Year <= 2022)
#data <- data %>% filter(Year >= 2017)

##All economic depended variables
dependent_var_eco<-c("real.GDP.ca", "Real.GDP.Growth", "Private.Consumption", "Gov.Cons", "X", "M",
                     "Net.National.Income", "Gross.National.Income","GNI.Growth", 
                     "Net.Lending.Borrowing.Government", 
                     "Government.Debt.Ratio", "Integration", "Trade.in.Goods.and.Services", "Trade.Balance",
                     "BOP", "X68_Gross_capital_formation",  "Inflation_Rate", "CPI", "Unemployment.rate", "Youth.Unemployment.Rate",
                     "Older.Persons.Unemployment.Rate", "Debt")


dependent_var_eco2<-outcomes



###################

#########
##All control depended variables
dependent_var_ct <- c("X45_RnD","X235_Global_Innovation_Index",  "X48_Gross_exp_R.D_GERD","X117_Innovation_linkages", "X118_University_industry_R.D", "X168_Software_spending_GDP",
                      "X58_ICT_access", "X59_ICT_use", "X57_ICT_tech","X185_High_t_exports_t_trade",
                      "X91_Trade._diversification_market_scale", "X2_Human_capital_research_index", "X76_Credit","X82_Investment",
                      "Short.Term.Interest.Rate", "Long.Term.Interest.Rate", "X4_Market_sophistication_index", "X5_Business_sophistication_index",
                      "X24_Education","X71_GDP_unit_energy_use", "X63_General_infrastructure", "X3_Infrastructure_index",
                      "X9_Political_a_operational_stability","X14_Regulatory_environment", "X15_Regulatory_quality", "X16_Rule_of_law", "X12_Government_effectiveness", "X9_Political_a_operational_stability",
                      "X8_Political_environment", "X1_Institutions_index", "democracy_eiu", "X14_Regulatory_environment","X15_Regulatory_quality", "X65.and.over","X85.and.over","total_cases", 
                      "total_deaths", "total_deaths_per_million", "reproduction_rate",  "total_tests",  "PopulationVaccinated")

#####################################################
##################TWO-STAGES-DID (GARDENER)###################
##############################################################

#data <- filter(data, Country != "IRL")
##check countries in the Treatment Group
treatment_countries <- data %>%
  distinct(Country, treatment) %>%
  filter(treatment == 1) %>%
  pull(Country)

print(treatment_countries)

Government.Debt.Ratio
##Estimation ATET for testing of single variables
did3g<- did2s(data, yname= "Government.Debt.Ratio",
      treatment= "treatment",
      cluster_var="Country",
      first_stage = ~ 0 | time  + Country, 
      second_stage = ~ i(treatment, ref= FALSE),
      bootstrap = FALSE,
      n_bootstraps = 10000,
      return_bootstrap = TRUE,
      verbose = TRUE)

summary(did3g, cluster="Country")

###AUSGABE BOOTSTRAP
# Falls Matrix, zuerst in DataFrame umwandeln
boot_df <- as.data.frame(did3g)

# Spaltennamen setzen (je nach Inhalt!)
colnames(boot_df) <- "qtreatment::1"


boot_summary <- boot_df %>%
  summarise(
    estimate = mean(`qtreatment::1`),
    std.error = sd(`qtreatment::1`),
    t_value = estimate / std.error,
    conf.low = quantile(`qtreatment::1`, 0.025),
    conf.high = quantile(`qtreatment::1`, 0.975),
    p_value = 2 * (1 - pnorm(abs(t_value)))
  )

boot_summary



##Estimation Event-Study for testing of single variables
did4g <- did2s(data,
            yname = "Government.Debt.Ratio", treatment = "treatment", cluster_var = "Country",
            first_stage = ~ 0| time + Country,
            second_stage = ~ i(rel_year, ref = c(0, Inf)),
            bootstrap = FALSE,
            n_bootstraps = 5000,
            return_bootstrap = FALSE,
            verbose = TRUE)

summary(did4g, cluster="Country")


fixest::iplot(
  did4g, 
  main = "Event study: Staggered treatment", 
  xlab = "Relative time to treatment", 
  col = "black", ref.line = 0
)



pdf("did41.pdf", width = 7, height = 5)  # Breite und H?he in Zoll
did41 <- fixest::iplot(
  did4g, 
  main = "Effect on Private Consumption Growth", 
  xlab = "Relative time to treatment", 
  col = "black", ref.line = -0,
  cluster = "Country"
)
dev.off()


##Function for all economic variables (ATET and Event-Study)

##possibility to filter the years
dependent_vars2<-outcomes2
dependent_vars3<-outcomes3
dependent_vars4<-outcomes4
#data <- data %>% filter(Year <= 2022)
data <- data %>%
  filter(Country != "CRI")

run_analysis <- function(dependent_vars4) {
  
  # (3) Fixed Effects mit Panel-Daten
  did3 <- did2s(data = data,
                yname = dependent_vars4, 
                treatment = "treatment",
                cluster_var = "Country",
                first_stage = ~ 0 | time + Country, 
                second_stage = ~ i(treatment, ref = FALSE),
                bootstrap = FALSE,
                n_bootstraps = 1000,
                return_bootstrap = FALSE,
                verbose = TRUE)
  

  # (4) Event Study mit der neuen did2s Methode
  did4 <- did2s(data = data,
                yname = dependent_vars4, treatment = "treatment", cluster_var = "Country",
                first_stage = ~ 0 | time + Country,
                second_stage = ~ i(rel_year, ref = c(0, Inf)),
                bootstrap = FALSE,
                n_bootstraps = 10000,
                return_bootstrap = FALSE,
                verbose = TRUE)

  results_eco <- etable(did3, did4, 
                        cluster="Country", 
                        headers = c("Country + Yearly FE", "Event study"))
  
  return(results_eco)
}

results_list_eco <- lapply(dependent_vars4, run_analysis)

page(results_list_eco, method = "print")##Show results in R
results_list_eco



anyNA(data$rd_inw_gdp)

################################
##Same for all control variables
dependent_var_ct2 <- c("X45_RnD","X235_Global_Innovation_Index",  "X48_Gross_exp_R.D_GERD","X117_Innovation_linkages", "X118_University_industry_R.D", "X168_Software_spending_GDP",
                       "X58_ICT_access", "X59_ICT_use", "X57_ICT_tech","X185_High_t_exports_t_trade",
                       "X91_Trade._diversification_market_scale", "X2_Human_capital_research_index", "X76_Credit","X82_Investment",
                       "Short.Term.Interest.Rate", "Long.Term.Interest.Rate","X4_Market_sophistication_index", "X5_Business_sophistication_index",
                       "X24_Education","X63_General_infrastructure", "X3_Infrastructure_index",  
                       "X16_Rule_of_law", "X12_Government_effectiveness", "X9_Political_a_operational_stability",
                       "X8_Political_environment", "X1_Institutions_index", "democracy_eiu", "X14_Regulatory_environment","X15_Regulatory_quality", "X65.and.over","X85.and.over","total_cases", 
                       "total_deaths", "total_deaths_per_million", "reproduction_rate",  "total_tests",  "PopulationVaccinated",
                       "POLCONIII_VDEM")



run_analysis2 <- function(dependent_var_ct2) {
  
  # (3) Fixed Effects mit Panel-Daten
  did3 <- did2s(data = data,
                yname = dependent_var_ct2, 
                treatment = "treatment",
                cluster_var = "Country",
                first_stage = ~ 0 | time + Country, 
                second_stage = ~ i(treatment, ref = FALSE),
                bootstrap = FALSE,
                n_bootstraps = 10000,
                return_bootstrap = FALSE,
                verbose = TRUE)
  
  
  # (4) Event Study mit der neuen did2s Methode
  did4 <- did2s(data = data,
                yname = dependent_var_ct2, treatment = "treatment", cluster_var = "Country",
                first_stage = ~ 0 | time + Country,
                second_stage = ~ i(rel_year, ref = c(0, Inf)),
                bootstrap = FALSE,
                n_bootstraps = 10000,
                return_bootstrap = FALSE,
                verbose = TRUE)
  

  results_ct2 <- etable(did3,did4, 
                        cluster="Country", 
                        headers = c("Country + Yearly FE", "Event study"))
  
  return(results_ct2)
}

results_list_ct2 <- lapply(dependent_var_ct2, run_analysis2)

results_list_ct2
page(results_list_ct2, method = "print")



##Event-study-command->compare to others stateof-the-art estimators

data$g <- ifelse(data$strict == 0, 0, 2020)

data$rel_year<-ifelse(data$strict==0,Inf,data$rel_year)
#data$CountryID <- as.integer(factor(data$Country))


custom_plot_event_study <- function (out, separate = TRUE, horizon = NULL) {
  estimators = unique(out$estimator)
  levels = c("TWFE", "Borusyak, Jaravel, Spiess (2021)", 
             "Callaway and Sant'Anna (2020)", "Gardner (2021)", 
             "Roth and Sant'Anna (2021)", "Sun and Abraham (2020)")
  levels = levels[levels %in% estimators]
  out$estimator = factor(out$estimator, levels = levels)
  color_scale = c(TWFE = "#374E55", `Gardner (2021)` = "#DF8F44", 
                  `Callaway and Sant'Anna (2020)` = "#00A1D5", 
                  `Sun and Abraham (2020)` = "#B24745", `Roth and Sant'Anna (2021)` = "#79AF97", 
                  `Borusyak, Jaravel, Spiess (2021)` = "#6A6599")
  color_scale = color_scale[names(color_scale) %in% estimators]
  out$ci_lower = out$estimate - 1.96 * out$std.error
  out$ci_upper = out$estimate + 1.96 * out$std.error
  if (separate) 
    position = "identity"
  else position = ggplot2::position_dodge(width = 0.5)
  if (!is.null(horizon)) {
    out = out[out$term >= horizon[1] & out$term <= horizon[2], ]
  }
  y_lims = c(min(out$ci_lower), max(out$ci_upper)) * 1.05
  x_lims = c(min(out$term) - 1, max(out$term) + 1)
  ggplot2::ggplot(data = out, mapping = ggplot2::aes(x = .data$term, 
                                                     y = .data$estimate, color = .data$estimator, ymin = .data$ci_lower, 
                                                     ymax = .data$ci_upper)) + {
                                                       if (separate) 
                                                         ggplot2::facet_wrap(~estimator, scales = "free")
                                                     } + ggplot2::geom_point(position = position) + ggplot2::geom_errorbar(position = position) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +  # Changed this line
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") + 
    ggplot2::labs(y = "Point Estimate and 95% Confidence Interval", 
                  x = "Event Time", color = "Estimator") + 
    {
      if (separate) 
        ggplot2::scale_y_continuous(limits = y_lims)
    } + {
      if (separate) 
        ggplot2::scale_x_continuous(limits = x_lims)
    } + ggplot2::theme_minimal(base_size = 16) + ggplot2::scale_color_manual(values = color_scale) + 
    ggplot2::guides(color = ggplot2::guide_legend(title.position = "top", 
                                                  nrow = 2)) + ggplot2::theme(legend.position = "bottom")
}

attach(data)

data(data, package = "did2s")
out = event_study(
  data = data, yname = "Real.GDP.Growth", idname = "CountryID",
  tname = "Year", gname = "g", estimator = "all"
)


head(out)
out
plot_event_study(out, horizon = c(-3, 3))



##Robustness checks


##Including controls

X<-c("X45_RnD", "X58_ICT_access", "X59_ICT_use", "X57_ICT_tech", "X2_Human_capital_research_index", 
     "X64_Electricity_output","democracy_eiu", "X16_Rule_of_law",
     "X12_Government_effectiveness", "X9_Political_a_operational_stability", "X1_Institutions_index",
     "X3_Infrastructure_index",  "X64_Electricity_output",  
     "X8_Political_environment", "X15_Regulatory_quality", "X14_Regulatory_environment", "PopulationVaccinated")



# Erstelle einen String f?r die Formel
covariate_formula_string <- paste(X, collapse = " + ")

# F?ge die unabh?ngigen Variablen zu einer Formel zusammen
first_stage_formula <- as.formula(paste("~", covariate_formula_string, "| time + Country"))


did3gb<- did2s(data, yname= "Real.GDP.Growth",
               treatment= "treatment",
               cluster_var="Country",
               first_stage = first_stage_formula, 
               second_stage = ~ i(treatment, ref= FALSE),
               bootstrap = FALSE,
               n_bootstraps = 10000,
               return_bootstrap = FALSE,
               verbose = TRUE)

summary(did3gb)

##event-study

did4gb <- did2s(data,
                yname = "Real.GDP.Growth", treatment = "treatment", cluster_var = "Country",
                first_stage = first_stage_formula,
                second_stage = ~ i(rel_year, ref = c(0, Inf)),
                bootstrap = FALSE,
                n_bootstraps = 10000,
                return_bootstrap = FALSE,
                verbose = TRUE)

summary(did4gb)

fixest::iplot(
  did4gb, 
  main = "Event study: Staggered treatment", 
  xlab = "Relative time to treatment", 
  col = "steelblue", ref.line = -0,
  cluster = "Country"
)





##########################################

##Fiscal Measures

fiscal_spending<-c("ab_per", "on_per","health_per",
                    "nhealth_per","acc_per",
                   "of_per",	"be_per","gu_per",	"share_on_mo",	"share_nhealth_mo",
                   	"share_health_mo",		"share_acc_mo",	
                   "share_nhealth_on",	"share_health_on",	"share_of_mo",	"share_be_mo",
                  	"share_gu_mo",	"share_be_of",	"share_gu_of",	"total_intensity_per",
                   	"total_mo_per" )



run_analysis <- function(fiscal_spending) {
  
  # (3) Fixed Effects mit Panel-Daten
  did3 <- did2s(data = data,
                yname = fiscal_spending, 
                treatment = "treatment",
                cluster_var = "Country",
                first_stage = ~ 0 | time + Country, 
                second_stage = ~ i(treatment, ref = FALSE),
                bootstrap = FALSE,
                n_bootstraps = 10000,
                return_bootstrap = FALSE,
                verbose = TRUE)
  
  
  # (4) Event Study mit der neuen did2s Methode
  did4 <- did2s(data = data,
                yname = fiscal_spending, treatment = "treatment", cluster_var = "Country",
                first_stage = ~ 0 | time + Country,
                second_stage = ~ i(rel_year, ref = c(0, Inf)),
                bootstrap = FALSE,
                n_bootstraps = 10000,
                return_bootstrap = FALSE,
                verbose = TRUE)
  
  results_fs <- etable(did3, did4, 
                         cluster="Country", 
                         headers = c("Country + Yearly FE", "Event study"))
  
  return(results_fs)
}

results_list_fs <- lapply(fiscal_spending, run_analysis)

page(results_list_fs, method = "print")




df_plot <- data %>%
  group_by(Country, treatment) %>%
  summarise(
    Stringency_Avg = median(StringencyIndex_Average[Year %in% 2020:2021], na.rm = TRUE),
    GDP_Growth_Avg = mean(Real.GDP.Growth2[Year %in% 2020:2024], na.rm = TRUE),
    .groups = "drop"
  )

ggplot(df_plot, aes(x = Stringency_Avg, y = GDP_Growth_Avg, color = factor(treatment), label = Country)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.7, size = 3) +
  scale_color_manual(values = c("0" = "firebrick", "1" = "steelblue"),
                     labels = c("Control", "Treatment"),
                     name = "Gruppe") +
  labs(
    x = "Durchschnittlicher Stringency Index (2020-2022)",
    y = "Durchschnittliches reales BIP-Wachstum (2020-2024)",
    title = "Stringency vs. BIP-Wachstum nach Gruppe"
  ) +
  theme_minimal()





















###############################################################################
####################World Bank Data Count of Policies##########################
###############################################################################

csvdata<-read.csv("C:/Users/aulis/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/csv2.csv", header=TRUE, sep=",")

csvdata <- csvdata %>% rename(Year = Jahr.implementiert, Targetability.c=Targetability..converted., Speed.c=Speed..converted., Abuse_Resistance.c=Abuse_Resistance..converted.,
                              Affordability.c=Affordability..converted., Predictability_Cost_Control.c=Predictability_Cost_Control..converted., Reversibility.c=Reversibility..converted.,
                              Administrative_Complexity.c=Administrative_Complexity..converted., Scalability.c=Scalability..converted., Resilience_Health_Measures.c=Resilience_Health_Measures..converted.)


csvdata <- csvdata %>%
  filter(Country %in% oecd_countries)

##N/A mit o"other" ersetzen
csvdata <- csvdata %>%
  mutate(
    PolicyCategory = replace_na(PolicyCategory, "other"),
    PolicyCode = replace_na(PolicyCode, "other")
  )


csvdata <- csvdata %>%
  filter(!(PolicyCategory == 6 & PolicyCode == 34))
csvdata <- csvdata %>%
  filter(!(PolicyCategory == 9 & PolicyCode == 31))

###only first reponses
#csvdata<-subset(csvdata, Year==2020)

##adding key economic variables

data$GDP.per.Capita


extra_filtered <- data %>%
  filter(Year %in% c(2020, 2021)) %>%
  select(Country, Year, GDP.per.Capita, total_cases, total_deaths_per_million, total_tests, gov.effect., rule.of.law, control.of.corruption, 
         pol.stab.viole, regulatory.quality,total_intensity_per, StringencyIndex_Average, Population, Population.Density, Government.Debt.Ratio, total_intensity_per)


csvdata <- csvdata %>%
  left_join(extra_filtered, by = c("Country", "Year"))

csvdata <- csvdata %>%
  mutate(log_GDP = log(GDP.per.Capita))


csvdata <- csvdata %>%
  mutate(log_pop = log(Population))

csvdata <- csvdata %>%
  mutate(log_cases = log(total_cases))


##Graphical Interpretation of the Interpendency

# 1) Massnahmen pro Land (alle Jahre)
measures_per_country <- csvdata %>%
  group_by(Country) %>%
  summarise(n_measures = n(), .groups = "drop")

# 2) log-Variablen mitteln (nur 2020-2021)
logs_2020_2021 <- csvdata %>%
  filter(Year %in% c(2020, 2021)) %>%
  group_by(Country) %>%
  summarise(across(c(log_GDP, log_pop, log_cases, gov.effect.,rule.of.law, control.of.corruption, pol.stab.viole, regulatory.quality, Government.Debt.Ratio, total_intensity_per), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop")

# 3) Treatment pro Land (eindeutig)
treat_by_country <- csvdata %>%
  select(Country, Treatment) %>%
  distinct() %>%
  group_by(Country) %>%
  summarise(Treatment = dplyr::first(Treatment), .groups = "drop")
# falls Treatment je Land doch variiert, nimm stattdessen den Modus:
# summarise(Treatment = as.integer(names(sort(table(Treatment), TRUE))[1]))

# 4) Zusammenfuehren
measures_per_country <- measures_per_country %>%
  left_join(logs_2020_2021, by = "Country") %>%
  left_join(treat_by_country, by = "Country")


##variables on the x-achse
x_vars <- c("log_GDP", "log_pop", "log_cases", "gov.effect.",  "rule.of.law", "control.of.corruption", "pol.stab.viole", "regulatory.quality", "Government.Debt.Ratio", "total_intensity_per")

for (var in x_vars) {
  p <- ggplot(measures_per_country, aes_string(x = var, y = "n_measures")) +
    geom_point(aes(color = factor(Treatment)), size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, color = "black", size = 1.2) +
    geom_text_repel(aes(label = Country), size = 3, max.overlaps = 20) +
    labs(
      title = paste("Zusammenhang zwischen", var, "und Anzahl Massnahmen"),
      x = var,
      y = "Anzahl Massnahmen",
      color = "Treatment"
    ) +
    theme_minimal()
  
  print(p)  # <--- MUSS IN der Schleife stehen, nicht danach!
  
  ggsave(paste0("plot_", var, ".png"), p, width = 8, height = 6)
}

##checking for intensity between countries

## 1) Intensitaet pro Land zaehlen
##    (n() = alle Eintraege; wenn du nur eindeutige Policies willst: n_distinct(PolicyCode))
intensity <- csvdata %>%
  # optional nur 2020-2021:
  # filter(Year %in% c(2020, 2021)) %>%
  group_by(Country) %>%
  summarise(n_measures = n(), .groups = "drop")

## 2) Welt-Geometrie laden (ISO3 im Feld iso_a3)
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  dplyr::select(iso_a3, name_long, geometry)


##count measures

## 3) Join per ISO3 (Country)
map_df <- map_df %>%
  filter(iso_a3 != "ATA")

## 4) Plotten
ggplot(map_df) +
  geom_sf(aes(fill = n_measures), color = NA) +
  scale_fill_viridis(option = "C", direction = -1, na.value = "grey90",
                     name = "Anzahl Massnahmen") +
  labs(title = "COVID-19: Intensitaet der Massnahmen pro Land") +
  theme_void() +
  theme(legend.position = "right")

##total_intensity_per

# Join mit total_intensity_per (falls noch nicht gemacht)
map_df <- world %>%
  left_join(measures_per_country, by = c("iso_a3" = "Country")) %>%
  filter(iso_a3 != "ATA")   # Antarktis raus

# Plot mit Intensit?t
ggplot(map_df) +
  geom_sf(aes(fill = total_intensity_per), color = NA) +
  scale_fill_viridis(option = "C", direction = -1, na.value = "grey90",
                     name = "Intensit?t (total_intensity_per)") +
  labs(title = "COVID-19: Intensit?t der Massnahmen pro Land") +
  theme_void() +
  theme(legend.position = "right")

##RadarMap for both groups overlay



# Optional als PDF speichern:
pdf("radar_treatment_overlay.pdf", width = 8, height = 6)
# 1) Kriterien (converted)

crit_vars <- c(
  "Targetability.c","Speed.c","Abuse_Resistance.c","Affordability.c",
  "Predictability_Cost_Control.c","Reversibility.c","Scalability.c",
  "Administrative_Complexity.c","Resilience_Health_Measures.c"
)

rad_means <- csvdata %>%
  group_by(Treatment) %>%
  summarise(across(all_of(crit_vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  arrange(Treatment)

rad_mat <- rad_means %>% select(all_of(crit_vars))

## Option A: Immer bis 1 skalieren (empfohlen, da deine Werte < 1 sind)
max_vec <- rep(1, ncol(rad_mat))      # oberes Limit = 1 je Achse
min_vec <- rep(0, ncol(rad_mat))      # unteres Limit = 0

## Option B: Falls einzelne Achsen >1 haben: pro Achse mind. 1 nehmen
# max_vec <- pmax(1, sapply(rad_mat, function(x) max(x, na.rm = TRUE)))
# min_vec <- rep(0, ncol(rad_mat))

rad_fmsb <- as.data.frame(rbind(
  max = max_vec,
  min = min_vec,
  rad_mat
))
rownames(rad_fmsb) <- c("max","min", paste0("Treatment_", rad_means$Treatment))

cols  <- c("0" = "#1f77b4", "1" = "red")
fills <- alpha(cols, 0.30)
pcol_vec  <- unname(cols[as.character(rad_means$Treatment)])
pfcol_vec <- unname(fills[as.character(rad_means$Treatment)])

radarchart(
  rad_fmsb,
  pcol  = pcol_vec, pfcol = pfcol_vec, plwd = 1, plty = 1,
  pty   = 32,                            # keine Punkte, optional
  cglcol = "grey80", cglty = 1, cglwd = 0.8,
  axistype = 1, axislabcol = "grey40",
  seg = 5,                               # 5 Segmente zwischen 0 und 1
  caxislabels = sprintf("%.1f", seq(0, max(max_vec), length.out = 6)),
  vlcex = 0.8,
  title = "Radar: Treatment 0 vs. 1"
)

legend("topright",
       legend = c("Treatment 0","Treatment 1"),
       fill   = unname(fills[c("0","1")]),
       border = unname(cols[c("0","1")]),
       bty = "n", cex = 0.9)

# (obigen radarchart()-Block hier hinein kopieren)
dev.off()



###f?r jedes Land einzeln

# Durchschnitt pro Land
rad_means <- csvdata %>%
  group_by(Country, Treatment) %>%
  summarise(across(all_of(crit_vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

# Durchschnitt pro Gruppe (Treatment = 0 / 1)
group_means <- rad_means %>%
  group_by(Treatment) %>%
  summarise(across(all_of(crit_vars), mean, na.rm = TRUE), .groups = "drop")

# Loop ?ber L?nder
for (i in seq_len(nrow(rad_means))) {
  cn   <- rad_means$Country[i]
  tr   <- rad_means$Treatment[i]
  vals <- as.numeric(rad_means[i, crit_vars])
  
  # NA ersetzen
  vals[is.nan(vals)] <- NA
  vals[is.na(vals)]  <- 0
  
  # Gruppe Durchschnitt holen
  group_vals <- as.numeric(group_means[group_means$Treatment == tr, crit_vars])
  
  rad_fmsb <- as.data.frame(rbind(
    max = rep(1, length(crit_vars)),   # oberes Limit = 1
    min = rep(0, length(crit_vars)),   # unteres Limit = 0
    vals,
    group_vals
  ))
  colnames(rad_fmsb) <- crit_vars
  rownames(rad_fmsb) <- c("max", "min", cn, paste0("Group ", tr))
  
  radarchart(
    rad_fmsb,
    pcol = c("#1f77b4", "red"),                      # Blau = Land, Rot = Gruppe
    pfcol = c(alpha("#1f77b4", 0.3), NA),            # Gruppe nur als Linie
    plwd  = c(3, 1),                                 # Land dick, Gruppe fein
    plty  = c(1, 2),                                 # Gruppe gestrichelt
    cglcol = "grey80", cglty = 1, cglwd = 0.8,
    axistype = 1, axislabcol = "grey40",
    seg = 5,                                         # 5 Segmente = 0.2 Schritte
    caxislabels = c("0","0.2","0.4","0.6","0.8","1"),# bis 1.0 beschriften
    vlcex = 0.8,
    title = paste("Radar -", cn, "(Treatment:", tr, ")")
  )
  
  legend("topright", legend = c(cn, paste("Group", tr)),
         col = c("#1f77b4", "red"), lwd = c(3, 1), lty = c(1, 2), bty = "n")
  
  readline("Weiter mit [Enter] ...")
}



##Ausgabe als PDF


# Variablennamen fuer die Radarachsen
crit_vars <- c(
  "Targetability.c","Speed.c","Abuse_Resistance.c","Affordability.c",
  "Predictability_Cost_Control.c","Reversibility.c","Scalability.c",
  "Administrative_Complexity.c","Resilience_Health_Measures.c"
)

# Mittelwerte pro Land (Treatment separat)
rad_means <- csvdata %>%
  group_by(Country, Treatment) %>%
  summarise(across(all_of(crit_vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

# Mittelwerte pro Gruppe (zum Overlay)
group_means <- rad_means %>%
  group_by(Treatment) %>%
  summarise(across(all_of(crit_vars), mean, na.rm = TRUE), .groups = "drop")

# Sortierung der Ausgabe
rad_means_sorted <- rad_means %>% arrange(Treatment, Country)

pdf("radar_by_country_sorted.pdf", width = 10, height = 8)
op <- par(mfrow = c(2, 2), mar = c(1.2, 2, 2, 1.2))  # 2x2 Panels pro Seite

for (i in seq_len(nrow(rad_means_sorted))) {
  cn <- rad_means_sorted$Country[i]
  tr <- rad_means_sorted$Treatment[i]
  
  vals <- as.numeric(rad_means_sorted[i, crit_vars])
  vals[is.nan(vals)] <- NA
  vals[is.na(vals)]  <- 0
  
  grp_vals <- as.numeric(group_means[group_means$Treatment == tr, crit_vars])
  
  rad_df <- as.data.frame(rbind(
    max = rep(1, length(crit_vars)),   # Skala bis 1
    min = rep(0, length(crit_vars)),   # Skala ab 0
    vals,
    grp_vals
  ))
  colnames(rad_df) <- crit_vars
  rownames(rad_df) <- c("max", "min", cn, paste0("Group ", tr))
  
  # Farben je nach Treatment (WICHTIG: if/else komplett in EINEM Block)
  if (tr == 0) {
    land_col  <- "#1f77b4"                 # Blau
    land_fill <- alpha("#1f77b4", 0.3)
  } else {
    land_col  <- "red"                      # Rot
    land_fill <- alpha("red", 0.3)
  }
  
  radarchart(
    rad_df,
    pcol  = c(land_col, "black"),   # Land (blau/rot), Gruppe (schwarz)
    pfcol = c(land_fill, NA),       # Gruppe nur Linie
    plwd  = c(3, 1.5),
    plty  = c(1, 2),
    cglcol = "grey80", cglty = 1, cglwd = 0.8,
    axistype = 1, axislabcol = "grey40",
    seg = 5,                              # 0, 0.2, ..., 1.0
    caxislabels = c("0","0.2","0.4","0.6","0.8","1"),
    vlcex = 0.85,
    title = paste0("Radar - ", cn, "  (Treatment: ", tr, ")")
  )
  
  legend("topright",
         legend = c(cn, paste("Group", tr)),
         col = c(land_col, "black"),
         lwd = c(3, 1.5), lty = c(1, 2),
         bty = "n", cex = 0.9)
}

par(op)
dev.off()





################################################################################


##ein Level h?her

##Descriptives on and off Budget

offbudget_counts <- csvdata %>%
  group_by(Treatment, OffBudget) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(share = n / sum(n))   # Prozent pro Gruppe

print(offbudget_counts)


##normiert
# Laenderanzahl je Gruppe
n_countries <- csvdata %>%
  distinct(Country, Treatment) %>%
  count(Treatment, name = "n_countries")

offbudget_norm <- csvdata %>%
  group_by(Treatment, OffBudget) %>%
  summarise(n = n(), .groups = "drop") %>%
  left_join(n_countries, by = "Treatment") %>%
  mutate(avg_per_country = n / n_countries) %>%
  group_by(Treatment) %>%                                   # <- wichtig!
  mutate(share_per_group = avg_per_country / sum(avg_per_country)) %>%
  ungroup()

offbudget_norm

##PolicyCategory und PolicyCode Kombination genormt auf eine Landeseinheit

##simple descriptives
# --- 1) Anzahl L?nder pro Gruppe ---
n_countries <- csvdata %>%
  distinct(Country, Treatment) %>%
  count(Treatment, name = "n_countries")

print(n_countries)
# Erwartet: Treatment=0 -> 23, Treatment=1 -> 15

# --- 2) Zaehle Massnahmen pro Land und Kombination (PolicyCategory, PolicyCode) ---
country_combo_counts <- csvdata %>%
  group_by(Treatment, Country, PolicyCategory, PolicyCode) %>%
  summarise(n = n(), .groups = "drop")

# --- 3) Aggregiere auf Gruppen-Ebene und normiere auf Laenderzahl ---
combo_by_group <- country_combo_counts %>%
  group_by(Treatment, PolicyCategory, PolicyCode) %>%
  summarise(
    total_n        = sum(n),         # absolute Anzahl in der Gruppe
    countries_with = sum(n > 0),     # wie viele Laender haben mind. 1 Massnahme
    .groups = "drop"
  ) %>%
  left_join(n_countries, by = "Treatment") %>%
  mutate(
    avg_per_country = total_n / n_countries,     # normiert auf Gruppengroesse
    share_countries = countries_with / n_countries
  ) %>%
  select(Treatment, PolicyCategory, PolicyCode,
         total_n, n_countries, avg_per_country, share_countries) %>%
  arrange(Treatment, PolicyCategory, PolicyCode)

# --- 4) Pivot: Werte nebeneinander f?r T=0 und T=1 ---
# Pivot: Werte nebeneinander f?r T=0 und T=1
combo_wide <- combo_by_group %>%
  select(Treatment, PolicyCategory, PolicyCode, total_n, avg_per_country, share_countries) %>%
  pivot_wider(
    names_from  = Treatment,
    values_from = c(total_n, avg_per_country, share_countries),
    names_sep   = "_"
  ) %>%
  mutate(across(where(is.numeric), ~ coalesce(as.numeric(.), 0))) %>%
  # sicherstellen, dass Category/Code numerisch sortiert werden
  mutate(
    PolicyCategory = as.numeric(PolicyCategory),
    PolicyCode     = suppressWarnings(as.numeric(PolicyCode))
  ) %>%
  mutate(
    Total        = total_n_0 + total_n_1,
    Avg_per_Land = (avg_per_country_0 + avg_per_country_1) / 2,
    Share_avg    = (share_countries_0 + share_countries_1) / 2
  ) %>%
  arrange(PolicyCategory, PolicyCode)   # aufsteigend nach beiden

# Spalten umbenennen
combo_wide <- combo_wide %>%
  rename(
    "PolicyCategory"   = PolicyCategory,
    "PolicyCode"       = PolicyCode,
    "Count T=0"        = total_n_0,
    "? per Land T=0"   = avg_per_country_0,
    "Share T=0"        = share_countries_0,
    "Count T=1"        = total_n_1,
    "? per Land T=1"   = avg_per_country_1,
    "Share T=1"        = share_countries_1,
    "Total"            = Total,
    "? pro Land (avg)" = Avg_per_Land,
    "Share avg"        = Share_avg
  )


combo_results <- combo_wide %>%
  mutate(
    diff_count = `Count T=1` - `Count T=0`,
    diff_mean  = `? per Land T=1` - `? per Land T=0`,
    diff_share = `Share T=1` - `Share T=0`
  )

# Ausgabe in LaTeX (automatisch skaliert auf A4)
kable(combo_results,
      format   = "latex",
      booktabs = TRUE,
      digits   = 2,
      caption  = "PolicyCategory ? PolicyCode: Vergleich zwischen Treatment (1) und Control (0)"
) %>%
  kable_styling(latex_options = "scale_down")


head(combo_wide)
##stacked bar chart

bar_data <- csvdata %>%
  group_by(Country, PolicyCategory) %>%
  summarise(n = n(), .groups = "drop")

ggplot(bar_data, aes(x = Country, y = n, fill = factor(PolicyCategory))) +
  geom_bar(stat = "identity") +
  labs(x = "Land", y = "Anzahl Ma?nahmen", fill = "Policy Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

##Treemap



#Anzahl Massnahmen pro PolicyCategory und Gruppe nicht normiert
treemap_data <- csvdata %>%
  group_by(Treatment, PolicyCategory) %>%
  summarise(n = n(), .groups = "drop")

# Treemap zeichnen: nur PolicyCategory
ggplot(treemap_data, 
       aes(area = n,
           fill = factor(PolicyCategory),
           label = PolicyCategory)) +
  geom_treemap() +
  geom_treemap_text(colour = "black", place = "centre", reflow = TRUE, size = 12) +
  facet_wrap(~Treatment) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Treemap der COVID-19 Massnahmen",
       subtitle = "PolicyCategory je Gruppe (Treatment 0 vs. 1)",
       fill = "PolicyCategory") +
  theme_minimal()



# Absolute H?ufigkeiten pro Gruppe und Kategorie nich tnormiert mit Anzahl
treemap_data_abs <- csvdata %>%
  group_by(Treatment, PolicyCategory) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(label = paste0(PolicyCategory, "\n", n))

# Treemap mit absoluten Werten
ggplot(treemap_data_abs, 
       aes(area = n, 
           fill = factor(PolicyCategory),
           label = label)) +
  geom_treemap() +
  geom_treemap_text(colour = "black", 
                    place = "centre", 
                    reflow = TRUE, 
                    size = 12) +
  facet_wrap(~Treatment) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Treemap der COVID-19 Massnahmen",
       subtitle = "Absolute Anzahl Massnahmen je PolicyCategory und Treatment",
       fill = "PolicyCategory") +
  theme_minimal()


# Anzahl L?nder pro Gruppe bestimmen normiert
n_countries <- csvdata %>%
  distinct(Country, Treatment) %>%
  count(Treatment, name = "n_countries")

# Normierte H?ufigkeit pro PolicyCategory und Gruppe
treemap_data_norm <- csvdata %>%
  group_by(Treatment, PolicyCategory) %>%
  summarise(n = n(), .groups = "drop") %>%
  left_join(n_countries, by = "Treatment") %>%
  mutate(norm_per_country = n / n_countries,
         label = paste0(PolicyCategory, "\n", round(norm_per_country, 2)))

# Treemap mit normierten Werten
ggplot(treemap_data_norm, 
       aes(area = norm_per_country, 
           fill = factor(PolicyCategory),
           label = label)) +
  geom_treemap() +
  geom_treemap_text(colour = "black", 
                    place = "centre", 
                    reflow = TRUE, 
                    size = 12) +
  facet_wrap(~Treatment) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Treemap der COVID-19 Massnahmen",
       subtitle = "Normiert: Anzahl Massnahmen pro Land",
       fill = "PolicyCategory") +
  theme_minimal()



##beide in einem

# 1) Laenderanzahl je Gruppe
n_countries <- csvdata %>%
  distinct(Country, Treatment) %>%
  count(Treatment, name = "n_countries")

# 2) Normierte Massnahmen je Kategorie (pro Land)
treemap_data_norm <- csvdata %>%
  group_by(Treatment, PolicyCategory) %>%
  summarise(n = n(), .groups = "drop") %>%
  left_join(n_countries, by = "Treatment") %>%
  mutate(norm_per_country = n / n_countries,
         Treatment_lbl = ifelse(Treatment == 1, "Treatment (1)", "Control (0)"),
         label = paste0(PolicyCategory, "\n", round(norm_per_country, 2)))

# 3) Eine gemeinsame Treemap mit Subgruppen = Treatment
ggplot(
  treemap_data_norm,
  aes(
    area = norm_per_country,
    fill = factor(PolicyCategory),
    label = label,
    subgroup = Treatment_lbl
  )
) +
  geom_treemap() +
  geom_treemap_subgroup_border(color = "white", size = 2) +
  geom_treemap_subgroup_text(place = "bottom", grow = FALSE, colour = "grey15") +
  geom_treemap_text(colour = "black", place = "centre", reflow = TRUE, size = 10) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "COVID-19: normierte Massnahmen pro Land",
    subtitle = "Flaeche = Massnahmen je Kategorie pro Land; Subgruppen = Treatment/Control",
    fill = "PolicyCategory"
  ) +
  theme_minimal()


##PolicyCategory x PolicyCode

ggplot(combo_by_group, aes(x = interaction(PolicyCategory, PolicyCode), 
                           y = avg_per_country, 
                           fill = factor(Treatment))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
                    labels = c("Control (0)", "Treatment (1)")) +
  labs(
    x = "PolicyCategory ? PolicyCode",
    y = "? Massnahmen pro Land",
    fill = "Gruppe",
    title = "Durchschnittliche Massnahmen pro Kombination"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggplot(combo_by_group, aes(x = PolicyCode, y = PolicyCategory, 
                           fill = avg_per_country)) +
  geom_tile(color = "white") +
  facet_wrap(~ Treatment, ncol = 2) +
  scale_fill_gradient(low = "white", high = "darkred") +
  labs(
    title = "? Massnahmen pro Land und Kombination",
    subtitle = "Vergleich zwischen Control (0) und Treatment (1)",
    x = "PolicyCode", y = "PolicyCategory", fill = "? pro Land"
  ) +
  theme_minimal()

##Lollipop
# 1) Daten breit machen (T=0 und T=1 nebeneinander)
combo_diff <- combo_by_group %>%
  select(Treatment, PolicyCategory, PolicyCode, avg_per_country) %>%
  pivot_wider(
    names_from = Treatment,
    values_from = avg_per_country,
    names_prefix = "T"
  ) %>%
  mutate(
    diff = T1 - T0,
    combo = paste0("C", PolicyCategory, "-P", PolicyCode)  # lesbarer X-Name
  )

# 2) Lollipop Plot
ggplot(combo_diff, aes(x = combo)) +
  geom_segment(aes(xend = combo, y = T0, yend = T1), color = "grey70") +
  geom_point(aes(y = T0, color = "Control (0)"), size = 3) +
  geom_point(aes(y = T1, color = "Treatment (1)"), size = 3) +
  geom_text(aes(y = (T0 + T1) / 2, label = sprintf("%.2f", diff)),
            vjust = -0.5, size = 3.2, color = "black") +
  scale_color_manual(values = c("Control (0)" = "#1f77b4", "Treatment (1)" = "#ff7f0e")) +
  labs(
    title = "Lollipop-Plot: ? Massnahmen pro PolicyCategory ? PolicyCode",
    subtitle = "Differenz = Treatment - Control",
    x = "PolicyCategory ? PolicyCode",
    y = "? Massnahmen pro Land",
    color = "Gruppe"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

head(csvdata)

# 1) Daten vorbereiten (wie beim Lollipop)
combo_diff <- combo_by_group %>%
  select(Treatment, PolicyCategory, PolicyCode, avg_per_country) %>%
  pivot_wider(
    names_from = Treatment,
    values_from = avg_per_country,
    names_prefix = "T"
  ) %>%
  mutate(
    diff  = T1 - T0,
    combo = paste0("C", PolicyCategory, "-P", PolicyCode)  # Label
  )

# 2) Difference Barplot
ggplot(combo_diff, aes(x = reorder(combo, diff), y = diff, fill = diff > 0)) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "black", size = 0.4) +
  geom_text(aes(label = sprintf("%.2f", diff)),
            hjust = ifelse(combo_diff$diff > 0, -0.1, 1.1),
            color = "black", size = 3) +
  scale_fill_manual(values = c("TRUE" = "#ff7f0e", "FALSE" = "#1f77b4")) +
  labs(
    title = "Difference-Barplot: Treatment (1) - Control (0)",
    subtitle = "? Massnahmen pro PolicyCategory ? PolicyCode",
    x = "PolicyCategory ? PolicyCode",
    y = "Differenz (T1 - T0)"
  ) +
  coord_flip() +
  theme_minimal()

policy_map <- tribble(
  ~PolicyCode, ~PolicyCode_Desc, ~PolicyCategory, ~PolicyCategory_Desc,
  1, "Accelerated asset depreciation (CIT)", 1, "Revenue Measures to protect businesses",
  2, "Extend loss carry-forward (CIT)", 1, "Revenue Measures to protect businesses",
  3, "Broaden tax deductibility", 1, "Revenue Measures to protect businesses",
  4, "Tax credits", 1, "Revenue Measures to protect businesses",
  5, "Deferral of tax filing", 1, "Revenue Measures to protect businesses",
  6, "Deferral of tax payments", 1, "Revenue Measures to protect businesses",
  7, "Tax rate reduction", 1, "Revenue Measures to protect businesses",
  8, "Tax amnesty / incentives", 1, "Revenue Measures to protect businesses",
  9, "Accelerating refunds", 1, "Revenue Measures to protect businesses",
  10, "Lower advance payment", 1, "Revenue Measures to protect businesses",
  11, "Suspend debt collection", 1, "Revenue Measures to protect businesses",
  12, "Suspend audit activities", 1, "Revenue Measures to protect businesses",
  13, "Deferral of tax filing", 2, "Revenue Measures to protect individuals",
  14, "Deferral of tax payments", 2, "Revenue Measures to protect individuals",
  15, "Tax rate reduction", 2, "Revenue Measures to protect individuals",
  16, "Tax amnesty / incentives", 2, "Revenue Measures to protect individuals",
  17, "Broaden tax deductibility", 2, "Revenue Measures to protect individuals",
  18, "Tax credits", 2, "Revenue Measures to protect individuals",
  19, "Lower tax rates for medical items", 3, "Revenue Measures to promote availability of medical items",
  20, "Lower tax rates", 4, "Revenue Measures to boost consumption / demand",
  21, "Supply of low cost medical items", 5, "Health Expenditures measures",
  22, "Supply of high cost medical items", 5, "Health Expenditures measures",
  23, "Targeted infrastructure investments", 5, "Health Expenditures measures",
  24, "Expansion of human resources", 5, "Health Expenditures measures",
  25, "Direct cash transfers for individuals", 6, "Expenditure Measures for cash transfers to individuals",
  26, "Expansion of unemployment benefits", 6, "Expenditure Measures for cash transfers to individuals",
  27, "Temporary expansion of existing benefits", 6, "Expenditure Measures for cash transfers to individuals",
  28, "Supplementary ad hoc programs", 6, "Expenditure Measures for cash transfers to individuals",
  29, "Wage compensation/ enhanced paid leave", 6, "Expenditure Measures for cash transfers to individuals",
  30, "Preferential loans to firms", 7, "Credit and equity measures",
  31, "One-off grants", 7, "Credit and equity measures",
  32, "Preferential loans to households", 7, "Credit and equity measures",
  33, "Revenue increase", 8, "Revenue Measures to raise revenue",
  34, "Income support", 9, "Expenditure Measures for businesses"
)

# Mapping-Vektor: PolicyCode als Name, Beschreibung als Wert
policy_labels <- setNames(policy_map$PolicyCode_Desc, policy_map$PolicyCode)


combo_diff <- combo_diff %>%
  group_by(PolicyCode) %>%
  summarise(diff = mean(diff, na.rm = TRUE), .groups = "drop")
# Im Plot die Labels direkt ersetzen
p<-ggplot(combo_diff, aes(x = reorder(PolicyCode, diff), y = diff, fill = diff > 0)) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "black", size = 0.4) +
  geom_text(aes(label = sprintf("%.2f", diff)),
            hjust = ifelse(combo_diff$diff > 0, -0.1, 1.1),
            color = "black", size = 3) +
  scale_fill_manual(values = c("TRUE" = "#ff7f0e", "FALSE" = "#1f77b4")) +
  scale_x_discrete(labels = policy_labels) +   # <<-- Labels ersetzen
  labs(
    title = "Difference-Barplot: Treatment (1) - Control (0)",
    subtitle = "? Massnahmen pro PolicyCategory ? PolicyCode",
    x = "PolicyCode",
    y = "Differenz (T1 - T0)"
  ) +
  coord_flip() +
  theme_minimal()

ggsave("diff_barplot.pdf", plot = p, width = 10, height = 6)

##evtl. noch die Category davor
##T-Test Category und Code



combo_tests <- combo_wide %>%
  rowwise() %>%
  mutate(
    test = list(
      tryCatch(
        t.test(
          x = rep(1, round(`? per Land T=0` * 100)),  # Dummy-Werte f?r Gruppe 0
          y = rep(1, round(`? per Land T=1` * 100))   # Dummy-Werte f?r Gruppe 1
        ),
        error = function(e) NULL
      )
    )
  ) %>%
  mutate(test = if (!is.null(test)) list(tidy(test)) else list(NULL)) %>%
  unnest(test, keep_empty = TRUE)



print(combo_tests, n=Inf, width = Inf)







##function for t-tests
run_policy_ttests <- function(data,
                              cats = NULL,          # z.B. c(6,7) fuer Kategorien 6 und 7
                              codes = NULL) {       # optional: z.B. c("25","29","other")
  # 1) vorbereiten
  df <- data %>%
    mutate(
      PolicyCategory = as.character(PolicyCategory),
      PolicyCode     = as.character(PolicyCode),
      PolicyCode     = ifelse(is.na(PolicyCode), "other", PolicyCode),
      PolicyKey      = paste0("C", PolicyCategory, "_", PolicyCode)
    )
  
  # 2) filtern nach gewuenschten Kategorien/Codes
  if (!is.null(cats))  df <- df %>% filter(PolicyCategory %in% as.character(cats))
  if (!is.null(codes)) df <- df %>% filter(PolicyCode %in% as.character(codes))
  
  # falls nach dem Filtern nichts uebrig ist
  if (nrow(df) == 0) return(tibble())
  
  # 3) pro Country ? Treatment ? PolicyKey zaehlen
  counts_cty <- df %>%
    count(Country, Treatment, PolicyKey, name = "count")
  
  # 4) fehlende Laender als 0 fuellen (pro Treatment und PolicyKey)
  all_cty_trt <- df %>% distinct(Country, Treatment)
  all_keys    <- df %>% distinct(PolicyKey)
  counts_cty0 <- all_cty_trt %>%
    crossing(all_keys) %>%
    left_join(counts_cty, by = c("Country","Treatment","PolicyKey")) %>%
    mutate(count = tidyr::replace_na(count, 0L))
  
  # 5) Gruppendurchschnitte (Kontrolle: entspricht ? pro Land)
  means_by_trt <- counts_cty0 %>%
    group_by(PolicyKey, Treatment) %>%
    summarise(mean_per_country = mean(count),
              n_countries = n(), .groups = "drop") %>%
    pivot_wider(names_from = Treatment, values_from = c(mean_per_country, n_countries),
                names_prefix = "T")
  
  # 6) sicherer t-Test pro Kombination (nur wenn moeglich)
  safe_t <- function(df_key) {
    n0 <- sum(df_key$Treatment == 0); n1 <- sum(df_key$Treatment == 1)
    v0 <- var(df_key$count[df_key$Treatment == 0])
    v1 <- var(df_key$count[df_key$Treatment == 1])
    if (n0 > 1 && n1 > 1 && (is.finite(v0) && v0 > 0 || is.finite(v1) && v1 > 0)) {
      tidy(t.test(count ~ Treatment, data = df_key))
    } else {
      tibble(estimate = NA_real_, statistic = NA_real_, p.value = NA_real_,
             conf.low = NA_real_, conf.high = NA_real_, parameter = NA_real_,
             method = NA_character_, alternative = NA_character_)
    }
  }
  
  ttests <- counts_cty0 %>%
    group_by(PolicyKey) %>%
    group_modify(~ safe_t(.x)) %>%
    ungroup()
  
  # 7) zusammenfuehren und p-Werte innerhalb der getesteten Menge adjustieren
  results <- means_by_trt %>%
    mutate(diff_mean = mean_per_country_T1 - mean_per_country_T0) %>%
    left_join(ttests, by = "PolicyKey") %>%
    mutate(p_raw = p.value,
           p_adj = p.adjust(p_raw, method = "BH")) %>%
    select(PolicyKey,
           mean_per_country_T0, mean_per_country_T1, diff_mean,
           n_countries_T0, n_countries_T1,
           p_raw, p_adj, conf.low, conf.high, statistic, parameter, method)
  
  return(results)
}


##Nur eine Kategorie  testen
res_cat6 <- run_policy_ttests(csvdata, cats = 6)

print(res_cat6, n=Inf, width = Inf)


##mehree Kategorien Testen
res_cat67 <- run_policy_ttests(csvdata, cats = c(6,7))

print(res_cat67)


##Nur bestimmte Code in Categorys testen
res_cat6_subset <- run_policy_ttests(csvdata, cats = 6, codes = c("25","29","other"))

print(res_cat6_subset)



































###############################################################################
#####################################criterias#################################
###############################################################################



##T-Test f?r alle Kombinationen

# 1) Z?hle pro Land & Kombination (PolicyCategory ? PolicyCode)
country_combo_counts <- csvdata %>%
  group_by(Treatment, Country, PolicyCategory, PolicyCode) %>%
  summarise(n = n(), .groups = "drop")

# 2) F?r alle L?nder sicherstellen, dass fehlende Kombinationen als 0 gez?hlt werden
all_combos <- expand.grid(
  Country = unique(csvdata$Country),
  PolicyCategory = unique(csvdata$PolicyCategory),
  PolicyCode = unique(csvdata$PolicyCode)
)

country_combo_full <- all_combos %>%
  left_join(distinct(csvdata, Country, Treatment), by = "Country") %>%
  left_join(country_combo_counts, 
            by = c("Country", "Treatment", "PolicyCategory", "PolicyCode")) %>%
  mutate(n = ifelse(is.na(n), 0, n))

# 3) T-Test je Kombination
t_test_results <- country_combo_full %>%
  group_by(PolicyCategory, PolicyCode) %>%
  summarise(
    ttest = list(t.test(n ~ Treatment)),
    .groups = "drop"
  ) %>%
  mutate(
    p.value = sapply(ttest, function(x) x$p.value),
    estimate_group0 = sapply(ttest, function(x) x$estimate[1]),  # Mean Control
    estimate_group1 = sapply(ttest, function(x) x$estimate[2]),  # Mean Treatment
    diff = estimate_group1 - estimate_group0
  ) %>%
  select(PolicyCategory, PolicyCode, estimate_group0, estimate_group1, diff, p.value)

# 4) Ergebnisse sortieren (kleinster p-Wert zuerst)
t_test_results <- t_test_results %>%
  arrange(p.value)

print(t_test_results, n=Inf)

##datenset nur  mit signifikanten Kombinationen erstellen


library(dplyr)

# gew?nschte Kombinationen - alles als character
wanted_combos <- tribble(
  ~PolicyCategory, ~PolicyCode,
  "6", "25",
  "5", "24",
  "5", "23",
  "6", "28",
  "1", "other",
  "1", "11",
  "6", "27",
  "3", "19"
)

# sicherstellen, dass im Datensatz auch Strings stehen
csvdata <- csvdata %>%
  mutate(
    PolicyCategory = as.character(PolicyCategory),
    PolicyCode     = as.character(PolicyCode)
  )

# Filter anwenden
subset_data <- csvdata %>%
  inner_join(wanted_combos,
             by = c("PolicyCategory", "PolicyCode"))

# ?bersicht pro Kombination
subset_data %>%
  count(PolicyCategory, PolicyCode)



##############

############

##
##















##FacetGrid

ggplot(bar_data, aes(x = Country, y = n, fill = factor(PolicyCategory))) +
  geom_col() +
  facet_wrap(~ PolicyCategory, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




















###Auswertung nach Kriterien (unconverted)

vars <- c("Targetability.c","Speed.c","Abuse_Resistance.c","Affordability.c",
          "Predictability_Cost_Control.c","Reversibility.c","Scalability.c",
          "Administrative_Complexity.c","Resilience_Health_Measures.c")

table_all<- csvdata %>%
  group_by(Treatment) %>%
  summarise(
    across(all_of(vars),
           list(mean = ~mean(.x, na.rm=TRUE),
                sd   = ~sd(.x, na.rm=TRUE),
                med  = ~median(.x, na.rm=TRUE)),
           .names = "{.col}_{.fn}")
  )

print(table_all, width = Inf)


##T-Test Standard

Resilience_Health_Measures.c

x <- subset(csvdata, Treatment == 0)[["Targetability.c"]]
y <- subset(csvdata, Treatment == 1)[["Targetability.c"]]

t.test(x, y, var.equal = FALSE, alternative = "less") 
wilcox.test(x, y, alternative = "less", exact = FALSE)




# Anteilstest (zweiseitig; fuer einseitig: alternative="greater"/"less")
x1 <- sum(csvdata$Targetability.c[csvdata$Treatment == 1] == 1, na.rm=TRUE)
n1 <- sum(!is.na(csvdata$Targetability.c[csvdata$Treatment == 1]))
x0 <- sum(csvdata$Targetability.c[csvdata$Treatment == 0] == 1, na.rm=TRUE)
n0 <- sum(!is.na(csvdata$Targetability.c[csvdata$Treatment == 0]))

prop.test(x = c(x1, x0), n = c(n1, n0), alternative = "greater")  # H1: T1 > T0


#ohne Kontinuit?tstest
prop.test(c(x1, x0), c(n1, n0), alternative = "greater", correct = FALSE)

##FisherTest
M <- matrix(c(x1, n1 - x1, x0, n0 - x0), nrow = 2, byrow = TRUE)
fisher.test(M, alternative = "greater")


##Logit mit 90%-CI
se <- sqrt(vcovHC(m, type="HC1")["Treatment","Treatment"])
beta <- coef(m)["Treatment"]
ci90 <- beta + c(-1.645, 1.645)*se
exp(ci90)  # 90%-CI der OR



# Logistische Regression mit robusten SE (und optionalen Kovariaten)
library(sandwich)
library(lmtest)

m <- glm(Targetability.c ~ Treatment, data = csvdata, family = binomial)
coeftest(m, vcov = vcovHC(m, type="HC1"))             # robuste SE

# Cluster-robuste SE (z. B. nach Country)
# vcovCL erfordert einen Cluster-Vektor gleicher Laenge wie Daten
# coeftest(m, vcov = vcovCL(m, cluster = ~ Country, type = "HC1"))

# Interpretierbar machen: Odds Ratio + CI
or  <- exp(coef(m)["Treatment"])
ci  <- exp(confint(m, parm="Treatment"))
c(OR = or, LCL = ci[1], UCL = ci[2])


###################Alles Zusammen#########################
# Zusammenfassung in einem tibble
library(dplyr); library(sandwich); library(lmtest)

# Anteile
p1 <- x1 / n1; p0 <- x0 / n0
diff <- p1 - p0

# prop.test
pt_corr  <- prop.test(c(x1,x0), c(n1,n0), alternative="greater", correct=TRUE)
pt_noc   <- prop.test(c(x1,x0), c(n1,n0), alternative="greater", correct=FALSE)

# Fisher
M <- matrix(c(x1, n1 - x1, x0, n0 - x0), nrow=2, byrow=TRUE)
ft <- fisher.test(M, alternative="greater")

# Logit mit robusten SE
m  <- glm(Targetability.c ~ Treatment, data=csvdata, family=binomial)
rob <- vcovHC(m, type="HC1")
z   <- coeftest(m, vcov=rob)["Treatment", "z value"]
p1s_logit <- pnorm(z, lower.tail=FALSE)  # einseitig T1>T0
OR <- exp(coef(m)["Treatment"])
se <- sqrt(rob["Treatment","Treatment"])
ci90 <- exp(coef(m)["Treatment"] + c(-1.645, 1.645)*se)

tibble::tibble(
  n1 = n1, n0 = n0, p1 = p1, p0 = p0, diff = diff,
  prop_p_greater_corr = pt_corr$p.value,
  prop_p_greater_noc  = pt_noc$p.value,
  fisher_p_greater    = ft$p.value,
  logit_OR = OR, logit_OR_90L = ci90[1], logit_OR_90U = ci90[2],
  logit_p_one_sided   = p1s_logit
)



















library(effsize)

cohen.d(y, x, hedges.correction = TRUE)   # y = T1, x = T0

scatterplot(StringencyIndex_Average, Real.GDP.Growth2)


# Differenz der Anteile
p0 <- mean(x, na.rm = TRUE); n0 <- sum(!is.na(x))
p1 <- mean(y, na.rm = TRUE); n1 <- sum(!is.na(y))
diff <- p1 - p0

# Cohen's h (fuer Anteile)
h <- 2*asin(sqrt(p1)) - 2*asin(sqrt(p0))

# Zwei-Stichproben-Test fuer Anteile (Alternative zum t-Test)
prop.test(x = c(sum(y, na.rm=TRUE), sum(x, na.rm=TRUE)),
          n = c(n1, n0), correct = FALSE)







csvdata %>%
  group_by(Treatment) %>%
  summarise(
    n        = sum(!is.na(Targetability)),
    mean     = mean(Targetability, na.rm = TRUE),
    sd       = sd(Targetability, na.rm = TRUE),
    median   = median(Targetability, na.rm = TRUE),
    iqr      = IQR(Targetability, na.rm = TRUE),
    min      = min(Targetability, na.rm = TRUE),
    max      = max(Targetability, na.rm = TRUE)
  )


csvdata %>%
  group_by(Treatment) %>%
  summarise(
    n        = sum(!is.na(Targetability..converted.)),
    mean     = mean(Targetability..converted., na.rm = TRUE),
    sd       = sd(Targetability..converted., na.rm = TRUE),
    median   = median(Targetability..converted., na.rm = TRUE),
    iqr      = IQR(Targetability..converted., na.rm = TRUE),
    min      = min(Targetability..converted., na.rm = TRUE),
    max      = max(Targetability..converted., na.rm = TRUE)
  )















write.csv(csvdata, ("C:/Users/aulis/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Master-Thesis/New/Working/output r/policies.csv"), row.names = FALSE)


####################END######################################################

