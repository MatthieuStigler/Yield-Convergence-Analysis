********************************************************************************************
* This was the old data that had limited countries
*import delimited using "../dataRaw/data_from_AFRI_AERC/data_merged_ALL_Cntry_Commod_Year_mAd.csv", clear
*destring fao* dai* cru*, replace ignore("NaN" "NA")
********************************************************************************************

import delimited using "../dataRaw/data_from_AFRI_AERC/data_merged_ALL_Cntry_Commod_Year_outerJoin_mAd.csv", clear
destring fao* dai* cru*, replace ignore("NaN" "NA")
***************** Notes on the weather data *******************************
* Spatial aggregation was done by 3 different weighting methods
* 1) "w_fin" The proportion of each weather grid in the country border.
* 2) "w_finarea10" The proportion of weather grid in country for those grids with 
*                  production of the crop
* 3) "w_finarea" The proportion of the weather grid in country times the area with
*                production of the crop in that grid 
* Only have "w_finarea10" for "ave3" temporal aggregation
*
* Temporal aggreation was done by 3 different methods
* 1) "ave1" Average of the previous 12 month, starting from the harvest month.
* 2) "ave2" 12 lags of monthly weather data, starting from harvest month
* 3) "ave3" Average of all months between planting and harvest

keep if commodity=="maize" | commodity=="wheat" | commodity=="rice"
replace commodity="Maize" if commodity=="maize"
replace commodity="Wheat" if commodity=="wheat"
replace commodity="Rice" if commodity=="rice"


drop if fao3__yield==. 

save "../temp/revisedMatthieu_regression_data", replace


import delimited using "../dataRaw/data_from_AFRI_AERC/data_merged_ALL_Cntry_Year_mAd.csv", clear
destring fao* dai* fug* incr* pol* wdi*, replace ignore("NaN" "NA")
drop incr*
drop pol*
save "../temp/Cntry_Year_data", replace


* Production data directly from FAO (has more countries than Matthieu's data)
import delimited using "../dataRaw/Production_Crops_E_All_Data.csv", clear
tab item
replace item="Rice" if item=="Rice, paddy"
keep if item=="Wheat" | item=="Maize" | item=="Rice" | item=="Soybeans"
replace element="area" if element=="Area harvested"
replace element="yield" if element=="Yield"
replace element="production" if element=="Production"
keep if element=="area" | element=="yield" | element=="production"
drop y*f
drop unit elementcode itemcode
ren areacode countrycode
ren area country
reshape long y, i(countrycode country item element) j(year)
reshape wide y, i(countrycode country item year) j(element) string
ren yarea area
ren yproduction production
ren yyield yield
ren item commodity
merge m:1 countrycode using "../temp/FAOSTAT_countrycodes", keep(match master)
tab country if _merge==1
drop if _merge==1
drop _merge
tab country
drop if country=="World"
drop if country=="Africa"
drop if country=="Americas"
drop if iso3==""
* FAO data lists China and China, mainland
drop if iso3=="CPR"
drop if yield==. | area==. | production==.

save "../temp/FAO_crop_production", replace


* Polity Data
import delimited using "../dataRaw/iso3_Country_list.csv", clear varnames(1)
ren ïiso3 iso3
save "..\temp\iso3_Country_list", replace

import excel using "../dataRaw/p4v2016.xls", sheet("p4v2016") firstrow clear
keep if year>=1961 & year<=2011
* Use Germany West for Germany before 1990
drop if country=="Germany West" & year==1990
replace country="Germany" if country=="Germany West"
* Use Vientam North for Vietnam before 1976
drop if country=="Vietnam North" & year==1976
replace country="Vietnam" if country=="Vietnam North"
merge m:1 country using "..\temp\iso3_Country_list"
sort country year
* Manually define iso3 codes for those countries that did not merge correctly
replace iso3="TWN" if country=="Taiwan"
replace iso3="CIV" if country=="Ivory Coast"
replace iso3="KOR" if country=="Korea South"
replace iso3="RUS" if country=="Russia"
replace iso3="SVK" if country=="Slovak Republic"
replace iso3="TZA" if country=="Tanzania"
replace iso3="VNM" if country=="Vietnam"
tab country if _merge==1
tab country if _merge==2
drop if _merge==2
drop if iso3==""
drop _merge
duplicates report iso3 year
duplicates drop iso3 year, force
save "../temp/p4v2016", replace

* Penn World Table data
use "../dataRaw/pwt90", clear
ren countrycode iso3
save "../temp/pwt90_iso3", replace


* GAEZ data
foreach j in int low {
import delimited using "../dataRaw/GAEZ/maize_rain_`j'_1961-1990.csv", clear
merge m:1 country using "../temp/FAOSTAT_countrycodes", keep(match) nogen
keep countrycode iso3 country weightedaverage
gen commodity="Maize"
ren weightedaverage potent_rain_`j'
save "../temp/maize_rain_`j'_1961-1990", replace
}

foreach j in high int {
import delimited using "../dataRaw/GAEZ/maize_irr_`j'_1961-1990.csv", clear 
merge m:1 country using "../temp/FAOSTAT_countrycodes", keep(match) nogen
keep countrycode iso3 country weightedaverage
gen commodity="Maize"
ren weightedaverage potent_irr_`j'
save "../temp/maize_irr_`j'_1961-1990", replace
}

foreach j in high int low {
import delimited using "../dataRaw/GAEZ/wheat_rain_`j'_1961-1990.csv", clear
merge m:1 country using "../temp/FAOSTAT_countrycodes", keep(match) nogen
keep countrycode iso3 country weightedaverage
gen commodity="Wheat"
ren weightedaverage potent_rain_`j'
save "../temp/wheat_rain_`j'_1961-1990", replace
}

foreach j in high int {
import delimited using "../dataRaw/GAEZ/wheat_irr_`j'_1961-1990.csv", clear
merge m:1 country using "../temp/FAOSTAT_countrycodes", keep(match) nogen
keep countrycode iso3 country weightedaverage
gen commodity="Wheat"
ren weightedaverage potent_irr_`j'
save "../temp/wheat_irr_`j'_1961-1990", replace
}

foreach j in irr irrrain rain {
import delimited using "../dataRaw/GAEZ/ygap_maize_`j'.csv", clear rowrange(12:) varnames(12) 
merge m:1 country using "../temp/FAOSTAT_countrycodes", keep(match) nogen
egen total_pixels=rowtotal(v3-v9)
gen ygapGAEZ_`j'=(v8*0.05+v3*.175+v4*0.325+v5*0.475+v6*0.625+v7*0.775+v9*0.925)/total_pixels
keep countrycode iso3 country ygapGAEZ*
gen commodity="Maize"
save "../temp/ygapGAEZ_maize_`j'", replace
}

foreach j in irr irrrain rain {
import delimited using "../dataRaw/GAEZ/ygap_wheat_`j'.csv", clear rowrange(12:) varnames(12) 
merge m:1 country using "../temp/FAOSTAT_countrycodes", keep(match) nogen
egen total_pixels=rowtotal(v3-v9)
gen ygapGAEZ_`j'=(v8*0.05+v3*.175+v4*0.325+v5*0.475+v6*0.625+v7*0.775+v9*0.925)/total_pixels
keep countrycode iso3 country ygapGAEZ*
gen commodity="Wheat"
save "../temp/ygapGAEZ_wheat_`j'", replace
}

foreach j in irr irrrain rain {
import delimited using "../dataRaw/GAEZ/ygap_rice_`j'.csv", clear rowrange(12:) varnames(12) 
merge m:1 country using "../temp/FAOSTAT_countrycodes", keep(match) nogen
egen total_pixels=rowtotal(v3-v9)
gen ygapGAEZ_`j'=(v8*0.05+v3*.175+v4*0.325+v5*0.475+v6*0.625+v7*0.775+v9*0.925)/total_pixels
keep countrycode iso3 country ygapGAEZ*
gen commodity="Rice"
save "../temp/ygapGAEZ_rice_`j'", replace
}

use "../temp/maize_rain_int_1961-1990", clear
merge 1:1 iso3 using "../temp/maize_rain_low_1961-1990", nogen
merge 1:1 iso3 using "../temp/maize_irr_high_1961-1990", nogen
merge 1:1 iso3 using "../temp/maize_irr_int_1961-1990", nogen
save "../temp/maize_yield_potential", replace

use "../temp/wheat_rain_high_1961-1990", clear
merge 1:1 iso3 using "../temp/wheat_rain_int_1961-1990", nogen
merge 1:1 iso3 using "../temp/wheat_rain_low_1961-1990", nogen
merge 1:1 iso3 using "../temp/wheat_irr_high_1961-1990", nogen
merge 1:1 iso3 using "../temp/wheat_irr_int_1961-1990", nogen
save "../temp/wheat_yield_potential", replace

use "../temp/ygapGAEZ_maize_irr", clear
merge 1:1 iso3 using "../temp/ygapGAEZ_maize_irrrain", nogen
merge 1:1 iso3 using "../temp/ygapGAEZ_maize_rain", nogen
save "../temp/ygapGAEZ_maize", replace

use "../temp/ygapGAEZ_wheat_irr", clear
merge 1:1 iso3 using "../temp/ygapGAEZ_wheat_irrrain", nogen
merge 1:1 iso3 using "../temp/ygapGAEZ_wheat_rain", nogen
save "../temp/ygapGAEZ_wheat", replace

use "../temp/ygapGAEZ_rice_irr", clear
merge 1:1 iso3 using "../temp/ygapGAEZ_rice_irrrain", nogen
merge 1:1 iso3 using "../temp/ygapGAEZ_rice_rain", nogen
save "../temp/ygapGAEZ_rice", replace


* Caloric Suitability Index from Galor and Ozak (2016) available at: https://ozak.github.io/Caloric-Suitability-Index/
use "../dataRaw/GAEZ/country_Calories_stats_web", clear
ren ISO_A3 iso3
keep iso3 post1500AverageCaloriesmean post1500AverageCalories0mean
save "../temp/country_Calories_potential", replace


* R&D Expenditure 
import excel using "../dataRaw/RDexpenditures/DATA_Ag RD Expenditure and Stock.xlsx", sheet("PUBLIC RD STOCK 25y lag") clear cellrange(A3:FR63) firstrow
drop in 1/26
foreach v of varlist B-FR {
   local x : variable label `v'
   rename `v' rd25lag`x'
}
ren FAON year
reshape long rd25lag, i(year) j(countrycode)
sort countrycode year
destring year, replace
destring rd25lag, replace
save "../temp/rd25lag", replace


* ICRG Political Risk Variables
import excel using "E:/Institutions/ICRG Data/3BResearchersDataset2020.xls", sheet("A-Government Stability") cellrange(A8:AK154) firstrow clear
ren Country country
replace country="United States of America" if country=="United States"
replace country="Democratic Republic of the Congo" if country=="Congo, DR"
replace country="Bolivia (Plurinational State of)" if country=="Bolivia"
replace country="CÃ´te d'Ivoire" if country=="Cote d'Ivoire"
replace country="Democratic People's Republic of Korea" if country=="Korea, DPR"
replace country="China, mainland" if country=="China"
replace country="United Republic of Tanzania" if country=="Tanzania"
replace country="Venezuela (Bolivarian Republic of)" if country=="Venezuela"
replace country="Viet Nam" if country=="Vietnam"
replace country="China, Taiwan Province of" if country=="Taiwan"
replace country="Republic of Korea" if country=="South Korea"
replace country="Iran (Islamic Republic of)" if country=="Iran"
replace country="Syrian Arab Republic" if country=="Syria"
merge 1:1 country using "../temp/FAOSTAT_countrycodes", keep(match master)
tab country if _merge==1
drop if _merge==1
drop _merge
foreach v of varlist B-AK {
   destring `v', replace
   local x : variable label `v'
   rename `v' gov_stab_icrg`x'
}
reshape long gov_stab_icrg, i(country countrycode iso3) j(year)
save "E:/Institutions/ICRG Data/stata/a_gov_stab", replace

import excel using "E:/Institutions/ICRG Data/3BResearchersDataset2020.xls", sheet("B-Socioeconomic Conditions") cellrange(A8:AK154) firstrow clear
ren Country country
replace country="United States of America" if country=="United States"
replace country="Democratic Republic of the Congo" if country=="Congo, DR"
replace country="Bolivia (Plurinational State of)" if country=="Bolivia"
replace country="CÃ´te d'Ivoire" if country=="Cote d'Ivoire"
replace country="Democratic People's Republic of Korea" if country=="Korea, DPR"
replace country="China, mainland" if country=="China"
replace country="United Republic of Tanzania" if country=="Tanzania"
replace country="Venezuela (Bolivarian Republic of)" if country=="Venezuela"
replace country="Viet Nam" if country=="Vietnam"
replace country="China, Taiwan Province of" if country=="Taiwan"
replace country="Republic of Korea" if country=="South Korea"
replace country="Iran (Islamic Republic of)" if country=="Iran"
replace country="Syrian Arab Republic" if country=="Syria"
merge 1:1 country using "../temp/FAOSTAT_countrycodes", keep(match) nogen
foreach v of varlist B-AK {
   destring `v', replace
   local x : variable label `v'
   rename `v' socio_cond_icrg`x'
}
reshape long socio_cond_icrg, i(country countrycode iso3) j(year)
save "E:/Institutions/ICRG Data/stata/b_socio_cond", replace

import excel using "E:/Institutions/ICRG Data/3BResearchersDataset2020.xls", sheet("C-Investment Profile") cellrange(A8:AK154) firstrow clear
ren Country country
replace country="United States of America" if country=="United States"
replace country="Democratic Republic of the Congo" if country=="Congo, DR"
replace country="Bolivia (Plurinational State of)" if country=="Bolivia"
replace country="CÃ´te d'Ivoire" if country=="Cote d'Ivoire"
replace country="Democratic People's Republic of Korea" if country=="Korea, DPR"
replace country="China, mainland" if country=="China"
replace country="United Republic of Tanzania" if country=="Tanzania"
replace country="Venezuela (Bolivarian Republic of)" if country=="Venezuela"
replace country="Viet Nam" if country=="Vietnam"
replace country="China, Taiwan Province of" if country=="Taiwan"
replace country="Republic of Korea" if country=="South Korea"
replace country="Iran (Islamic Republic of)" if country=="Iran"
replace country="Syrian Arab Republic" if country=="Syria"
merge 1:1 country using "../temp/FAOSTAT_countrycodes", keep(match) nogen
foreach v of varlist B-AK {
   destring `v', replace
   local x : variable label `v'
   rename `v' inv_prof_icrg`x'
}
reshape long inv_prof_icrg, i(country countrycode iso3) j(year)
save "E:/Institutions/ICRG Data/stata/c_inv_prof", replace

import excel using "E:/Institutions/ICRG Data/3BResearchersDataset2020.xls", sheet("D-Internal Conflict") cellrange(A8:AK154) firstrow clear
ren Country country
replace country="United States of America" if country=="United States"
replace country="Democratic Republic of the Congo" if country=="Congo, DR"
replace country="Bolivia (Plurinational State of)" if country=="Bolivia"
replace country="CÃ´te d'Ivoire" if country=="Cote d'Ivoire"
replace country="Democratic People's Republic of Korea" if country=="Korea, DPR"
replace country="China, mainland" if country=="China"
replace country="United Republic of Tanzania" if country=="Tanzania"
replace country="Venezuela (Bolivarian Republic of)" if country=="Venezuela"
replace country="Viet Nam" if country=="Vietnam"
replace country="China, Taiwan Province of" if country=="Taiwan"
replace country="Republic of Korea" if country=="South Korea"
replace country="Iran (Islamic Republic of)" if country=="Iran"
replace country="Syrian Arab Republic" if country=="Syria"
merge 1:1 country using "../temp/FAOSTAT_countrycodes", keep(match) nogen
foreach v of varlist B-AK {
   destring `v', replace
   local x : variable label `v'
   rename `v' int_conflict_icrg`x'
}
reshape long int_conflict_icrg, i(country countrycode iso3) j(year)
save "E:/Institutions/ICRG Data/stata/d_int_conflict", replace

import excel using "E:/Institutions/ICRG Data/3BResearchersDataset2020.xls", sheet("E-External Conflict") cellrange(A8:AK154) firstrow clear
ren Country country
replace country="United States of America" if country=="United States"
replace country="Democratic Republic of the Congo" if country=="Congo, DR"
replace country="Bolivia (Plurinational State of)" if country=="Bolivia"
replace country="CÃ´te d'Ivoire" if country=="Cote d'Ivoire"
replace country="Democratic People's Republic of Korea" if country=="Korea, DPR"
replace country="China, mainland" if country=="China"
replace country="United Republic of Tanzania" if country=="Tanzania"
replace country="Venezuela (Bolivarian Republic of)" if country=="Venezuela"
replace country="Viet Nam" if country=="Vietnam"
replace country="China, Taiwan Province of" if country=="Taiwan"
replace country="Republic of Korea" if country=="South Korea"
replace country="Iran (Islamic Republic of)" if country=="Iran"
replace country="Syrian Arab Republic" if country=="Syria"
merge 1:1 country using "../temp/FAOSTAT_countrycodes", keep(match) nogen
foreach v of varlist B-AK {
   destring `v', replace
   local x : variable label `v'
   rename `v' ext_conflict_icrg`x'
}
reshape long ext_conflict_icrg, i(country countrycode iso3) j(year)
save "E:/Institutions/ICRG Data/stata/e_ext_conflict", replace

import excel using "E:/Institutions/ICRG Data/3BResearchersDataset2020.xls", sheet("F-Corruption") cellrange(A8:AK154) firstrow clear
ren Country country
replace country="United States of America" if country=="United States"
replace country="Democratic Republic of the Congo" if country=="Congo, DR"
replace country="Bolivia (Plurinational State of)" if country=="Bolivia"
replace country="CÃ´te d'Ivoire" if country=="Cote d'Ivoire"
replace country="Democratic People's Republic of Korea" if country=="Korea, DPR"
replace country="China, mainland" if country=="China"
replace country="United Republic of Tanzania" if country=="Tanzania"
replace country="Venezuela (Bolivarian Republic of)" if country=="Venezuela"
replace country="Viet Nam" if country=="Vietnam"
replace country="China, Taiwan Province of" if country=="Taiwan"
replace country="Republic of Korea" if country=="South Korea"
replace country="Iran (Islamic Republic of)" if country=="Iran"
replace country="Syrian Arab Republic" if country=="Syria"
merge 1:1 country using "../temp/FAOSTAT_countrycodes", keep(match) nogen
foreach v of varlist B-AK {
   destring `v', replace
   local x : variable label `v'
   rename `v' corruption_icrg`x'
}
reshape long corruption_icrg, i(country countrycode iso3) j(year)
save "E:/Institutions/ICRG Data/stata/f_corruption", replace

import excel using "E:/Institutions/ICRG Data/3BResearchersDataset2020.xls", sheet("G-Military in Politics") cellrange(A8:AK154) firstrow clear
ren Country country
replace country="United States of America" if country=="United States"
replace country="Democratic Republic of the Congo" if country=="Congo, DR"
replace country="Bolivia (Plurinational State of)" if country=="Bolivia"
replace country="CÃ´te d'Ivoire" if country=="Cote d'Ivoire"
replace country="Democratic People's Republic of Korea" if country=="Korea, DPR"
replace country="China, mainland" if country=="China"
replace country="United Republic of Tanzania" if country=="Tanzania"
replace country="Venezuela (Bolivarian Republic of)" if country=="Venezuela"
replace country="Viet Nam" if country=="Vietnam"
replace country="China, Taiwan Province of" if country=="Taiwan"
replace country="Republic of Korea" if country=="South Korea"
replace country="Iran (Islamic Republic of)" if country=="Iran"
replace country="Syrian Arab Republic" if country=="Syria"
merge 1:1 country using "../temp/FAOSTAT_countrycodes", keep(match) nogen
foreach v of varlist B-AK {
   destring `v', replace
   local x : variable label `v'
   rename `v' milit_politics_icrg`x'
}
reshape long milit_politics_icrg, i(country countrycode iso3) j(year)
save "E:/Institutions/ICRG Data/stata/g_milit_politics", replace

import excel using "E:/Institutions/ICRG Data/3BResearchersDataset2020.xls", sheet("H-Religious Tensions") cellrange(A8:AK154) firstrow clear
ren Country country
replace country="United States of America" if country=="United States"
replace country="Democratic Republic of the Congo" if country=="Congo, DR"
replace country="Bolivia (Plurinational State of)" if country=="Bolivia"
replace country="CÃ´te d'Ivoire" if country=="Cote d'Ivoire"
replace country="Democratic People's Republic of Korea" if country=="Korea, DPR"
replace country="China, mainland" if country=="China"
replace country="United Republic of Tanzania" if country=="Tanzania"
replace country="Venezuela (Bolivarian Republic of)" if country=="Venezuela"
replace country="Viet Nam" if country=="Vietnam"
replace country="China, Taiwan Province of" if country=="Taiwan"
replace country="Republic of Korea" if country=="South Korea"
replace country="Iran (Islamic Republic of)" if country=="Iran"
replace country="Syrian Arab Republic" if country=="Syria"
merge 1:1 country using "../temp/FAOSTAT_countrycodes", keep(match) nogen
foreach v of varlist B-AK {
   destring `v', replace
   local x : variable label `v'
   rename `v' relig_tens_icrg`x'
}
reshape long relig_tens_icrg, i(country countrycode iso3) j(year)
save "E:/Institutions/ICRG Data/stata/h_relig_tens", replace

import excel using "E:/Institutions/ICRG Data/3BResearchersDataset2020.xls", sheet("I-Law and Order") cellrange(A8:AK154) firstrow clear
ren Country country
replace country="United States of America" if country=="United States"
replace country="Democratic Republic of the Congo" if country=="Congo, DR"
replace country="Bolivia (Plurinational State of)" if country=="Bolivia"
replace country="CÃ´te d'Ivoire" if country=="Cote d'Ivoire"
replace country="Democratic People's Republic of Korea" if country=="Korea, DPR"
replace country="China, mainland" if country=="China"
replace country="United Republic of Tanzania" if country=="Tanzania"
replace country="Venezuela (Bolivarian Republic of)" if country=="Venezuela"
replace country="Viet Nam" if country=="Vietnam"
replace country="China, Taiwan Province of" if country=="Taiwan"
replace country="Republic of Korea" if country=="South Korea"
replace country="Iran (Islamic Republic of)" if country=="Iran"
replace country="Syrian Arab Republic" if country=="Syria"
merge 1:1 country using "../temp/FAOSTAT_countrycodes", keep(match) nogen
foreach v of varlist B-AK {
   destring `v', replace
   local x : variable label `v'
   rename `v' law_order_icrg`x'
}
reshape long law_order_icrg, i(country countrycode iso3) j(year)
save "E:/Institutions/ICRG Data/stata/i_law_order", replace

import excel using "E:/Institutions/ICRG Data/3BResearchersDataset2020.xls", sheet("J-Ethnic Tensions") cellrange(A8:AK154) firstrow clear
ren Country country
replace country="United States of America" if country=="United States"
replace country="Democratic Republic of the Congo" if country=="Congo, DR"
replace country="Bolivia (Plurinational State of)" if country=="Bolivia"
replace country="CÃ´te d'Ivoire" if country=="Cote d'Ivoire"
replace country="Democratic People's Republic of Korea" if country=="Korea, DPR"
replace country="China, mainland" if country=="China"
replace country="United Republic of Tanzania" if country=="Tanzania"
replace country="Venezuela (Bolivarian Republic of)" if country=="Venezuela"
replace country="Viet Nam" if country=="Vietnam"
replace country="China, Taiwan Province of" if country=="Taiwan"
replace country="Republic of Korea" if country=="South Korea"
replace country="Iran (Islamic Republic of)" if country=="Iran"
replace country="Syrian Arab Republic" if country=="Syria"
merge 1:1 country using "../temp/FAOSTAT_countrycodes", keep(match) nogen
foreach v of varlist B-AK {
   destring `v', replace
   local x : variable label `v'
   rename `v' ethnic_tens_icrg`x'
}
reshape long ethnic_tens_icrg, i(country countrycode iso3) j(year)
save "E:/Institutions/ICRG Data/stata/j_ethnic_tens", replace

import excel using "E:/Institutions/ICRG Data/3BResearchersDataset2020.xls", sheet("K-Democratic Accountability") cellrange(A8:AK154) firstrow clear
ren Country country
replace country="United States of America" if country=="United States"
replace country="Democratic Republic of the Congo" if country=="Congo, DR"
replace country="Bolivia (Plurinational State of)" if country=="Bolivia"
replace country="CÃ´te d'Ivoire" if country=="Cote d'Ivoire"
replace country="Democratic People's Republic of Korea" if country=="Korea, DPR"
replace country="China, mainland" if country=="China"
replace country="United Republic of Tanzania" if country=="Tanzania"
replace country="Venezuela (Bolivarian Republic of)" if country=="Venezuela"
replace country="Viet Nam" if country=="Vietnam"
replace country="China, Taiwan Province of" if country=="Taiwan"
replace country="Republic of Korea" if country=="South Korea"
replace country="Iran (Islamic Republic of)" if country=="Iran"
replace country="Syrian Arab Republic" if country=="Syria"
merge 1:1 country using "../temp/FAOSTAT_countrycodes", keep(match) nogen
foreach v of varlist B-AK {
   destring `v', replace
   local x : variable label `v'
   rename `v' demo_account_icrg`x'
}
reshape long demo_account_icrg, i(country countrycode iso3) j(year)
save "E:/Institutions/ICRG Data/stata/k_demo_account", replace

import excel using "E:/Institutions/ICRG Data/3BResearchersDataset2020.xls", sheet("L-Bureaucracy Quality") cellrange(A8:AK154) firstrow clear
ren Country country
replace country="United States of America" if country=="United States"
replace country="Democratic Republic of the Congo" if country=="Congo, DR"
replace country="Bolivia (Plurinational State of)" if country=="Bolivia"
replace country="CÃ´te d'Ivoire" if country=="Cote d'Ivoire"
replace country="Democratic People's Republic of Korea" if country=="Korea, DPR"
replace country="China, mainland" if country=="China"
replace country="United Republic of Tanzania" if country=="Tanzania"
replace country="Venezuela (Bolivarian Republic of)" if country=="Venezuela"
replace country="Viet Nam" if country=="Vietnam"
replace country="China, Taiwan Province of" if country=="Taiwan"
replace country="Republic of Korea" if country=="South Korea"
replace country="Iran (Islamic Republic of)" if country=="Iran"
replace country="Syrian Arab Republic" if country=="Syria"
merge 1:1 country using "../temp/FAOSTAT_countrycodes", keep(match) nogen
foreach v of varlist B-AK {
   destring `v', replace
   local x : variable label `v'
   rename `v' bureauc_qual_icrg`x'
}
reshape long bureauc_qual_icrg, i(country countrycode iso3) j(year)
save "E:/Institutions/ICRG Data/stata/l_bureauc_qual", replace

use "E:/Institutions/ICRG Data/stata/a_gov_stab", clear
merge 1:1 country year using "E:/Institutions/ICRG Data/stata/b_socio_cond", nogen
merge 1:1 country year using "E:/Institutions/ICRG Data/stata/c_inv_prof", nogen
merge 1:1 country year using "E:/Institutions/ICRG Data/stata/d_int_conflict", nogen
merge 1:1 country year using "E:/Institutions/ICRG Data/stata/e_ext_conflict", nogen
merge 1:1 country year using "E:/Institutions/ICRG Data/stata/f_corruption", nogen
merge 1:1 country year using "E:/Institutions/ICRG Data/stata/g_milit_politics", nogen
merge 1:1 country year using "E:/Institutions/ICRG Data/stata/h_relig_tens", nogen
merge 1:1 country year using "E:/Institutions/ICRG Data/stata/i_law_order", nogen
merge 1:1 country year using "E:/Institutions/ICRG Data/stata/j_ethnic_tens", nogen
merge 1:1 country year using "E:/Institutions/ICRG Data/stata/k_demo_account", nogen
merge 1:1 country year using "E:/Institutions/ICRG Data/stata/l_bureauc_qual", nogen
save "E:/Institutions/ICRG Data/stata/ICRG Political Risk", replace
