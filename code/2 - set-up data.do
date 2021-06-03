
foreach com in Maize Wheat Rice {
*local com Maize
*------------------------------------------------------------------------
* Set sample used for trend yield regressions
*------------------------------------------------------------------------
*use "../temp/revisedMatthieu_regression_data", clear
use "../temp/FAO_crop_production", clear
keep if commodity=="`com'"
bysort country: egen first_year=min(year)
bysort country: egen last_year=max(year)
tab first_year
tab last_year
local myfirst=1961
*local mylast=2011
local mylast=2016
keep if first_year==`myfirst'
keep if last_year>=`mylast'
drop if year>`mylast'
* make sure it is a balanced panel
bysort country: gen maxN=_N
summ maxN
drop if maxN<r(max)
tab country
egen country_grp=group(country), label

*gen pre = cru__ave3_pre_w_finarea
* Use temperatures 3 months before harvest. In USA, harvest is in October, 
* so this uses July temperatures in USA. 
*gen tmp_lag3month=cru__ave2_tmp_w_finarea_l_3

save "../temp/trend_regression_data_`com'", replace


*------------------------------------------------------------------------
* Predict trend yield with natural cubic spline knot trend
*------------------------------------------------------------------------
tempfile  yield_pred
 
use "../temp/trend_regression_data_`com'", clear
summ country_grp
local max_country=r(max)

qui{
forvalues j=1/`max_country' {
* Load data for regression
use "../temp/trend_regression_data_`com'", clear
keep if country_grp==`j'

capture drop year_spline
mkspline year_spline=year, cubic nknots(5)

reg yield year_spline* `controls' 
predict yield_hat

reg area year_spline* `controls' 
predict area_hat

*save the data and append together across countries
keep country_grp country iso3 countrycode year yield yield_hat area area_hat production
capture append using `yield_pred'
save `yield_pred', replace


} // end of country loop
} // end of qui

*------------------------------------------------------------------------
* Remove countries with a small growing area 
*------------------------------------------------------------------------
use `yield_pred', clear

bysort country: egen avg_area=mean(area)
* List countries as percent of total global growing area of the crop
total avg_area if year==2011 
gen prop_global_area=avg_area/_b[avg_area]
gsort - prop_global_area
list country prop_global_area if year==2011 

* List countries as percent of total global growing area among non-top 5% growing countries
summ avg_area, detail
local area95=r(p95)
total avg_area if year==2011 & avg_area<`area95'
capture drop prop_global_area
gen prop_global_area=avg_area/_b[avg_area]

count if year==2011 & prop_global_area>0.005
gsort - prop_global_area
list country prop_global_area if year==2011 & prop_global_area>0.005
list country prop_global_area if year==2011 & prop_global_area<0.005
drop if prop_global_area<0.005

save "..\temp\yield_pred_`com'", replace
export delimited "..\temp\yield_pred_`com'.csv", replace

} // end of commodity loop


*------------------------------------------------------------------------
* Create dataset of smoothed NRA values
*------------------------------------------------------------------------
foreach com in Maize Wheat Rice {
* Create dataset of smoothed NRA values
use "../temp/revisedMatthieu_regression_data", clear
keep if commodity=="`com'"
ren dai3__nra nra
drop if nra==.
capture drop country_grp
egen country_grp=group(country), label
xtset country_grp year
gen nra_hat=.

summ country_grp
local max_country=r(max)
forvalues j=1/`max_country' {

capture drop year_spline*
mkspline year_spline=year, cubic nknots(5)

qui reg nra year_spline* if country_grp==`j'
qui predict yhat if country_grp==`j'
qui replace nra_hat=yhat if country_grp==`j'
drop yhat 
}
keep country commodity year nra_hat
save "../temp/nra_hat_`com'", replace
} // end com loop


*------------------------------------------------------------------------
* Merge datasets together to create final dataset for estimation
*------------------------------------------------------------------------
foreach com in Maize Wheat Rice {
*local com Wheat
* Define which variable to use to measure potential yield
if "`com'"=="Maize" | "`com'"=="Wheat"  {
local potential_yield potent_rain_int
}
if "`com'"=="Rice" {
local potential_yield post1500AverageCaloriesmean
}


use "..\temp\yield_pred_`com'", clear
gen commodity="`com'"

* Polity data
merge 1:1 iso3 year using "..\temp\p4v2016", keep(match master) nogen
* Smoothed NRA
merge 1:1 country commodity year using "../temp/nra_hat_`com'", keep(match master) nogen
* Merge the rest of the NRA data
merge 1:1 country commodity year using "../temp/revisedMatthieu_regression_data", keep(match master) nogen
merge 1:1 country year using "../temp/Cntry_Year_data", keep(match master) nogen
* Penn World table
merge 1:1 iso3 year using "../temp/pwt90_iso3", keep(match master) nogen
* Agriculture R&D Expenditures
merge 1:1 countrycode year using "../temp/rd25lag", keep(match master) nogen
gen lnrd25lag=ln(rd25lag)
* Caloric data from GAEZ obtained from here: https://ozak.github.io/Caloric-Suitability-Index/
* Should cite Oded Galor and Ömer Özak, 2016. “The Agricultural Origins of Time Preference,” American Economic Review, 2016, 106(10): 3064–3103.
merge m:1 iso3 using "../temp/country_Calories_potential", keep(match master) nogen

* GAEZ yield potential and yield gap data
* This dataset was only available for Maize and Wheat
if "`com'"=="Maize" | "`com'"=="Wheat"  {
merge m:1 iso3 using "../temp/`com'_yield_potential", keep(match master) nogen
}

merge m:1 iso3 using "../temp/ygapGAEZ_`com'", keep(match master) nogen

*------------------------------------
* Merge different measures of institutions
*-----------------------------------
* World Bank Worldwide Governance Indicators (1996-2019)
ren iso3 code
merge 1:1 code year using "../dataRaw/wgidataset_stata/wgidataset", keep(match master) nogen
ren code iso3
tab year if vae==.

summ rqe rle cce
gen wb_gov_antidiversion=(rqe + rle + cce)/3

* ICRG Political Risk
merge 1:1 countrycode year using "E:/Institutions/ICRG Data/stata/ICRG Political Risk", keep(match master) nogen

gen inv_prof_index=inv_prof_icrg/12
gen law_order_index=law_order_icrg/6
gen bureauc_qual_index=bureauc_qual_icrg/4
gen corruption_index=corruption_icrg/6

gen demo_account_index=demo_account_icrg/6

summ inv_prof_index law_order_index bureauc_qual_index corruption_index demo_account_index

* These are definitions from the Knack and Keefer Paper
gen property_rights=(inv_prof_index + law_order_index)/2
gen eff_gov_service=(bureauc_qual_index + corruption_index)/2
* Used by Hall and Jones
gen gov_antidiversion=(inv_prof_index + law_order_index + bureauc_qual_index + corruption_index)/4


* Polity dummy variables
gen polity_democ=(polity2>0) if polity2!=.
gen polity_vlow=(polity2<=-5) if polity2!=.
gen polity_low=(polity2>-5 & polity2<=0) if polity2!=.
gen polity_high=(polity2>0 & polity2<=5) if polity2!=.
gen polity_vhigh=(polity2>5) if polity2!=.
gen democ01=(democ>0) if democ!=. & democ!=-88 & democ!=-77 & democ!=-66
gen autoc01=(autoc>0) if autoc!=. & autoc!=-88 & autoc!=-77 & autoc!=-66

ren cru__ave3_pre_w_finarea pre
ren cru__ave3_tmn_w_finarea temp_min
ren cru__ave3_tmp_w_finarea temp_avg
ren cru__ave3_tmx_w_finarea temp_max
ren cru__ave3_vap_w_finarea vap
ren cru__ave3_pet_w_finarea pet
corr vap pet temp_max

* Use GDP per capita from Penn World Table
gen gdp_per_capita= rgdpna/pop
gen ln_gdp_per_capita=ln(gdp_per_capita)


gen antiAg=(nra_hat <0) if nra_hat!=.
*bysort country: egen sum_antiAg=total(antiAg)
*tab sum_antiAg
*replace antiAg=0 if sum_antiAg<10
gen proAg=(nra_hat >0) if nra_hat!=.
*bysort country: egen sum_proAg=total(proAg)
*tab sum_proAg
*replace proAg=0 if sum_proAg<10
summ antiAg proAg


gen ln_yield_hat=ln(yield_hat)

sort country_grp year
gen lln_yield_hat=l.ln_yield_hat
gen yield_hat_growth1=ln(yield_hat/l.yield_hat)
gen yield_hat_growth20=ln(yield_hat/l20.yield_hat)/20
gen yield_hat_growth30=ln(yield_hat/l30.yield_hat)/30
gen yield_hat_growth29=ln(yield_hat/l29.yield_hat)/29


gen ln_yield=ln(yield)

gen lln_yield=l.ln_yield
gen yield_growth1=ln(yield/l.yield)
gen yield_growth20=ln(yield/l20.yield)/20
gen yield_growth30=ln(yield/l30.yield)/30
gen yield_growth29=ln(yield/l29.yield)/29


gen ln_yield_pot=ln(`potential_yield')

*------------------------------------------------------------------------
* Drop countries with strange data
* These are strange for maize
if "`com'"=="Maize" {
drop if country=="Democratic People's Republic of Korea"
drop if country=="Togo"
}
* These are strange for rice
if "`com'"=="Rice" {
drop if country=="Guinea"
}
*--------------------------------------------------------------------------

save "../dataAnalysis/yield_convergence_data_`com'", replace
export delimited "../dataAnalysis/yield_convergence_data_`com'", replace
} // end com loop


