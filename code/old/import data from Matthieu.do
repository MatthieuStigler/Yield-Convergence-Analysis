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


ren fao3__yield yield
ren fao3__area area
ren fao3__production production

drop if yield==. 

save "../temp/revisedMatthieu_regression_data", replace




*ssc install loocv

foreach com in Maize Wheat Rice {
*local com Maize
*------------------------------------------------------------------------
* Set sample used for regressions
*------------------------------------------------------------------------
use "../temp/revisedMatthieu_regression_data", clear
keep if commodity=="`com'"
bysort country: egen first_year=min(year)
bysort country: egen last_year=max(year)
tab first_year
tab last_year
local myfirst=1961
local mylast=2011
keep if first_year==`myfirst'
keep if last_year>=`mylast'
drop if year>`mylast'
tab country
egen country_grp=group(country), label

*gen pre = cru__ave3_pre_w_finarea
* Use temperatures 3 months before harvest. In USA, harvest is in October, 
* so this uses July temperatures in USA. 
*gen tmp_lag3month=cru__ave2_tmp_w_finarea_l_3

save "../temp/trend_regression_data_`com'", replace


local controls 
*------------------------------------------------------------------------
* Predictions with natural cubic spline knot trend
*------------------------------------------------------------------------
tempfile  yield_pred
 
use "../temp/trend_regression_data_`com'", clear
summ country_grp
local max_country=r(max)

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
keep country_grp country iso3 year yield yield_hat area area_hat production
capture append using `yield_pred'
save `yield_pred', replace


} // end of country loop

use `yield_pred', clear
save "..\temp\yield_pred_`com'", replace


use "..\temp\yield_pred_`com'", clear
sort country year
twoway (line yield_hat year, lwidth(medthick)) (scatter yield year, msymbol(Oh) mcolor(gs8%60)), by(country, yrescale legend(off) note("")) scheme(538w) 
graph export "..\figures\yield_pred_`com'2.png", replace
*twoway (line yield_hat year) , by(country, yrescale legend(off) note("")) scheme(538w) 

} // end of commodity loop


/*
use "..\temp\yield_pred_Wheat", clear
keep if year==2011
gen production_hat=yield_hat*area_hat
replace production_hat=0 if production_hat<0 & production_hat!=.
drop if production_hat==.
summ yield_hat if year==2011, detail
summ yield_hat [aw=production_hat] if year==2011, detail

total production_hat if year==2011
local prod_10=0.1*_b[production_hat]
gen share_prod=production_hat/_b[production_hat]

sort yield_hat
gen cum_prod=production_hat if _n==1
replace cum_prod=production_hat + cum_prod[_n-1] if _n>1
gen cum_share_prod=cum_prod/_b[production_hat] 

collapse (p10) yield_hat [aw=production_hat], by(year)
*/

*--------------------------------------------------------------
* Graph the yield gap over time
*--------------------------------------------------------------
foreach com in Maize Wheat Rice {
use "..\temp\yield_pred_`com'", clear
gen lnyield_hat=ln(yield_hat)
gen production_hat=yield_hat*area_hat
replace production_hat=0 if production_hat<0 & production_hat!=.
drop if production_hat==.

twoway kdensity lnyield_hat if year==1965 || kdensity lnyield_hat if year==1975 || kdensity lnyield_hat if year==1985 || ///
	kdensity lnyield_hat if year==1995 || kdensity lnyield_hat if year==2005 || kdensity lnyield_hat if year==2014, ///
	legend(label(1 "1965" ) label(2 "1975") label(3 "1985") label(4 "1995") label(5 "2005") label(6 "2014")) scheme(538w) ///
	xtitle("Log `com' Yield") ytitle("Density")
graph export "..\figures\yield_density_`com'2.png", replace width(2000)


collapse (p5) yield05=yield_hat (p10) yield10=yield_hat (p50) yield50=yield_hat (p90) yield90=yield_hat (p95) yield95=yield_hat [aw=production_hat], by(year)
gen gap95_05=yield95 - yield05 
gen gap90_10=yield90 - yield10 
gen gap95_50=yield95 - yield50 
gen gap90_50=yield90 - yield50 

gen relgap95_05=(yield95 - yield05)/yield95 
gen relgap90_10=(yield90 - yield10)/yield90 
gen relgap95_50=(yield95 - yield50)/yield95 
gen relgap90_50=(yield90 - yield50)/yield90 

sort year
line gap95_05 gap90_10 year, scheme(538w) legend(label(1 "95-5 Gap") label(2 "90-10 Gap")) ///
	title("`com' Yield Gap with Lowest")
graph export "..\figures\yield_gap_low_`com'2.png", replace width(2000)
line gap95_50 gap90_50 year, scheme(538w) legend(label(1 "95-50 Gap") label(2 "90-50 Gap")) ///
	title("`com' Yield Gap with Median")
graph export "..\figures\yield_gap_mid_`com'2.png", replace width(2000)

line relgap95_05 relgap90_10 year, scheme(538w) legend(label(1 "95-5 Gap") label(2 "90-10 Gap")) ///
	title("`com' Relative Yield Gap with Lowest")
graph export "..\figures\yield_relgap_low_`com'2.png", replace width(2000)
line relgap95_50 relgap90_50 year, scheme(538w) legend(label(1 "95-50 Gap") label(2 "90-50 Gap")) ///
	title("`com' Relative Yield Gap with Median")
graph export "..\figures\yield_relgap_mid_`com'2.png", replace



line yield95 yield90 yield50 yield10 yield05 year, scheme(538w) legend(label(1 "95th percentile") label(2 "90th percentile") ///
	label(3 "50th percentile")  label(4 "10th percentile")  label(5 "5th percentile"))  ///
	title("`com' Yield Percentiles Over Time") xtitle("Year")
graph export "..\figures\yield_percentiles_`com'2.png", replace width(2000)


} // end of loop over commodities

use "..\temp\yield_pred_Maize", clear
gen production_hat=yield_hat*area_hat
replace production_hat=0 if production_hat<0 & production_hat!=.
drop if production_hat==.
collapse (p5) yield05=yield_hat (p10) yield10=yield_hat (p50) yield50=yield_hat (p90) yield90=yield_hat (p95) yield95=yield_hat [aw=production_hat], by(year)
save "..\temp\yield_percentiles_Maize", replace


* Add GDP to regression
local com Wheat
use "..\temp\yield_pred_`com'", clear
gen commodity="`com'"
merge m:1 year using "../temp/yield_percentiles_Maize", keep(match master) nogen
merge 1:1 country commodity year using "../temp/revisedMatthieu_regression_data", keep(match master) nogen
ren dai3__nra nra
gen yield_gap=(yield95 - yield_hat)/yield95 
xtset country_grp year
drop if nra==.
capture drop country_grp
egen country_grp=group(country), label
gen nra_hat=.

summ country_grp
local max_country=r(mean)
forvalues j=1/`max_country' {

capture drop year_spline*
mkspline year_spline=year, cubic nknots(5)

reg nra year_spline* if country_grp==`j'
predict yhat if country_grp==`j'
replace nra_hat=yhat if country_grp==`j'
drop yhat 
}

summ yield_gap
summ nra

reg yield_gap nra_hat
reg yield_gap nra_hat if year==2011
reg yield_gap nra_hat if year==1990
xtreg yield_gap nra_hat, fe
xtreg yield_gap nra_hat i.year, fe

sort country_grp year
reg d.yield_gap d.nra_hat i.year
reg d5.yield_gap d5.nra_hat i.year
reg d10.yield_gap d10.nra_hat i.year
reg d20.yield_gap d20.nra_hat i.year

