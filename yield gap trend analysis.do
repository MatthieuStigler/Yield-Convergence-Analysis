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


*ssc install loocv

local com Maize
*------------------------------------------------------------------------
* Set sample used for regressions
*------------------------------------------------------------------------
use "../temp/fullFAO_regression_data", clear
keep if commodity=="`com'"
bysort country: egen first_year=min(year)
bysort country: egen last_year=max(year)
tab first_year
tab last_year
local myfirst=1961
local mylast=2014
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


*save the data and append together across countries
keep country_grp country year yield yield_hat
capture append using `yield_pred'
save `yield_pred', replace


} // end of country loop

use `yield_pred', clear
save "..\temp\yield_pred_`com'", replace


use "..\temp\yield_pred_`com'", clear
sort country year
twoway (line yield_hat year) (scatter yield year, msymbol(Oh) mcolor(gs8)), by(country, yrescale legend(off) note("")) scheme(538w) 
twoway (line yield_hat year) , by(country, yrescale legend(off) note("")) scheme(538w) 
graph export "..\figures\yield_pred_`com'.png", replace




*--------------------------------------------------------------
* Graph the yield gap over time
*--------------------------------------------------------------
local com Maize
use "..\temp\yield_pred_`com'", clear
gen lnyield_hat=ln(yield_hat)


twoway kdensity lnyield_hat if year==1965 || kdensity lnyield_hat if year==1975 || kdensity lnyield_hat if year==1985 || ///
	kdensity lnyield_hat if year==1995 || kdensity lnyield_hat if year==2005 || kdensity lnyield_hat if year==2014, ///
	legend(label(1 "1965" ) label(2 "1975") label(3 "1985") label(4 "1995") label(5 "2005") label(6 "2014")) scheme(538w) ///
	xtitle("Log `com' Yield") ytitle("Density")
graph export "..\figures\yield_density_`com'.png", replace width(2000)


collapse(p5) yield05=yield_hat (p10) yield10=yield_hat (p50) yield50=yield_hat (p90) yield90=yield_hat (p95) yield95=yield_hat, by(year)
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
graph export "..\figures\yield_gap_low_`com'.png", replace width(2000)
line gap95_50 gap90_50 year, scheme(538w) legend(label(1 "95-50 Gap") label(2 "90-50 Gap")) ///
	title("`com' Yield Gap with Median")
graph export "..\figures\yield_gap_mid_`com'.png", replace width(2000)

line relgap95_05 relgap90_10 year, scheme(538w) legend(label(1 "95-5 Gap") label(2 "90-10 Gap")) ///
	title("`com' Relative Yield Gap with Lowest")
graph export "..\figures\yield_relgap_low_`com'.png", replace width(2000)
line relgap95_50 relgap90_50 year, scheme(538w) legend(label(1 "95-50 Gap") label(2 "90-50 Gap")) ///
	title("`com' Relative Yield Gap with Median")
graph export "..\figures\yield_relgap_mid_`com'.png", replace



line yield95 yield90 yield50 yield10 yield05 year, scheme(538w) legend(label(1 "95th percentile") label(2 "90th percentile") ///
	label(3 "50th percentile")  label(4 "10th percentile")  label(5 "5th percentile"))  ///
	title("`com' Yield Percentiles Over Time") xtitle("Year")
graph export "..\figures\yield_percentiles_`com'.png", replace width(2000)


