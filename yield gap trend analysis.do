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

local com Rice
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
* Cross Validation to determine optimal spline knots
*------------------------------------------------------------------------
use "../temp/trend_regression_data_`com'", clear
summ country_grp
local max_country=r(max)

* Setup file for saving CV results
tempname memhold
tempfile results
postfile `memhold' country_grp countrycode knot1 knot2 ///
	rmse mae pseudoR2 using "..\temp\cv_results_`com'", replace

forvalues j=1/`max_country' {
use "../temp/trend_regression_data_`com'", clear
keep if country_grp==`j'

summ country_grp
local my_country_grp=r(mean)
summ countrycode
local my_countrycode=r(mean)	
	
* Linear splines with 2 knots
local knot_spacing=10
summ year
local minyear=r(min)+`knot_spacing'
local maxyear=r(max)-`knot_spacing'
local maxyearM1=`maxyear'-`knot_spacing'
forvalues knot1=`minyear'/`maxyearM1' { 
	local minj=`knot1'+`knot_spacing'
forvalues knot2=`minj'/`maxyear' { 

	display "Knot 1 = `knot1', Knot 2 = `knot2'"

	capture drop year1 year2 year3
	mkspline year1 `knot1' year2 `knot2' year3 = year

	qui loocv reg yield year1 year2 year3 `controls'
	post `memhold' (`my_country_grp') (`my_countrycode') (`knot1') (`knot2') (r(rmse)) (r(mae)) (r(r2)) 

}
}

* Linear splines with 1 knot
forvalues knot1=`minyear'/`maxyear' { 
	display "Knot 1 = `knot1'"
	local knot2=0

	capture drop year1 year2
	mkspline year1 `knot1' year2 = year

	qui loocv reg yield year1 year2 `controls'
	post `memhold' (`my_country_grp') (`my_countrycode') (`knot1') (`knot2') (r(rmse)) (r(mae)) (r(r2)) 

}

* No knots
local knot1=0
local knot2=0
display "No knots"
qui loocv reg yield year `controls'
post `memhold' (`my_country_grp') (`my_countrycode') (`knot1') (`knot2') (r(rmse)) (r(mae)) (r(r2))

}
postclose `memhold'





*------------------------------------------------------------------------
* Predictions with optimal spline knots
*------------------------------------------------------------------------
tempfile  yield_pred_rmse
tempfile  yield_pred_mae
 

use "..\temp\cv_results_`com'", clear
summ country_grp
local max_country=r(max)


forvalues j=1/`max_country' {
* Find optimal knots for country j according to rmse and mae
use "..\temp\cv_results_`com'", clear
keep if country_grp==`j'
sort rmse
mean knot1 knot2 in 1
local knot1_rmse=_b[knot1]
local knot2_rmse=_b[knot2]
sort mae
mean knot1 knot2 in 1
local knot1_mae=_b[knot1]
local knot2_mae=_b[knot2]

* Regression with optimal spline knots according to minimum RMSE
foreach g in rmse mae {

* Load data for regression
use "../temp/trend_regression_data_`com'", clear
keep if country_grp==`j'

if `knot1_`g''>0 & `knot2_`g''>0 {
capture drop year1 year2 year3
mkspline year1 `knot1_`g'' year2 `knot2_`g'' year3 = year

reg yield year1 year2 year3 `controls' 
est sto yield_reg_`j'_`g'
test year1=year2
gen knots2_test1=r(p)
test year1=year3
gen knots2_test2=r(p)
test year2=year3
gen knots2_test3=r(p)
}

if `knot1_`g''>0 & `knot2_`g''==0 {
capture drop year1 year2
mkspline year1 `knot1_`g'' year2 = year
reg yield year1 year2 `controls' 
est sto yield_reg_`j'_`g'
test year1=year2
gen knots1_test=r(p)
}

if `knot1_`g''==0 & `knot2_`g''==0 {
reg yield year `controls'
est sto yield_reg_`j'_`g'
gen knots0_test=.
}

est restore yield_reg_`j'_`g'
predict yield_hat_`g'
gen knot1=`knot1_`g''
gen knot2=`knot2_`g''
est drop yield_reg_`j'_`g'

*save the data and append together across countries
keep country_grp country year yield yield_hat_`g' knot1 knot2 knots*
capture append using `yield_pred_`g''
save `yield_pred_`g'', replace

} // end of loop foreach g in rmse mae
} // end of country loop

use `yield_pred_rmse', clear
save "..\temp\yield_pred_rmse_`com'", replace
use `yield_pred_mae', clear
save "..\temp\yield_pred_mae_`com'", replace

use "..\temp\yield_pred_rmse_`com'", clear
sort country year
*twoway (line yield_hat_rmse year) (scatter yield year, msymbol(Oh) mcolor(gs8)), by(country, yrescale legend(off) note("")) scheme(538w) 
twoway (line yield_hat_rmse year) , by(country, yrescale legend(off) note("")) scheme(538w) 
graph export "..\figures\yield_pred_rmse_`com'.png", replace

use "..\temp\yield_pred_mae_`com'", clear
sort country year
*twoway (line yield_hat_rmse year) (scatter yield year, msymbol(Oh) mcolor(gs8)), by(country, yrescale legend(off) note("")) scheme(538w) 
twoway (line yield_hat_mae year) , by(country, yrescale legend(off) note("")) scheme(538w) 
graph export "..\figures\yield_pred_mae_`com'.png", replace


*--------------------------------------------------------------
* Graph the yield gap over time
*--------------------------------------------------------------
local com Rice
use "..\temp\yield_pred_rmse_`com'", clear
ren yield_hat_rmse yield_hat
gen lnyield_hat=ln(yield_hat)

/*
forvalues y=1961/2014 {
qui summ yield_hat if year==`y', detail
replace yield_hat=r(p5) if yield_hat<r(p5) & yield_hat!=. & year==`y'
replace yield_hat=r(p95) if yield_hat>r(p95) & yield_hat!=. & year==`y'
}
*/

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

/*

line relgap95_05_logn relgap90_10_logn year, scheme(538w) legend(label(1 "95-5 Gap") label(2 "90-10 Gap")) ///
	title("`com' Relative Yield Gap with Lowest")
*graph export "..\figures\yield_relgap_low_`com'.png", replace
line relgap95_50_logn relgap90_50_logn year, scheme(538w) legend(label(1 "95-50 Gap") label(2 "90-50 Gap")) ///
	title("`com' Relative Yield Gap with Median")
*graph export "..\figures\yield_relgap_mid_`com'.png", replace

*/
