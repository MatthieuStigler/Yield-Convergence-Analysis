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

*------------------------------------------------------------------------
* Set sample used for regressions
*------------------------------------------------------------------------
use "../temp/fullFAO_regression_data", clear
keep if commodity=="Maize"
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

save "../temp/trend_regression_data", replace


local controls 

*------------------------------------------------------------------------
* Cross Validation to determine optimal spline knots
*------------------------------------------------------------------------
use "../temp/trend_regression_data", clear
summ country_grp
local max_country=r(max)

* Setup file for saving CV results
tempname memhold
tempfile results
postfile `memhold' country_grp countrycode knot1 knot2 ///
	rmse mae pseudoR2 using "..\temp\cv_results", replace

forvalues j=1/`max_country' {
use "../temp/trend_regression_data", clear
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
 

use "..\temp\cv_results", clear
summ country_grp
local max_country=r(max)


forvalues j=1/`max_country' {
* Find optimal knots for country j according to rmse and mae
use "..\temp\cv_results", clear
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
use "../temp/trend_regression_data", clear
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
save "..\temp\yield_pred_rmse", replace
use `yield_pred_mae', clear
save "..\temp\yield_pred_mae", replace

use "..\temp\yield_pred_rmse", clear
sort country year
*twoway (line yield_hat_rmse year) (scatter yield year, msymbol(Oh) mcolor(gs8)), by(country, yrescale legend(off) note("")) scheme(538w) 
twoway (line yield_hat_rmse year) , by(country, yrescale legend(off) note("")) scheme(538w) 
graph export "..\figures\yield_pred_rmse.png", replace

use "..\temp\yield_pred_mae", clear
sort country year
*twoway (line yield_hat_rmse year) (scatter yield year, msymbol(Oh) mcolor(gs8)), by(country, yrescale legend(off) note("")) scheme(538w) 
twoway (line yield_hat_mae year) , by(country, yrescale legend(off) note("")) scheme(538w) 
graph export "..\figures\yield_pred_mae.png", replace


