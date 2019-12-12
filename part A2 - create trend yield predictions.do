
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

} // end of commodity loop

