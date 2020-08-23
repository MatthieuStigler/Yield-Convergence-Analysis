
*ssc install loocv

foreach com in Maize Wheat Rice {
*local com Maize
*------------------------------------------------------------------------
* Set sample used for regressions
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


local controls 
*------------------------------------------------------------------------
* Predictions with natural cubic spline knot trend
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

