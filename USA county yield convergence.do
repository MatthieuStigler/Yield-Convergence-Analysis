import delimited using "../dataRaw/USA_county_corn_yields.csv", clear
keep state_name county_name state_ansi county_ansi year value
order state_name county_name state_ansi county_ansi year value
ren value yield
tab county_ansi
drop if county_ansi==.
drop if state_ansi==.
gen fips=state_ansi*1000 + county_ansi
duplicates tag fips year, gen(dup)
tab dup 
drop dup

bysort fips: egen first_year=min(year)
bysort fips: egen last_year=max(year)
tab first_year
tab last_year
local myfirst=1961
local mylast=2014
keep if first_year==`myfirst'
keep if last_year>=`mylast'
drop if year>`mylast'
bysort fips: gen maxN=_N
tab maxN
drop if maxN<46
egen county_grp=group(fips), label
summ county_grp
drop first_year last_year maxN

save "../temp/USA_county_data_Maize", replace


local controls 
local com Maize
*------------------------------------------------------------------------
* Predictions with natural cubic spline knot trend
*------------------------------------------------------------------------
tempfile  yield_pred
 
use "../temp/USA_county_data_maize", clear
summ county_grp
local max_county=r(max)

forvalues j=1/`max_county' {
* Load data for regression
use "../temp/USA_county_data_maize", clear
keep if county_grp==`j'

capture drop year_spline
mkspline year_spline=year, cubic nknots(5)

reg yield year_spline* `controls' 
predict yield_hat


*save the data and append together across counties
keep county_grp fips year yield yield_hat
capture append using `yield_pred'
save `yield_pred', replace


} // end of country loop

use `yield_pred', clear
save "..\temp\USA_county_yield_pred_`com'", replace

/*
use "..\temp\USA_county_yield_pred_`com'", clear
sort fips year
twoway (line yield_hat year, lwidth(medthick)) (scatter yield year, msymbol(Oh) mcolor(gs8%60)), by(country, yrescale legend(off) note("")) scheme(538w) 
graph export "..\figures\USA_county_yield_pred_`com'.png", replace
*/





*--------------------------------------------------------------
* Graph the yield gap over time
*--------------------------------------------------------------
use "..\temp\USA_county_yield_pred_`com'", clear
gen lnyield_hat=ln(yield_hat)


twoway kdensity lnyield_hat if year==1965 || kdensity lnyield_hat if year==1975 || kdensity lnyield_hat if year==1985 || ///
	kdensity lnyield_hat if year==1995 || kdensity lnyield_hat if year==2005 || kdensity lnyield_hat if year==2014, ///
	legend(label(1 "1965" ) label(2 "1975") label(3 "1985") label(4 "1995") label(5 "2005") label(6 "2014")) scheme(538w) ///
	xtitle("Log `com' Yield") ytitle("Density")
graph export "..\figures\USA_county_yield_density_`com'.png", replace width(2000)


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
	title("USA County `com' Yield Gap with Lowest")
graph export "..\figures\USA_county_yield_gap_low_`com'.png", replace width(2000)
line gap95_50 gap90_50 year, scheme(538w) legend(label(1 "95-50 Gap") label(2 "90-50 Gap")) ///
	title("USA County `com' Yield Gap with Median")
graph export "..\figures\USA_county_yield_gap_mid_`com'.png", replace width(2000)

line relgap95_05 relgap90_10 year, scheme(538w) legend(label(1 "95-5 Gap") label(2 "90-10 Gap")) ///
	title("USA County `com' Relative Yield Gap with Lowest")
graph export "..\figures\USA_county_yield_relgap_low_`com'.png", replace width(2000)
line relgap95_50 relgap90_50 year, scheme(538w) legend(label(1 "95-50 Gap") label(2 "90-50 Gap")) ///
	title("USA County `com' Relative Yield Gap with Median")
graph export "..\figures\USA_county_yield_relgap_mid_`com'.png", replace



line yield95 yield90 yield50 yield10 yield05 year, scheme(538w) legend(label(1 "95th percentile") label(2 "90th percentile") ///
	label(3 "50th percentile")  label(4 "10th percentile")  label(5 "5th percentile"))  ///
	title("USA County `com' Yield Percentiles Over Time") xtitle("Year")
graph export "..\figures\USA_county_yield_percentiles_`com'.png", replace width(2000)


