foreach com in Maize Wheat Rice {
use "..\temp\yield_pred_`com'", clear
sort country year
twoway (line yield_hat year, lwidth(medthick)) (scatter yield year, msymbol(Oh) mcolor(gs8%60)), by(country, yrescale legend(off) note("")) scheme(538w) 
graph export "..\figures\yield_pred_`com'2.png", replace
*twoway (line yield_hat year) , by(country, yrescale legend(off) note("")) scheme(538w) 


*--------------------------------------------------------------
* Graph the yield gap over time
*--------------------------------------------------------------
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
