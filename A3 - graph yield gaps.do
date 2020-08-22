*local com Rice
*use "..\temp\yield_pred_`com'", clear

*--------------------------------------------------------------
* Graph the yield trends
*--------------------------------------------------------------
foreach com in Maize Wheat Rice {
use "..\temp\yield_pred_`com'", clear
sort country year
ren year Year
twoway (line yield_hat Year, lwidth(medthick)) (scatter yield Year, msymbol(Oh) mcolor(gs8%60)), by(country, yrescale legend(off) note("")) scheme(538w) 
graph export "..\figures\yield_pred_`com'2.png", replace width(2000)
*twoway (line yield_hat year) , by(country, yrescale legend(off) note("")) scheme(538w) 
}

*--------------------------------------------------------------
* Graph the yield gap over time
*--------------------------------------------------------------
foreach com in Maize Wheat Rice {
use "..\temp\yield_pred_`com'", clear
gen lnyield_hat=ln(yield_hat)
*gen production_hat=yield_hat*area_hat
*replace production_hat=0 if production_hat<0 & production_hat!=.
*drop if production_hat==.



sort country year

twoway kdensity lnyield_hat if year==1965 || kdensity lnyield_hat if year==1975 || kdensity lnyield_hat if year==1985 || ///
	kdensity lnyield_hat if year==1995 || kdensity lnyield_hat if year==2005 || kdensity lnyield_hat if year==2015, ///
	legend(label(1 "1965" ) label(2 "1975") label(3 "1985") label(4 "1995") label(5 "2005") label(6 "2015")) scheme(538w) ///
	xtitle("Log `com' Yield") ytitle("Density") name(density_`com', replace)
graph export "..\figures\yield_density_`com'2.png", replace 


collapse (p5) yield05=yield_hat (p10) yield10=yield_hat (p50) yield50=yield_hat (p90) yield90=yield_hat (p95) yield95=yield_hat, by(year)
gen gap95_05=yield05 - yield95
gen gap90_10=yield10 - yield90
gen gap95_50=yield50 - yield95
gen gap90_50=yield50 - yield90

gen relgap95_05=yield05/yield95 
gen relgap90_10=yield10/yield90 
gen relgap95_50=yield50/yield95 
gen relgap90_50=yield50/yield90 

gen lnyield05=ln(yield05)
gen lnyield10=ln(yield10)
gen lnyield50=ln(yield50)
gen lnyield90=ln(yield90)
gen lnyield95=ln(yield95)

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


line yield95 yield90 yield50 yield10 yield05 year, scheme(538w) legend(label(1 "95th percentile") label(2 "90th percentile") ///
	label(3 "50th percentile")  label(4 "10th percentile")  label(5 "5th percentile"))  ///
	title("`com'") xtitle("Year") name(percentile_`com', replace) legend(position (5) col(5) off)
	
line lnyield95 lnyield90 lnyield50 lnyield10 lnyield05 year, scheme(538w) legend(label(1 "95th percentile") label(2 "90th percentile") ///
	label(3 "50th percentile")  label(4 "10th percentile")  label(5 "5th percentile"))  ///
	title("Log `com' Yield Percentiles Over Time") xtitle("Year")
graph export "..\figures\lnyield_percentiles_`com'2.png", replace width(2000)


line lnyield95 lnyield90 lnyield50 lnyield10 lnyield05 year, scheme(538w) legend(label(1 "95th percentile") label(2 "90th percentile") ///
	label(3 "50th percentile")  label(4 "10th percentile")  label(5 "5th percentile"))  ///
	title("`com'") xtitle("Year") name(lnpercentile_`com', replace) legend(position (5) col(5) off)	

save "..\temp\yield_percentiles_`com'", replace
} // end of loop over commodities


line yield95 yield90 yield50 yield10 yield05 year, scheme(538w) legend(label(1 "95th") label(2 "90th") ///
	label(3 "50th")  label(4 "10th")  label(5 "5th"))  ///
	title("Rice") xtitle("Year") name(percentile_Rice, replace) legend(position (5) col(5))

graph combine percentile_Maize percentile_Wheat percentile_Rice, cols(1) iscale(1.2) xsize(3.5in) ysize(7in) scheme(538w)
graph export "..\figures\yield_percentiles_combined2.png", replace width(2000)

line lnyield95 lnyield90 lnyield50 lnyield10 lnyield05 year, scheme(538w) legend(label(1 "95th") label(2 "90th") ///
	label(3 "50th")  label(4 "10th")  label(5 "5th"))  ///
	title("Rice") xtitle("Year") name(lnpercentile_Rice, replace) legend(position (3) col(1))

graph combine lnpercentile_Maize lnpercentile_Wheat lnpercentile_Rice, cols(1) iscale(1.2) xsize(3.5in) ysize(7in) scheme(538w)
graph export "..\figures\lnyield_percentiles_combined2.png", replace width(2000)



graph combine density_Maize density_Wheat density_Rice lnpercentile_Maize lnpercentile_Wheat lnpercentile_Rice, cols(3) iscale(1) xsize(15in) ysize(7in) scheme(538w)
graph export "..\figures\density_percentiles_combined.png", replace width(2000)



use "..\temp\yield_percentiles_Maize", clear
gen commodity="Maize"
append using "..\temp\yield_percentiles_Wheat"
replace commodity="Wheat" if commodity==""
append using "..\temp\yield_percentiles_Rice"
replace commodity="Rice" if commodity==""
reshape wide yield* lnyield* gap* relgap*, i(year) j(commodity) string

summ lnyield10Maize if year==1961
local yield1=r(mean)
summ lnyield10Maize if year==2016
local yield2=r(mean)
di (`yield2' - `yield1')

summ lnyield90Maize if year==1961
local yield1=r(mean)
summ lnyield90Maize if year==2016
local yield2=r(mean)
di (`yield2' - `yield1')

summ lnyield50Rice if year==1961
local yield1=r(mean)
summ lnyield50Rice if year==2016
local yield2=r(mean)
di (`yield2' - `yield1')

summ lnyield90Rice if year==1961
local yield1=r(mean)
summ lnyield90Rice if year==2016
local yield2=r(mean)
di (`yield2' - `yield1')

summ lnyield10Rice if year==1961
local yield1=r(mean)
summ lnyield10Rice if year==2016
local yield2=r(mean)
di (`yield2' - `yield1')

summ lnyield10Wheat if year==1961
local yield1=r(mean)
summ lnyield10Wheat if year==2016
local yield2=r(mean)
di (`yield2' - `yield1')

summ lnyield90Wheat if year==1961
local yield1=r(mean)
summ lnyield90Wheat if year==2016
local yield2=r(mean)
di (`yield2' - `yield1')

summ lnyield90Wheat if year==1996
local yield1=r(mean)
summ lnyield90Wheat if year==2016
local yield2=r(mean)
di (`yield2' - `yield1')

line relgap90_10Maize relgap90_10Wheat relgap90_10Rice year, scheme(538w) legend(label(1 "Maize") label(2 "Wheat") label(3 "Rice")) ///
	title("Relative Yield Gap Between 90th and 10th Percentiles")
graph export "..\figures\yield_relgap90_10.png", replace width(2000)

line relgap90_50Maize relgap90_50Wheat relgap90_50Rice year, scheme(538w) legend(label(1 "Maize") label(2 "Wheat") label(3 "Rice")) ///
	title("Relative Yield Gap Between 90th and 50th Percentiles")
	graph export "..\figures\yield_relgap90_50.png", replace width(2000)
	
*grc1leg2 percentile_Maize percentile_Wheat percentile_Rice, cols(1) iscale(1.2) xsize(4in) ysize(10in) legendfrom(percentile_Maize)  position(6) 

