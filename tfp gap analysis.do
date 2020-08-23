import delimited using "../dataRaw/TFP/Ag TFP/Growth rate (%).csv", clear rowrange(3:) varnames(3)
forvalues j=11/70 {
	local x : variable label v`j'
	rename v`j' v`x'
}
drop if faon==.
ren wdicode iso3
ren faon countrycode
drop order region subregion inci incii note datasourcesnotes
reshape long v, i(countrycode iso3) j(year)
ren v tfp_growth
drop if tfp_growth==.

merge m:1 iso3 year using "../temp/pwt90_iso3"
* drop year not in TFP data
drop if year <1962 | year>2013
tab country if _merge==1
tab country if _merge==2
drop if _merge==1 | _merge==2
drop _merge
drop if tfp_growth==. 
save "../temp/TFP_gdp_regression_data", replace







use "../temp/TFP_gdp_regression_data", clear
gen gdp_per_capita= rgdpna/pop
drop if gdp_per_capita==.
drop if tfp_growth==.
gen ln_gdp_per_capita=ln(gdp_per_capita)


bysort country: egen first_year=min(year)
bysort country: egen last_year=max(year)
tab first_year
tab last_year
local myfirst=1962
local mylast=2013
keep if first_year==`myfirst'
keep if last_year>=`mylast'
drop if year>`mylast'
tab country

* Make sure no country is missing substantial amounts of data in the middle
bysort country: gen cntryN=_N
tab cntryN
drop if cntryN<`mylast' - `myfirst' +1
tab cntryN

sort iso3 year
bysort iso3: gen ln_gdp_per_capita61_miss=ln_gdp_per_capita if year==1962
bysort iso3: egen ln_gdp_per_capita61=mean(ln_gdp_per_capita61_miss)
bysort iso3: gen ln_gdp_per_capita14_miss=ln_gdp_per_capita if year==2013
bysort iso3: egen ln_gdp_per_capita14=mean(ln_gdp_per_capita14_miss)

local gdp ln_gdp_per_capita61

* winsorize the data
drop if tfp_growth==.
summ tfp_growth, detail
replace tfp_growth=r(p1) if tfp_growth<r(p1)
replace tfp_growth=r(p99) if tfp_growth>r(p99)

capture drop tfp_growth_hat tfp_growth_hat_lo tfp_growth_hat_hi
gen tfp_growth_hat=.
gen tfp_growth_hat_lo=.
gen tfp_growth_hat_hi=.
forvalues y=`myfirst'/`mylast' {
capture drop gdp_spline*
mkspline gdp_spline = `gdp', cubic nknots(3) displayknots
matrix knot_mat=r(knots)
local knot1_`y'=knot_mat[1,1]
local knot2_`y'=knot_mat[1,2]
local knot3_`y'=knot_mat[1,3]
reg tfp_growth gdp_spline* if year==`y'
est sto tfp_growth_reg_`y'
predict yhat if year==`y'
predict yhat_se if year==`y', stdp
replace tfp_growth_hat=yhat if year==`y'
replace tfp_growth_hat_lo= yhat - 1.96*yhat_se
replace tfp_growth_hat_hi= yhat + 1.96*yhat_se
drop yhat yhat_se
}

sort year `gdp'
*twoway (line tfp_growth_hat `gdp') , by(year) scheme(538w)
twoway (line tfp_growth_hat `gdp') (scatter tfp_growth `gdp', msymbol(Oh) mcolor(%35) ), ///
		by(year, legend(off) note("")) scheme(538w) ///
		xtitle("Log GDP per Capita") ytitle("TFP Growth")
graph export "..\figures\tfp_growth_gdp_`com'.png", replace

preserve
collapse (p10) gdp10=`gdp' (p50) gdp50=`gdp' (p90) gdp90=`gdp', by(year)
gen `gdp'=.
foreach perc in 10 50 90 {
gen tfp_growth`perc'=.
forvalues y=`myfirst'/`mylast' {
	est restore tfp_growth_reg_`y'
	replace `gdp'=gdp`perc' if year==`y'
	capture drop gdp_spline*
	mkspline gdp_spline = `gdp', cubic knots(`knot1_`y'' `knot2_`y'' `knot3_`y'')
	predict yhat if year==`y'
	replace tfp_growth`perc'=yhat if year==`y'
	drop yhat
}
}
sort year
gen gap9010=tfp_growth90-tfp_growth10
gen gap9050=tfp_growth90-tfp_growth50
mkspline year_spline=year, cubic nknots(5)
reg gap9010 year_spline*
predict gap9010_hat
reg gap9050 year_spline*
predict gap9050_hat
line gap9010 gap9010_hat year,  scheme(538w) title("TFP Growth 90-10 Gap")
graph export "..\figures\gap9010_tfp.png", replace 
line gap9050 gap9050_hat year,  scheme(538w) title("TFP Growth 90-50 Gap")
graph export "..\figures\gap9050_tfp.png", replace 
restore

