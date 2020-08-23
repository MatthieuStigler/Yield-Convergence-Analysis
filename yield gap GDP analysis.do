foreach com in Maize Wheat Rice Soybeans {
*local com Rice
use "../temp/fullFAO_regression_data", clear
keep if commodity=="`com'"
gen gdp_per_capita= rgdpna/pop
drop if gdp_per_capita==.
drop if yield==.
gen ln_gdp_per_capita=ln(gdp_per_capita)
gen ln_yield=ln(yield)


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

* Make sure no country is missing substantial amounts of data in the middle
bysort country: gen cntryN=_N
tab cntryN
drop if cntryN<`mylast' - `myfirst' +1
tab cntryN

sort iso3 year
bysort iso3: gen ln_gdp_per_capita61_miss=ln_gdp_per_capita if year==1961
bysort iso3: egen ln_gdp_per_capita61=mean(ln_gdp_per_capita61_miss)
bysort iso3: gen ln_gdp_per_capita14_miss=ln_gdp_per_capita if year==2014
bysort iso3: egen ln_gdp_per_capita14=mean(ln_gdp_per_capita14_miss)

local gdp ln_gdp_per_capita61

capture drop yield_hat yield_hat_lo yield_hat_hi
gen yield_hat=.
gen yield_hat_lo=.
gen yield_hat_hi=.
forvalues y=`myfirst'/`mylast' {
capture drop gdp_spline*
mkspline gdp_spline = `gdp', cubic nknots(3) displayknots
matrix knot_mat=r(knots)
local knot1_`y'=knot_mat[1,1]
local knot2_`y'=knot_mat[1,2]
local knot3_`y'=knot_mat[1,3]
reg ln_yield gdp_spline* if year==`y'
est sto yield_reg_`y'
predict yhat if year==`y'
predict yhat_se if year==`y', stdp
replace yield_hat=yhat if year==`y'
replace yield_hat_lo= yhat - 1.96*yhat_se
replace yield_hat_hi= yhat + 1.96*yhat_se
drop yhat yhat_se
}

sort year `gdp'
*twoway (line yield_hat `gdp') , by(year) scheme(538w)
twoway (line yield_hat `gdp') (scatter ln_yield `gdp', msymbol(Oh) mcolor(%35) ), ///
		by(year, legend(off) note("")) scheme(538w) ///
		xtitle("Log GDP per Capita") ytitle("Log `com' Yield")
graph export "..\figures\yield_gdp_`com'.png", replace

preserve
collapse (p10) gdp10=`gdp' (p50) gdp50=`gdp' (p90) gdp90=`gdp', by(year)
gen `gdp'=.
foreach perc in 10 50 90 {
gen yield`perc'=.
forvalues y=`myfirst'/`mylast' {
	est restore yield_reg_`y'
	replace `gdp'=gdp`perc' if year==`y'
	capture drop gdp_spline*
	mkspline gdp_spline = `gdp', cubic knots(`knot1_`y'' `knot2_`y'' `knot3_`y'')
	predict yhat if year==`y'
	replace yield`perc'=yhat if year==`y'
	drop yhat
}
}
sort year
gen gap9010=yield90-yield10
gen gap9050=yield90-yield50
mkspline year_spline=year, cubic nknots(5)
reg gap9010 year_spline*
predict gap9010_hat
reg gap9050 year_spline*
predict gap9050_hat
line gap9010 gap9010_hat year,  scheme(538w) title("`com' 90-10 Gap")
graph export "..\figures\gap9010_`com'.png", replace 
line gap9050 gap9050_hat year,  scheme(538w) title("`com' 90-50 Gap")
graph export "..\figures\gap9050_`com'.png", replace 
restore
}
