foreach com in maize wheat {
use "../temp/commodity-level_regression_data", clear
keep if commodity=="`com'"

bysort country: gen cntryN=_N
tab cntryN
drop if cntryN<44

capture drop yield_hat yield_hat_lo yield_hat_hi
gen yield_hat=.
gen yield_hat_lo=.
gen yield_hat_hi=.
forvalues y=1962/2010 {
capture drop gdp_spline*
mkspline gdp_spline = ln_gdp_per_capita, cubic nknots(3) displayknots
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

sort year ln_gdp_per_capita
*twoway (line yield_hat ln_gdp_per_capita) , by(year)
*twoway (line yield_hat ln_gdp_per_capita) (scatter ln_yield ln_gdp_per_capita), by(year)

preserve
collapse (p10) gdp10=ln_gdp_per_capita (p90) gdp90=ln_gdp_per_capita, by(year)
gen ln_gdp_per_capita=.
gen yield10=.
gen yield90=.
forvalues y=1962/2010 {
	est restore yield_reg_`y'
	replace ln_gdp_per_capita=gdp10 if year==`y'
	capture drop gdp_spline*
	mkspline gdp_spline = ln_gdp_per_capita, cubic knots(`knot1_`y'' `knot2_`y'' `knot3_`y'')
	predict yhat if year==`y'
	replace yield10=yhat if year==`y'
	drop yhat
	replace ln_gdp_per_capita=gdp90 if year==`y'
	capture drop gdp_spline*
	mkspline gdp_spline = ln_gdp_per_capita, cubic knots(`knot1_`y'' `knot2_`y'' `knot3_`y'')
	predict yhat if year==`y'
	replace yield90=yhat if year==`y'
	drop yhat
}
sort year
gen gap9010=yield90-yield10
mkspline year_spline=year, cubic nknots(3)
reg gap9010 year_spline*
predict gap9010_hat
line gap9010 gap9010_hat year,  scheme(538w) title("`com'")
graph export "..\figures\gap9010_`com'.png", replace 
restore
}
