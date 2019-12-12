foreach com in Maize Wheat Rice {
use "..\temp\yield_pred_`com'", clear
gen production_hat=yield_hat*area_hat
replace production_hat=0 if production_hat<0 & production_hat!=.
drop if production_hat==.
collapse (p5) yield05=yield_hat (p10) yield10=yield_hat (p50) yield50=yield_hat (p90) yield90=yield_hat (p95) yield95=yield_hat [aw=production_hat], by(year)
save "..\temp\yield_percentiles_`com'", replace

} // end of loop over commodities



local com Wheat
use "..\temp\yield_pred_`com'", clear
gen commodity="`com'"
merge m:1 year using "../temp/yield_percentiles_`com'", keep(match master) nogen
merge 1:1 country commodity year using "../temp/revisedMatthieu_regression_data", keep(match master) nogen
merge 1:1 country year using "../temp/Cntry_Year_data", keep(match master)
ren dai3__nra nra
ren wdi__gdp_ppp_2011 gdp
gen lngdp=ln(gdp)
gen yield_gap=(yield95 - yield_hat)/yield95 
xtset country_grp year
drop if nra==.
capture drop country_grp
egen country_grp=group(country), label
gen nra_hat=.

summ country_grp
local max_country=r(mean)
forvalues j=1/`max_country' {

capture drop year_spline*
mkspline year_spline=year, cubic nknots(5)

reg nra year_spline* if country_grp==`j'
predict yhat if country_grp==`j'
replace nra_hat=yhat if country_grp==`j'
drop yhat 
}

summ yield_gap
summ nra

reg yield_gap nra_hat lngdp
eststo ols
xtreg yield_gap nra_hat lngdp, fe
eststo fe
xtreg yield_gap nra_hat lngdp i.year, fe
eststo fe2

esttab ols fe fe2, keep(nra_hat lngdp)

sort country_grp year
reg S1.(yield_gap nra_hat lngdp) i.year
eststo S1
reg S5.(yield_gap nra_hat lngdp) i.year
eststo S5
reg S10.(yield_gap nra_hat lngdp) i.year
eststo S10
reg S20.(yield_gap nra_hat lngdp) i.year
eststo S20

esttab S1 S5 S10 S20, keep(S.nra_hat S5.nra_hat S10.nra_hat S20.nra_hat S.lngdp  S5.lngdp  S10.lngdp  S20.lngdp)
