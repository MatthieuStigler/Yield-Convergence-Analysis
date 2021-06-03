




/*

/*
* NRA 
scatter ln_yield_hat nra_hat
local controls nra_hat ln_yield_pot
qui{
reg ln_yield_hat `controls' i.year,  vce(cluster country_grp)
eststo ols
reg ln_yield_hat `controls' i.year pre vap,  vce(cluster country_grp)
eststo ols2
xtreg ln_yield_hat `controls' i.year, fe vce(cluster country_grp)
eststo fe
xtreg ln_yield_hat `controls' i.year pre vap, fe vce(cluster country_grp)
eststo fe2
}

esttab ols ols2 fe fe2, keep( `controls' pre vap) star(* 0.10 ** 0.05 *** 0.01)
*/

* Polity
scatter ln_yield_hat polity2 if year==2000
local controls polity2 ln_yield_pot
qui{
reg ln_yield_hat `controls' i.year,  vce(cluster country_grp)
eststo ols
reg ln_yield_hat `controls' i.year pre temp_max,  vce(cluster country_grp)
eststo ols2
xtreg ln_yield_hat `controls' i.year, fe vce(cluster country_grp)
eststo fe
xtreg ln_yield_hat `controls' i.year pre temp_max, fe vce(cluster country_grp)
eststo fe2
}

esttab ols ols2 fe fe2, keep( `controls' pre temp_max) star(* 0.10 ** 0.05 *** 0.01)



local institutions gov_antidiversion 

qui{
reg ln_yield_hat `institutions' ln_yield_pot i.year,  vce(cluster country_grp)
eststo ols
reg ln_yield_hat `institutions' ln_yield_pot pre temp_max i.year,  vce(cluster country_grp)
eststo ols2
xtreg ln_yield_hat `institutions'  i.year, fe vce(cluster country_grp)
eststo fe
xtreg ln_yield_hat `institutions' i.year pre temp_max, fe vce(cluster country_grp)
eststo fe2
}

esttab ols ols2 fe fe2, keep( `institutions' ln_yield_pot pre temp_max) star(* 0.10 ** 0.05 *** 0.01)

*/



*---------------------------------------------------------------------------------------------------
* 
*---------------------------------------------------------------------------------------------------
* World Bank governance indicators
* wb_gov_antidiversion is average of regulatory quality, rule of law, and control of corruption
* rle is rule of law. Part of this includes quality of contract enforcement and property rights
* vae "voice and accountability" is like a measure of democracy

* Use Acemoglu instrument

* Institutional quality from ICRG: property_rights, gov_antidiversion, inv_prof_index
* property_rights is how Knack and Keefer define property rights
* gov_antidiversion is like what is used in Hall and Jones
* inv_prof_index includes "contract viability/expropriation" similar to Acemoglu (2001)
* demo_account_index is a measure of democratic accountability from ICRG 


* Governance indicators from World Bank: wb_gov_antidiversion, rle

* Democracy measures: polity2 demo_account_index vae

local com Maize
local institutions property_rights
use "../dataAnalysis/yield_convergence_data_`com'", clear
reg ln_yield_hat `institutions' i.year#c.lnrd25lag i.year#c.ln_yield_pot i.year#c.pre i.year#c.temp_max i.year,  vce(cluster country_grp)



local inst_list property_rights gov_antidiversion demo_account_index
foreach com in Maize Wheat Rice {
use "../dataAnalysis/yield_convergence_data_`com'", clear 
bysort country: egen avg_ln_yield_hat=mean(ln_yield_hat) if ln_yield_hat!=. & property_rights!=.
foreach i in `inst_list' {
bysort country: egen avg_`i'=mean(`i') if ln_yield_hat!=. & `i'!=.
}
label var avg_property_rights "Property Rights"
label var avg_gov_antidiversion "Anti-Diversion"
label var avg_demo_account_index "Democratic Accountability"
foreach var of varlist lnrd25lag ln_yield_pot pre temp_max {
	summ `var' if ln_yield_hat!=. & property_rights!=.
	replace `var'=r(mean)
}
summ year if ln_yield_hat!=. & property_rights!=.
local myyear=round(r(mean))
keep if year==`myyear'
keep country countrycode iso3 commodity year lnrd25lag ln_yield_pot pre temp_max avg_ln_yield_hat property_rights gov_antidiversion demo_account_index avg_property_rights avg_gov_antidiversion avg_demo_account_index
save "../temp/institutionsICRG_pred_`com'", replace
}


tempfile myfile
foreach com in Maize Wheat Rice {
local institutions property_rights 
use "../dataAnalysis/yield_convergence_data_`com'", clear
reg ln_yield_hat `institutions' i.year#c.lnrd25lag i.year#c.ln_yield_pot i.year#c.pre i.year#c.temp_max i.year,  vce(cluster country_grp)
use "../temp/institutionsICRG_pred_`com'", clear
replace `institutions'=avg_`institutions'
predict yhat, xb
predict yhat_se, stdp
gen yhat_lo=yhat-1.645*yhat_se
gen yhat_hi=yhat+1.645*yhat_se
gen beta=_b[`institutions']
local t = _b[`institutions']/_se[`institutions']
gen pvalue = 2*ttail(e(df_r),abs(`t'))
gen r2=e(r2)

keep country countrycode iso3 commodity year property_rights ln_yield_pot pre temp_max yhat yhat_se yhat_lo yhat_hi avg_ln_yield_hat avg_`institutions' beta pvalue r2
capture append using `myfile'
save `myfile', replace
}

use `myfile', clear
save "../temp/`institutions'_graph_data", replace

use "../temp/`institutions'_graph_data", clear

sort avg_`institutions'
twoway (rarea yhat_hi yhat_lo avg_`institutions', color(gs12)) (line yhat avg_`institutions') (scatter avg_ln_yield_hat avg_`institutions') , xsize(10in) scheme(538w) by(commodity, legend(off) note("") cols(3)) name(property_rights, replace)


tempfile myfile
foreach com in Maize Wheat Rice {
local institutions gov_antidiversion 
use "../dataAnalysis/yield_convergence_data_`com'", clear
reg ln_yield_hat `institutions' i.year#c.lnrd25lag i.year#c.ln_yield_pot i.year#c.pre i.year#c.temp_max i.year,  vce(cluster country_grp)
use "../temp/institutionsICRG_pred_`com'", clear
replace `institutions'=avg_`institutions'
predict yhat, xb
predict yhat_se, stdp
gen yhat_lo=yhat-1.645*yhat_se
gen yhat_hi=yhat+1.645*yhat_se
gen beta=_b[`institutions']
local t = _b[`institutions']/_se[`institutions']
gen pvalue = 2*ttail(e(df_r),abs(`t'))
gen r2=e(r2)

keep country countrycode iso3 commodity year property_rights ln_yield_pot pre temp_max yhat yhat_se yhat_lo yhat_hi avg_ln_yield_hat avg_`institutions' beta pvalue r2
capture append using `myfile'
save `myfile', replace
}

use `myfile', clear
save "../temp/`institutions'_graph_data", replace

use "../temp/`institutions'_graph_data", clear

sort avg_`institutions'
twoway (rarea yhat_hi yhat_lo avg_`institutions', color(gs12)) (line yhat avg_`institutions') (scatter avg_ln_yield_hat avg_`institutions') , xsize(10in) scheme(538w) by(commodity, legend(off) note("") cols(3)) name(gov_antidiversion, replace)

tempfile myfile
foreach com in Maize Wheat Rice {
local institutions demo_account_index 
use "../dataAnalysis/yield_convergence_data_`com'", clear
reg ln_yield_hat `institutions' i.year#c.lnrd25lag i.year#c.ln_yield_pot i.year#c.pre i.year#c.temp_max i.year,  vce(cluster country_grp)
use "../temp/institutionsICRG_pred_`com'", clear
replace `institutions'=avg_`institutions'
predict yhat, xb
predict yhat_se, stdp
gen yhat_lo=yhat-1.645*yhat_se
gen yhat_hi=yhat+1.645*yhat_se
gen beta=_b[`institutions']
local t = _b[`institutions']/_se[`institutions']
gen pvalue = 2*ttail(e(df_r),abs(`t'))
gen r2=e(r2)

keep country countrycode iso3 commodity year property_rights ln_yield_pot pre temp_max yhat yhat_se yhat_lo yhat_hi avg_ln_yield_hat avg_`institutions' beta pvalue r2
capture append using `myfile'
save `myfile', replace
}

use `myfile', clear
save "../temp/`institutions'_graph_data", replace

use "../temp/`institutions'_graph_data", clear

sort avg_`institutions'
twoway (rarea yhat_hi yhat_lo avg_`institutions', color(gs12)) (line yhat avg_`institutions') (scatter avg_ln_yield_hat avg_`institutions') , xsize(10in) scheme(538w) by(commodity, legend(off) note("") cols(3)) name(demo_account_index, replace)



graph combine property_rights gov_antidiversion demo_account_index, cols(1) iscale(0.6) xsize(10in) ysize(10in) scheme(538w)
graph export "../figures/institutionsICRG_graph_combined.png", replace width(2000)


foreach institutions in property_rights gov_antidiversion demo_account_index {
use "../temp/`institutions'_graph_data", clear
foreach com in Maize Rice Wheat {
	di "`institutions' `com'"
	summ pvalue if commodity=="`com'"
}
}

foreach institutions in property_rights gov_antidiversion demo_account_index {
use "../temp/`institutions'_graph_data", clear
foreach com in Maize Rice Wheat {
	di "`institutions' `com'"
	summ avg_`institutions' if commodity=="`com'"
}
}

foreach institutions in property_rights gov_antidiversion demo_account_index {
use "../temp/`institutions'_graph_data", clear
foreach com in Maize Rice Wheat {
	di "`institutions' `com'"
	summ r2 if commodity=="`com'"
}
}

use "../temp/property_rights_graph_data", clear
keep if commodity=="Maize"
sort avg_property_rights
mean avg_property_rights avg_ln_yield_hat yhat if country=="Nigeria"
local yhat1=_b[yhat]
mean avg_property_rights avg_ln_yield_hat yhat if country=="United States of America"
local yhat2=_b[yhat]
di `yhat2' - `yhat1'

use "../temp/property_rights_graph_data", clear
keep if commodity=="Rice"
sort avg_property_rights
mean avg_property_rights avg_ln_yield_hat yhat if country=="Nigeria"
local yhat1=_b[yhat]
mean avg_property_rights avg_ln_yield_hat yhat if country=="United States of America"
local yhat2=_b[yhat]
di `yhat2' - `yhat1'



use "../temp/gov_antidiversion_graph_data", clear
keep if commodity=="Maize"
sort avg_gov_antidiversion
mean avg_gov_antidiversion avg_ln_yield_hat yhat if country=="Nigeria"
local yhat1=_b[yhat]
mean avg_gov_antidiversion avg_ln_yield_hat yhat if country=="United States of America"
local yhat2=_b[yhat]
di `yhat2' - `yhat1'

use "../temp/gov_antidiversion_graph_data", clear
keep if commodity=="Rice"
sort avg_gov_antidiversion
mean avg_gov_antidiversion avg_ln_yield_hat yhat if country=="Nigeria"
local yhat1=_b[yhat]
mean avg_gov_antidiversion avg_ln_yield_hat yhat if country=="United States of America"
local yhat2=_b[yhat]
di `yhat2' - `yhat1'

/*


tab year if ln_yield_hat!=.

foreach com in Maize Wheat Rice {
local institutions property_rights 
capture drop avg_ln_yield_hat
capture drop avg_`institutions'
bysort country: egen avg_ln_yield_hat=mean(ln_yield_hat) if ln_yield_hat!=. & `institutions'!=.
bysort country: egen avg_`institutions'=mean(`institutions') if ln_yield_hat!=. & `institutions'!=.
label var avg_`institutions' "Property Rights"
reg ln_yield_hat `institutions' i.year#c.ln_yield_pot i.year#c.pre i.year#c.temp_max i.year,  vce(cluster country_grp)
foreach var of varlist ln_yield_pot pre temp_max {
	summ `var' if ln_yield_hat!=. & `institutions'!=.
	replace `var'=r(mean)
}
summ year if ln_yield_hat!=. & `institutions'!=.
local myyear=round(r(mean))
replace year=`myyear'
replace `institutions'=avg_`institutions'
capture drop yhat yhat_se yhat_lo yhat_hi
predict yhat, xb
predict yhat_se, stdp
gen yhat_lo=yhat-1.645*yhat_se
gen yhat_hi=yhat+1.645*yhat_se
sort avg_`institutions'
twoway (rarea yhat_hi yhat_lo avg_`institutions', color(gs12)) (line yhat avg_`institutions') (scatter avg_ln_yield_hat avg_`institutions') ,  legend(off) scheme(538w) name(`institutions'_`com', replace)
}


graph combine property_rights_Maize property_rights_Wheat property_rights_Rice, cols(3) iscale(1) xsize(15in) ysize(7in) scheme(538w)
graph export "../figures/institutions_combined.png", replace	width(2000)	