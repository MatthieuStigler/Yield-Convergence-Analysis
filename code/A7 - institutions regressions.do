

foreach com in Maize Wheat Rice {
*local com Maize
* Define which variable to use to measure potential yield
if "`com'"=="Maize" | "`com'"=="Wheat"  {
local potential_yield potent_rain_int
}
if "`com'"=="Rice" {
local potential_yield post1500AverageCaloriesmean
}
* Create dataset of smoothed NRA values
use "../temp/revisedMatthieu_regression_data", clear
keep if commodity=="`com'"
ren dai3__nra nra
drop if nra==.
capture drop country_grp
egen country_grp=group(country), label
xtset country_grp year
gen nra_hat=.

summ country_grp
local max_country=r(max)
forvalues j=1/`max_country' {

capture drop year_spline*
mkspline year_spline=year, cubic nknots(5)

qui reg nra year_spline* if country_grp==`j'
qui predict yhat if country_grp==`j'
qui replace nra_hat=yhat if country_grp==`j'
drop yhat 
}
keep country commodity year nra_hat
save "../temp/nra_hat_`com'", replace

use "..\temp\yield_pred_`com'", clear
gen commodity="`com'"
merge m:1 year using "../temp/yield_percentiles_`com'", keep(match master) nogen

* Polity data
merge 1:1 iso3 year using "..\temp\p4v2016", keep(match master) nogen
* Smoothed NRA
merge 1:1 country commodity year using "../temp/nra_hat_`com'", keep(match master) nogen
* Merge the rest of the NRA data
merge 1:1 country commodity year using "../temp/revisedMatthieu_regression_data", keep(match master) nogen
merge 1:1 country year using "../temp/Cntry_Year_data", keep(match master) nogen
* Penn World table
merge 1:1 iso3 year using "../temp/pwt90_iso3", keep(match master) nogen
* Agriculture R&D Expenditures
merge 1:1 countrycode year using "../temp/rd25lag", keep(match master) nogen
gen lnrd25lag=ln(rd25lag)
* Caloric data from GAEZ obtained from here: https://ozak.github.io/Caloric-Suitability-Index/
* Should cite Oded Galor and Ömer Özak, 2016. “The Agricultural Origins of Time Preference,” American Economic Review, 2016, 106(10): 3064–3103.
merge m:1 iso3 using "../temp/country_Calories_potential", keep(match master) nogen

* GAEZ yield potential and yield gap data
* This dataset was only available for Maize and Wheat
if "`com'"=="Maize" | "`com'"=="Wheat"  {
merge m:1 iso3 using "../temp/`com'_yield_potential", keep(match master) nogen
}

merge m:1 iso3 using "../temp/ygapGAEZ_`com'", keep(match master) nogen

*------------------------------------------------------------------------------------------
* Different measures of institutions
*------------------------------------------------------------------------------------------
* World Bank Worldwide Governance Indicators (1996-2019)
ren iso3 code
merge 1:1 code year using "../dataRaw/wgidataset_stata/wgidataset", keep(match master) nogen
ren code iso3
tab year if vae==.

summ rqe rle cce
gen wb_gov_antidiversion=(rqe + rle + cce)/3

* ICRG Political Risk
merge 1:1 countrycode year using "E:/Institutions/ICRG Data/stata/ICRG Political Risk", keep(match master) nogen

gen inv_prof_index=inv_prof_icrg/12
gen law_order_index=law_order_icrg/6
gen bureauc_qual_index=bureauc_qual_icrg/4
gen corruption_index=corruption_icrg/6

gen demo_account_index=demo_account_icrg/6

summ inv_prof_index law_order_index bureauc_qual_index corruption_index demo_account_index

* These are definitions from the Knack and Keefer Paper
gen property_rights=(inv_prof_index + law_order_index)/2
gen eff_gov_service=(bureauc_qual_index + corruption_index)/2
* Used by Hall and Jones
gen gov_antidiversion=(inv_prof_index + law_order_index + bureauc_qual_index + corruption_index)/4

* Polity dummy variables
gen polity_democ=(polity2>0) if polity2!=.
gen polity_vlow=(polity2<=-5) if polity2!=.
gen polity_low=(polity2>-5 & polity2<=0) if polity2!=.
gen polity_high=(polity2>0 & polity2<=5) if polity2!=.
gen polity_vhigh=(polity2>5) if polity2!=.
gen democ01=(democ>0) if democ!=. & democ!=-88 & democ!=-77 & democ!=-66
gen autoc01=(autoc>0) if autoc!=. & autoc!=-88 & autoc!=-77 & autoc!=-66

ren cru__ave3_pre_w_finarea pre
ren cru__ave3_tmn_w_finarea temp_min
ren cru__ave3_tmp_w_finarea temp_avg
ren cru__ave3_tmx_w_finarea temp_max
ren cru__ave3_vap_w_finarea vap
ren cru__ave3_pet_w_finarea pet
corr vap pet temp_max

* Use GDP per capita from Penn World Table
gen gdp_per_capita= rgdpna/pop
gen ln_gdp_per_capita=ln(gdp_per_capita)


gen antiAg=(nra_hat <0) if nra_hat!=.
*bysort country: egen sum_antiAg=total(antiAg)
*tab sum_antiAg
*replace antiAg=0 if sum_antiAg<10
gen proAg=(nra_hat >0) if nra_hat!=.
*bysort country: egen sum_proAg=total(proAg)
*tab sum_proAg
*replace proAg=0 if sum_proAg<10
summ antiAg proAg



gen yield_gap=yield_hat/yield90

gen ln_yield=ln(yield_hat)
gen ln_yield_pot=ln(`potential_yield')

sort country_grp year
*gen yield_growth1=d.ln_yield
gen lln_yield=l.ln_yield
gen yield_growth1=ln(yield_hat/l.yield_hat)
gen yield_growth20=ln(yield_hat/l20.yield_hat)/20
gen yield_growth30=ln(yield_hat/l30.yield_hat)/30
*gen yield_chg=d.yield_hat

*------------------------------------------------------------------------
* Drop countries with strange data
* These are strange for maize
if "`com'"=="Maize" {
drop if country=="Democratic People's Republic of Korea"
drop if country=="Togo"
}
* These are strange for rice
if "`com'"=="Rice" {
drop if country=="Guinea"
}
*--------------------------------------------------------------------------

summ yield_growth*
summ nra polity_democ pre vap pet

save "../temp/institutions_reg_data_`com'", replace
}



/*

/*
* NRA 
scatter ln_yield nra_hat
local controls nra_hat ln_yield_pot
qui{
reg ln_yield `controls' i.year,  vce(cluster country_grp)
eststo ols
reg ln_yield `controls' i.year pre vap,  vce(cluster country_grp)
eststo ols2
xtreg ln_yield `controls' i.year, fe vce(cluster country_grp)
eststo fe
xtreg ln_yield `controls' i.year pre vap, fe vce(cluster country_grp)
eststo fe2
}

esttab ols ols2 fe fe2, keep( `controls' pre vap) star(* 0.10 ** 0.05 *** 0.01)
*/

* Polity
scatter ln_yield polity2 if year==2000
local controls polity2 ln_yield_pot
qui{
reg ln_yield `controls' i.year,  vce(cluster country_grp)
eststo ols
reg ln_yield `controls' i.year pre temp_max,  vce(cluster country_grp)
eststo ols2
xtreg ln_yield `controls' i.year, fe vce(cluster country_grp)
eststo fe
xtreg ln_yield `controls' i.year pre temp_max, fe vce(cluster country_grp)
eststo fe2
}

esttab ols ols2 fe fe2, keep( `controls' pre temp_max) star(* 0.10 ** 0.05 *** 0.01)



local institutions gov_antidiversion 

qui{
reg ln_yield `institutions' ln_yield_pot i.year,  vce(cluster country_grp)
eststo ols
reg ln_yield `institutions' ln_yield_pot pre temp_max i.year,  vce(cluster country_grp)
eststo ols2
xtreg ln_yield `institutions'  i.year, fe vce(cluster country_grp)
eststo fe
xtreg ln_yield `institutions' i.year pre temp_max, fe vce(cluster country_grp)
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
use "../temp/institutions_reg_data_`com'", clear
reg ln_yield `institutions' i.year#c.lnrd25lag i.year#c.ln_yield_pot i.year#c.pre i.year#c.temp_max i.year,  vce(cluster country_grp)



local inst_list property_rights gov_antidiversion demo_account_index
foreach com in Maize Wheat Rice {
use "../temp/institutions_reg_data_`com'", clear 
bysort country: egen avg_ln_yield=mean(ln_yield) if ln_yield!=. & property_rights!=.
foreach i in `inst_list' {
bysort country: egen avg_`i'=mean(`i') if ln_yield!=. & `i'!=.
}
label var avg_property_rights "Property Rights"
label var avg_gov_antidiversion "Anti-Diversion"
label var avg_demo_account_index "Democratic Accountability"
foreach var of varlist lnrd25lag ln_yield_pot pre temp_max {
	summ `var' if ln_yield!=. & property_rights!=.
	replace `var'=r(mean)
}
summ year if ln_yield!=. & property_rights!=.
local myyear=round(r(mean))
keep if year==`myyear'
keep country countrycode iso3 commodity year lnrd25lag ln_yield_pot pre temp_max avg_ln_yield property_rights gov_antidiversion demo_account_index avg_property_rights avg_gov_antidiversion avg_demo_account_index
save "../temp/institutionsICRG_pred_`com'", replace
}


tempfile myfile
foreach com in Maize Wheat Rice {
local institutions property_rights 
use "../temp/institutions_reg_data_`com'", clear
reg ln_yield `institutions' i.year#c.lnrd25lag i.year#c.ln_yield_pot i.year#c.pre i.year#c.temp_max i.year,  vce(cluster country_grp)
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

keep country countrycode iso3 commodity year property_rights ln_yield_pot pre temp_max yhat yhat_se yhat_lo yhat_hi avg_ln_yield avg_`institutions' beta pvalue r2
capture append using `myfile'
save `myfile', replace
}

use `myfile', clear
save "../temp/`institutions'_graph_data", replace

use "../temp/`institutions'_graph_data", clear

sort avg_`institutions'
twoway (rarea yhat_hi yhat_lo avg_`institutions', color(gs12)) (line yhat avg_`institutions') (scatter avg_ln_yield avg_`institutions') , xsize(10in) scheme(538w) by(commodity, legend(off) note("") cols(3)) name(property_rights, replace)


tempfile myfile
foreach com in Maize Wheat Rice {
local institutions gov_antidiversion 
use "../temp/institutions_reg_data_`com'", clear
reg ln_yield `institutions' i.year#c.lnrd25lag i.year#c.ln_yield_pot i.year#c.pre i.year#c.temp_max i.year,  vce(cluster country_grp)
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

keep country countrycode iso3 commodity year property_rights ln_yield_pot pre temp_max yhat yhat_se yhat_lo yhat_hi avg_ln_yield avg_`institutions' beta pvalue r2
capture append using `myfile'
save `myfile', replace
}

use `myfile', clear
save "../temp/`institutions'_graph_data", replace

use "../temp/`institutions'_graph_data", clear

sort avg_`institutions'
twoway (rarea yhat_hi yhat_lo avg_`institutions', color(gs12)) (line yhat avg_`institutions') (scatter avg_ln_yield avg_`institutions') , xsize(10in) scheme(538w) by(commodity, legend(off) note("") cols(3)) name(gov_antidiversion, replace)

tempfile myfile
foreach com in Maize Wheat Rice {
local institutions demo_account_index 
use "../temp/institutions_reg_data_`com'", clear
reg ln_yield `institutions' i.year#c.lnrd25lag i.year#c.ln_yield_pot i.year#c.pre i.year#c.temp_max i.year,  vce(cluster country_grp)
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

keep country countrycode iso3 commodity year property_rights ln_yield_pot pre temp_max yhat yhat_se yhat_lo yhat_hi avg_ln_yield avg_`institutions' beta pvalue r2
capture append using `myfile'
save `myfile', replace
}

use `myfile', clear
save "../temp/`institutions'_graph_data", replace

use "../temp/`institutions'_graph_data", clear

sort avg_`institutions'
twoway (rarea yhat_hi yhat_lo avg_`institutions', color(gs12)) (line yhat avg_`institutions') (scatter avg_ln_yield avg_`institutions') , xsize(10in) scheme(538w) by(commodity, legend(off) note("") cols(3)) name(demo_account_index, replace)



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
mean avg_property_rights avg_ln_yield yhat if country=="Nigeria"
local yhat1=_b[yhat]
mean avg_property_rights avg_ln_yield yhat if country=="United States of America"
local yhat2=_b[yhat]
di `yhat2' - `yhat1'

use "../temp/property_rights_graph_data", clear
keep if commodity=="Rice"
sort avg_property_rights
mean avg_property_rights avg_ln_yield yhat if country=="Nigeria"
local yhat1=_b[yhat]
mean avg_property_rights avg_ln_yield yhat if country=="United States of America"
local yhat2=_b[yhat]
di `yhat2' - `yhat1'



use "../temp/gov_antidiversion_graph_data", clear
keep if commodity=="Maize"
sort avg_gov_antidiversion
mean avg_gov_antidiversion avg_ln_yield yhat if country=="Nigeria"
local yhat1=_b[yhat]
mean avg_gov_antidiversion avg_ln_yield yhat if country=="United States of America"
local yhat2=_b[yhat]
di `yhat2' - `yhat1'

use "../temp/gov_antidiversion_graph_data", clear
keep if commodity=="Rice"
sort avg_gov_antidiversion
mean avg_gov_antidiversion avg_ln_yield yhat if country=="Nigeria"
local yhat1=_b[yhat]
mean avg_gov_antidiversion avg_ln_yield yhat if country=="United States of America"
local yhat2=_b[yhat]
di `yhat2' - `yhat1'

/*


tab year if ln_yield!=.

foreach com in Maize Wheat Rice {
local institutions property_rights 
capture drop avg_ln_yield
capture drop avg_`institutions'
bysort country: egen avg_ln_yield=mean(ln_yield) if ln_yield!=. & `institutions'!=.
bysort country: egen avg_`institutions'=mean(`institutions') if ln_yield!=. & `institutions'!=.
label var avg_`institutions' "Property Rights"
reg ln_yield `institutions' i.year#c.ln_yield_pot i.year#c.pre i.year#c.temp_max i.year,  vce(cluster country_grp)
foreach var of varlist ln_yield_pot pre temp_max {
	summ `var' if ln_yield!=. & `institutions'!=.
	replace `var'=r(mean)
}
summ year if ln_yield!=. & `institutions'!=.
local myyear=round(r(mean))
replace year=`myyear'
replace `institutions'=avg_`institutions'
capture drop yhat yhat_se yhat_lo yhat_hi
predict yhat, xb
predict yhat_se, stdp
gen yhat_lo=yhat-1.645*yhat_se
gen yhat_hi=yhat+1.645*yhat_se
sort avg_`institutions'
twoway (rarea yhat_hi yhat_lo avg_`institutions', color(gs12)) (line yhat avg_`institutions') (scatter avg_ln_yield avg_`institutions') ,  legend(off) scheme(538w) name(`institutions'_`com', replace)
}


graph combine property_rights_Maize property_rights_Wheat property_rights_Rice, cols(3) iscale(1) xsize(15in) ysize(7in) scheme(538w)
graph export "../figures/institutions_combined.png", replace	width(2000)	