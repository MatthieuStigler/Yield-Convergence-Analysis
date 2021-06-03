import excel using "../dataRaw/World Bank Region Classifications.xlsx", clear sheet("List of economies") cellrange(D5:F224) firstrow
drop if _n==1
drop X
ren Code iso3
ren Region WBregion
save "../temp/World Bank Region Classifications", replace

local com Maize 
use "../dataAnalysis/yield_convergence_data_`com'", clear

merge m:1 iso3 using "../temp/World Bank Region Classifications", keep(match master)



gen myregion=.
replace myregion=1 if WBregion=="Sub-Saharan Africa"
replace myregion=2 if WBregion=="Latin America & Caribbean"
replace myregion=3 if WBregion=="East Asia & Pacific" | WBregion=="South Asia"
replace myregion=4 if WBregion=="Europe & Central Asia" | region=="North America" 

sort country_grp year

twoway (scatter yield_hat_growth29 l29.ln_yield_hat if year==2010 & myregion==1, mlabel(iso3) mlabposition(0) msymbol(none) mlabcolor(blue)) ///
		(lfit yield_hat_growth29 l29.ln_yield_hat if year==2010 & myregion==1, lcolor(blue)) ///
		(scatter yield_hat_growth29 l29.ln_yield_hat if year==2010 & myregion==2, mlabel(iso3) mlabposition(0) msymbol(none) mlabcolor(red)) ///
		(lfit yield_hat_growth29 l29.ln_yield_hat if year==2010 & myregion==2, lcolor(red)) ///
		(scatter yield_hat_growth29 l29.ln_yield_hat if year==2010 & myregion==3, mlabel(iso3) mlabposition(0) msymbol(none) mlabcolor(green)) ///
		(lfit yield_hat_growth29 l29.ln_yield_hat if year==2010 & myregion==3, lcolor(green)) ///
		(scatter yield_hat_growth29 l29.ln_yield_hat if year==2010 & myregion==4, mlabel(iso3) mlabposition(0) msymbol(none) mlabcolor(black)) ///
		(lfit yield_hat_growth29 l29.ln_yield_hat if year==2010 & myregion==4, lcolor(black)), ///
 scale(1.25)   ytitle("Yield Growth 1981-2010") xtitle("1981 Log Yield") scheme(538w) 

 
gen income_grp=.
replace income_grp= 1 if gdp_per_capita<755 & gdp_per_capita!=.
replace income_grp= 2 if  gdp_per_capita>=755 & gdp_per_capita<9265 & gdp_per_capita!=.
replace income_grp= 3 if  gdp_per_capita>=9265 & gdp_per_capita!=.
 
twoway (scatter yield_hat_growth29 l29.ln_yield_hat if year==2010 & income_grp==1, mlabel(iso3) mlabposition(0) msymbol(none) mlabcolor(blue)) ///
		(lfit yield_hat_growth29 l29.ln_yield_hat if year==2010 & income_grp==1, lcolor(blue)) ///
		(scatter yield_hat_growth29 l29.ln_yield_hat if year==2010 & income_grp==2, mlabel(iso3) mlabposition(0) msymbol(none) mlabcolor(red)) ///
		(lfit yield_hat_growth29 l29.ln_yield_hat if year==2010 & income_grp==2, lcolor(red)) ///
		(scatter yield_hat_growth29 l29.ln_yield_hat if year==2010 & income_grp==3, mlabel(iso3) mlabposition(0) msymbol(none) mlabcolor(green)) ///
		(lfit yield_hat_growth29 l29.ln_yield_hat if year==2010 & income_grp==3, lcolor(green)) , ///
 scale(1.25)   ytitle("Yield Growth 1981-2010") xtitle("1981 Log Yield") scheme(538w) 
 
 
 
foreach com in Maize Wheat Rice {

use "../dataAnalysis/yield_convergence_data_`com'", clear
	

summ yield_hat_growth*
summ nra polity_democ pre vap pet




*------------------------------------------------------------------------
* Plots of growth versus initial
*------------------------------------------------------------------------
scatter yield_hat_growth29 l29.ln_yield_hat if year==2010, mlabel(iso3) mlabposition(0) msymbol(none) ///
 scale(1.25)   ytitle("Yield Growth 1981-2010") xtitle("1981 Log Yield") scheme(538w) name(growth_v_init_`com'2010, replace)

scatter yield_hat_growth29 l29.ln_yield_hat if year==1990, mlabel(iso3) mlabposition(0) msymbol(none) ///
 scale(1.25)   ytitle("Yield Growth 1961-1990") xtitle("1961 Log Yield") scheme(538w) name(growth_v_init_`com'1990, replace)
*------------------------------------------------------------------------
* Conditional test for yield convergence
*------------------------------------------------------------------------
summ year
reg yield_hat_growth1 l.ln_yield_hat ln_yield_pot i.year, vce(cluster country_grp)
eststo ols_`com'
xtreg yield_hat_growth1 l.ln_yield_hat i.year, fe vce(cluster country_grp)
eststo fe_`com'

esttab ols_`com' fe_`com', keep( L.ln_yield_hat ln_yield_pot) star(* 0.10 ** 0.05 *** 0.01)

*------------------------------------------------------------------------
* Graphs of cross-section estimates over time
*------------------------------------------------------------------------
summ year if yield_hat_growth1!=. & l.ln_yield_hat!=.
local min_year=r(min)
local max_year=r(max)
tempname memhold
tempfile conv_results
postfile `memhold' year beta low high using `conv_results', replace
forvalues y=`min_year'/`max_year' {
qui reg yield_hat_growth1 l.ln_yield_hat `potential_yield' if year==`y', vce(cluster country_grp)
post `memhold' (`y') (_b[L.ln_yield_hat]) (_b[L.ln_yield_hat]-1.645*_se[L.ln_yield_hat]) (_b[L.ln_yield_hat]+1.645*_se[L.ln_yield_hat]) 
}
postclose `memhold'

use `conv_results', clear

twoway  (rarea high low year, color("238 108 77") fintensity(inten20) lwidth(none)) (line beta year, lcolor("238 108 77")), ///
		ytitle("Convergence Coefficient") xtitle("") graphregion(color(white)) scale(1.25) ylabel(, nogrid angle(horizontal)) ///
		xlabel(, angle(horizontal) nogrid) yline(0, lcolor(gs10))  ///
		legend(off) title("`com'") ///
		scheme(538w) name(conv_`com', replace)
graph export "../figures/convergence_coefficient_`com'.png", replace
* export data to create graph in R
export delimited "../temp/conv_results_`com'.csv", replace
}		

graph combine conv_Maize conv_Wheat conv_Rice, cols(1) ysize(10) ycommon scheme(538w)
graph export "../figures/convergence_coefficient_combined.png", replace	width(2000)	
		
graph combine growth_v_init_Maize2010 growth_v_init_Wheat2010 growth_v_init_Rice2010 growth_v_init_Maize1990 growth_v_init_Wheat1990 growth_v_init_Rice1990, cols(3)iscale(1) xsize(15in) ysize(7in) scheme(538w)
graph export "../figures/growth_v_init_combined.png", replace	width(2000)
		
esttab ols_Maize fe_Maize using "../tables/convergence_tests", keep( L.ln_yield_hat ln_yield_pot) b(%9.3f) se(%9.3f) se star(* 0.10 ** 0.05 *** 0.01) mtitles("Maize OLS" "Maize FE") coeflabels(L.ln_yield_hat "Log Yield\$_{t-1}\$" ln_yield_pot "Log Potential Yield") substitute(\_ _)	tex replace	
esttab ols_Wheat fe_Wheat using "../tables/convergence_tests", keep( L.ln_yield_hat ln_yield_pot) b(%9.3f) se(%9.3f) se star(* 0.10 ** 0.05 *** 0.01) mtitles("Wheat OLS" "Wheat FE") coeflabels(L.ln_yield_hat "Log Yield\$_{t-1}\$" ln_yield_pot "Log Potential Yield") substitute(\_ _)	tex append
esttab ols_Rice fe_Rice using "../tables/convergence_tests", keep( L.ln_yield_hat ln_yield_pot) b(%9.3f) se(%9.3f) se star(* 0.10 ** 0.05 *** 0.01) mtitles("Rice OLS" "Rice FE") coeflabels(L.ln_yield_hat "Log Yield\$_{t-1}\$" ln_yield_pot "Log Potential Yield") substitute(\_ _)	tex append				
		
		
		
		
		
		
		
		
		
		
		
		
		
		
/*

reg ygapGAEZ_rain polity2 if year==2000, vce(cluster country_grp)
reg yield_growth1 polity2 if year==2000, vce(cluster country_grp)
reg yield_growth1 l.ln_yield polity2 if year==2000, vce(cluster country_grp)
reg ygapGAEZ_rain nra_hat if year==2000, vce(cluster country_grp)


reg ygapGAEZ_rain polity2 if year==2000, vce(cluster country_grp)
reg ln_ygapGAEZ_rain polity2 if year==2000, vce(cluster country_grp)
reg ln_ygapGAEZ_rain polity2 potent_rain_int if year==2000, vce(cluster country_grp)
reg ln_yield polity2  if year==2000, vce(cluster country_grp)
reg ln_yield polity2 potent_rain_int if year==2000, vce(cluster country_grp)


reg mygap polity2 if year==2000, vce(cluster country_grp)

gen ln_yieldGAEZ=ln_ygapGAEZ_rain + ln_yield_pot

reg ln_yieldGAEZ polity2 if year==2000, vce(cluster country_grp)
reg ln_yieldGAEZ polity2 ln_yield_pot if year==2000, vce(cluster country_grp)


reg yield_growth1  polity2 l.ln_yield if year==2000, vce(cluster country_grp)
reg ln_yield  polity2 l.ln_yield if year==2000, vce(cluster country_grp)
reg ln_yield polity2 ln_yield_pot if year==2000, vce(cluster country_grp)



reg ln_ygapGAEZ_rain nra_hat polity2 if year==2000, vce(cluster country_grp)
reg ln_yield nra_hat polity2 ln_yield_pot if year==2000, vce(cluster country_grp)

reg yield_growth1 polity2 if year==2000, vce(cluster country_grp)
reg yield_growth1  polity2 l.ln_yield if year==2000, vce(cluster country_grp)
reg ln_yield  polity2 l.ln_yield if year==2000, vce(cluster country_grp)
reg ln_yield  polity2 l.ln_yield ln_yield_pot if year==2000, vce(cluster country_grp)

local controls  polity2 ln_yield_pot
*local controls  polity_low polity_high polity_vhigh ln_yield_pot
qui{
reg ln_yield nra_hat `controls' if year==2000,  vce(cluster country_grp)
eststo ols2000
reg ln_yield nra_hat `controls' i.year,  vce(cluster country_grp)
eststo ols
xtreg ln_yield nra_hat `controls' year, fe vce(cluster country_grp)
eststo fe
xtreg ln_yield nra_hat `controls' i.year, fe vce(cluster country_grp)
eststo fe2
}

esttab ols2000 ols fe fe2, keep(nra_hat  `controls') star(* 0.1 ** 0.05 *** 0.01)



local controls  polity2 L.ln_yield 
*local controls  polity_low polity_high polity_vhigh ln_yield_pot
qui{
reg yield_growth1 nra_hat `controls' if year==2000,  vce(cluster country_grp)
eststo ols2000
reg yield_growth1 nra_hat `controls' i.year,  vce(cluster country_grp)
eststo ols
xtreg yield_growth1 nra_hat `controls' year, fe vce(cluster country_grp)
eststo fe
xtreg yield_growth1 nra_hat `controls' i.year, fe vce(cluster country_grp)
eststo fe2
}

esttab ols2000 ols fe fe2, keep(nra_hat  `controls') star(* 0.1 ** 0.05 *** 0.01)


twoway (lfit ln_yield polity2) (scatter ln_yield polity2, msymbol(Oh) mcolor(gs8%60)) if year==2000,  scheme(538w) ytitle("Actual Yield/Potential Yield") xtitle("Polity Score") title("`com'")
graph export "..\figures\yieldgap_polity_`com'.png", replace width(2000)

twoway (lfit ln_yield nra_hat) (scatter ln_yield nra_hat, msymbol(Oh) mcolor(gs8%60)) if year==2000,  scheme(538w) ytitle("Actual Yield/Potential Yield") xtitle("NRA") title("`com'")
graph export "..\figures\yieldgap_nra_`com'.png"

scatter ln_yield ln_yield_pot if year==2000, mlabel(iso3)


*------------------------------------------------------------------------
* Unconditional test for yield convergence
*------------------------------------------------------------------------
reg yield_growth1 l.ln_yield if year==1965, vce(cluster country_grp)
scatter yield_growth1 l1.ln_yield if year==1965
reg yield_growth1 l.ln_yield if year==1985, vce(cluster country_grp)
reg yield_growth1 l.ln_yield if year==1995, vce(cluster country_grp)
reg yield_growth1 l.ln_yield if year==2005, vce(cluster country_grp)


reg yield_growth20 l20.ln_yield if year==1985, vce(cluster country_grp)
reg yield_growth20 l20.ln_yield  if year==2005, vce(cluster country_grp)

reg yield_growth30 l30.ln_yield if year==2010, vce(cluster country_grp)
scatter yield_growth30 l30.ln_yield if year==2010

reg yield_growth30 l30.ln_yield if year==1991, vce(cluster country_grp)
scatter yield_growth30 l30.ln_yield if year==1991

*------------------------------------------------------------------------
* Conditional test for yield convergence
*------------------------------------------------------------------------
reg yield_growth1 l.ln_yield `potential_yield' if year==1965, vce(cluster country_grp)
scatter yield_growth1 l1.ln_yield if year==1965
reg yield_growth1 l.ln_yield `potential_yield' if year==1985, vce(cluster country_grp)
reg yield_growth1 l.ln_yield `potential_yield' if year==1995, vce(cluster country_grp)
reg yield_growth1 l.ln_yield `potential_yield' if year==2005, vce(cluster country_grp)


reg yield_growth20 l20.ln_yield `potential_yield' if year==1985, vce(cluster country_grp)
reg yield_growth20 l20.ln_yield `potential_yield' if year==2005, vce(cluster country_grp)

reg yield_growth30 l30.ln_yield `potential_yield' if year==2010, vce(cluster country_grp)
scatter yield_growth30 l30.ln_yield if year==2010

reg yield_growth30 l30.ln_yield `potential_yield' if year==1991, vce(cluster country_grp)
scatter yield_growth30 l30.ln_yield if year==1991

*------------------------------------------------------------------------
* Without NRA
*------------------------------------------------------------------------
*local controls  polity2 potent_rain_low
*local potential_yield potent_rain_int
local controls  polity2 `potential_yield' ln_gdp_per_capita


reg yield_growth1 l.ln_yield `controls' i.year,  vce(cluster country_grp)
eststo ols
xtreg yield_growth1 l.ln_yield `controls', fe vce(cluster country_grp)
eststo fe
xtreg yield_growth1 l.ln_yield `controls' i.year, fe vce(cluster country_grp)
eststo fe2


esttab ols fe fe2, keep(L.ln_yield polity2 `potential_yield' ln_gdp_per_capita) star(* 0.1 ** 0.05 *** 0.01)



*------------------------------------------------------------------------
* Cross-Sectional and Fixed Effects Model of Drivers of yield growth
*------------------------------------------------------------------------
*local controls  polity2 potent_rain_low
*local potential_yield potent_rain_int
local controls  polity2 `potential_yield' pre vap

reg yield_growth1 l.ln_yield nra_hat `controls',  vce(cluster country_grp)
eststo ols
reg yield_growth1 l.ln_yield nra_hat `controls' i.year,  vce(cluster country_grp)
eststo ols2
xtreg yield_growth1 l.ln_yield nra_hat `controls', fe vce(cluster country_grp)
eststo fe
xtreg yield_growth1 l.ln_yield nra_hat `controls' i.year, fe vce(cluster country_grp)
eststo fe2


esttab ols ols2 fe fe2, keep(L.ln_yield nra_hat polity2 `potential_yield' pre vap) star(* 0.1 ** 0.05 *** 0.01)


reg S20.yield_growth1 L20.ln_yield S20.nra_hat if year==1990
twoway scatter S20.yield_growth1 S20.nra_hat if year==1990




/*
xtreg yield_growth1  nra_hat , fe vce(cluster country_grp)

bysort country_grp: egen avg_yield_growth1=mean(yield_growth1)
bysort country_grp: egen avg_nra_hat=mean(nra_hat)
gen yield_growth1_tilde=yield_growth1 - avg_yield_growth1
gen nra_hat_tilde=nra_hat - avg_nra_hat

bysort country_grp: egen avg_yield_growth1_tilde=mean(yield_growth1_tilde)
bysort country_grp: egen avg_nra_hat_tilde=mean(nra_hat_tilde)


reg yield_growth1_tilde nra_hat_tilde
reg avg_yield_growth1_tilde avg_nra_hat_tilde


twoway (lfit yield_growth1_tilde nra_hat_tilde) (scatter yield_growth1_tilde nra_hat_tilde)

*scatter S20.yield_growth1 S20.polity2 if year==1995
*scatter ln_yield nra_hat if year==1995


*/

/*
*------------------------------------------------------------------------
* Long Difference Model of Drivers of yield growth
*------------------------------------------------------------------------
sort country_grp year
local yield yield_growth1
local potential_yield potent_rain_int
local controls  polity2 `potential_yield' c.ln_gdp_per_capita##c.ln_gdp_per_capita
reg S1.(`yield' nra_hat `controls') `potential_yield' i.year, vce(cluster country_grp)
eststo S1
reg S5.(`yield' nra_hat `controls') `potential_yield' i.year, vce(cluster country_grp)
eststo S5
reg S10.(`yield' nra_hat `controls') `potential_yield' i.year, vce(cluster country_grp)
eststo S10
reg S20.(`yield' nra_hat `controls') `potential_yield' i.year, vce(cluster country_grp)
eststo S20
reg S30.(`yield' nra_hat `controls') `potential_yield' i.year, vce(cluster country_grp)
eststo S30

esttab S1 S5 S10 S20 S30, keep(S.nra_hat S5.nra_hat S10.nra_hat S20.nra_hat S30.nra_hat S.polity2 S5.polity2 S10.polity2 S20.polity2 S30.polity2) star(* 0.1 ** 0.05 *** 0.01)



xtreg ln_yield l.ln_yield potent_rain_int nra_hat polity2  c.ln_gdp_per_capita##c.ln_gdp_per_capita, fe vce(cluster country_grp)

xtabond ln_yield nra_hat polity2  ln_gdp_per_capita, vce(robust)



xtmg ln_yield lln_yield nra_hat polity2  ln_gdp_per_capita, robust

*/

* Could collapse to 5 or 10-year periods and run Arellano-Bond