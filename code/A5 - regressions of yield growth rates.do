


local com Maize
* Define which variable to use to measure potential yield
local potential_yield potent_rain_int
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
* Caloric data from GAEZ obtained from here: https://ozak.github.io/Caloric-Suitability-Index/
* Should cite Oded Galor and Ömer Özak, 2016. “The Agricultural Origins of Time Preference,” American Economic Review, 2016, 106(10): 3064–3103.
merge m:1 iso3 using "../temp/country_Calories_potential", keep(match master) nogen
* GAEZ yield potential and yield gap data
merge m:1 iso3 using "../temp/`com'_yield_potential", keep(match master) nogen
merge m:1 iso3 using "../temp/ygapGAEZ_`com'", keep(match master) nogen



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


*gen yield_gap=(yield_hat - yield95)/yield95
gen yield_gap=yield_hat/yield90

gen ln_yield=ln(yield_hat)
sort country_grp year
gen yield_growth=d.ln_yield
gen yield_chg=d.yield_hat

gen antiAg=(nra_hat <0) if nra_hat!=.
*bysort country: egen sum_antiAg=total(antiAg)
*tab sum_antiAg
*replace antiAg=0 if sum_antiAg<10
gen proAg=(nra_hat >0) if nra_hat!=.
*bysort country: egen sum_proAg=total(proAg)
*tab sum_proAg
*replace proAg=0 if sum_proAg<10
summ antiAg proAg

summ yield_gap
summ nra polity_democ pre vap pet


/*
sort country_grp year
gen S20yield_growth=S20.yield_growth
gen S20nra_hat=S20.nra_hat

*/

sort country_grp year
local yield yield_growth
local potential_yield potent_rain_int
local controls  S20polity2 S20`potential_yield' c.S20ln_gdp_per_capita##c.S20ln_gdp_per_capita
foreach var of varlist `yield' nra_hat polity2 `potential_yield' ln_gdp_per_capita {
gen S20`var'=S20.`var'
capture drop S20`var'_save
gen S20`var'_save=S20`var'
}
bysort country_grp: egen avgS20_yield_growth=mean(S20yield_growth)
bysort country_grp: egen avgS20_nra_hat=mean(S20nra_hat)


reg S20`yield' S20nra_hat `controls' S20`potential_yield' i.year, vce(cluster country_grp)
foreach var of varlist polity2 `potential_yield' ln_gdp_per_capita {
replace S20`var'=0
}
replace year=2000
replace S20nra_hat=avgS20_nra_hat
capture drop yield_nra
sort country_grp year 
predict yield_nra
foreach var of varlist nra_hat polity2 `potential_yield' ln_gdp_per_capita {
replace S20`var'=S20`var'_save
}
summ S20`yield'
local mean_y=r(mean)
summ yield_nra
local mean_yhat=r(mean)
replace yield_nra=yield_nra+`mean_y'-`mean_yhat'


sort avgS20_nra_hat
twoway (line yield_nra avgS20_nra_hat) (scatter avgS20_yield_growth avgS20_nra_hat  , msymbol(Oh) mcolor(gs8%60)),  scheme(538w)




*-----------------------------------------------------------------------------
* Relationship with GDP per capita
*-----------------------------------------------------------------------------


scatter yield_growth ln_gdp_per_capita if year==2011, msymbol(Oh) mcolor(gs8%60)  scheme(538w) 
scatter yield_chg ln_gdp_per_capita if year==2011, msymbol(Oh) mcolor(gs8%60)  scheme(538w)


qui{
capture drop yield_growth_hat 
capture drop potent_save

gen potent_save=`potential_yield'
gen yield_growth_hat=.
summ year if yield_growth!=. & ln_gdp_per_capita!=.
local max_year=r(max)
forvalues j=1962/`max_year' {
capture drop gdp_spline*
mkspline gdp_spline=ln_gdp_per_capita if year==`j', cubic nknots(4)

reg yield_chg gdp_spline* `potential_yield' if year==`j' 
summ `potential_yield'
replace `potential_yield'=r(mean)
predict yhat if year==`j' 
replace yield_growth_hat=yhat if year==`j'
drop yhat
replace `potential_yield'=potent_save

} // end of year loop
} // end of qui

sort year ln_gdp_per_capita
twoway (line yield_growth_hat ln_gdp_per_capita if year==1965, lwidth(medthick)) (line yield_growth_hat ln_gdp_per_capita if year==1975, lwidth(medthick)) (line yield_growth_hat ln_gdp_per_capita if year==1985, lwidth(medthick)) (line yield_growth_hat ln_gdp_per_capita if year==1995, lwidth(medthick)) (line yield_growth_hat ln_gdp_per_capita if year==2005, lwidth(medthick)) (line yield_growth_hat ln_gdp_per_capita if year==2014, lwidth(medthick)),  scheme(538w) 


*-----------------------------------------------------------------------------
* Relationship with Polity Score
*-----------------------------------------------------------------------------
scatter yield_growth polity2 if year==2011, msymbol(Oh) mcolor(gs8%60)  scheme(538w) 
scatter yield_chg polity2 if year==2011, msymbol(Oh) mcolor(gs8%60)  scheme(538w)

qui{
capture drop yield_growth_hat 
capture drop potent_save

gen potent_save=`potential_yield'
gen yield_growth_hat=.
summ year if yield_growth!=. & polity2!=.
local max_year=r(max)
forvalues j=1962/`max_year' {
capture drop polity2_spline*
mkspline polity2_spline=polity2 if year==`j', cubic nknots(3)

reg yield_growth polity2* `potential_yield' if year==`j' 
summ `potential_yield'
replace `potential_yield'=r(mean)
predict yhat if year==`j' 
replace yield_growth_hat=yhat if year==`j'
drop yhat
replace `potential_yield'=potent_save

} // end of year loop
} // end of qui

sort year polity2
twoway (line yield_growth_hat polity2 if year==1965, lwidth(medthick)) (line yield_growth_hat polity2 if year==1975, lwidth(medthick)) (line yield_growth_hat polity2 if year==1985, lwidth(medthick)) (line yield_growth_hat polity2 if year==1995, lwidth(medthick)) (line yield_growth_hat polity2 if year==2005, lwidth(medthick)) (line yield_growth_hat polity2 if year==2011, lwidth(medthick)),  scheme(538w) 


*-----------------------------------------------------------------------------
* Relationship with NRA
*-----------------------------------------------------------------------------
*replace nra_hat=. if country=="Nigeria"

scatter yield_growth nra_hat if year==1965, msymbol(Oh) mcolor(gs8%60)  scheme(538w) 
scatter yield_chg nra_hat if year==1965, msymbol(Oh) mcolor(gs8%60)  scheme(538w)

qui{
capture drop yield_growth_hat 
capture drop potent_save

gen potent_save=`potential_yield'
gen yield_growth_hat=.
summ year if yield_growth!=. & polity2!=.
local max_year=r(max)
forvalues j=1962/`max_year' {
capture drop nra_hat_spline*
mkspline nra_hat_spline=nra_hat if year==`j', cubic nknots(3)

reg yield_chg nra_hat* `potential_yield' if year==`j' 
summ `potential_yield'
replace `potential_yield'=r(mean)
predict yhat if year==`j' 
replace yield_growth_hat=yhat if year==`j'
drop yhat
replace `potential_yield'=potent_save

} // end of year loop
} // end of qui

sort year nra_hat
twoway (line yield_growth_hat nra_hat if year==1965, lwidth(medthick)) (line yield_growth_hat nra_hat if year==1975, lwidth(medthick)) (line yield_growth_hat nra_hat if year==1985, lwidth(medthick)) (line yield_growth_hat nra_hat if year==1995, lwidth(medthick)) (line yield_growth_hat nra_hat if year==2005, lwidth(medthick)) (line yield_growth_hat nra_hat if year==2011, lwidth(medthick)),  scheme(538w) 




*-----------------------------------------------------------------------------
* Regresssions
*-----------------------------------------------------------------------------
qui{
reg yield_growth nra_hat `potential_yield', vce(cluster country_grp) 
eststo ols
xtreg yield_growth nra_hat `potential_yield', fe vce(cluster country_grp)
eststo fe
xtreg yield_growth nra_hat `potential_yield'  i.year, fe  vce(cluster country_grp)
eststo fe2
}

esttab ols fe fe2, keep(nra_hat ) star(* 0.1 ** 0.05 *** 0.01)

* ln_gdp_per_capita c.ln_gdp_per_capita#c.ln_gdp_per_capita 
*cru__ave3_pre_w_finarea cru__ave3_tmn_w_finarea cru__ave3_tmp_w_finarea cru__ave3_tmx_w_finarea cru__ave3_vap_w_finarea cru__ave3_pet_w_finarea


*local controls  polity2 potent_rain_low
local potential_yield potent_rain_int
local controls  polity2 `potential_yield' c.`potential_yield'#c.year

qui{
reg yield_growth `controls',  vce(cluster country_grp)
eststo ols
xtreg yield_growth `controls', fe vce(cluster country_grp)
eststo fe
xtreg yield_growth `controls' i.year, fe vce(cluster country_grp)
eststo fe2
}

esttab ols fe fe2, keep( `controls') star(* 0.10 ** 0.05 *** 0.01)


qui{
reg yield_growth nra_hat `controls',  vce(cluster country_grp)
eststo ols
xtreg yield_growth nra_hat `controls', fe vce(cluster country_grp)
eststo fe
xtreg yield_growth nra_hat `controls' i.year, fe vce(cluster country_grp)
eststo fe2
}

esttab ols fe fe2, keep(nra_hat `controls') star(* 0.1 ** 0.05 *** 0.01)

qui{
reg yield_growth nra_hat `controls' if antiAg==1,  vce(cluster country_grp)
eststo ols
xtreg yield_growth nra_hat `controls' if antiAg==1, fe vce(cluster country_grp)
eststo fe
xtreg yield_growth nra_hat `controls' i.year if antiAg==1, fe vce(cluster country_grp)
eststo fe2
}
esttab ols fe fe2, keep(nra_hat `controls') star(* 0.1 ** 0.05 *** 0.01)

qui{
reg yield_growth nra_hat `controls' if proAg==1, vce(cluster country_grp)
eststo ols
xtreg yield_growth nra_hat `controls' if proAg==1, fe vce(cluster country_grp)
eststo fe
xtreg yield_growth nra_hat `controls' i.year if proAg==1, fe vce(cluster country_grp)
eststo fe2
}
esttab ols fe fe2, keep(nra_hat `controls') star(* 0.1 ** 0.05 *** 0.01)




sort country_grp year
gen S30yield_growth=S30.yield_growth
gen S30yield_chg=S30.yield_chg

histogram S30yield_growth if year==1995 
sort S30yield_growth
list country S30yield_growth if year==1995

histogram S30yield_chg if year==1995 
sort S30yield_chg
list country S30yield_chg if year==1995  

sort country_grp year
scatter S20.yield_growth S20.nra_hat if year==1985 , msymbol(Oh) mcolor(gs8%60)  scheme(538w)

scatter S30.yield_growth S30.polity2 if year==1995 , msymbol(Oh) mcolor(gs8%60)  scheme(538w)

/*
* Translate shapefile to stata format
cd "../dataRaw/WorldMap/"
shp2dta using Country_boundaries_vect_CLEAN_winkel_tripel, ///
		database(world_db) coordinates(world_coord) genid(id) replace
*/	



sort country_grp year
local yield yield_growth
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



ren countrycode FAO_CODE
merge m:1 FAO_CODE using "../dataRaw/WorldMap/world_db", keep(match using) nogenerate




spmap S30yield_growth using "../dataRaw/WorldMap/world_coord" if year==1995 | year==., id(id) ocolor(none ..) ndocolor(none ..)  	///
			ndfcolor(gs11) clmethod(quantile) cln(5)  legorder(hilo) ///
			legend(position(8) size(4)) plotregion(margin(l=10)) ///
			fcolor(RdYlGn)	

spmap yield_growth using "../dataRaw/WorldMap/world_coord" if year==2010 | year==., id(id) ocolor(none ..) ndocolor(none ..)  	///
			ndfcolor(gs11) clmethod(quantile) cln(5)  legorder(hilo) ///
			legend(position(8) size(4)) plotregion(margin(l=10)) ///
			fcolor(RdYlGn)	
/*
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
*/
