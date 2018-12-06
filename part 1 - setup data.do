import delimited using "../dataRaw/data_merged_ALL_Cntry_Commod_Year_mAd.csv", clear




clear
version 14.2
set more off

import delimited using "../dataAnalysis/data_merged_ALL_Cntry_Year_mAd.csv", clear
destring dai* fao* fug* wdi*, replace ignore("NaN" "NA")
keep country countrycode iso3 region subregion year dai2__gdppcp00

save "../temp/country_level_variables", replace

import delimited using "../dataAnalysis/data_merged_ALL_Cntry_Commod_Year_mAd.csv", clear
destring fao* dai* cru*, replace ignore("NaN" "NA")

merge m:1 countrycode year using "../temp/country_level_variables", nogen keep(match master)


gen distortion= dai3__nra + 1

gen area=fao3__area 
gen prod=fao3__production 
gen yield=fao3__yield
gen myyield=prod*10000/area
gen diff=yield-myyield
summ diff, detail
drop myyield diff

gen animal=(commodity=="beef" | commodity=="egg" | commodity=="milk" | commodity=="pigmeat" ///
			| commodity=="sheepmeat" | commodity=="wool" | commodity=="poultry" | commodity=="camel")
			
gen gdp_per_capita = dai2__gdppcp00
gen pre = cru__ave3_pre_w_finarea
gen tmp = cru__ave3_tmp_w_finarea
*gen pre = cru__ave1_pre_w_finarea
*gen tmp = cru__ave1_tmp_w_finarea
gen import =0
replace import=1 if meta_trstat2=="M"
replace import=. if meta_trstat2=="NA"

gen export = 0
replace export = 1 if meta_trstat2=="X"
replace export = . if meta_trstat2=="NA"

bysort countrycode commodity: egen import_avg=mean(import)
bysort countrycode commodity: egen export_avg=mean(export)

gen my_import=0
replace my_import=1 if import_avg>=0.5
replace my_import=. if import_avg==.

gen my_export=0
replace my_export=1 if export_avg>0.5
replace my_export=. if export_avg==.

* Why does DAI VOP differ from the calcuation of q*price?
* Which Value of Production number should I choose?
bysort country commodity: egen avg_vop=mean(dai3__vop_prod)

summ distortion* prod* area* gdp_per_capita pre tmp


summ distortion* prod* area* gdp_per_capita pre tmp


gen ln_distortion=ln(distortion)
gen ln_prod=ln(prod)
gen ln_area=ln(area)
gen ln_yield=ln(yield)
gen ln_gdp_per_capita=ln(gdp_per_capita)
gen ln_gdp_per_capita2=ln_gdp_per_capita^2
gen ln_pre=ln(pre)
gen ln_pre2=ln_pre^2
gen pre2=pre^2
gen ln_tmp=ln(tmp)
gen ln_tmp2=ln_tmp^2
gen tmp2=tmp^2

save "../temp/commodity-level_regression_data", replace
