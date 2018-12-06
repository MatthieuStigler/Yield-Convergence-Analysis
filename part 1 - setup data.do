import delimited using "../dataRaw/FAOSTAT_countrycodes.csv", clear
drop if iso3code==""
duplicates tag countrycode, gen(dup)
list if dup==1
drop if dup==1
ren iso3code iso3
keep countrycode country iso3
save  "../temp/FAOSTAT_countrycodes", replace



import delimited using "../dataRaw/Production_Crops_E_All_Data.csv", clear
keep if item=="Wheat" | item=="Maize"
replace element="area" if element=="Area harvested"
replace element="yield" if element=="Yield"
replace element="production" if element=="Production"
keep if element=="area" | element=="yield" | element=="production"
drop y*f
drop unit elementcode itemcode
ren areacode countrycode
ren area country
reshape long y, i(countrycode country item element) j(year)
reshape wide y, i(countrycode country item year) j(element) string
ren yarea area
ren yproduction production
ren yyield yield
ren item commodity
merge m:1 countrycode using "../temp/FAOSTAT_countrycodes", keep(match master)
tab country if _merge==1
drop if _merge==1
drop _merge
tab country
drop if country=="World"
drop if iso3==""
save "../temp/FAO_crop_production", replace

use "../dataRaw/pwt90", clear
ren countrycode iso3
save "../temp/pwt90_iso3", replace

use "../temp/FAO_crop_production", clear
* only keep years in Penn World Table
drop if year>2014 
merge m:1 iso3 year using "../temp/pwt90_iso3"
* drop year not in FAO
drop if year <1961
tab country if _merge==1
tab country if _merge==2
drop if _merge==1 | _merge==2
drop _merge
drop if production==. & area==. & yield==.
save "../temp/fullFAO_regression_data", replace




