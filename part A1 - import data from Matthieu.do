********************************************************************************************
* This was the old data that had limited countries
*import delimited using "../dataAnalysis/data_merged_ALL_Cntry_Commod_Year_mAd.csv", clear
*destring fao* dai* cru*, replace ignore("NaN" "NA")
********************************************************************************************

import delimited using "../dataAnalysis/data_merged_ALL_Cntry_Commod_Year_outerJoin_mAd.csv", clear
destring fao* dai* cru*, replace ignore("NaN" "NA")
***************** Notes on the weather data *******************************
* Spatial aggregation was done by 3 different weighting methods
* 1) "w_fin" The proportion of each weather grid in the country border.
* 2) "w_finarea10" The proportion of weather grid in country for those grids with 
*                  production of the crop
* 3) "w_finarea" The proportion of the weather grid in country times the area with
*                production of the crop in that grid 
* Only have "w_finarea10" for "ave3" temporal aggregation
*
* Temporal aggreation was done by 3 different methods
* 1) "ave1" Average of the previous 12 month, starting from the harvest month.
* 2) "ave2" 12 lags of monthly weather data, starting from harvest month
* 3) "ave3" Average of all months between planting and harvest

keep if commodity=="maize" | commodity=="wheat" | commodity=="rice"
replace commodity="Maize" if commodity=="maize"
replace commodity="Wheat" if commodity=="wheat"
replace commodity="Rice" if commodity=="rice"


ren fao3__yield yield
ren fao3__area area
ren fao3__production production

drop if yield==. 

save "../temp/revisedMatthieu_regression_data", replace


import delimited using "../dataAnalysis/data_merged_ALL_Cntry_Year_mAd.csv", clear
destring fao* dai* fug* incr* pol* wdi*, replace ignore("NaN" "NA")
drop incr*
drop pol*
save "../temp/Cntry_Year_data", replace

