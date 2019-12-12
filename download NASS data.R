library(devtools)
install_github('potterzot/rnassqs')

library("rnassqs")
library(dplyr)
library(readr)

api_key <- "45C2DCFE-C475-3B8F-A19E-C0CAF0A1E916"

# https://quickstats.nass.usda.gov/api

my_years = seq(1961,2017) 

get_data <- function(my_year) {
  county_corn_yield = list(source_desc = "SURVEY", sector_desc = "CROPS",commodity_desc = "CORN",
                           short_desc="CORN, GRAIN - YIELD, MEASURED IN BU / ACRE", agg_level_desc = "COUNTY", 
                           year=my_year)
  
  #as.integer(nassqs_record_count(county_corn_yield, key=api_key)[[1]])
  data <- nassqs(county_corn_yield, key = api_key)
}

datOut <- lapply(my_years, get_data) %>% bind_rows()

write_csv(datOut, "C:/Users/Nathan/Dropbox/Yield Gaps/dataRaw/USA_county_corn_yields.csv", na="")
