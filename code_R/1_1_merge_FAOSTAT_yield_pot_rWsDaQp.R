#' ---
#' Title: "Merge yld, add lags etc"
#' Author: "Matthieu"
#' Date: 2022-08-20
#' runMat: TRUE
#' ---



library(matPkg)
library(haven)
library(tidyverse)
source("code_R/888_misc_functions.R")

################################
#'## Read data
################################

yld_smooth <- read_csv("temp/yield_pred_3crops.csv")
FAO_codes <- read_csv("dataRaw/FAOSTAT_countrycodes.csv") 

yld_pot <- read_dta("data_intermediary/yld_potential_rJcYdFg.dta")

################################
#'## Prepare data
################################

# clean codes
FAO_codes_c <- FAO_codes %>% 
  rename_all(~str_replace_all(., " ", "_"))%>% 
  rename(iso2=`ISO2_Code`,
         iso3_check=`ISO3_Code`,
         countrycode=Country_Code) %>%  
  mutate(iso2=str_to_lower(iso2))

## clean/merge ylds
yld_smooth_c <- yld_smooth %>% 
  select(-production, -country_grp, -prop_global_area,
         -area_hat, -area) %>% 
  prj_clean_crop_names() %>% 
  left_join(FAO_codes_c %>% 
              select(countrycode, starts_with("iso")), by = "countrycode") %>% 
  left_join(yld_pot %>% 
              select(iso3, crop, yield_pot), by = c("iso3", "crop"))

yld_smooth_c

## CHECK: missing YP?
yld_smooth_c %>% 
  filter(is.na(yield_pot)) %>% 
  count(country, countrycode, iso3, crop) %>% 
  spread(crop, n, fill = 0)


yld_pot %>% 
  filter(str_detect(country, "China|ivo|Taiwan|Lao|Korea"))

################################
#'## Further cleaning
################################


## clean further
yld_smooth_c %>%  
  filter(iso3!= iso3_check) %>% 
  mat_check_0row()

yld_smooth_c2 <- yld_smooth_c %>% 
  select(-iso3_check) %>% 
  relocate(iso3, .after = country)

yld_smooth_c2

## CHECK: balanced? OK!
yld_smooth_c2 %>% 
  filter(!is.na(yield)) %>% 
  count(countrycode, crop, name = "n_years") %>% 
  count(n_years) %>% 
  filter(n_years!=56) %>% 
  mat_check_0row(message_ok = "OK, 56 obs for all crop-country pairs present")

max(yld_smooth_c2$year)

################################
#'## Export
################################

write_csv(yld_smooth_c2, "dataAnalysis/Ylds_raw_hat_pot_2016_rWsDaQp.rds")
## READ AS: ylds_final <- read_csv("dataAnalysis/Ylds_raw_hat_pot_2016_rWsDaQp.rds")
# rWsDaQp