#' ---
#' Title: "Assemble yield potential data"
#' Author: "Matthieu"
#' Date: 2020-06-19
#' runMat: TRUE
#' ---

library(matPkg)
library(haven)

################################
#'## Read data
################################

## show all
mat_list_dir("temp", pattern = "potential|ygap") %>% 
  mutate(crop= str_extract(filename, "maize|wheat|rice"),
         data=map(full_path, haven::read_dta)) %>% 
  filter(!str_detect(filename, "Calorie")) %>% 
  select(1:3) %>% 
  knitr::kable()

##
yldpot_wheat <- read_dta("temp/wheat_yield_potential.dta")
yldpot_maize <- read_dta("temp/maize_yield_potential.dta")
yldpot_rice <- read_dta("temp/ygapGAEZ_rice.dta")
yldpot_rice_raw <- read_dta("dataRaw/GAEZ/country_Calories_stats_web.dta") %>% 
  select(ISO_A3, post1500AverageCaloriesmean)




################################
#'## Prepare data
################################

yldpot_all <- yldpot_wheat %>%  
  select(country, countrycode, iso3, commodity, potent_rain_int) %>% 
  rbind(yldpot_maize %>% 
           select(country, countrycode, iso3, commodity, potent_rain_int)) %>% 
  spread(commodity, potent_rain_int) %>% 
  left_join(yldpot_rice_raw%>% 
              rename(Rice=post1500AverageCaloriesmean) %>% 
              mutate(Rice=log(Rice)),
            by = c("iso3"="ISO_A3"))

yldpot_all

# long
yldpot_all_l <- yldpot_all %>% 
  gather(crop, yield_pot, Maize, Rice, Wheat)

yldpot_all_l

################################
#'## Export data
################################

write_dta(yldpot_all_l, "data_intermediary/yld_potential_rJcYdFg.dta")
## READ AS: yld_pot <- read_dta("data_intermediary/yld_potential_rJcYdFg.dta")
