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

## read one raw
yld_gap_irr_raw <- read_csv("dataRaw/GAEZ/ygap_rice_irr.csv", skip=10)

## Read processed by Nathan
yldpot_wheat <- read_dta("temp/wheat_yield_potential.dta")
yldpot_maize <- read_dta("temp/maize_yield_potential.dta")
yldpot_rice <- read_dta("temp/ygapGAEZ_rice.dta")
yldpot_rice_raw <- read_dta("dataRaw/GAEZ/country_Calories_stats_web.dta") %>% 
  select(ISO_A3, post1500AverageCaloriesmean)

## FAO codes
FAO_codes <- read_csv("dataRaw/FAOSTAT_countrycodes.csv") 

## FAOSTAT data
FAO_2020_Y <- read_rds("data_intermediary/FAOSTAT_3crops_2020_Yonly_rSeWhYs.rds")

################################
#'## Prepare data
################################

## Bind all maize/wheat/rice
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
#'## Check iso
################################

## GAEZ raw? It is very hard to understand whether this includes China !! We will assume not...
yld_gap_irr_raw %>% 
  filter(str_detect(Country, "China|Taiwan"))

## GAEZ gives for China = mainland + Taiwan!
yldpot_all_l %>% 
  filter(str_detect(country, "China|Taiwan"))

## what are the possible codes?
FAO_codes %>% 
  filter(str_detect(Country, "China")) %>% 
  print(n=10)

### FAOSTAT gives for mainland (41) and Taiwan!
FAO_2020_Y %>% 
  distinct(countrycode, country, iso3) %>% 
  filter(str_detect(country, "China")) %>% 
  print(n=10)

## SOLUTION: assign 41/CHN
yldpot_all_l_c <- yldpot_all_l %>% 
  mutate(countrycode=if_else(country=="China",
                             41,countrycode),
         iso3=if_else(country=="China",
                             "CHN",iso3))

yldpot_all_l_c %>% 
  filter(str_detect(country, "China|Taiwan"))

################################
#'## Export data
################################

write_dta(yldpot_all_l_c, "data_intermediary/yld_potential_rJcYdFg.dta")
## READ AS: yld_pot <- read_dta("data_intermediary/yld_potential_rJcYdFg.dta")
