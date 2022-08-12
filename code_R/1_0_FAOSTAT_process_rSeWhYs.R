#' ---
#' Title: "FAOSTAT clean"
#' Author: "Matthieu"
#' Date: 2022-08-11
#' runMat: TRUE
#' ---

library(tools)
library(tidyverse)

################################
#'## Read data
################################

FAOSTAT_bulk_raw <- read_csv("dataRaw/FAOSTAT/Production_Crops_Livestock_E_All_Data_(Normalized).csv")

FAO_before <- read_csv("dataRaw/Production_Crops_E_All_Data.csv")
yld_smooth <- read_csv("temp/yield_pred_3crops.csv")

FAO_country_codes <- read_csv("dataRaw/FAOSTAT_countrycodes.csv")
tools::showNonASCII(FAO_country_codes$Country)

## compare with:
# save "../temp/FAO_crop_production", replace

################################
#'## Prepare data
################################

FAOSTAT_bulk_raw

## clean
FAOSTAT_bulk_raw_countries <- distinct(FAOSTAT_bulk_raw, Area)
showNonASCII(FAOSTAT_bulk_raw_countries$Area)
showNonASCII(FAO_country_codes$Country) # <c3><b4> is correct unicode!

filter(FAOSTAT_bulk_raw_countries, str_detect(Area, "Ivoire"))

## Step 1: clean name, add iso3
FAOSTAT_bulk_raw_c <- FAOSTAT_bulk_raw %>% 
  rename_with(~str_replace_all(., " ", "_")) %>%
  select(-Area) %>% 
  filter(Area_Code<5000) %>% 
  left_join(FAO_country_codes %>% 
              select(Area_Code= `Country Code`, iso3 = `ISO3 Code`, Area=Country),
            by = "Area_Code") %>% 
  relocate(iso3, .after = Area_Code) 
  # mutate(Area = str_replace(Area, "Co\xf4te d'Ivoire", "C<c3><b4>te d'Ivoire"))

FAOSTAT_bulk_raw_c %>% 
  filter(str_detect(Area, "Ivoire"))
showNonASCII(unique(FAOSTAT_bulk_raw_c$Area))

## any NAs in iso3?
FAOSTAT_bulk_raw_c %>% 
  filter(is.na(iso3)) %>% 
  distinct(Area_Code, Area, iso3)

## Clean also alter old version
FAO_before_c <- FAO_before %>% 
  rename_with(~str_replace_all(., " ", "_"))

################################
#'## Select variables
################################


## check Items=Commodity
FAOSTAT_bulk_raw_c %>% 
  count(Item, Item_Code) %>% 
  filter(str_detect(Item, "Maize|Rice, paddy|Wheat|Soybeans")) 

code_commodity_keep <- c(15, 56, 27, 236)



## Variable
 ## from previous data
FAO_before_c %>% 
  count(Element, Element_Code) 

FAOSTAT_bulk_raw_c %>% 
  count(Element, Element_Code, Unit) %>% 
  filter(str_detect(Element, "Area|Yield|Production")) 

code_variable_keep <- c(5312, 5510, 5419)

## what is year code? Redundant
FAOSTAT_bulk_raw_c %>% 
  filter(Year!=Year_Code)

## Flags?
FAOSTAT_bulk_raw_c %>% 
  filter(Element_Code %in% code_variable_keep) %>% 
  count(Element, Flag) %>% 
  spread(Flag, n)


################################
#'## Do filtering on selected
################################

FAO_keep <- FAOSTAT_bulk_raw_c %>% 
  select(-Year_Code) %>% 
  filter(Item_Code %in% code_commodity_keep,
         Element_Code %in% code_variable_keep,
         Area_Code<5000)

FAO_keep

## Clean vars, change names
FAO_keep_c <- FAO_keep %>% 
  mutate(Item = str_replace(Item, "Rice, paddy", "Rice") %>% 
           str_to_lower(),
         Element = str_replace(Element, "Area harvested", "Area") %>% 
           str_to_lower()) %>% 
  select(-Item_Code, -Element_Code) %>% 
  rename(crop=Item,
         countrycode = Area_Code,
         country = Area)

FAO_keep_c

## NAs?
FAO_keep_c %>% 
  filter(is.na(Value)) %>% 
  count(Value, Flag)


## Final
FAO_keep_quasi_final <- FAO_keep_c %>% 
  select(-Flag, -Unit) %>% 
  rename_with(str_to_lower)
  
FAO_keep_quasi_final

################################
#'## Compare
################################

yld_smooth_prep <- yld_smooth %>% 
  select(-contains("hat"), -country_grp, -avg_area, -prop_global_area)

yld_smooth_prep

## long version
yld_smooth_prep_l <- yld_smooth_prep %>% 
  pivot_longer(c(area, production, yield), names_to ="element") %>% 
  select(all_of(colnames(FAO_keep_quasi_final))) %>% 
  arrange(countrycode, crop, element, year)


## compare now
yld_smooth_prep_l


## max year?
max(yld_smooth_prep_l$year)
max(FAO_keep_quasi_final$year)

mer_vars <- c("countrycode", "iso3", "crop", "element", "year")

## missing in old?
yld_smooth_prep_l %>% 
  anti_join(FAO_keep_quasi_final, by = mer_vars) %>% 
  count(country, countrycode, iso3)

## missing in old?
FAO_keep_quasi_final%>% 
  filter(year<=2016) %>% 
  anti_join(yld_smooth_prep_l, by = mer_vars) %>% 
  count(country, crop, element) %>% 
  spread(element, n)


## merge
merged <- yld_smooth_prep_l %>% 
  inner_join(FAO_keep_quasi_final, by = mer_vars, 
            suffix = c("_old", "_new"))

merged

## some data revisions! Mostly in 2016
diff_new_old <- merged %>% 
  filter(value_old!=value_new) %>% 
  mutate(diff = abs(value_new-value_old),
         diff_perc = diff/value_old) %>% 
  arrange(desc(diff_perc))

diff_new_old

diff_new_old %>% 
  count(year)

################################
#'## Select
################################

## keep only those we are interested in
FAO_keep_final <- FAO_keep_quasi_final %>% 
  semi_join(yld_smooth_prep_l, by = c("countrycode")) %>% 
  left_join(yld_smooth_prep_l %>% 
               distinct(countrycode, crop) %>% 
              mutate(is_in_old = TRUE), by = c("countrycode", "crop")) %>% 
  mutate(is_in_old = replace_na(is_in_old, FALSE))

FAO_keep_final %>% 
  filter(!is_in_old)

## re-check diff
FAO_keep_final %>% 
  filter(is_in_old) %>% 
  filter(year<=2016) %>% 
  full_join(yld_smooth_prep_l, by = mer_vars, 
             suffix = c("_old", "_new")) %>% 
  mutate(diff = abs(value_old-value_new),
         diff_perc = 100 * diff/value_old) %>% 
  group_by(crop, element) %>% 
  summarise(mean_diff_perc = mean(diff_perc))  %>% 
  spread(element, mean_diff_perc)

## country encoding problem?
tools::showNonASCII(unique(FAO_keep_final$country))

################################
#'## Prep yields only
################################

FAO_keep_final_Y <- FAO_keep_final %>% 
  filter(is_in_old) %>% 
  filter(element=="yield") %>% 
  rename(yield = value) %>% 
  select(-is_in_old, -element)

FAO_keep_final_Y

################################
#'## Export data
################################

write_csv(FAO_keep_final, "data_intermediary/FAOSTAT_3crops_2020_rSeWhYs.csv")
## READ AS: FAO_2020 <- read_csv("data_intermediary/FAOSTAT_3crops_2020_rSeWhYs.csv")

write_rds(FAO_keep_final_Y, "data_intermediary/FAOSTAT_3crops_2020_Yonly_rSeWhYs.rds")
## READ AS: FAO_2020_Y <- read_rds("data_intermediary/FAOSTAT_3crops_2020_Yonly_rSeWhYs.rds")

# rSeWhYs