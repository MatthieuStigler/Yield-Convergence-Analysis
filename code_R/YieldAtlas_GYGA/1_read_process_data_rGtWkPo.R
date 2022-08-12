#' ---
#' Title: "Read all"
#' Author: "Matthieu"
#' Date: 2022-08-04
#' runMat: TRUE
#' ---

library(readxl)
library(matPkg)
library(tidyverse)

################################
#'## Read data
################################

## list files
files_dnwld <- mat_list_dir("dataRaw/yield_potential_GYGA/", pattern = "xls") %>% 
  mutate(crop = str_extract(filename, "Maize|Rice|Wheat"),
         type = str_extract(filename, "Rainfed|Irrigated"),
         continent = str_remove_all(filename, "Gyga|(Irrigated|Rainfed)(Maize|Wheat|Rice)|\\.xlsx")) 

files_dnwld %>% 
  count(continent)

################################
#'## Prepare data
################################

## read 1
readxl::read_xlsx(files_dnwld$full_path[[2]], sheet=4)

## read all
dat <- files_dnwld %>% 
  # head(5) %>% 
  mutate(data = map(full_path, readxl::read_xlsx, sheet=4)) %>% 
  select(-full_path)

dat$data[[2]]

## combine
dat_all <- dat %>%
  # filter(continent!="Australia") %>% 
  unnest(data) %>% 
  mutate(crop = if_else(continent=="Australia",
                        str_extract(CROP, "rapeseed|wheat") %>% str_to_title(),
                        crop),
         type = if_else(continent=="Australia",
                        str_extract(CROP, "Rainfed|Irrigated"),
                        type))


dat_all

################################
#'## Process
################################

dat_all %>% glimpse()


## CHECK: only one per country? OK
dat_all %>% 
  add_count(COUNTRY, crop, type) %>% 
  filter(n>1) %>% 
  mat_check_0row()


# CHECK: CROP has same info?
dat_all %>% 
  mutate(CROP2 = paste(type, str_to_lower(crop))) %>% 
  filter(CROP!=CROP2) %>% 
  mat_check_0row()

## keep vars of interest
dat_all_c <- dat_all %>% 
  select(crop,type, Country=COUNTRY, YA, YW, YP, TOTAL_AREA_HA, CROPPING_INTENSITY) %>% 
  filter(crop!="Rapeseed")

dat_all_c



################################
#'## Export data
################################

write_csv(dat_all_c, "data_intermediary/yield_potential_GYGA_clean_rGtWkPo.csv")
## READ AS: YP_GYGA <- read_csv("data_intermediary/yield_potential_GYGA_clean_rGtWkPo.csv")

