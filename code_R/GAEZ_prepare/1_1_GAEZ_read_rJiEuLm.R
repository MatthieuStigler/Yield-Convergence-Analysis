#' ---
#' Title: "Check rasters"
#' Author: "Matthieu"
#' Date: 2021-06-03
#' runMat: TRUE
#' ---

library(matPkg)
library(raster)
library(sf)
library(tidyverse)
library(exactextractr)

select <- dplyr::select

################################
#'## Read data
################################

##
world_map <- sf::st_read("dataRaw/WorldMap/Country_boundaries_vect_CLEAN_winkel_tripel.shp")

## list rasters
raster_path <- mat_list_dir("dataRaw/GAEZ v4", pattern = "\\.tif") %>% 
  mutate(crop = str_extract(filename, "rcw|mze|rcd|whe|soy"),
         code = str_extract(filename, "(?<=yc)[HL][grs]")) %>% 
  separate(code, into = c("input_level", "irrigation_code"), remove=FALSE, sep=1) %>% 
  mutate(irrigation_description = recode(irrigation_code, "g"="gravity",
                                         r="rainfed", s="sprinkler"))

raster_path

## list cropmaps
cropMaps_path <- mat_list_dir("dataRaw/EarthScan_cropmap/", pattern = "\\.tif") %>% 
  mutate(crop = str_extract(filename, "maize|rice|wheat|soybean")) %>% 
  relocate(crop)
  
cropMaps_path


################################
#'## Prepare data
################################

## count rasters
raster_path %>% 
  select(-filename, -full_path) %>% 
  mutate(is_there=1) %>% 
  spread(crop, is_there) %>% 
  mutate(across(where(is.numeric), ~if_else(is.na(.), "", as.character(.)))) %>% 
  knitr::kable()

## Read 1
rGAEZ_1 <- raster(raster_path$full_path[[1]])
rGAEZ_1_vals <- values(rGAEZ_1)
rGAEZ_1

any(rGAEZ_1_vals== -1)

rcrMap_1 <- raster(cropMaps_path$full_path[[1]])
rcrMap_1

rcrMap_1_vals <- values(rcrMap_1)

## merge? Not needed
rGAEZ_crM <- raster::stack(rGAEZ_1, rcrMap_1)
rGAEZ_crM

## Extract
FR <- filter(world_map, COUNTRY%in%c("France", "Italy"))
test_unweighted <- exact_extract(rGAEZ_1, world_map, 'mean')
test <- exact_extract(rGAEZ_1, world_map, 'weighted_mean', weights=rcrMap_1)

tibble(Country = world_map$COUNTRY,
       no_W=test_unweighted,
       w= test) %>% 
  filter(no_W==-1)

## Check -1!?
n_neg <-  exact_extract(rGAEZ_1, world_map, fun = function(values, coverage_fractions, ...) mean(values== -1))

tibble(Country = world_map$COUNTRY,
       n_neg) %>% 
  filter(!is.na(n_neg))

# rJiEuLm