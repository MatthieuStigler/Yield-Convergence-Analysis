#' ---
#' Title: "DOwnload FAO data"
#' Author: "Matthieu"
#' Date: 2022-08-11
#' runMat: TRUE
#' ---

library(FAOSTAT)
library(tidyverse)

################################
#'## Explore data
################################


fao_metadata <- FAOsearch()   %>% 
  as_tibble()

fao_metadata %>% 
  count(datasetname) %>% 
  pull(datasetname)

fao_metadata %>% 
  filter(str_detect(datasetname, fixed("crop",ignore_case = TRUE)))


fao_FO <- FAOsearch(code="QCL") %>% 
  as_tibble()
fao_FO$filelocation


################################
#'## Download data
################################

out_file <- paste0("dataRaw/FAOSTAT/", basename(fao_FO$filelocation))
download.file(fao_FO$filelocation,
              out_file)

################################
#'## Extract data
################################

unzip(out_file, exdir = dirname(out_file))

################################
#'## Quick overview
################################

list.files(dirname(out_file))
file <- read_csv("dataRaw/FAOSTAT/Production_Crops_Livestock_E_All_Data_(Normalized).csv")

file

