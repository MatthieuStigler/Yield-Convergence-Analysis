#' ---
#' Title: "Prepare data"
#' Author: "Matthieu"
#' Date: 2020-05-28
#' runMat: TRUE
#' ---

library(matPkg)
library(tidyverse)

################################
#'## Read data
################################

dat_wheat <- read_csv("temp/yield_pred_Wheat.csv")
dat_rice <- read_csv("temp/yield_pred_Rice.csv")
dat_maize <- read_csv("temp/yield_pred_Maize.csv")

################################
#'## Prepare data
################################

dat_all <- rbind(dat_wheat %>% 
                   mutate(crop="wheat"),
                 dat_rice %>% 
                   mutate(crop="rice"),
                 dat_maize %>% 
                   mutate(crop="maize"))

################################
#'## Visu
################################

################################
#'## Export data
################################

write_csv(dat_all, "temp/yield_pred_3crops.csv")

## save plots  
# ggsave(..., height = gg_height, width = gg_width,
#        filename = "output/figures/xxx")

