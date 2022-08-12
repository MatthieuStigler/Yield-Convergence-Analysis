#' ---
#' Title: "VisuYP GYGA"
#' Author: "Matthieu"
#' Date: 2022-08-04
#' runMat: TRUE
#' ---

library(tidyverse)

################################
#'## Read data
################################

YP_GYGA <- read_csv("data_intermediary/yield_potential_GYGA_clean_rGtWkPo.csv")
yld_pot <- haven::read_dta("data_intermediary/yld_potential_rJcYdFg.dta")

################################
#'## Prepare data
################################

yld_pot_all <- yld_pot%>% 
  select(Country=country, crop, yield_pot) %>% 
  mutate(source="GAEZ") %>% 
  rbind(YP_GYGA %>% 
          filter(type=="Rainfed") %>% 
          select(Country, crop, yield_pot=YP) %>% 
          mutate(source="GYGA"))
  

yld_pot_all

################################
#'## Visu
################################

YP_GYGA %>% 
  ggplot(aes(x=YP))+
  geom_histogram(position="dodge")+
  facet_grid(type~crop, scales = "free_y")

## Show density
pl_density_GYGA <- YP_GYGA %>% 
  filter(type=="Rainfed") %>% 
  group_by(crop) %>% 
  mutate(n= n(),
         w = TOTAL_AREA_HA/sum(TOTAL_AREA_HA)) %>% 
  ungroup() %>% 
  mutate(crop_count = paste0(crop, " (N=", n, ")")) %>% 
  ggplot(aes(x=YP, color=crop))+
  geom_density(aes(weight=w), trim=TRUE)+
  geom_rug()+
  facet_wrap(~crop_count, scales = "free_y", ncol=1)+
  ggtitle("Yield potential from Yield Atlas, rainfed")+
  ylab("Density (weighted by country area)")

pl_density_GYGA

################################
#'## Export data
################################


## save plots  
ggsave(pl_density_GYGA, height = 5, width = 8,
       filename = "figures/sigma_convergence/yield_potential_density_GYGA_rHyWqAl.png")

# 