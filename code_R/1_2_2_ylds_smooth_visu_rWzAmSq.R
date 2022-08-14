#' ---
#' Title: "Visu yields smooth"
#' Author: "Matthieu"
#' Date: 2022-08-13
#' runMat: TRUE
#' ---

library(tidyverse)

################################
#'## Read data
################################

yld_smooth_FAO2020 <- read_rds("data_intermediary/FAOSTAT_3crops_2020_Yonly_smoothed_rSwApAm.rds")  # from 1_2

################################
#'## Prepare data
################################

## convert units: yield is hg/ha, i.e. hectogram per hectare
yld_smooth_FAO2020_c <- yld_smooth_FAO2020 %>% 
  mutate(across(c(yield, deviation, trend), ~./10000))

yld_smooth_FAO2020_c

################################
#'## Visu
################################

pl_yld_smooth_wheat <- yld_smooth_FAO2020_c %>% 
  filter(crop=="wheat") %>% 
  filter(pred =="smooth_2016") %>%
  # mat_head_group(countrycode, n_head = 16) %>%
  ggplot(aes(x = year, y=trend))+
  geom_line(color ="blue", lwd=0.8, alpha=0.8)+
  geom_point(aes(y=yield), size =0.3)+
  facet_wrap(~country, scales = "free_y")+
  xlab("Year")+ylab("Yield [ton/hectare]")+
  theme_light()+
  theme(panel.border = element_blank(),
        strip.background =element_rect(fill="white"),
        strip.text = element_text(colour = 'black'),
        text = element_text(size=9),
        axis.text.x=element_text(size=rel(0.8)),
        axis.text.y=element_text(size=rel(0.7)))

pl_yld_smooth_wheat

## same for maize & rice
pl_yld_smooth_maize <- pl_yld_smooth_wheat %+% 
  filter(yld_smooth_FAO2020_c, crop=="maize" & pred =="smooth_2016") 
pl_yld_smooth_rice <- pl_yld_smooth_wheat %+% 
  filter(yld_smooth_FAO2020_c, crop=="rice" & pred =="smooth_2016") 

################################
#'## Export data
################################


## save plots  
ggsave(pl_yld_smooth_wheat, height = 6, width = 8,
       filename = "figures/yld_smooth_Wheat_rWzAmSq.png")
ggsave(pl_yld_smooth_maize, height = 6, width = 8,
       filename = "figures/yld_smooth_Maize_rWzAmSq.png")
ggsave(pl_yld_smooth_rice, height = 6, width = 8,
       filename = "figures/yld_smooth_Rice_rWzAmSq.png")
# rWzAmSq