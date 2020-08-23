#' ---
#' Title: "Plot initial "
#' Author: "Matthieu"
#' Date: 2020-05-22
#' runMat: TRUE
#' ---

library(matPkg)o
library(tidyverse)

################################
#'## Read data
################################

dat_all <- read_csv("dataRaw/data_from_AFRI_AERC/data_merged_ALL_Cntry_Commod_Year_mAd.csv")
yld_smooth <- read_csv("temp/yield_pred_3crops.csv")

################################
#'## Prepare data
################################

## prepare
dat_all_c <- dat_all %>% 
  select(Country, CountryCode, iso3, Year, Commodity, FAO3__Yield) %>% 
  filter(Commodity%in%c("soybean", "maize", "wheat")) %>% 
  rename(yield=FAO3__Yield,
         crop=Commodity,
         year=Year)

dat_all_c

## prepare smooth
yld_smooth_c <- yld_smooth 


## add quantiles
dat_quant <- yld_smooth_c %>% 
  group_by(year, crop) %>% 
  mutate(quantile=cut(yield_hat, quantile(yield_hat, seq(0, 1, by=0.2)), include.lowest = TRUE) %>% 
           as.integer()) %>% 
  group_by(year, crop, quantile) %>% 
  summarise(mean_yield=mean(yield_hat, na.rm = TRUE)) %>% 
  ungroup()

dat_quant 

## initial quant
quant_inits <- yld_smooth_c %>% 
  filter(between(year, 1960, 1965)) %>%
  group_by(iso3, crop) %>%
  summarise(mean_yield = mean(yield, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(crop)%>%
  mutate(quantile=cut(mean_yield, quantile(mean_yield, seq(0, 1, by=0.2)), include.lowest = TRUE) %>% 
           as.integer())%>%
  ungroup()

quant_inits%>%
  filter(is.na(quantile))

## add quant, average
dat_quant_init <- yld_smooth_c %>% 
  left_join(quant_inits, by = c("iso3", "crop"))%>%
  group_by(year, crop, quantile) %>% 
  summarise(mean_yield=mean(yield_hat, na.rm = TRUE),
            median_yield=median(yield_hat, na.rm = TRUE)) %>% 
  ungroup()

## some country not classified in the beginning
dat_quant_init %>%
  filter(is.na(quantile))

################################
#'## Visu
################################

pl_quant_update<- dat_quant %>% 
  ggplot(aes(x=year, y=mean_yield,
             color=factor(quantile)))+
  geom_line()+
  facet_grid(crop~., scales = "free") + 
  mat_gg_legend_bottom +
  ylab("Yield")+
  labs(color = "Quantile:") +
  ggtitle("Continuously updated quantiles")

pl_quant_update

pl_quant_initial <-pl_quant_update %+% filter(dat_quant_init, !is.na(quantile)) +
  ggtitle("Initial (1960-65) quantiles")

pl_quant_initial


## initial, meian 
pl_quant_initial_med <-pl_quant_update %+% filter(dat_quant_init, !is.na(quantile)) +
  ggtitle("Initial (1960-65) quantile, medians") +aes(y=median_yield)

pl_quant_initial_med

################################
#'## Export data
################################


## save plots  
ggsave(pl_quant_update, height = gg_height, width = gg_width,
       filename = "figures/mat_desc_quantiles/smooth_quantiles_byY_updated_rUifsdm.png")
ggsave(pl_quant_initial, height = gg_height, width = gg_width,
       filename = "figures/mat_desc_quantiles/smooth_quantiles_byY_initial_rUifsdm.png")
ggsave(pl_quant_initial_med, height = gg_height, width = gg_width,
       filename = "figures/mat_desc_quantiles/smooth_quantiles_byY_initial__median_rUifsdm.png")
