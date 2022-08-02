#' ---
#' Title: "Plot initial "
#' Author: "Matthieu"
#' Date: 2020-05-22
#' runMat: TRUE
#' ---

library(matPkg)
library(tidyverse)
library(RColorBrewer)
library(patchwork)
# library(tidybayes) # stat_eye
library(ggdist) # stat_eye

source("code_R/888_misc_functions.R")

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

## true quantiles (not intervals)
quants <- c(5, 10, 50, 90, 95)/100
dat_quant_only <- yld_smooth_c %>% 
  mutate(yield_hat_log=log(yield_hat)) %>% 
  group_by(year, crop) %>% 
  summarise(q_name = as_factor(paste0(100*quants, "%")),
            q_value = quantile(yield_hat_log, quants)) %>% 
  ungroup()

dat_quant_only
levels(dat_quant_only$q_name)

## add quantiles


dat_quant <- yld_smooth_c %>% 
  group_by(year, crop) %>% 
  mutate(quantile=cut(yield_hat, quantile(yield_hat,  seq(0, 1, by=0.2)), include.lowest = TRUE) %>% 
           as.integer()) %>% 
  group_by(year, crop, quantile) %>% 
  summarise(mean_yield=mean(yield_hat, na.rm = TRUE),
            n_obs=n()) %>% 
  ungroup() %>% 
  mat_add_perc(year, crop, .name=n_obs)

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
#'## Quantile interval means
################################

pl_quant_update<- dat_quant %>% 
  ggplot(aes(x=year, y=mean_yield,
             color=factor(quantile)))+
  geom_line()+
  facet_grid(crop~., scales = "free") + 
  mat_gg_legend_bottom +
  ylab("Yield")+
  labs(color = "Quantile:") +
  ggtitle("Continuously updated quantiles intervals means")

pl_quant_update

pl_quant_initial <-pl_quant_update %+% filter(dat_quant_init, !is.na(quantile)) +
  ggtitle("Initial (1960-65) quantile interval means")

pl_quant_initial


## initial, median 
pl_quant_initial_med <-pl_quant_update %+% filter(dat_quant_init, !is.na(quantile)) +
  ggtitle("Initial (1960-65) quantile intervals medians") +aes(y=median_yield)

pl_quant_initial_med

################################
#'## Quant only
#'################################

pl_quant_true <- dat_quant_only %>% 
  prj_clean_crop_names %>% 
  ggplot(aes(x=year, y=q_value,
             color=q_name))+
  geom_line()+
  facet_wrap(crop~., scales = "fixed", nrow=1) + 
  # mat_gg_legend_bottom +
  ylab("Log yield")+xlab("Year")+
  labs(color = "Quantile:") +
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
  theme_bw() +
  guides(colour = guide_legend(reverse=T))

pl_quant_true

################################
#'## Densities
################################


pl_dens_5 <- yld_smooth_c %>% 
  mutate(yield_hat_log=log(yield_hat),
         type=year %in% c(1965, 1985, 2005)) %>% 
  prj_clean_crop_names %>% 
  filter(year %in% seq(1965, 2015, by=10)) %>% 
  ggplot(aes(x=yield_hat_log, color=factor(year)))+
  stat_density(geom="line",position="identity")  +
  facet_wrap(crop~., scales = "fixed", nrow=1) +
  xlab("Log yield")+ylab("Density")+
  labs(color="Year", linetype="Year") +
  scale_color_viridis_d()+
  # scale_color_manual(values = brewer.pal(7, "YlGnBu")[-1])+
  theme_bw() 
  # scale_linetype_manual("", values=c(1,2,1,2))

pl_dens_5

################################
#'## Violin
################################

## Prep yield data: every 10 Y
yld_sm_10Y <- yld_smooth_c %>% 
  mutate(yield_hat_log=log(yield_hat),
         type=year %in% c(1965, 1985, 2005)) %>% 
  prj_clean_crop_names %>% 
  filter(year %in% seq(1965, 2015, by=10))

## Violin vertical
pl_violin <-yld_sm_10Y  %>%
  ggplot(aes(x=factor(year), y=yield_hat_log))+
  stat_eye(side = "left", point_interval="median_qi",
           slab_colour="black", slab_size=.5)+
  facet_wrap(crop~., scales = "fixed", nrow=1) +
  xlab("Year")+ylab("Log yield")+
  theme_bw() +
  ggtitle("Density, with median and inter-quartile range")

pl_violin 

## inverse axis
pl_violin_horiz <-yld_sm_10Y  %>%
  ggplot(aes(x=yield_hat_log, y=factor(year)))+
  stat_eye(side = "right", point_interval = "median_qi",
           slab_colour="black", slab_size=.5,
           orientation= "horizontal", adjust=1)+
  facet_wrap(crop~., scales = "fixed", nrow=1) +
  ylab("Year")+xlab("Log yield")+
  theme_bw() +
  ggtitle("Density, with median and inter-quartile range")

pl_violin_horiz

## horizontal, years together
pl_violin_horiz_B <- pl_violin_horiz +
  facet_wrap(crop~., scales = "fixed", nrow=3)
  
pl_violin_horiz_B

################################
#'## Patch
################################

pl_dens_quant <- pl_dens_5 / pl_quant_true

################################
#'## Export data
################################

ggsave_paper(pl_quant_true, "figures/smooth_quantiles_true_byY_rUifsdm.png")
ggsave_paper(pl_dens_5, "figures/smooth_densities_byCY_rUifsdm.png")
ggsave_paper(pl_dens_quant, "figures/smooth_densities_quant_rUifsdm.png")

ggsave_paper(pl_violin, "figures/yield_densities/smooth_densities_byY_rUifsdm.png")
ggsave_paper(pl_violin_horiz, "figures/yield_densities/smooth_densities_byY_horiz_rUifsdm.png")


## save plots  
ggsave(pl_quant_update, height = gg_height, width = gg_width,
       filename = "figures/mat_desc_quantiles/smooth_quantiles_byY_updated_rUifsdm.png")
ggsave(pl_quant_initial, height = gg_height, width = gg_width,
       filename = "figures/mat_desc_quantiles/smooth_quantiles_byY_initial_rUifsdm.png")
ggsave(pl_quant_initial_med, height = gg_height, width = gg_width,
       filename = "figures/mat_desc_quantiles/smooth_quantiles_byY_initial__median_rUifsdm.png")
