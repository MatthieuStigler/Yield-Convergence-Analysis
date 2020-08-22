#' ---
#' Title: "Try again"
#' Author: "Matthieu"
#' Date: 2020-06-06
#' runMat: TRUE
#' ---

library(matPkg)

################################
#'## Read data
################################

fao_prod <- haven::read_dta("temp/FAO_crop_production.dta")

################################
#'## Prepare data
################################

fao_prod_c <- fao_prod %>% 
  rename(crop=commodity)

fao_prod_c %>% 
  distinct(crop, country) %>% 
  count(crop)

fao_area <- fao_prod_c %>% 
  group_by(crop, country, iso3) %>%
  summarise(area = mean(area)) %>%
  ungroup() %>% 
  group_by(crop) %>%
  mutate(rank=rank(-1*area)) %>% 
  arrange(crop, desc(area)) %>% 
  mutate(perc_tot=100*area/sum(area, na.rm=TRUE),
         perc_cumul=cumsum(perc_tot),
         quant_5=quantile(area, 0.95),
         remove_q5=area>=quant_5,
         remove_top5=rank<=5,
         perc_no5=100*area/sum(area[rank>5], na.rm=TRUE),
         perc_no5_q=100*area/sum(area[area<=quant_5], na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(keep=perc_no5>0.5,
         keep_q5=perc_no5_q>0.5)


fao_area %>% 
  select(crop, country,rank, perc_tot, perc_cumul,
         remove_q5, remove_top5)


fao_area %>% 
  filter(keep_q5) %>% 
  count(crop)

fao_area %>% 
  filter(keep) %>% 
  count(crop)

fao_area %>% 
  group_by(crop) %>% 
  slice(n()+c(-3, 0))

################################
#'## Visu
################################


fao_area_ord <- fao_area %>% 
  arrange(crop, desc(perc_tot)) %>% 
  filter(perc_no5_q>0.5) %>% 
  mutate(order_plot=factor(1:n()))

fao_area_ord

##
pl_area_shares <- fao_area %>% 
  filter(perc_no5_q>0.5) %>% 
  ggplot(aes(x=rank, y=perc_tot, fill=remove_q5))+
  geom_col()+
  facet_wrap(crop~., scales="fixed")

pl_area_shares

## same. but with names
pl_area_shares <- fao_area_ord %>% 
  mutate(iso3_keep=if_else(rank<=5, iso3, "Other")) %>% 
  ggplot(aes(x=order_plot, y=perc_tot, fill=iso3_keep))+
  geom_col()+
  facet_wrap(crop~., scales="free")+
  scale_x_discrete(breaks=fao_area_ord$order_plot,
                   # labels=fao_area_ord$iso3,
                   guide = guide_axis(n.dodge = 2))+
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank())#text(angle=10, size=2))+

pl_area_shares

fao_area %>% 
  filter(keep_q5) %>% 
  ggplot(aes(x=))

################################
#'## Export data
################################

#write_rds(..., "data_intermediary/")

## save plots  
# ggsave(..., height = gg_height, width = gg_width,
#        filename = "output/figures/xxx")