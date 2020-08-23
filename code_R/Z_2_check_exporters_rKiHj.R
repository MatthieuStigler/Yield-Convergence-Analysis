#' ---
#' Title: "Check crops conditions "
#' Author: "Matthieu"
#' Date: 2020-05-22
#' runMat: TRUE
#' ---

library(matPkg)
library(tidyverse)

################################
#'## Read data
################################

dat_all <- read_csv("dataRaw/data_from_AFRI_AERC/data_merged_ALL_Cntry_Commod_Year_outerJoin_mAd.csv", guess_max = 5000)
yld_smooth <- read_csv("temp/yield_pred_3crops.csv")

################################
#'## Prepare
################################

dat_all_c <- dat_all %>% 
  select(Country, iso3, Year, Commodity, FAO3__Area, FAO3__Production, FAO3__Yield) %>% 
  rename_all(~str_replace(., "FAO3__", "")) %>% 
  filter(Commodity%in%c("soybean", "maize", "wheat", "rice")) 

dat_all_c

## long version
dat_all_l <- dat_all_c %>% 
  gather(stat, value, Area, Production, Yield)



################################
#'## Check missing values?
################################

## area NA?
dat_all_c %>% 
  filter(is.na(Area)) %>% 
  count(Country, iso3)

dat_all_c %>% 
  filter(!is.na(Area))

## initial count
dat_all_c %>% 
  filter(!is.na(Area)) %>% 
  distinct(Country, Commodity) %>% 
  count(Commodity)

yld_smooth %>% 
  distinct(crop, iso3) %>% 
  count(crop)

################################
#'## Compute shares
################################

dat_all_c %>% 
  filter(is.na(Area))

## aggregate
dat_all_c_ave <- dat_all_c %>% 
  filter(!is.na(Area)) %>% 
  group_by(Country, iso3, Commodity) %>%
  summarise(Area = mean(Area)) %>%
  ungroup() %>% 
  group_by(Commodity) %>%
  mutate(rank=rank(-1*Area)) %>% 
  arrange(Commodity, desc(Area)) %>% 
  mutate(perc_tot=100*Area/sum(Area, na.rm=TRUE),
         quant_5=quantile(Area, 0.95, na.rm=TRUE),
         remove_q5=!Area<=quant_5,
         perc_no5_q=100*Area/sum(Area[rank>5], na.rm=TRUE),
         perc_no5=100*Area/sum(Area[Area<=quant_5], na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(keep=perc_no5>0.5,
         keep_q5=perc_no5_q>0.5)

dat_all_c_ave

dat_all_c_ave %>% 
  filter(keep) %>% 
  count(Commodity) %>% 
  full_join(dat_all_c_ave %>% 
              filter(keep_q5) %>% 
              count(Commodity), by = "Commodity",
            suffix = c(".top5", ".quant5"))

## compare with 
yld_smooth %>% 
  distinct(crop, iso3) %>% 
  count(crop)

################################
#'## Same with yld_smooth data
################################

smooth_area_means <- yld_smooth %>% 
  group_by(crop, country, iso3) %>%
  summarise(area = mean(area)) %>%
  ungroup() %>% 
  group_by(crop) %>%
  mutate(rank=rank(-1*area)) %>% 
  arrange(crop, desc(area)) %>% 
  mutate(perc_tot=100*area/sum(area, na.rm=TRUE),
         quant_5=quantile(area, 0.95),
         remove_q5=!area<=quant_5,
         perc_no5_q=100*area/sum(area[rank>5], na.rm=TRUE),
         perc_no5=100*area/sum(area[area<=quant_5], na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(keep=perc_no5>0.5,
         keep_q5=perc_no5_q>0.5)

smooth_area_means

smooth_area_means%>% 
  filter(remove_q5)


smooth_area_means %>% 
  mat_count_separate(keep_q5, keep)

smooth_area_means%>% 
  filter(keep) %>% 
  count(crop)

yld_smooth %>% 
  filter(prop_global_area>0.5/100) %>% 
  distinct(crop, iso3) %>% 
  count(crop)

################################
#'## Nathan code
################################
# 
# bysort country: egen avg_area=mean(area)
# * List countries as percent of total global growing area of the crop
# total avg_area if year==2011 
# gen prop_global_area=avg_area/_b[avg_area]
# gsort - prop_global_area
# list country prop_global_area if year==2011 
# 
# * List countries as percent of total global growing area among non-top 5% growing countries
# summ avg_area, detail
# local area95=r(p95)
# total avg_area if year==2011 & avg_area<`area95'

################################
#'## Compare more
################################

yld_smooth %>% 
  distinct(iso3, crop, prop_global_area) %>%  
  mutate(prop_global_area=100*prop_global_area) %>% 
  left_join(dat_all_c_ave, by = c("iso3","crop"="Commodity")) 

yld_smooth

################################
#'## Weird
################################

library(tidyverse)
read_csv("/home/matifou/Dropbox/Communs/Yield Gaps/temp/yield_pred_3crops.csv") %>% 
  distinct(crop, country, prop_global_area) %>% 
  group_by(crop) %>%
  mutate(sum_prop_global_area=sum(prop_global_area)) %>% 
  top_n(5,  prop_global_area) %>% 
  knitr::kable()

################################
#'## Visu
################################

## reorder within
dat_all_ave_ord <- dat_all_c_ave %>% 
  arrange(Commodity, desc(perc_tot)) %>% 
  filter(perc_no5>0.5) %>% 
  mutate(order_plot=factor(1:n()))
  

dat_all_ave_ord %>% 
  mutate(iso3_keep=if_else(rank<=5, iso3, "Other")) %>% 
  ggplot(aes(x=order_plot, y=perc_tot, fill=iso3_keep))+
  geom_col()+
  facet_wrap(Commodity~., scales="free")+
  theme(axis.text.x = element_text(angle=10, size=5))+
  scale_x_discrete(breaks=dat_all_ave_ord$order_plot,
                   labels=dat_all_ave_ord$iso3,
                   guide = guide_axis(n.dodge = 2))



# rKiHj
