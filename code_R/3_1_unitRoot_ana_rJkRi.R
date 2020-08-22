#' ---
#' Title: "Analysis of unit roots"
#' Author: "Matthieu"
#' Date: 2020-05-31
#' runMat: TRUE
#' ---

library(matPkg)
library(urca)
library(plm)
library(tidyverse)

################################
#'## Read data
################################

yld_smooth <- read_csv("temp/yield_pred_3crops.csv")

################################
#'## Prepare
################################



yld_smooth_nest <- yld_smooth %>% 
  select(starts_with("country"), iso3, crop, year, yield, yield_hat) %>% 
  gather(yield_variable, yield, starts_with("yield")) %>% 
  arrange(countrycode, crop, yield_variable, year) %>% 
  group_nest(countrycode, country, iso3, crop, yield_variable)

yld_smooth_nest
yld_smooth_nest$data[[1]]
yld_smooth 

## nest for panel
yld_smooth_nest_panel <- yld_smooth %>% 
  select(starts_with("country"), iso3, crop, year, yield, yield_hat) %>% 
  gather(yield_variable, yield, starts_with("yield")) %>% 
  arrange(countrycode, crop, yield_variable, year) %>% 
  group_nest(crop, yield_variable)

################################
#'## Unit roots
################################
x<-  yld_smooth_nest$data[[1]]$yield

ur_tibble <- function(x){
  ur_const <- ur.ers(x)
  ur_trend <- ur.ers(x, model = "trend")
  cval_const <- c(ur_const@cval[1,], "not"=1000)
  cval_trend <- c(ur_trend@cval[1,], "not"=1000)
  ur_const_pval <- names(cval_const)[ur_const@teststat  <cval_const][1]
  ur_trend_pval <- names(cval_trend)[ur_trend@teststat  <cval_trend][1]
  tibble(ur_const_stat=ur_const@teststat, 
         ur_const_pval=ur_const_pval,
         ur_trend_stat=ur_trend@teststat, 
         ur_trend_pval=ur_trend_pval)
}

ur_tibble(x)

################################
#'## Run all ur tests#################

ur_test_all <- yld_smooth_nest %>% 
  mutate(data=map(data, ~ur_tibble(arrange(., year)$yield))) %>% 
  unnest(data)

ur_test_all


################################
#'## Visu
################################

ur_test_all %>% 
  select(-ends_with("pval")) %>% 
  gather(test_type, value, ends_with("stat")) %>% 
  ggplot(aes(x=value, color=test_type))+
  geom_density()+
  # geom_histogram(stat="count")+
  facet_grid(crop~yield_variable)

ur_test_all %>% 
  select(-ends_with("stat")) %>% 
  gather(test_type, value, ends_with("pval")) %>% 
  mutate(test_type=str_remove_all(test_type, "ur_|_pval")) %>% 
  ggplot(aes(x=value, fill=test_type))+
  # geom_density()+
  geom_histogram(stat="count", position="dodge")+
  facet_grid(crop~yield_variable) +
  ggtitle("Unit root test applied individually: most not rejected")

################################
#'## panel tests
################################

yld_smooth_nest_panel
yld_smooth_nest_panel$data[[1]] 
yld_smooth_nest_panel_plm <- yld_smooth_nest_panel %>% 
  mutate(data_plm=map(data, ~select(., iso3, year, yield) %>% 
                        pdata.frame(index = c("iso3", "year"))),
         data_wide = map(data, ~select(., iso3, year, yield) %>% 
                           spread(iso3, yield)))

head(yld_smooth_nest_panel_plm$data_plm[[1]])
out_1 <- purtest(yld_smooth_nest_panel_plm$data_wide[[1]])
out_1

pur_tidy <- function(df){
  out <- purtest(df)
  tibble(stat=out$statistic$statistic,
         p_val=out$statistic$p.value)
}

yld_smooth_nest_panel_plm %>% 
  mutate(pur_1=map(data_wide, pur_tidy)) %>% 
  select(crop, yield_variable, 
         pur_1) %>% 
  unnest(pur_1)

################################
#'## TITLE
################################


regs_dyn <- yld_smooth_nest_panel_plm[-2,] %>% 
  mutate(pggm_l3=map(data_plm, ~pgmm(yield ~ lag(yield, 1) | lag(yield, 3:10),
                              data=.,
                              effect="individual")))

regs_dyn$pggm_l3

################################
#'## Export data
################################

#write_rds(..., "data_intermediary/")

## save plots  
# ggsave(..., height = gg_height, width = gg_width,
#        filename = "output/figures/xxx")