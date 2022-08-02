#' ---
#' Title: "Vacirance, sigma convergence"
#' Author: "Matthieu"
#' Date: 2022-07-13
#' runMat: TRUE
#' ---

library(diptest)
library(multimode)
library(matPkg)
library(tidyverse)

################################
#'## Read data
################################

yld_smooth <- read_csv("temp/yield_pred_3crops.csv")

################################
#'## Prepare data
################################

## Long over yield vars
yld_smooth_l <- yld_smooth %>% 
  select(countrycode, crop, year, yield, yield_hat, area_hat, avg_area) %>% 
  rename(yield_raw=yield) %>% 
  mutate(across(starts_with("yield"), list(log = log))) %>% 
  gather(yield_var, yield, starts_with("yield")) %>% 
  mutate(y_log = str_detect(yield_var, "log"),
         y_type = str_extract(yield_var, "hat|raw"))
  
yld_smooth_l %>% 
  count(yield_var, y_log, y_type)



################################
#'## Bimodality test
################################

## We use diptest and the tests from multimode::modetest

## some of them are very slow
if(FALSE) {
  
  data=geyser
  mb <- microbenchmark::microbenchmark(HH = modetest(data, B=100, method = "HH"),
                                       CH = modetest(data, B=100, method = "CH"),
                                       FM = modetest(data, B=100, method = "FM"),
                                       ACR = modetest(data, B=100, method = "ACR"),
                                       SI = modetest(data, B=100, method = "SI"),
                                       HY = modetest(data, B=100, method = "HY"),
                                       times=2)
  
  mb
}


## aux function to run Excess mass and diptest
dip.test_tidy <- function(x, B=500) {
  
  ## run 
  # methods <- c("HH", "CH", "ACR", "FM", "SI", "HY") ## all
  # methods <- c("HH", "CH", "FM") ## FM very slow!
  methods <- c("HH", "CH")
  l <- lapply(methods,  \(m) multimode::modetest(x, B=B, method = m))
  out <- diptest::dip.test(x)
  
  ## arrange
  map_dfr(l, ~tibble(method = .$method, statistic =.$statistic, p.value = .$p.value )) %>% 
    bind_rows(tibble(method = "diptest", statistic = out$statistic, p.value = out$p.value))
  
}

if(FALSE) dip.test_tidy(geyser)

## Run for every year/every crop
set.seed(123)
dip_stats <- yld_smooth_l %>% 
  mutate(yield = log(yield/1000)) %>% 
  filter(y_type=="hat" & y_log==TRUE) %>% 
  group_by(yield_var, y_log, y_type,  year, crop) %>% 
  summarise(dip.test_tidy(yield)) %>% 
  ungroup() %>% 
  mutate(is_signif = p.value<0.05)

dip_stats

## Check min p values
dip_stats %>% 
  group_by(yield_var, method) %>% 
  slice_min(p.value)

## plot pvalues over time
dat_prep_bimod <- dip_stats %>% 
  filter(y_type=="hat" & y_log==TRUE) %>% 
  filter(method!="diptest")

pl_bimod <- dat_prep_bimod %>% 
  ggplot(aes(x = year, y = p.value, color = crop))+
  geom_line()+
  geom_hline(yintercept=0.1, linetype=2)+
  # geom_point(data=filter(dat_prep_bimod, year %in% seq(1965, 2015, by=10)))+
  geom_vline(xintercept = seq(1965, 2015, by=10), linetype = 2, color = "grey")+
  facet_wrap(~method, nrow=3) +
  ggtitle("P-values of unimodality vs multi tests over time")

pl_bimod


################################
#'## Export data
################################

#write_rds(..., "data_intermediary/")

## save plots  
ggsave(pl_bimod, height = 5, width = 8,
       filename = "figures/sigma_convergence/bimodality_stats_overTime_Log_rGtWeBm.png")


# 
