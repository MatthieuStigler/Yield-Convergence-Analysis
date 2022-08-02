#' ---
#' Title: "Vacirance, sigma convergence"
#' Author: "Matthieu"
#' Date: 2022-07-13
#' runMat: TRUE
#' ---

library(broom)
library(matPkg)
library(Hmisc)
library(moments)
# library(DescTools)
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
#'## Compute variance
################################


## Compute
var_byY_byC <- yld_smooth_l %>% 
  mutate(yield = log(yield/1000)) %>% 
  group_by(yield_var, y_log, y_type,  year, crop) %>% 
  summarise(mean = mean(yield),
            mean_w = weighted.mean(yield, weights = avg_area),
            Var_uw = var(yield),
            Var_w = wtd.var(x = yield, weights = avg_area, normwt=TRUE),
            InterQuant_uw = diff(quantile(yield, probs = c(0.1, .9))),
            InterQuant_w = diff(wtd.quantile(yield, probs = c(0.1, .9), weights = avg_area, normwt=TRUE)),
            CV_uw = sqrt(Var_uw)/mean,
            CV_w = sqrt(Var_w)/mean_w,
            skew_uw = moments::skewness(yield),
            kurto_uw = moments::kurtosis(yield)) %>% 
  ungroup()

var_byY_byC

## long over stat
var_byY_byC_l <- var_byY_byC %>% 
  gather(stat_full, value, matches("_uw$|_w$")) %>% 
  separate(stat_full, c("stat", "weight"), remove=FALSE) %>% 
  mutate(weight_type = if_else(weight=="w", "weighted", "no weight")) %>% 
  mutate(stat = fct_relevel(stat, c("Var", "InterQuant", "CV")))

var_byY_byC_l

var_byY_byC_l %>% 
  count(stat)

################################
#'## Visu
################################

pl_var_noLog <- var_byY_byC_l %>% 
  filter(y_log==FALSE) %>% 
  filter(stat %in% c("Var", "InterQuant", "CV")) %>% 
  ggplot(aes(x = year, y = value, color = crop, linetype= yield_var))+
  geom_line()+
  facet_grid(stat~weight_type, scales="free_y")+
  ggtitle("Results without log")+
  mat_gg_legend_bottom

pl_var_noLog

## log
pl_var_Log <- pl_var_noLog %+%  
  filter(var_byY_byC_l, y_log==TRUE & stat %in% c("Var", "InterQuant", "CV")) +
  ggtitle("Results with log")

pl_var_Log


##skewness/kurto
pl_skew_kurto <- var_byY_byC_l %>% 
  filter(y_log==TRUE & y_type=="hat") %>% 
  filter(stat %in% c("skew", "kurto")) %>% 
  ggplot(aes(x = year, y = value, color = crop, linetype))+
  geom_line()+
  facet_wrap(~stat, scales="free_y", nrow=2) +
  geom_hline(aes(yintercept = y), linetype=2, data = tibble(stat="skew", y=0))+
  ggtitle("Skewness/kurtosis over time")

pl_skew_kurto

################################
#'## Plain regs
################################

dat_reg_nest <- var_byY_byC_l %>% 
  filter(stat =="Var") %>% 
  nest(data =-c(crop, yield_var, stat,stat_full, weight_type, y_type, y_log))

dat_reg_nest

## test 1
tidy(lm(value ~ year, data=dat_reg_nest$data[[1]]), conf.int = TRUE)

## on all
sig_test <- dat_reg_nest %>% 
  mutate(data = map(data, ~lm(value ~ year, data=.) %>% 
                      tidy(conf.int = TRUE))) %>% 
  unnest(data) %>% 
  filter(term=="year") %>% 
  mat_tidy_clean()

sig_test

## visu
pl_sig_test <- sig_test %>% 
  mutate(y_log = if_else(y_log, "Log", "No log"),
         y_type = paste("Yield", y_type) %>% 
           fct_relevel("Yield raw")) %>% 
  mat_plot_coefs_tidy(x_var=y_type, fill_var = weight_type,
                      fac2_var = crop, fac1_var = y_log, scales = "free") +
  geom_hline(yintercept = 0, color = "black")+
  ggtitle("Coef test for the variance-trend regression")

pl_sig_test

################################
#'## Export data
################################

#write_rds(..., "data_intermediary/")

## save plots  
ggsave(pl_sig_test, height = 5, width = 8,
       filename = "figures/sigma_convergence/coefs_sigma_trend_test_rGtWeOp.png")
ggsave(pl_var_noLog, height = 5, width = 8,
       filename = "figures/sigma_convergence/lines_sigma_var_overTime_noLog_rGtWeOp.png")
ggsave(pl_var_Log, height = 5, width = 8,
       filename = "figures/sigma_convergence/lines_sigma_var_overTime_Log_rGtWeOp.png")
ggsave(pl_skew_kurto, height = 5, width = 8,
       filename = "figures/sigma_convergence/skew_kurto_overTime_Log_rGtWeOp.png")
