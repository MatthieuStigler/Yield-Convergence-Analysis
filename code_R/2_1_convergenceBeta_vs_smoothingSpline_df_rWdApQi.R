#' ---
#' Title: "Convergence analysis in R"
#' Author: "Matthieu"
#' Date: 2022-08-12
#' runMat: TRUE
#' ---

library(multiDiff) # devtools::install_github("MatthieuStigler/multiDiff")
library(broom)
library(matPkg)
library(texreg)
library(magrittr)
library(lfe)
if(packageVersion("fixest")<="0.10.4"){
  devtools::install_github("lrberge/fixest")
}
library(fixest)
library(tidyverse)

################################
#'## Read data
################################

yld_smooth_FAO2020 <- read_rds("data_intermediary/FAOSTAT_3crops_2020_Yonly_smoothed_rSwApAm.rds") # from 1_2


################################
#'## Prepare data
################################


## prep 1
FAO_2020_prep <- yld_smooth_FAO2020 %>% 
  rename(yield_hat = trend) %>% 
  filter(pred=="smooth_full") %>% 
  select(-deviation, -pred, -yield_hat) %>% 
  mutate(crop = str_to_title(crop)) 

FAO_2020_prep


## data is balanced!
FAO_2020_prep %>% 
  count(country, crop) %>% 
  count(n)

### smooth
splin_params_df <- tibble(df = c(2,5,10, 15, 20,30, 40, 45, 50, 55, 60))

do_spline_1 <- function(data, df = 6) {
  # if(df==0){
  #   pred <- data$yield
  # } else {
  pred <- fitted(lm(yield ~  splines::ns(year, df=df), data=data))
  data %>% 
    select(year, yield) %>% 
    mutate(yield = pred)
}

do_spline_all <- function(data) {
  
  splin_params_df %>% 
    mutate(data = map(df, ~do_spline_1(data, df=.))) %>% 
    unnest(data)
    
}

FAO_2020_prep_nst <- FAO_2020_prep %>% 
  nest(data=-c(country, countrycode, iso3, crop, n_obs)) 
  
FAO_2020_prep_nst$data[[1]] %>% do_spline_1(df=1)
FAO_2020_prep_nst$data[[1]] %>% do_spline_all() %>% 
  filter(year==1961)

## smooth all, varying df
ylds_spline_many <- FAO_2020_prep_nst %>% 
  mutate(data = map(data, do_spline_all)) %>% 
  unnest(data)



## add lags, etc
ylds_spline_many_c <- ylds_spline_many%>% 
  group_by(countrycode, crop, df) %>% 
  arrange(year) %>% 
  mutate(ln_yield = log(yield),
         ln_yield_lag= dplyr::lag(ln_yield, order_by = year),
         ln_yield_diff = ln_yield-ln_yield_lag) %>% 
  ungroup() %>% 
  mutate(yield_hat_growth1 = ln_yield_diff,
         l.ln_yield_hat= ln_yield_lag,
         country_grp = country,
         year_sq = year^2) %>% 
  filter(year!=1961) # lost because of diff

ylds_spline_many_c

## NAs?
ylds_spline_many_c %>% 
  filter(is.na(ln_yield))

## nest
FAO_smth_nst <- ylds_spline_many_c %>% 
  filter(year<=2016) %>%
  filter(yield>0) %>% 
  nest(data = -c(crop, df))

FAO_smth_nst

################################
#'## Run regs
################################

### run regs
regs_smth_many <- FAO_smth_nst %>% 
  mutate(reg_FE0 = map(data, ~felm(yield_hat_growth1 ~l.ln_yield_hat|0|0|country_grp, data = .)),
         reg_FE1 = map(data, ~felm(yield_hat_growth1 ~l.ln_yield_hat|year|0|country_grp, data = .)),
         reg_FE2 = map(data, ~felm(yield_hat_growth1 ~l.ln_yield_hat|year+country|0|country_grp, data = .)),
         reg_FE2_t = map(data, ~feols(yield_hat_growth1 ~l.ln_yield_hat|country[year]+country, data = ., vcov = ~country)),
         reg_FE2_t2 = map(data, ~feols(yield_hat_growth1 ~l.ln_yield_hat|country[year]+country[year_sq]+country, data = ., vcov = ~country))) %>% 
  select(-data) %>% 
  gather(reg_type, reg, starts_with("reg")) %>% 
  mutate(coef = map(reg, broom::tidy, conf.int=TRUE),
         mod_name = paste(crop, str_remove_all(reg_type, "reg_")) ,
         reg_type = fct_relevel(reg_type, c("reg_FE0", "reg_FE1"))) %>% 
  arrange(crop, reg_type)

regs_smth_many

## get coefs tidy
coefs_smth_df <- regs_smth_many %>% 
  select(-reg) %>% 
  unnest(coef) %>% 
  mat_tidy_clean() %>% 
  filter(term =="l.ln_yield_hat")

coefs_smth_df

################################
#'## Visu res
################################

pl_coef_along_smoothing <- coefs_smth_df%>% 
  mutate(is_signif_5 = p_value<0.05) %>% 
  filter(reg_type %in% c("reg_FE1", "reg_FE2")) %>% 
  ggplot(aes(x = df, y= estimate, color = is_signif_5))+
  geom_errorbar(aes(ymin = .data$conf_low, 
                    ymax = .data$conf_high), colour = I("black"))+
  geom_point()+
  facet_grid(crop~reg_type, scales="free_y")+
  geom_hline(yintercept = 0, linetype=2)+
  geom_vline(xintercept = 5, linetype=2)+
  theme(legend.position = "bottom")+
  ggtitle("Convergence Coef along the smoothing axis")+
  labs(subtitle = "Increasing x-axis means closer to raw yields, vertical bar is as in paper (5)")+
  xlab("Degree of smoothing, 60 = raw yields")

pl_coef_along_smoothing

################################
#'## Export
################################

ggsave(pl_coef_along_smoothing, width=8 ,height = 5,
       filename = "figures/beta_convergence/beta_over_smoothing_rWdApQi.png")

# rWdApQi