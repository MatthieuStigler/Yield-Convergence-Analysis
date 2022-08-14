#' ---
#' Title: "Smooth yields FAO"
#' Author: "Matthieu"
#' Date: 2022-08-12
#' runMat: TRUE
#' ---

library(splines)
library(matPkg)
library(tidyverse)

################################
#'## Read data
################################

FAO_2020_Y <- read_rds("data_intermediary/FAOSTAT_3crops_2020_Yonly_rSeWhYs.rds")

################################
#'## Prepare data
################################

## nest data by country-crop
FAO_2020_Y_nst <- FAO_2020_Y %>% 
  arrange(countrycode, crop, year) %>% 
  nest(data = -c(crop, country, countrycode, iso3)) %>% 
  mutate(n_obs = map_int(data, nrow)) 

FAO_2020_Y_nst
FAO_2020_Y_nst$data[[1]]


## is panel balanced? yes!
unique(FAO_2020_Y_nst$n_obs)

################################
#'## Run splines
################################

do_spline <- function(data, df = 6) {
  # t <- seq_along(x)
  reg <- lm(yield ~  ns(year, df=df), data=data)
  data %>% 
    select(year, yield) %>% 
    mutate(deviation = residuals(reg), 
           trend = fitted(reg))
}

if(FALSE){
  N <- 100
  X_t <- (1:N)-N/2
  y <-  5 + - 0.1*X_t^2 + rnorm(N, sd = 10)
  data <- tibble(year = X_t, yield = y)
  
  # plot
  data %>% 
    do_spline() %>% 
    ggplot(aes(x = year, y = trend))+
    geom_line()+
    geom_point(aes(y=yield), col = "blue")
}


## Run natural spline with 5 knots
splines_out <- FAO_2020_Y_nst %>% 
  mutate(smooth_full = map(data, ~do_spline(.)),
         smooth_2016 = map(data, ~do_spline(filter(., year<=2016))))

splines_out_df <- splines_out %>% 
  select(-data) %>% 
  gather(pred, data, starts_with("smooth")) %>% 
  unnest(data) 

splines_out_df

## long
splines_out_df_l <- splines_out_df %>% 
  gather(stat, value, deviation, trend, yield)

splines_out_df_l

################################
#'## Visu
################################

pl_yld_smooth_wheat <- splines_out_df %>% 
  filter(crop=="wheat") %>% 
  filter(pred =="smooth_2016") %>% 
  mat_head_group(countrycode, n_head = 16) %>%
  ggplot(aes(x = year, y=trend))+
  geom_line(color ="blue", lwd=1.2)+
  geom_point(aes(y=yield), size =0.2)+
  facet_wrap(~country, scales = "free_y")+
  xlab("Year")+ylab("Yield")

pl_yld_smooth_wheat

################################
#'## Export data
################################

write_rds(splines_out_df, "data_intermediary/FAOSTAT_3crops_2020_Yonly_smoothed_rSwApAm.rds")
## READ AS: yld_smooth_FAO2020 <- read_rds("data_intermediary/FAOSTAT_3crops_2020_Yonly_smoothed_rSwApAm.rds") # from 1_2

## save plots  
ggsave(pl_yld_smooth_wheat, height = gg_height, width = gg_width,
       filename = "output/figures/xxx")
# rSwApAm