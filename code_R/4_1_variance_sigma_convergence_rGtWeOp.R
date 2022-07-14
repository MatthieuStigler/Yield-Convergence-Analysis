#' ---
#' Title: "Vacirance, sigma convergence"
#' Author: "Matthieu"
#' Date: 2022-07-13
#' runMat: TRUE
#' ---

library(Hmisc)
library(matPkg)

################################
#'## Read data
################################

yld_smooth <- read_csv("temp/yield_pred_3crops.csv")

################################
#'## Prepare data
################################

var_byY_byC <- yld_smooth %>% 
  group_by(year, crop) %>% 
  summarise(mean = mean(yield_hat),
            var = var(yield_hat),
            var_w = wtd.var(x = yield_hat, weights = avg_area, normwt=TRUE),
            cv = sqrt(var)/mean) %>% 
  ungroup()

var_byY_byC

################################
#'## Visu
################################

var_byY_byC %>% 
  ggplot(aes(x = year, y = var_w, color = crop))+
  geom_line()

################################
#'## Export data
################################

#write_rds(..., "data_intermediary/")

## save plots  
# ggsave(..., height = gg_height, width = gg_width,
#        filename = "output/figures/xxx")
rGtWeOp