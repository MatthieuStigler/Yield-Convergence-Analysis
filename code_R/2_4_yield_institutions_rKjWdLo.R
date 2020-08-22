#' ---
#' Title: "New plot"
#' Author: "Matthieu"
#' Date: 2020-06-19
#' runMat: TRUE
#' ---

library(matPkg)
library(haven)
library(patchwork)

source("code_R/888_misc_functions.R")

################################
#'## Read data
################################

yld_smooth <- read_csv("temp/yield_pred_3crops.csv")

prop_right <- read_dta("temp/property_rights_graph_data.dta")
anti_div <- read_dta("temp/gov_antidiversion_graph_data.dta")
demo_acc <- read_dta("temp/demo_account_index_graph_data.dta")


################################
#'## Prepare data
################################

## check one
# demo_acc %>% 
#   select(countrycode, country, iso3, commodity, yhat, avg_demo_account_index) %>% 
#   ggplot(aes(x=yhat, y=avg_demo_account_index, color = commodity))+
#   geom_point()
#   
#   mutate(ratio=yhat/avg_demo_account_index)
# 
# ## merge
# dat_mer <- prop_right %>%
#   select(countrycode, country, iso3, commodity, yhat, avg_property_rights) %>% 
#   left_join(anti_div %>% 
#               select(countrycode, country, iso3, commodity, yhat, avg_gov_antidiversion),
#             by = c("countrycode", "country", "iso3", "commodity")) %>% 
#   left_join(demo_acc %>% 
#               select(countrycode, country, iso3, commodity, yhat, avg_demo_account_index),
#             by = c("countrycode", "country", "iso3", "commodity")) %>% 
#   rename(crop=commodity)
# 
# 
# 
# ## long
# dat_mer_l <- dat_mer %>% 
#   select(-yhat.y, -yhat.x) %>% 
#   gather(variable, value, avg_property_rights, avg_gov_antidiversion, avg_demo_account_index) %>% 
#   mutate(variable=case_when(variable=="avg_demo_account_index"~ "Democratic Accountability",
#                             variable=="avg_property_rights"~ "Property Rights",
#                             variable=="avg_gov_antidiversion"~ "Anti-Diversion") %>% 
#            fct_relevel("Property Rights"))
# 
# dat_mer_l %>% 
#   count(variable)

################################
#'## new correct code
################################

dat_mer <- prop_right %>%
  select(countrycode, country, iso3, commodity, yhat, avg_property_rights, yhat_hi, yhat_lo, avg_ln_yield) %>% 
  rename(value=avg_property_rights) %>% 
  mutate(variable="avg_property_rights") %>% 
  rbind(anti_div %>% 
              select(countrycode, country, iso3, commodity, yhat, avg_gov_antidiversion, yhat_hi, yhat_lo, avg_ln_yield) %>% 
          rename(value=avg_gov_antidiversion) %>% 
          mutate(variable="avg_gov_antidiversion")) %>% 
  rbind(demo_acc %>% 
              select(countrycode, country, iso3, commodity, yhat, avg_demo_account_index, yhat_hi, yhat_lo, avg_ln_yield) %>% 
          rename(value=avg_demo_account_index) %>% 
          mutate(variable="avg_demo_account_index")) %>% 
  rename(crop=commodity)%>% 
  mutate(variable=case_when(variable=="avg_demo_account_index"~ "Democratic Accountability",
                            variable=="avg_property_rights"~ "Property Rights",
                            variable=="avg_gov_antidiversion"~ "Anti-Diversion") %>% 
           fct_relevel("Property Rights"))


dat_mer

################################
#'## Visu facet_grid
################################

pl_yld_instit <- dat_mer %>% 
  ggplot(aes(x=value, y=avg_ln_yield))+
  geom_ribbon(aes(ymin=yhat_lo, ymax=yhat_hi), fill ="grey") +
  geom_point()+
  geom_line(aes(y=yhat)) +
  # geom_smooth(method="lm") +
  ylab("Average Log Yield (1984-2016)")+ xlab("Value")+
  facet_grid(variable~crop, switch="y") +
  theme_bw()+
  scale_y_continuous(breaks=c(9:12), limits = c(8, 12))

pl_yld_instit

################################
#'## Combo 3
################################

pl_one <- function(var="Property Rights"){
  dat_mer %>% 
    filter(variable==var) %>% 
    ggplot(aes(x=value, y=avg_ln_yield))+
    geom_ribbon(aes(ymin=yhat_lo, ymax=yhat_hi), fill ="grey") +
    geom_point()+
    geom_line(aes(y=yhat)) +
    # geom_smooth(method="lm") +
    ylab("Average Log Yield (1984-2016)")+ xlab(var)+
    facet_grid(.~crop, switch="y") +
    theme_bw()+
    scale_y_continuous(breaks=c(9:12), limits = c(8, 12))+
    scale_x_continuous(breaks=seq(0.2, 1, by=0.2), limits = c(0.2, 1))
}
  
pl_propRight <- pl_one()
pl_demAccount <- pl_one("Democratic Accountability")
pl_antiDiv <- pl_one("Anti-Diversion")


pl_3 <- pl_propRight / pl_antiDiv / pl_demAccount
pl_3

################################
#'## Export data
################################


## save plots  
ggsave_paper(pl_yld_instit, filename = "figures/institutionsICRG_graph_combined_fromR_rKjWdLo.png", 
             height=gg_width)
ggsave_paper(pl_3, filename = "figures/institutionsICRG_graph_combined_fromR_combo_rKjWdLo.png",
             height=gg_width) 
