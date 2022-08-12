#' ---
#' Title: "Convergence analysis in R"
#' Author: "Matthieu"
#' Date: 2022-08-12
#' runMat: TRUE
#' ---

library(multiDiff) # devtools::install_github("MatthieuStigler/multiDiff")
library(broom)
library(lfe)
library(matPkg)
library(texreg)
library(plm)
library(haven)

################################
#'## Read data
################################


y_M <- read_csv("dataAnalysis/yield_convergence_data_Maize.csv")
y_R <- read_csv("dataAnalysis/yield_convergence_data_Rice.csv")
y_W <- read_csv("dataAnalysis/yield_convergence_data_Wheat.csv")

readxl::read_xlsx("dataRaw/World Bank Region Classifications.xlsx", skip=3)

FAO_2020 <- read_csv("data_intermediary/FAOSTAT_3crops_2020_rSeWhYs.csv")

yld_smooth <- read_csv("temp/yield_pred_3crops.csv")
yld_pot <- read_dta("data_intermediary/yld_potential_rJcYdFg.dta")

# reg yield_hat_growth1 l.ln_yield_hat ln_yield_pot i.year, vce(cluster country_grp)
# eststo ols_`com'
# xtreg yield_hat_growth1 l.ln_yield_hat i.year, fe vce(cluster country_grp)

################################
#'## Prepare data
################################


## yields all
Y_all <- rbind(y_M %>% 
                 mutate(crop="Maize") %>% 
                 select(contains(c("country", "yield")), year, crop),
               y_R %>% 
                 mutate(crop="Rice") %>% 
                 select(contains(c("country", "yield")), year, crop),
               y_W %>% 
                 mutate(crop="Wheat") %>% 
                 select(contains(c("country", "yield")), year, crop)) %>% 
  arrange(country, crop, year)

## is panel complete?
Y_all %>% 
  count(country, crop) %>% 
  count(n)

## what is country group? SAME!
Y_all %>% 
  distinct(country, country_grp) %>% 
  filter(country!=country_grp)

Y_all_c <- Y_all %>% 
  select(crop, contains(c("country")), year,
         yield_hat_growth1, ln_yield_hat, ln_yield_pot) %>% 
  group_by(countrycode, crop) %>% 
  mutate(l.ln_yield_hat = dplyr::lag(ln_yield_hat, order_by = year)) %>% 
  ungroup()

Y_all_c

## just 1
tidy(lm(yield_hat_growth1 ~l.ln_yield_hat  + ln_yield_pot+ year, data = Y_all_c, subset = crop=="Wheat"))

## nest
Y_all_c_nst <- Y_all_c %>% 
  nest(data = -crop)


# vce(cluster country_grp)

regs_all <- Y_all_c_nst %>% 
  mutate(reg_FE0 = map(data, ~felm(yield_hat_growth1 ~l.ln_yield_hat+ln_yield_pot|0|0|country_grp, data = .)),
         reg_FE1 = map(data, ~felm(yield_hat_growth1 ~l.ln_yield_hat+ln_yield_pot|year|0|country_grp, data = .)),
         reg_FE1_noYP = map(data, ~felm(yield_hat_growth1 ~l.ln_yield_hat|year|0|country_grp, data = .)),
         reg_FE2 = map(data, ~felm(yield_hat_growth1 ~l.ln_yield_hat|year+country|0|country_grp, data = .))
         # reg_FE_AB = map(data, ~pgmm(ln_yield_hat ~lag(ln_yield_hat)|lag(ln_yield_hat,2:4), index = c("country", "year"), data = .))
         ) %>% 
  select(-data) %>% 
  gather(reg_type, reg, starts_with("reg")) %>% 
  mutate(coef = map(reg, broom::tidy, conf.int=TRUE),
         mod_name = paste(crop, str_remove_all(reg_type, "reg_")) ,
         reg_type = fct_relevel(reg_type, c("reg_FE0", "reg_FE1", "reg_FE1_noYP"))) %>% 
  arrange(crop, reg_type)

regs_all

screenreg(regs_all$reg, custom.model.names = regs_all$mod_name,
          custom.coef.map = list(l.ln_yield_hat=NA, ln_yield_pot=NA), digits=3,
          include.rsquared = FALSE,
          include.adjrs = FALSE,
          stars = c(0.01, 0.05, 0.1))

texreg(regs_all$reg, custom.model.names = regs_all$mod_name,
       custom.coef.map = list(l.ln_yield_hat=NA, ln_yield_pot=NA), digits=3,
       include.rsquared = FALSE,
       include.adjrs = FALSE,
       stars = c(0.01, 0.05, 0.1),
       caption = "Smooth yields (as in paper)",
       caption.above = TRUE,
       file = "tables/convergence_many_models_rGwQmPo.tex",
       booktabs = TRUE, use.packages =FALSE)

mat_table_to_pdf(x="tables/convergence_many_models_rGwQmPo.tex", is_path_x = TRUE)
mat_pdf_to_png("tables/convergence_many_models_rGwQmPo.pdf", correct_grayscale = TRUE)

################################
#'## Run with other dataset
################################

FAO_2020_prep <- FAO_2020 %>% 
  filter(is_in_old) %>% 
  filter(element=="yield") %>% 
  rename(yield = value) %>% 
  select(-is_in_old, -element) %>% 
  mutate(crop = str_to_title(crop)) %>% 
  left_join(yld_pot, by = c("countrycode", "iso3", "crop", "country"))

FAO_2020_prep %>% filter(year<=2016)

yld_smooth_prep <- yld_smooth %>% 
  mutate(crop = str_to_title(crop)) %>% 
  left_join(yld_pot %>% 
              select(-country), by = c("countrycode", "iso3", "crop")) %>% 
  select(all_of(colnames(FAO_2020_prep)), yield_hat)

## compare
yld_smooth_prep %>% 
  full_join(FAO_2020_prep %>% 
              filter(year<=2016), by = c("countrycode", "iso3", "crop", "year", "country"),
            suffix = c("_old", "_new")) %>% 
  filter(yield_new!=yield_old)


## data is balanced!
FAO_2020_prep %>% 
  count(country, crop) %>% 
  count(n)

## add lags, etc
# data_here <- yld_smooth_prep 
# data_here <- yld_smooth_prep %>% 
#   select(-yield) %>% 
#   rename(yield=yield_hat)
data_here <-FAO_2020_prep


FAO_2020_prep_2 <- data_here%>% 
  group_by(countrycode, crop) %>% 
  mutate(ln_yield = log(yield),
         ln_yield_lag= dplyr::lag(ln_yield, order_by = year),
         ln_yield_diff = ln_yield-ln_yield_lag) %>% 
  ungroup() %>% 
  mutate(yield_hat_growth1 = ln_yield_diff,
         l.ln_yield_hat= ln_yield_lag,
         ln_yield_pot = log(yield_pot),
         country_grp = country)

FAO_2020_prep_2

## nest
FAO_2020_nst <- FAO_2020_prep_2 %>% 
  filter(year<=2016) %>%
  nest(data = -crop)

### run regs
regs_all_2000 <- FAO_2020_nst %>% 
  mutate(reg_FE0 = map(data, ~felm(yield_hat_growth1 ~l.ln_yield_hat+ln_yield_pot|0|0|country_grp, data = .)),
         reg_FE1 = map(data, ~felm(yield_hat_growth1 ~l.ln_yield_hat+ln_yield_pot|year|0|country_grp, data = .)),
         reg_FE1_noYP = map(data, ~felm(yield_hat_growth1 ~l.ln_yield_hat|year|0|country_grp, data = .)),
         reg_FE2 = map(data, ~felm(yield_hat_growth1 ~l.ln_yield_hat|year+country|0|country_grp, data = .))
         # reg_FE_AB = map(data, ~pgmm(ln_yield_hat ~lag(ln_yield_hat)|lag(ln_yield_hat,2:4), index = c("country", "year"), data = .))
  ) %>% 
  select(-data) %>% 
  gather(reg_type, reg, starts_with("reg")) %>% 
  mutate(coef = map(reg, broom::tidy, conf.int=TRUE),
         mod_name = paste(crop, str_remove_all(reg_type, "reg_")) ,
         reg_type = fct_relevel(reg_type, c("reg_FE0", "reg_FE1", "reg_FE1_noYP"))) %>% 
  arrange(crop, reg_type)



screenreg(regs_all_2000$reg, custom.model.names = regs_all_2000$mod_name,
          custom.coef.map = list(l.ln_yield_hat=NA, ln_yield_pot=NA), digits=3,
          include.rsquared = FALSE,
          include.adjrs = FALSE,
          stars = c(0.01, 0.05, 0.1))


texreg(regs_all_2000$reg, custom.model.names = regs_all_2000$mod_name,
       custom.coef.map = list(l.ln_yield_hat=NA, ln_yield_pot=NA), digits=3,
       include.rsquared = FALSE,
       include.adjrs = FALSE,
       stars = c(0.01, 0.05, 0.1),
       caption = "Raw yields (new FAO update, yet  before 2016)",
       caption.above = TRUE,
       file = "tables/convergence_many_models_raw_rGwQmPo.tex",
       booktabs = TRUE, use.packages =FALSE)

mat_table_to_pdf(x="tables/convergence_many_models_raw_rGwQmPo.tex", is_path_x = TRUE)
mat_pdf_to_png("tables/convergence_many_models_raw_rGwQmPo.pdf", correct_grayscale = TRUE)


################################
#'## Visu
################################

################################
#'## Export data
################################

#write_rds(..., "data_intermediary/")

## save plots  
# ggsave(..., height = gg_height, width = gg_width,
#        filename = "output/figures/xxx")
# rGwQmPo
