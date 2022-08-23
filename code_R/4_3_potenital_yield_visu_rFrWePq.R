#' ---
#' Title: "Analyse yield potential"
#' Author: "Matthieu"
#' Date: 2022-08-02
#' runMat: TRUE
#' ---

library(haven)
library(tidyverse)
library(multimode)
library(diptest)
library(matPkg)

################################
#'## Read data
################################

yld_pot <- haven::read_dta("data_intermediary/yld_potential_rJcYdFg.dta")

################################
#'## Prepare data
################################

## check min?
min(yld_pot$yield_pot, na.rm=TRUE)
sum(yld_pot$yield_pot<0, na.rm=TRUE)

## compute bi-modality, copy from 4_2_bimodality_sigma_convergence_rGtWeBm
dip.test_tidy <- function(x, B=500) {
  
  ## run 
  methods <- c("HH", "CH", "ACR", "FM", "SI", "HY") ## all
  # methods <- c("HH", "CH", "FM") ## FM very slow!
  # methods <- c("HH", "CH")
  l <- lapply(methods,  \(m) multimode::modetest(x, B=B, method = m))
  out <- diptest::dip.test(x)
  
  ## arrange
  map_dfr(l, ~tibble(method = .$method, statistic =.$statistic, p.value = .$p.value )) %>% 
    bind_rows(tibble(method = "diptest", statistic = out$statistic, p.value = out$p.value))
  
}

ypot_bimod <- yld_pot %>% 
  filter(!is.na(yield_pot)) %>% 
  group_by(crop) %>% 
  summarise(dip.test_tidy(yield_pot)) %>% 
  mutate(test_type = str_extract(method, "(?<=\\) ).+"),
         test_paper = paste0(str_extract(method, ".+(?=\\))"), ")"))

ypot_bimod



################################
#'## Visu
################################

## Yield potential: density
pl_YP_density <- yld_pot %>% 
  ggplot(aes(x=yield_pot, color = crop))+
  geom_density(trim=TRUE) +
  ggtitle("Yield potential: density")

pl_YP_density

## bi modality tests
YP_bimod_tests <- ypot_bimod %>%
  filter(method!="diptest") %>% 
  ggplot(aes(y = test_paper, x=p.value, color = crop))+
  geom_point() +
  geom_vline(xintercept = c(0.05, 0.1), linetype=2)+
  # facet_grid(test_type~., scales="free", switch="both")+
  facet_wrap(test_type~., ncol=1, scales="free_y")+
  mat_gg_legend_bottom+
  ylab(NULL)+
  ggtitle("Yield potential: bimodality tests (p-values)")

YP_bimod_tests

################################
#'## Export data
################################

#write_rds(..., "data_intermediary/")

## save plots  
ggsave(pl_YP_density, height = 5, width = 8,
       filename = "figures/sigma_convergence/yield_potential_density_rFrWePq.png")
ggsave(YP_bimod_tests, height = 5, width = 8,
       filename = "figures/sigma_convergence/yield_potential_bimodality_tests_rFrWePq.png")




