library(haven)
library(tidyverse)
library(labelled)


################################
#'## Import data
################################

moglu_dat <- haven::read_dta("/home/matifou/Downloads/2014340data/replication_files_ddcg/DDCGdata_final.dta")

################################
#'## TITLE
################################

is.labelled(moglu_dat)

moglu_dat[,1] %>% map_chr(~attributes(.)$label)
var_label(moglu_dat) %>% 
  as_tibble()


count(moglu_dat, country_name)

## balanced? Yes!
moglu_dat %>% 
  add_count(country_name) %>% 
  count(n) 

## check democracy variables
moglu_dat %>% 
  select(country_name, year, starts_with("dem")) %>% 
  # select(-demt) %>%
  var_label() %>% 
  map(~if_else(is.null(.), "No desc", .)) %>%
  # discard()
  as_tibble() %>% 
  gather(key = variable, value = description)



