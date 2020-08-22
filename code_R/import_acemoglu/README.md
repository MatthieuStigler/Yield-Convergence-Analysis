---
title: "Data from Acemoglu 2019"
author: "Matthieu"
date: "8/22/2020"
output:
  html_document:
    keep_md: yes
---



# Data from Acemoglu 2019 JPE

Download steps (done on August 22): 

1. https://www.journals.uchicago.edu/doi/10.1086/700936
2. Supplemental Material
3. Download in `~/dataRaw/acemoglu_2019_JPE`
4. Uncompress

## Check democracy variables

Read data and get labels:


```r
library(haven)
library(tidyverse)
library(labelled)
library(knitr)

moglu_dat <- haven::read_dta("dataRaw/acemoglu_2019_JPE/2014340data/replication_files_ddcg/DDCGdata_final.dta")
# moglu_dat <- haven::read_dta("../../dataRaw/acemoglu_2019_JPE/2014340data/replication_files_ddcg/DDCGdata_final.dta")
```


Check variables:


```r
moglu_dat %>% 
  select(country_name, year, starts_with("dem")) %>% 
  labelled::var_label() %>% 
  map(~if_else(is.null(.), "No desc", .)) %>%
  as_tibble() %>% 
  gather(key = variable, value = description) %>% 
  knitr::kable()
```



|variable       |description                                                                      |
|:--------------|:--------------------------------------------------------------------------------|
|country_name   |Country name                                                                     |
|year           |Year (from 1960 to 2010)                                                         |
|demCGV         |Democracy measure by CGV                                                         |
|demBMR         |Democracy measure by BMR                                                         |
|dem            |Democracy measure by ANRR                                                        |
|demFH          |democracy measure based on Freedom House                                         |
|demPOL         |democracy measure based on Polity IV                                             |
|demPS          |democracy measure by PS                                                          |
|demPOL_xconst  |dummy for constraints on executive (based on polity)                             |
|demPOL_parcomp |dummy for competitiveness of participation (based on polity)                     |
|demPOL_exrec   |dummy for quality of executive recruitment process (based on Polity)             |
|demFH_pr       |Dummy for political rights (based on Freedom House)                              |
|demFH_cl       |Dummy for civil liberties (based on Freedom House)                               |
|demevent       |Event of democratization                                                         |
|democ          |Cummulative number of democratizations                                           |
|demext         |Democratic status at beginning of sample                                         |
|demreg         |Average democracy in the region*initial regime (leaving own country out)         |
|demreg60       |Average democracy in the region*initial regim (using regime in 1960, jackniffed) |
|demregDA       |Average democracy in the region*initial regim (using always democracy, jackniffe |
|demregREGIME   |Average democracy in the region*initial regime (detailed regimes, jackniffed)    |
|demt           |No desc                                                                          |

