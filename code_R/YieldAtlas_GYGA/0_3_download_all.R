#' ---
#' Title: ""
#' Author: "Matthieu"
#' Date: 2022-08-04
#' runMat: TRUE
#' ---

library(httr)
library(rvest)
library(matPkg)

################################
#'## Read data
################################

## Test 1
URL <- "https://www.yieldgap.org/sub-saharan-africa"
pg <- read_html(URL)

### process
read_links <- function(html_out){
  
  enframe(html_attr(html_nodes(html_out, "a"), "href"), value="url", name=NULL) %>% 
    filter(str_detect(url, "gygaserver")) %>% 
    mutate(crop = str_extract(url, "Maize|Rice|Wheat"),
           type = str_extract(url, "Rainfed|Irrigated"))
  
}


read_links(html_out=pg)


## Do ALL
continents <- c("asia", "sub-saharan-africa", "north-america", "south-america", "europe", "middle-east-north-africa", "australia")

## downlaod all
pages_dwnld <- tibble(continents, link = paste0("https://www.yieldgap.org/", continents)) %>% 
  mutate(html_dat = map(link, read_html))

## process all
pages_dwnld_c <- pages_dwnld %>% 
  mutate(html_dat = map(html_dat, read_links)) %>% 
  unnest(html_dat) %>% 
  mutate(url_last = basename(url),
         keep = !is.na(crop)| str_detect(url_last, "GygaAustralia"))

## Check the ones ignored
pages_dwnld_c %>% 
  filter(!keep) %>% 
  pull(url_last)

## keep
pages_dwnld_keep <- pages_dwnld_c %>% 
  filter(keep)

pages_dwnld_keep %>% 
  pull(url)

walk(pages_dwnld_keep$url, ~system(paste("firefox", .)))

# https://www.yieldgap.org/gygaserver/download?downloadurl=/gygamaps/excel/GygaRainfedMaizeLatinAmerica.xlsx
# http://www.yieldgap.org/gygaserver/download?downloadurl=/gygamaps/excel/GygaRainfedWheatLatinAmerica.xlsx
# http://www.yieldgap.org/gygaserver/download?downloadurl=/gygamaps/excel/GygaRainfedRiceLatinAmerica.xlsx
# http://www.yieldgap.org/gygaserver/download?downloadurl=/gygamaps/excel/GygaIrrigatedRiceLatinAmerica.xlsx
# 
# http://www.yieldgap.org/gygaserver/download?downloadurl=/gygamaps/excel/GygaRainfedMaizeSouthAsia.xlsx
# http://www.yieldgap.org/gygaserver/download?downloadurl=/gygamaps/excel/GygaRainfedMaizeSubSaharanAfrica.xlsx
# https://www.yieldgap.org/gygaserver/download?downloadurl=/gygamaps/excel/GygaRainfedMaizeNorthAmerica.xlsx
# http://www.yieldgap.org/gygaserver/download?downloadurl=/gygamaps/excel/GygaRainfedMaizeEurope.xlsx
# http://www.yieldgap.org/gygaserver/download?downloadurl=/gygamaps/excel/GygaRainfedWheatMiddleEastNorthAfrica.xlsx
# 
# http://www.yieldgap.org/gygaserver/download?downloadurl=/gygamaps/excel/GygaAustralia.xlsx


################################
#'## Compare downloaded
################################



files_dnwld <- mat_list_dir("dataRaw/yield_potential_GYGA/", pattern = "xls") %>% 
  mutate(crop = str_extract(filename, "Maize|Rice|Wheat"),
         type = str_extract(filename, "Rainfed|Irrigated"),
         cont = str_remove_all(filename, "Gyga(Irrigated|Rainfed)(Maize|Wheat|Rice)|\\.xlsx")) 

files_dnwld 

## compare cont
files_dnwld %>% 
  count(cont) %>% 
  pull(cont) %>% 
  sort()

pages_dwnld_keep %>% 
  count(continents) %>% 
  pull(continents) %>% 
  sort()




equiv <- tibble(cont = c("Europe", "LatinAmerica", "MiddleEastNorthAfrica", "NorthAmerica", 
                "SouthAsia", "SubSaharanAfrica", "GygaAustralia"),
       continents = c("europe", "south-america", "middle-east-north-africa", 
                      "north-america", "asia", "sub-saharan-africa", "australia"))

## merge with what wanted
files_check <- files_dnwld %>% 
  left_join(equiv, by = "cont") %>% 
  full_join(pages_dwnld_keep %>% 
              select(crop, type, continents), by = c("crop", "type", "continents")) 


files_check%>% 
  filter(is.na(filename))


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