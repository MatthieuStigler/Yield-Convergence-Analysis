### Desc: Download area data for all commodities
### Author: Matthieu
### Main input(s): 
###        -download file from http://www.earthstat.org/data-download/
### Output: 
###        - unzip file, in external folder
###        - meta file: commodity conversion, in data_intermediary/Metadata_intermediary/conv_table_commods_WRI_ES.csv


library(raster)
library(maptools)
library(rgdal)
library(sp)

# source("codeSetup/8_functions_raster_to_df.R")
select <- dplyr:::select

#################################
###### Import data
#################################


OLD_downl_link <- "https://s3-us-west-2.amazonaws.com/earthstat/earthstat/wp-content/uploads/filebase/cropareayieldtop/cropareayield/allcropsareayield/HarvestedAreaYield175Crops_Geotiff.zip"
downl_link <- "https://s3.us-east-2.amazonaws.com/earthstatdata/HarvestedAreaYield175Crops_Geotiff.zip"

user <-Sys.info()["user"]
if(user=="matifou") {
  dest_file <- "~/pCloudDrive/Documents/Heavy_datasets/CropMap/HarvestedAreaYield175Crops_Geotiff.zip"
  dest_dir <- "~/pCloudDrive/Documents/Heavy_datasets/CropMap/unziped/ES_HarvestedArea_select_Crops_Geotiff"
} else {
  
}


### Download
if(!file.exists(dest_file)){
  download.file(downl_link, destfile = dest_file)
}

## list files 
files_li <- unzip(dest_file, list=TRUE) %>% as_tibble %>%
  mutate(levels=str_count(Name, "/")) %>%
  select(levels, Name, everything())

files_li

## Cleaner
files_li_c <- files_li %>%
  separate(Name, into=paste("Level", 1:max(.$levels), sep="_"), sep="/", remove = FALSE, fill ="warn") %>%
  mutate(Commod_ES_name=str_replace(Level_2, "_HarvAreaYield2000_Geotiff", ""),
         is_TIF=str_detect(Name, "\\.tif$"),
         Variable=ifelse(is_TIF, str_replace_all(Level_3, "^[a-z]+_|\\.tif", ""), NA),
         path=paste(dest_dir, Name, sep="/"),
         Date = as.Date(Date)) %>%
  filter(Commod_ES_name!="") %>%
  filter(!str_detect(Commod_ES_name, "^_METADATA")) %>%
  select(Commod_ES_name, Name, everything())

## select ones of interest (I assume here Geotiff is relevant folder!?)
crops_keep <-  c("maize", "rice", "wheat", "soybean")
files_keep <- files_li_c %>% 
  filter(Level_2=="GeoTiff") %>% 
  filter(Level_3 %in% crops_keep) %>% 
  mutate(Variable = str_remove(Level_4, paste(crops_keep, collapse = "|")) %>% 
           str_remove_all("^_|\\.tif(.+)?"),
         crop= Level_3) %>% 
  relocate(crop, Variable)

files_keep

files_keep %>% 
  count(Variable)

## Selec var
files_keep_vars <- files_keep %>% 
  filter(Variable%in% c("HarvestedAreaFraction", "HarvestedAreaHectares")) %>%
  filter(is_TIF) 

files_keep_vars


#################################
###### Unzip Area
#################################

## Unzip selected ones
files_li_TIF_area <- files_keep_vars %>%
  select(Commod_ES_name, Variable, everything()) %>%
  mutate(Data=map(Name, ~unzip(dest_file, files=., exdir=dest_dir)))

## move files
files_unzip <- mat_list_dir(dest_dir, pattern = "\\.tif") %>% 
  mutate(dest_path_local =paste0("/home/matifou/pCloudDrive/Documents/Heavy_datasets/CropMap/unziped/", filename),
         dest_path_proj =paste0("dataRaw/EarthScan_cropmap/", filename))

## move local
walk2(files_unzip$full_path, files_unzip$dest_path_local, ~fs::file_move(.x, .y))

## copy to project
walk2(files_unzip$dest_path_local, files_unzip$dest_path_proj, ~fs::file_copy(.x, .y))

  
#################################
###### Export data: list
#################################
write_csv(commod_list_corresp, "data_intermediary/Metadata_intermediary/conv_table_commods_WRI_ES_dAES.csv")

if(FALSE){
  export_col_specs("./data_intermediary/Metadata_intermediary/conv_table_commods_WRI_ES_dAES.csv")
}
