#' ---
#' Title: "Plot convergence"
#' Author: "Matthieu"
#' Date: 2020-06-05
#' runMat: TRUE
#' ---

library(matPkg)

################################
#'## Read data
################################

files <- mat_list_dir("temp/", pattern = "conv_results.*\\.csv") %>% 
  mutate(crop=str_extract(filename, "Maize|Rice|Wheat"))

data_conv <- files %>% 
  mutate(data=map(full_path, read_csv)) %>% 
  select(crop, data) %>% 
  unnest(data)

################################
#'## Prepare data
################################

data_conv

################################
#'## Visu
################################

pl_conv_wrap <- data_conv %>% 
  ggplot(aes(x=year, y=beta))+
  geom_ribbon(aes(ymin=low, ymax=high), fill="grey70") +
  geom_line()+
  facet_wrap(crop~., nrow=3)+
  geom_hline(yintercept=0, linetype=2) +
  xlab("Year")+ylab("Convergence coefficient") +
  theme_bw()

pl_conv_wrap  

################################
#'## Export data
################################


## save plots  
ggsave(pl_conv_wrap, height = gg_height, width = gg_width,
       filename = "figures/convergence_coefficient_3Crops_wrap_rFdKhWs.png")
ggsave(pl_conv_wrap, height = gg_height*1.2, width = gg_width,
       filename = "figures/convergence_coefficient_3Crops_wrap_taller_rFdKhWs.png")
ggsave(pl_conv_wrap, height = gg_height*1.4, width = gg_width,
       filename = "figures/convergence_coefficient_3Crops_wrap_tallest_rFdKhWs.png")
