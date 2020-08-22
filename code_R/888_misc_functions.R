ggsave_paper <- function(x, filename, height = gg_height, width = gg_width){
  x_clean <- x+
    theme_bw()
  ggsave(plot=x_clean, filename =filename, width =  width, height = height)
}

prj_clean_crop_names <- function(df){
  df %>% 
    mutate(crop=str_to_title(crop))
}
