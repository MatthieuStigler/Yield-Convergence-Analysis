#' ---
#' Title: "Growth scatter plots"
#' Author: "Matthieu"
#' Date: 2020-06-10
#' runMat: TRUE
#' ---

remotes::install_github("rensa/ggflags")
devtools::install_github("slowkow/ggrepel")

library(matPkg)
library(ggrepel)
library(haven)
library(patchwork)
library(ggflags)
source("code_R/888_misc_functions.R")

################################
#'## Read data
################################

yld_smooth <- read_csv("temp/yield_pred_3crops.csv")
FAO_codes <- read_csv("dataRaw/FAOSTAT_countrycodes.csv") 

yld_pot <- read_dta("data_intermediary/yld_potential_rJcYdFg.dta")

################################
#'## Prepare data
################################

# clean codes
FAO_codes_c <- FAO_codes %>% 
  rename_all(~str_replace_all(., " ", "_"))%>% 
  rename(iso2=`ISO2_Code`,
         iso3_check=`ISO3_Code`,
         countrycode=Country_Code) %>%  
  mutate(iso2=str_to_lower(iso2))

## clean ylds
yld_smooth_c <- yld_smooth %>% 
  select(-production, -country_grp, -prop_global_area,
         -area_hat, -area) %>% 
  prj_clean_crop_names() %>% 
  left_join(FAO_codes_c %>% 
              select(countrycode, starts_with("iso")), by = "countrycode")

yld_smooth_c %>%  
  filter(iso3!= iso3_check) %>% 
  mat_check_0row()




## prep
yld_smooth_prep <- yld_smooth_c %>% 
  mutate(yield_log=log(yield_hat)) %>% 
  arrange(crop, iso3, year) %>% 
  group_by(crop, iso3) %>% 
  mutate(yield_log_lag=dplyr::lag(yield_log),
         yield_lag=dplyr::lag(yield_hat),
         growth=100*(yield_log-yield_log_lag),
         perc=100*(yield_hat-yield_lag)/yield_lag) %>% 
  ungroup()

yld_smooth_prep


## 1961-1990
yld_smooth_sub1 <- yld_smooth_prep %>% 
  filter(year %in% c(1961:1990)) %>%
  group_by(crop, iso3, iso2) %>% 
  summarise(initial=yield_log[year==1961],
            change_mean=mean(growth, na.rm=TRUE),
            change_perc=mean(perc, na.rm=TRUE),
            change_range=100*(yield_log[year==1990]-yield_log[year==1961]),
            change_range_abs=100*(yield_hat[year==1990]-yield_hat[year==1961])/yield_hat[year==1961]) %>% 
  ungroup()

yld_smooth_sub1

yld_smooth_sub1 %>%  
  mutate(ratio=change_mean/change_range,
         new= (1+change_mean/100)^30)

## 1981-2010
yld_smooth_sub2 <- yld_smooth_prep %>% 
  filter(year %in% c(1981:2010)) %>%
  group_by(crop, iso3, iso2) %>% 
  summarise(initial=yield_log[year==1981],
            change_mean=mean(growth, na.rm=TRUE),
            change_perc=mean(perc, na.rm=TRUE),
            change_range=100*(yield_log[year==2010]-yield_log[year==1981]),
            change_range_abs=100*(yield_hat[year==2010]-yield_hat[year==1981])/yield_hat[year==1981]) %>% 
  ungroup()

yld_smooth_sub2

## combine
yld_smooth_sub12 <- yld_smooth_sub1 %>%  
  mutate(period= "1961-1980") %>% 
  rbind(yld_smooth_sub2 %>%  
          mutate(period= "1981-2010"))

################################
#'## Regressions
################################

regs_df <- yld_smooth_sub12 %>% 
  nest(data=-c(crop, period)) %>% 
  mutate(reg=map(data, ~lm(change_range~initial, data=.)),
         coefs=map(reg, broom::tidy),
         r2=map_dbl(reg, ~summary(.)$r.squared)) %>% 
  select(-data)  %>% 
  arrange(crop)

regs_df

## Table
regs_li <- regs_df$reg
names(regs_li) <- paste(regs_df$crop, regs_df$period)
tab_tex <- stargazer::stargazer(regs_li, column.labels=names(regs_li))
  

coefs_df <- regs_df %>% 
  select(-reg) %>% 
  unnest(c(coefs, r2)) %>% 
  filter(term=="initial")
  
coefs_df

r2s_df <- regs_df %>% 
  select(-reg, -coefs) %>% 
  unnest(r2)

r2s_df

################################
#'## Reg with potential yields
################################

regs_yp_df <- yld_smooth_sub12 %>% 
  left_join(yld_pot %>% 
              select(iso3, crop, yield_pot), by = c("iso3", "crop")) %>% 
  nest(data=-c(crop, period)) %>% 
  mutate(reg=map(data, ~lm(change_range~initial + yield_pot, data=.)),
         coefs=map(reg, broom::tidy),
         r2=map_dbl(reg, ~summary(.)$r.squared)) %>% 
  arrange(crop)

regs_yp_df


##
coefs_yp_df <- regs_yp_df %>% 
  select(-reg, -data) %>% 
  unnest(c(coefs, r2)) %>% 
  filter(term=="initial")

coefs_yp_df

## predictions
preds_yp <- regs_yp_df %>% 
  mutate(preds = map2(reg, data, ~mutate(.y, pred=predict(.x, newdata = mutate(.y, yield_pot=mean(yield_pot, na.rm=TRUE))))))

preds_yp$data[[1]]

preds_yp_df <- preds_yp %>% 
  select(crop, period, preds) %>%  
  unnest(preds) %>% 
  select(crop, period, iso3, initial, pred)
preds_yp_df

## enhance table
regs_all <- bind_rows(regs_df, regs_yp_df) %>% 
  select(crop, period, reg) %>% 
  mutate(type= rep(c("no cov", "cov"), each = 6)) %>% 
  arrange(crop, period) %>% 
  mutate(name= paste(crop, period))
regs_li_all <- regs_all$reg
tab_all_tex <- stargazer::stargazer(regs_li_all, omit.stat=c("f", "ser"), 
                                    column.labels=regs_all$name, report=('vc*p'))


################################
#'## Visu
################################

yld_smooth_sub12 %>% 
  # group_by(period) %>%  
  summarise(across(c(change_range, initial), range))


pl_per1_rawissimo <- yld_smooth_sub1 %>% 
  ggplot(aes(x=initial, y=change_range))+
  geom_hline(yintercept = 0, linetype=2)+
  facet_wrap(crop~.) +
  xlab("Log yield 1961") + ylab("Yield Growth 1961-1990") +
  theme_bw() +
  scale_x_continuous(breaks=9:11, limits = c(8.15,11.2))+
  scale_y_continuous(breaks=seq(-1, 1.5, by=0.5)*100, limits=c(-95, 160),
                     labels = function(x) paste0(x, "%"))

pl_per1_raw <- pl_per1_rawissimo+
  geom_text_repel(aes(label=iso3), size=1.5,
                  force=0.3,
                  force_pull=5,
                  segment.size=0.2,
                  segment.alpha=0.8)

pl_per1_point <- pl_per1_raw+
  geom_point(size=1.3)

pl_per1_point_only <- pl_per1_rawissimo+
  geom_point(size=1.3)

pl_per1_flag <- pl_per1_raw+
  geom_flag(aes(country=iso2), size=1.8)

pl_per1_flag_only <- pl_per1_rawissimo+
  geom_flag(aes(country=iso2), size=2)

## per2
pl_per2_point <- pl_per1_point %+% yld_smooth_sub2+
  xlab("Log yield 1981") + ylab("Yield Growth 1981-2010") 
pl_per2_flag <- pl_per1_flag %+% yld_smooth_sub2+
  xlab("Log yield 1981") + ylab("Yield Growth 1981-2010") 
pl_per2_point_only <- pl_per1_point_only %+% yld_smooth_sub2+
  xlab("Log yield 1981") + ylab("Yield Growth 1981-2010")
pl_per2_flag_only <- pl_per1_flag_only %+% yld_smooth_sub2+
  xlab("Log yield 1981") + ylab("Yield Growth 1981-2010")

pl_per2_point
pl_per2_flag

################################
#'## Add reg stats
################################

# coefs_df_char <- coefs_df %>% 
#   mutate(r2_text=expression(paste(R^{2}, ":", round(r2,2))))
coefs_yp_df %>% 
  mutate(p_val_char = format.pval(p.value,1))


add_stats1 <- function(plot, per="1961-1980") {
  plot +
    geom_text(x=10.5, y=-50, size=3, aes(label=paste("R2:", round(r2,2))),
              data=coefs_df %>% 
                filter(period==per)) +
    geom_text(x=10.5, y=-80, size=3, aes(label=paste("pval:", round(p.value,2))),
                          data=coefs_df %>% 
                            filter(period==per))
}

add_stats_yp <- function(plot, per="1961-1980") {
  plot +
    geom_text(x=10.5, y=-50, size=3, aes(label=paste("R2:", round(r2,2))),
              data=coefs_yp_df %>% 
                filter(period==per)) +
    geom_text(x=10.5, y=-80, size=3, aes(label=paste("pval:", format.pval(p.value,1))),
              data=coefs_yp_df %>% 
                filter(period==per))
}

pl_per1_point_stats <- add_stats1(pl_per1_point) 
pl_per2_point_stats <- add_stats1(pl_per2_point, per = "1981-2010") 

pl_per1_point_only_stats <- add_stats1(pl_per1_point_only) 
pl_per2_point_only_stats <- add_stats1(pl_per2_point_only, per = "1981-2010") 

## with yp
pl_per1_point_stats_yp <- add_stats_yp(pl_per1_point) 
pl_per2_point_stats_yp <- add_stats_yp(pl_per2_point, per = "1981-2010") 

pl_per1_point_only_stats_yp <- add_stats_yp(pl_per1_point_only) 
pl_per2_point_only_stats_yp <- add_stats_yp(pl_per2_point_only, per = "1981-2010") 


################################
#'## Average growth
################################

yld_smooth_sub12 %>% 
  mutate(change_mean=100*change_mean,
         change_range=100*(change_range)) %>% 
  ggplot(aes(x=change_range, y=change_mean))+
  geom_point()+
  facet_wrap(crop~.)+
  mat_gg_abline_01

################################
#'## Combine
################################

rem_strip <- theme(strip.background = element_blank(),
                   strip.text.x = element_blank())


## points
pl_point_combo <- pl_per1_point / (pl_per2_point + rem_strip)
pl_point_only_combo <- pl_per1_point_only / (pl_per2_point_only + rem_strip)
pl_flag_combo <- pl_per1_flag / (pl_per2_flag + rem_strip)
pl_flag_combo_only <- pl_per1_flag_only / (pl_per2_flag_only + rem_strip)
pl_point_stats_combo <- pl_per1_point_stats / (pl_per2_point_stats + rem_strip)

pl_flag_combo_only



################################
#'## add regression lines
################################


pl_point_only_combo_smooth <- (pl_per1_point_only + mat_geom_lm(color="grey")) / (pl_per2_point_only+ mat_geom_lm(color="grey") + rem_strip)
pl_point_only_combo_smooth 

##
pl_point_stats_smooth_combo <-  (pl_per1_point_stats + mat_geom_lm(color="grey")) / (pl_per2_point_stats+ mat_geom_lm(color="grey") + rem_strip)
pl_point_only_stats_smooth_combo <-  (pl_per1_point_only_stats + mat_geom_lm(color="grey")) / (pl_per2_point_only_stats+ mat_geom_lm(color="grey") + rem_strip)

## add lines with yp
pl_point_only_stats_smooth_combo_yp <- (pl_per1_point_only_stats_yp +
                                          geom_line(aes(y=pred), data=filter(preds_yp_df, period=="1961-1980"), color="grey", size=1, alpha=0.8))/
  (pl_per2_point_only_stats_yp +
     geom_line(aes(y=pred), data=filter(preds_yp_df, period=="1981-2010"), color="grey", size=1, alpha=0.8))
  
pl_point_only_stats_smooth_combo_yp

################################
#'## Combine 2 wrap
################################

yld_smooth_sub12 %>% 
  ggplot(aes(x=initial, y=change_range))+
  geom_hline(yintercept = 0, linetype=2)+
  geom_text_repel(aes(label=iso3))+
  facet_grid(period~crop) +
  xlab("Log yield 1961") + ylab("Yield Growth 1961-1990") +
  theme_bw()


################################
#'## Export data
################################

ggsave_paper(pl_point_combo, "figures/scatter_convergence_initial/scatter_growth_points_rYuClGn.png")
ggsave_paper(pl_point_only_combo, "figures/scatter_convergence_initial/scatter_growth_points_only_rYuClGn.png")
ggsave_paper(pl_point_only_combo_smooth, "figures/scatter_convergence_initial/scatter_growth_points_only_regLine_rYuClGn.png")
ggsave_paper(pl_flag_combo, "figures/scatter_convergence_initial/scatter_growth_flags_rYuClGn.png")
ggsave_paper(pl_flag_combo_only, "figures/scatter_convergence_initial/scatter_growth_flags_only_rYuClGn.png")

ggsave_paper(pl_point_stats_smooth_combo, "figures/scatter_convergence_initial/scatter_growth_points_regLine_stats_rYuClGn.png")
ggsave_paper(pl_point_only_stats_smooth_combo, "figures/scatter_convergence_initial/scatter_growth_points_only_regLine_stats_rYuClGn.png")

## yp
ggsave_paper(pl_point_only_stats_smooth_combo_yp, "figures/scatter_convergence_initial/scatter_growth_points_only_regLine_yp_stats_rYuClGn.png")


mat_table_to_pdf(tab_tex, filename = "tables/cross_section_initial.pdf",
                 is_path_x = FALSE)
mat_table_to_pdf(tab_all_tex, filename = "tables/cross_section_initial_yp.pdf",
                 is_path_x = FALSE)

