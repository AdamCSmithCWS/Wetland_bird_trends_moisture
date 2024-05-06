setwd("C:/GitHub/Wetland_bird_trends_moisture")
# summarise base and cov fit ----------------------------------------------
library(bbsBayes2)
library(tidyverse)
library(patchwork)

BLTE_gen = 5.682 # generation time for Black Tern - Bird et al. 2020
BLTE_3Gen = round(BLTE_gen*3) # Three generations to calculate COSEWIC and IUCN trend thresholds
yr_pairs <- data.frame(sy = c(1970,1995,1970),
                       ey = c(1995,2022,2022))


#strata_sel <- readRDS("output/custom_latlong_bcr_stratification.rds")
inds_save <- NULL
trends_save <- NULL
cov_spei_lag_out <- NULL
cov_spei_out <- NULL
cov_nao_out <- NULL
inds_out <- NULL


for(model in c("gamye")){
# load the fitted models --------------------------------------------------
for(j in 3){#nrow(yr_pairs)){
  ey <-yr_pairs[j,"ey"]
  sy <- yr_pairs[j,"sy"]

if(!file.exists(paste0("output/",model,"_",sy,"_",ey,"_base.rds"))){next}

fit <- readRDS(paste0("output/",model,"_",sy,"_",ey,"_base.rds")) # read in the base model fit
fit_cov <- readRDS(paste0("output/",model,"_",sy,"_",ey,"_2covariate_varying.rds")) # read in the covariate model fit
fit_cov_lag <- readRDS(paste0("output/",model,"_",sy,"_",ey,"_3covariate_varying.rds")) # read in the covariate model fit

# summ <- readRDS(paste("summary",model,sy,ey,"2covariate_varying_lag.rds",
#                       sep = "_"))
#
# cov_eff <- summ %>%
#   filter(grepl("BETA",variable))
# # summ <- readRDS("output/convergence_parameter_summaries.rds")
# # summ %>% filter(variable == "beta_cov")

# trajectories trends and maps --------------------------------------------
if(model == "gamye"){

  inds_cov <- generate_indices(fit_cov,alternate_n = "n_smooth")
  inds_cov_out <- inds_cov$indices %>%
    mutate(model = "covariates",
           base_model = model,
           type = "smooth")

  inds_covalt <- generate_indices(fit_cov,alternate_n = "n")
  inds_cov_out2 <- inds_covalt$indices %>%
    mutate(model = "covariates",
           base_model = model,
           type = "full")
  inds_cov_out <- bind_rows(inds_cov_out,
                            inds_cov_out2)
  inds_cov_rand <- generate_indices(fit_cov,alternate_n = "n_random")
  inds_cov_rand_out <- inds_cov_rand$indices %>%
    mutate(model = "covariates",
           base_model = model,
           type = "no_covariates")
  inds_cov_out <- bind_rows(inds_cov_out,
                            inds_cov_rand_out)


  inds_out <- bind_rows(inds_out,inds_cov_out)
# 3 covariate -------------------------------------------------------------



  inds_cov_lag <- generate_indices(fit_cov_lag,alternate_n = "n_smooth")
  inds_cov_lag_out <- inds_cov_lag$indices %>%
    mutate(model = "covariates_lag",
           base_model = model,
           type = "smooth")

  inds_covalt_lag <- generate_indices(fit_cov_lag,alternate_n = "n")
  inds_cov_lag_out2 <- inds_covalt_lag$indices %>%
    mutate(model = "covariates_lag",
           base_model = model,
           type = "full")
  inds_cov_lag_out <- bind_rows(inds_cov_lag_out,
                                inds_cov_lag_out2)
  inds_cov_lag_rand <- generate_indices(fit_cov_lag,alternate_n = "n_random")
  inds_cov_lag_rand_out <- inds_cov_lag_rand$indices %>%
    mutate(model = "covariates_lag",
           base_model = model,
           type = "no_covariates")
  inds_cov_lag_out <- bind_rows(inds_cov_lag_out,
                            inds_cov_lag_rand_out)


  inds_out <- bind_rows(inds_out,inds_cov_lag_out)

# base model --------------------------------------------------------------


   inds <- generate_indices(fit,alternate_n = "n_smooth")

  inds_outt <- inds$indices %>%
    mutate(model = "base",
           base_model = model,
           type = "smooth")

  inds_out <- bind_rows(inds_out,
                        inds_outt)

  indsf <- generate_indices(fit)

  inds_outf <- indsf$indices %>%
    mutate(model = "base",
           base_model = model,
           type = "full")

  inds_out <- bind_rows(inds_out,
                        inds_outf)

}else{
  inds_cov <- generate_indices(fit_cov,alternate_n = "n_random")
  inds_cov_out <- inds_cov$indices %>%
    mutate(model = "covariates",
           base_model = model,
           type = "no_covariate")

  inds_covalt <- generate_indices(fit_cov,alternate_n = "n")
  inds_cov_out2 <- inds_covalt$indices %>%
    mutate(model = "covariates",
           base_model = model,
           type = "full")
  inds_cov_out <- bind_rows(inds_cov_out,
                            inds_cov_out2)
  inds <- generate_indices(fit)#,alternate_n = "n_smooth")

  inds_out <- inds$indices %>%
    mutate(model = "base",
           base_model = model,
           type = "full")

  inds_out <- bind_rows(inds_out,
                        inds_cov_out)

}




#inds_out <- readRDS("data/temp_inds_out.rds")
inds_plot_cont <- inds_out %>%
  filter(region == "continent",
         type != "smooth")

traj_t <- ggplot(data = inds_plot_cont,
                 aes(x = year,y = index))+
  geom_ribbon(aes(x = year,y = index,
                  ymin = index_q_0.05,
                  ymax = index_q_0.95,
                  fill = type),
              alpha = 0.25)+
  geom_line(aes(colour = type))+
  scale_colour_viridis_d(aesthetics = c("fill","colour"))+
  facet_wrap(vars(model))

if(model == "gamye"){
  inds_plot_cont <- inds_out %>%
    filter(region == "continent",
           type == "smooth")
trajsmooth <- ggplot(data = inds_plot_cont,
                     aes(x = year,y = index))+
  geom_ribbon(aes(x = year,y = index,
                  ymin = index_q_0.05,
                  ymax = index_q_0.95,
                  fill = model),
              alpha = 0.25)+
  geom_line(aes(colour = model))+
  scale_colour_viridis_d(aesthetics = c("fill","colour"))

traj_tfinal <- traj_t / trajsmooth
}else{
  traj_tfinal <- traj_t
}
pdf(paste0("Figures/trajectories_",model,"_",sy,"_",ey,".pdf"),
    width = 11,
    height = 8.5)
traj_tfinal
dev.off()





# Trends estimates --------------------------------------------------------


#inds_save <- bind_rows(inds_save,inds_out)
#inds <- generate_indices(fit,alternate_n = "n_smooth")
trends_cov <- generate_trends(inds_cov, min_year = sy,
                              prob_decrease = c(0,30,50))

trends_out <- trends_cov$trends %>%
  mutate(type = "long-term",
         base_model = model,
         model = "covariate")


# trends_cov$trends[1,c("percent_change","percent_change_q_0.05","percent_change_q_0.95")]
# trends_cov$trends[1,c("trend","width_of_95_percent_credible_interval")]
#
# trajs_cov <- plot_indices(inds_cov)
# print(trajs_cov[[1]])


trends_cov_3gen <- generate_trends(inds_cov, min_year = ey-BLTE_3Gen,
                                   prob_decrease = c(0,30,50))

trendt <- trends_cov_3gen$trends %>%
  mutate(type = "three-generation",
         base_model = model,
         model = "covariate")

trends_out <- bind_rows(trends_out,trendt)




trends_cov_lag <- generate_trends(inds_cov_lag, min_year = sy,
                              prob_decrease = c(0,30,50))

trendt <- trends_cov_lag$trends %>%
  mutate(type = "long-term",
         base_model = model,
         model = "covariates_lag")

trends_out <- bind_rows(trends_out,trendt)

trends_cov_lag_3gen <- generate_trends(inds_cov_lag, min_year = ey-BLTE_3Gen,
                                   prob_decrease = c(0,30,50))
trendt <- trends_cov_lag_3gen$trends %>%
  mutate(type = "three-generation",
         base_model = model,
         model = "covariates_lag")

trends_out <- bind_rows(trends_out,trendt)





trends <- generate_trends(inds, min_year = sy,
                          prob_decrease = c(0,30,50))
trendst <- trends$trends %>%
  mutate(type = "long-term",
         base_model = model,
         model = "base")
trends_out <- bind_rows(trends_out,trendst)

# trends$trends[1,c("percent_change","percent_change_q_0.05","percent_change_q_0.95")]
# trends$trends[1,c("trend","width_of_95_percent_credible_interval")]
#
# trajs <- plot_indices(inds, add_observed_means = TRUE, add_number_routes = TRUE)
# print(trajs[[1]])


trends_3gen <- generate_trends(inds, min_year = ey-BLTE_3Gen,
                               prob_decrease = c(0,30,50))

trendst <- trends$trends %>%
  mutate(type = "three-generation",
         base_model = model,
         model = "base")
trends_out <- bind_rows(trends_out,trendst)



trends_save <- bind_rows(trends_save,trends_out)

# Trend maps --------------------------------------------------------------
#
# map_cov_long <- plot_map(trends,
#                          strata_custom = strata_sel)
# map_cov_3gen <- plot_map(trends_cov_3gen,
#                          strata_custom = strata_sel)
# map_long <- plot_map(trends,
#                      strata_custom = strata_sel)
# map_3gen <- plot_map(trends_3gen,
#                      strata_custom = strata_sel)

# load original data
ps <- readRDS(paste0("data/prepared_data_",sy,"-",ey,".rds"))

base_map <- load_map(ps$meta_data$stratify_by) %>%
  select(-area_sq_km) %>%
  inner_join(ps$meta_strata,
             by = "strata_name")

bbox <- sf::st_bbox(base_map)


map_cov_long <- plot_map(trends_cov) +
  labs(subtitle = "Covariate long-term trends")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])
map_cov_3gen <- plot_map(trends_cov_3gen)+
  labs(subtitle = "Covariate three-generation trends")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])


map_cov_lag_long <- plot_map(trends_cov_lag) +
  labs(subtitle = "Covariate long-term trends")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])
map_cov_lag_3gen <- plot_map(trends_cov_lag_3gen)+
  labs(subtitle = "Covariate three-generation trends")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])


map_long <- plot_map(trends)+
  labs(subtitle = "Base long-term trends")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])
map_3gen <- plot_map(trends_3gen)+
  labs(subtitle = "Base three-generation trends")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])



all_maps <- map_long + map_cov_long + map_cov_lag_long + map_3gen + map_cov_3gen + map_cov_lag_3gen + plot_layout(ncol = 3,
                                                                            nrow = 2,
                                                                            byrow = TRUE,
                                                                            guides = "collect")

pdf(paste0("figures/trend_maps_varying_covariate",model,"_",sy,"_",ey,".pdf"),
    width = 11,height = 8.5)
print(all_maps)
dev.off()






# mapping abundance -------------------------------------------------------


abund_overall <- plot_map(trends_cov_lag_3gen,
                          alternate_column = "rel_abundance")

abund_overall
# mapping covariates ------------------------------------------------------

cov_spei <- get_summary(fit_cov,variables = "beta_cov") %>%
  mutate(strata = row_number(),
         model = "covariate",
         base_model = model,
         predictor = "SPEI")

cov_spei_out <- bind_rows(cov_spei_out,
                          cov_spei)

strata_map <- load_map("bbs_usgs")

map_spei <- base_map %>%
  inner_join(.,cov_spei)


spei_map <- ggplot()+
  geom_sf(data = strata_map,
          fill = "white")+
  geom_sf(data = map_spei,
          aes(fill = mean))+
  colorspace::scale_fill_continuous_diverging(rev = TRUE)+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])+
  labs(title = paste0("Effect of spring SPEI on annual abundance",model,"_",sy,"-",ey))+
  theme_bw()

print(spei_map)


cov_nao <- get_summary(fit_cov,variables = "beta_ann_cov") %>%
  mutate(strata = row_number(),
         model = "covariate",
         base_model = model,
         predictor = "NAOI")

cov_nao_out <- bind_rows(cov_nao_out,
                         cov_nao)

map_nao <- base_map %>%
  inner_join(.,cov_nao,
             by = "strata")


nao_map <- ggplot()+
  geom_sf(data = strata_map,
          fill = "white")+
  geom_sf(data = map_nao,
          aes(fill = mean))+
  colorspace::scale_fill_continuous_diverging(rev = TRUE)+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])+
  labs(title = paste0("Effect of NAO on annual abundance",model,"_",sy,"-",ey))+
  theme_bw()

print(nao_map)

pdf(paste0("Figures/Spatial variation in covariate effects ",model,"_",sy,"-",ey,".pdf"),
    width = 11,
    height = 8.5)
print(spei_map + nao_map)
dev.off()


# #SPEI lag maps ----------------------------------------------------------


cov_spei_after_lag <- get_summary(fit_cov_lag,variables = "beta_cov") %>%
  mutate(strata = row_number(),
         model = "covariate",
         base_model = model,
         predictor = "SPEI_after_lag")

cov_spei_out <- bind_rows(cov_spei_out,
                          cov_spei_after_lag)


map_spei_after_lag <- base_map %>%
  inner_join(.,cov_spei_after_lag)


spei_after_lag_map <- ggplot()+
  geom_sf(data = strata_map,
          fill = "white")+
  geom_sf(data = map_spei_after_lag,
          aes(fill = mean))+
  colorspace::scale_fill_continuous_diverging(rev = TRUE)+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])+
  labs(title = paste0("Effect of spring SPEI on annual abundance after SPEI lag",model,"_",sy,"-",ey))+
  theme_bw()

print(spei_after_lag_map)




cov_spei_lag <- get_summary(fit_cov_lag,variables = "beta_lag_cov") %>%
  mutate(strata = row_number(),
         model = "covariate",
         base_model = model,
         predictor = "SPEI_lag")

cov_spei_out <- bind_rows(cov_spei_out,
                          cov_spei_lag)


map_spei_lag <- base_map %>%
  inner_join(.,cov_spei_lag)


spei_lag_map <- ggplot()+
  geom_sf(data = strata_map,
          fill = "white")+
  geom_sf(data = map_spei_lag,
          aes(fill = mean))+
  colorspace::scale_fill_continuous_diverging(rev = TRUE)+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])+
  labs(title = paste0("Effect of lagged spring SPEI on annual abundance ",model,"_",sy,"-",ey))+
  theme_bw()

print(spei_lag_map)



cov_nao <- get_summary(fit_cov_lag,variables = "beta_ann_cov") %>%
  mutate(strata = row_number(),
         model = "covariate",
         base_model = model,
         predictor = "NAOI after lag SPEI")

cov_nao_out <- bind_rows(cov_nao_out,
                         cov_nao)

map_nao <- base_map %>%
  inner_join(.,cov_nao,
             by = "strata")


nao_map <- ggplot()+
  geom_sf(data = strata_map,
          fill = "white")+
  geom_sf(data = map_nao,
          aes(fill = mean))+
  colorspace::scale_fill_continuous_diverging(rev = TRUE)+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])+
  labs(title = paste0("Effect of NAO on annual abundance",model,"_",sy,"-",ey))+
  theme_bw()

print(nao_map)

pdf(paste0("Figures/Spatial variation in covariate effects w lag",model,"_",sy,"-",ey,".pdf"),
    height = 11,
    width = 8.5)
print(spei_after_lag_map / spei_lag_map / nao_map)
dev.off()





# Centroid of distribution ------------------------------------------------

# load strata-map with strata areas and centroids

#centroids
cent_base_map <- base_map %>%
  sf::st_centroid() %>%
  sf::st_set_crs(st_crs(base_map))

centroids <- cent_base_map%>%
  sf::st_coordinates()

cent_base_map <- cent_base_map %>%
  mutate(x_coord = centroids[,1],
         y_coord = centroids[,2]) %>%
  sf::st_drop_geometry()
# generate annual mean x-coordinate and annual mean y-coordinate
#

inds_full <- inds_out %>%
  filter(.,type == "full") %>%
  inner_join(.,cent_base_map,
            by = c("region" = "strata_name"))


weighted_center_coord <- function(coord,area,index){

  w <- area*index
  coord1 <- sum((coord*w)/sum(w))
  return(coord1)
}

population_centers <- inds_full %>%
  group_by(year, model) %>%
  summarise(mean_x = weighted_center_coord(x_coord,
                                           area_sq_km,
                                           index),
            mean_y = weighted_center_coord(y_coord,
                                           area_sq_km,
                                           index))


# reload covariates -------------------------------------------------------
lag_time_spei <- 1
cov_all <- readRDS("data/annual_latlong_june_spei03.rds")

strata_incl <- ps$meta_strata
years_incl <- min(ps$raw_data$year) : max(ps$raw_data$year)
years_incl_lag <- c(min(ps$raw_data$year) : max(ps$raw_data$year))-lag_time_spei

#mean overall spei
cov_incl <-  strata_incl %>%
  inner_join(.,cov_all,
             by = "strata_name") %>%
  select(strata_name,
         matches(as.character(years_incl))) %>%
  pivot_longer(.,cols = matches(as.character(years_incl)),
               names_to = "year") %>%
  rename(spei = value) %>%
  group_by(year) %>%
  summarise(spei = mean(spei)) %>%
  mutate(year = as.integer(year))


cov_lag_incl <-  strata_incl %>%
  inner_join(.,cov_all,
             by = "strata_name") %>%
  select(strata_name,
         matches(as.character(years_incl_lag))) %>%
  pivot_longer(.,cols = matches(as.character(years_incl_lag)),
               names_to = "year") %>%
  mutate(year = as.integer(year),
         year = year+lag_time_spei)%>%
  rename(spei_lag = value)%>%
  group_by(year) %>%
  summarise(spei_lag = mean(spei_lag))

## global annual covariate
lag_nao <- 1 #1-year lag for NAO data
nao <- readRDS("data/nao.rds")
nao <- nao %>%
  rowwise() %>%
  mutate(.,winter = mean(c(January:May))) %>%
  filter(year %in% c(years_incl-lag_nao)) %>%
  arrange(year) %>%
  mutate(year = year+lag_nao) %>%
  rename(nao = winter)

covariates_all <- cov_incl %>%
  inner_join(.,cov_lag_incl,
             by = c("year")) %>%
  inner_join(.,nao,
             by = c("year"))


population_centers <- population_centers %>%
  left_join(.,covariates_all,
            by = "year")


northing_plot <- ggplot(data = population_centers) +
  geom_point(aes(x = spei_lag,y = mean_y, colour = model))
northing_plot

easting_plot <- ggplot(data = population_centers) +
  geom_point(aes(x = spei,y = mean_x, colour = model))
easting_plot


centre_map <- population_centers %>%
  #filter(year > 1985) %>%
  sf::st_as_sf(.,coords = c("mean_x","mean_y"),
               crs = sf::st_crs(base_map))

centre_map_spei <- ggplot()+
  #geom_sf(data = base_map)+
  geom_sf(data = centre_map,
          aes(colour = spei))+
  labs(title = "SPEI")
centre_map_scope <- ggplot()+
  geom_sf(data = base_map)+
  geom_sf(data = centre_map)+
  labs(title = "With range for scale")
centre_map_spei_lag <- ggplot()+
  #geom_sf(data = base_map)+
  geom_sf(data = centre_map,
          aes(colour = spei_lag))+
  labs(title = "SPEI_lag")
centre_map_nao <- ggplot()+
  #geom_sf(data = base_map)+
  geom_sf(data = centre_map,
          aes(colour = nao))+
  labs(title = "NAO")

centre_map_time <- ggplot()+
  #geom_sf(data = base_map)+
  geom_sf(data = centre_map,
          aes(colour = year))+
  labs(title = "Year")


map_view <- centre_map_spei + centre_map_nao + centre_map_spei_lag + centre_map_time + centre_map_scope +
  plot_layout(ncol = 2, nrow = 3, byrow = TRUE)
map_view

pdf("Figures/centroid_movements_w_lag_allyears.pdf",
    height = 11,
    width = 8.5)

print(map_view)
dev.off()




}
}








