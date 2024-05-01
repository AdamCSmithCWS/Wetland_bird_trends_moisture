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
cov_spei_out <- NULL
cov_nao_out <- NULL


for(model in c("gamye","first_diff")[1]){
# load the fitted models --------------------------------------------------
for(j in 1:3){#nrow(yr_pairs)){
  ey <-yr_pairs[j,"ey"]
  sy <- yr_pairs[j,"sy"]

if(!file.exists(paste0("output/",sy,"_",ey,"_base.rds"))){next}

fit <- readRDS(paste0("output/",sy,"_",ey,"_base.rds")) # read in the base model fit
fit_cov <- readRDS(paste0("output/",model,"_",sy,"_",ey,"_2covariate_varying_lag.rds")) # read in the covariate model fit

summ <- readRDS(paste("summary",model,sy,ey,"2covariate_varying_lag.rds",
                      sep = "_"))

cov_eff <- summ %>%
  filter(grepl("BETA",variable))
# summ <- readRDS("output/convergence_parameter_summaries.rds")
# summ %>% filter(variable == "beta_cov")

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

   inds <- generate_indices(fit,alternate_n = "n_smooth")

  inds_out <- inds$indices %>%
    mutate(model = "base",
           base_model = model,
           type = "smooth")

  inds_out <- bind_rows(inds_out,
                        inds_cov_out)

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
  filter(region == "continent")

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


inds_save <- bind_rows(inds_save,inds_out)
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



map_cov_long <- plot_map(trends) +
  labs(subtitle = "Covariate long-term trends")
map_cov_3gen <- plot_map(trends_cov_3gen)+
  labs(subtitle = "Covariate three-generation trends")
map_long <- plot_map(trends)+
  labs(subtitle = "Base long-term trends")
map_3gen <- plot_map(trends_3gen)+
  labs(subtitle = "Base three-generation trends")



all_maps <- map_long + map_cov_long + map_3gen + map_cov_3gen + plot_layout(ncol = 2,
                                                                            nrow = 2,
                                                                            byrow = TRUE,
                                                                            guides = "collect")

pdf(paste0("figures/trend_maps_varying_covariate",model,"_",sy,"_",ey,".pdf"),
    width = 11,height = 8.5)
print(all_maps)
dev.off()



# mapping covariates ------------------------------------------------------

cov_spei <- get_summary(fit_cov,variables = "beta_cov") %>%
  mutate(strata = row_number(),
         model = "covariate",
         base_model = model,
         predictor = "SPEI")

cov_spei_out <- bind_rows(cov_spei_out,
                          cov_spei)
# load original data
ps <- readRDS(paste0("data/prepared_data_",sy,"-",ey,".rds"))

base_map <- load_map(ps$meta_data$stratify_by) %>%
  inner_join(ps$meta_strata,
             by = "strata_name")

bbox <- sf::st_bbox(base_map)

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

}
}





# Centroid of distribution ------------------------------------------------





