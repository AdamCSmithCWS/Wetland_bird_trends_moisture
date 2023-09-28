setwd("C:/GitHub/Wetland_bird_trends_moisture")
# summarise base and cov fit ----------------------------------------------
library(bbsBayes2)
library(tidyverse)
library(patchwork)

BLTE_gen = 5.682 # generation time for Black Tern - Bird et al. 2020
BLTE_3Gen = round(BLTE_gen*3) # Three generations to calculate COSEWIC and IUCN trend thresholds

#strata_sel <- readRDS("output/custom_latlong_bcr_stratification.rds")
inds_save <- NULL
trends_save <- NULL
cov_spei_out <- NULL
cov_nao_out <- NULL


model = "gamye"

  ey <-2022
  sy <- 1966

fit <- readRDS(paste0("output/",sy,"_",ey,"_base.rds")) # read in the base model fit
fit_cov <- readRDS(paste0("output/",sy,"_",ey,"_2covariate_varying.rds")) # read in the covariate model fit


# trajectories trends and maps --------------------------------------------
 calc_indices <- FALSE
if(calc_indices){
  inds_cov <- generate_indices(fit_cov,alternate_n = "n_smooth",
                               hpdi = TRUE)
  saveRDS(inds_cov,paste0("output/inds_cov_smooth_",model,"_",sy,"_",ey,".rds"))

  inds_cov_full <- generate_indices(fit_cov,alternate_n = "n",
                                    hpdi = TRUE)
  saveRDS(inds_cov_full,paste0("output/inds_cov_full_",model,"_",sy,"_",ey,".rds"))

  inds <- generate_indices(fit,alternate_n = "n_smooth",
                           hpdi = TRUE)
  saveRDS(inds,paste0("output/inds_base_smooth_",model,"_",sy,"_",ey,".rds"))

  indsf <- generate_indices(fit,
                            hpdi = TRUE)
  saveRDS(indsf,paste0("output/inds_base_full_",model,"_",sy,"_",ey,".rds"))
}else{
  inds_cov <- readRDS(paste0("output/inds_cov_smooth_",model,"_",sy,"_",ey,".rds"))
  inds_cov_full <- readRDS(paste0("output/inds_cov_full_",model,"_",sy,"_",ey,".rds"))
  inds <- readRDS(paste0("output/inds_base_smooth_",model,"_",sy,"_",ey,".rds"))
  indsf <- readRDS(paste0("output/inds_base_full_",model,"_",sy,"_",ey,".rds"))
}



# Regional indices --------------------------------------------------------

  # table to join BCRs into composite regions
  BLTE_BCR_composites <- data.frame(strata_name = paste0("BCR",c(12,13,23,14,
                                                    11,17,6,22,
                                                    10,9,15)),
                                    BLTE_region = c(rep("Great Lakes and East",4),
                                                    rep("Prairies and Boreal",4),
                                                    rep("West",3)))


# compile BCR polygons into composite regions spatial polygons
  bcrs <- load_map("bcr") %>%
    inner_join(.,BLTE_BCR_composites,
               by = "strata_name") %>%
    group_by(BLTE_region) %>%
    summarise()

  # spatial join of composite regions with strata used in analysis
  strata_join <- load_map("latlong") %>%
    filter(strata_name %in% fit_cov$meta_strata$strata_name) %>%
    sf::st_join(.,bcrs,
                largest = TRUE,
                left = TRUE) %>%
    sf::st_drop_geometry() %>%
    mutate(BLTE_region = ifelse(is.na(BLTE_region),"other",BLTE_region))

  # generate trajectories for composite regions
  inds_cov_comp <- generate_indices(fit_cov,alternate_n = "n_smooth",
                                    regions = "BLTE_region",
                                    regions_index = strata_join,
                                    hpdi = TRUE)
  saveRDS(inds_cov_comp,paste0("output/inds_cov_smooth_comp_",model,"_",sy,"_",ey,".rds"))

  inds_cov_full_comp <- generate_indices(fit_cov,alternate_n = "n",
                                         regions = "BLTE_region",
                                         regions_index = strata_join,
                                         hpdi = TRUE)
  saveRDS(inds_cov_full_comp,paste0("output/inds_cov_full_comp_",model,"_",sy,"_",ey,".rds"))

  inds_comp <- generate_indices(fit,alternate_n = "n_smooth",
                                regions = "BLTE_region",
                                regions_index = strata_join,
                                hpdi = TRUE)
  saveRDS(inds_comp,paste0("output/inds_base_smooth_comp_",model,"_",sy,"_",ey,".rds"))

  inds_full_comp <- generate_indices(fit,
                                     regions = "BLTE_region",
                                     regions_index = strata_join,
                                     hpdi = TRUE)
  saveRDS(inds_full_comp,paste0("output/inds_base_full_comp_",model,"_",sy,"_",ey,".rds"))

  trajs_test <- plot_indices(inds_cov_full_comp)



# Trends for continent and composite regions ------------------------------

t_years <- data.frame(start_year = c(1966,1970,1990,ey-BLTE_3Gen,ey-(2*BLTE_3Gen),1970),
                      end_year = c(ey,ey,ey,ey,ey-BLTE_3Gen,1990),
                      trend_type = c("Long-term (1966)",
                                     "Long-term (1970)",
                                     "Since 1990",
                                     "Three Generation",
                                     "Previous Three Generation",
                                     "First 20-years"))
for(tt in c(1:6)){

  sy1 <- t_years[tt,"start_year"]
  ey1 <- t_years[tt,"end_year"]
  ttype <- t_years[tt,"trend_type"]
trends_cov_t <- generate_trends(inds_cov, min_year = sy1,
                              max_year = ey1,
                              prob_decrease = c(0,30,50),
                              hpdi = TRUE)

trends_out_t <- trends_cov_t$trends %>%
  mutate(base_model = model,
         model = "Covariate Model",
         trend_type = ttype)
trends_save <- bind_rows(trends_save,trends_out_t)

trends_cov_comp_t <- generate_trends(inds_cov_comp, min_year = sy1,
                                max_year = ey1,
                                prob_decrease = c(0,30,50),
                                hpdi = TRUE)

trends_out_cov_comp_t <- trends_cov_comp_t$trends %>%
  mutate(base_model = model,
         model = "Covariate Model",
         trend_type = ttype)
trends_save <- bind_rows(trends_save,trends_out_cov_comp_t)

trends_t <- generate_trends(inds, min_year = sy1,
                                max_year = ey1,
                                prob_decrease = c(0,30,50),
                                hpdi = TRUE)

trends_t <- trends_t$trends %>%
  mutate(base_model = model,
         model = "Base model",
         trend_type = ttype)
trends_save <- bind_rows(trends_save,trends_t)

trends_comp_t <- generate_trends(inds_comp, min_year = sy1,
                                     max_year = ey1,
                                     prob_decrease = c(0,30,50),
                                     hpdi = TRUE)

trends_out_comp_t <- trends_comp_t$trends %>%
  mutate(base_model = model,
         model = "Base model",
         trend_type = ttype)
trends_save <- bind_rows(trends_save,trends_out_comp_t)

}

  write.csv(trends_save,"output/trend_estimates.csv")
trends_broad <- trends_save %>%
  filter(region_type != "stratum",
         region != "other",
         trend_type %in% c("Long-term (1970)",
                           "Since 1990",
                           "Three Generation")) %>%
  mutate(n_years = end_year - start_year,
         region = ifelse(region == "continent","Survey-wide",region),
         region = factor(region,levels = rev(c("Survey-wide",
                                           "Prairies and Boreal",
                                           "West",
                                           "Great Lakes and East")),
                         ordered = TRUE))

trends_plot <- ggplot(data = trends_broad)+
  geom_point(aes(x = region, y = trend, colour = model, group = model),
             position = position_dodge(width = 0.3))+
  geom_errorbar(aes(x = region, ymin = trend_q_0.05,
                    ymax = trend_q_0.95, colour = model, group = model),
             position = position_dodge(width = 0.3), alpha = 0.3, width = 0)+
  geom_hline(yintercept = 0,colour = grey(0.2))+
  scale_colour_viridis_d(begin = 0.1,end = 0.8)+
  xlab("")+
  ylab("Trend (%/year)")+
  coord_flip()+
  facet_wrap(vars(trend_type))+
  theme_bw()+
  theme(legend.position = "bottom")+
  guides(colour = guide_legend(reverse = TRUE,
                               title = ""))

pdf("figures/trends_plot.pdf",
    width = 9,
    height = 4.5)
trends_plot
dev.off()


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


trends_cov_t <- generate_trends(inds_cov, min_year = 1970,
                                max_year = 2022,
                                prob_decrease = c(0,30,50),
                                hpdi = TRUE)
bbox <- load_map("latlong") %>%
  filter(strata_name %in% fit_cov$meta_strata$strata_name) %>%
  sf::st_bbox()
map_long <- plot_map(trends_cov_t)+
  ggnewscale::new_scale_colour()+
  geom_sf(data = bcrs,
          fill = NA,
          linewidth = 0.5,
          aes(colour = BLTE_region),
          show.legend = FALSE)+
  scale_colour_brewer(palette = "Set2")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])
map_long


trends_cov_t <- generate_trends(inds_cov, min_year = 2005,
                                max_year = 2022,
                                prob_decrease = c(0,30,50),
                                hpdi = TRUE)
map_3g <- plot_map(trends_cov_t)+
  ggnewscale::new_scale_colour()+
  geom_sf(data = bcrs,
          fill = NA,
          linewidth = 0.5,
          aes(colour = BLTE_region),
          show.legend = FALSE)+
  scale_colour_brewer(palette = "Set2")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])
print(map_3g)


pdf(paste0("figures/trend_map_1970-2022.pdf"),
    width = 9,height = 9)
print(map_long)
dev.off()

pdf(paste0("figures/trend_map_Three_generation.pdf"),
    width = 9,height = 9)
print(map_3g)
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
  colorspace::scale_fill_continuous_diverging(rev = TRUE,
                                              palette = "Blue-Red 3")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])+
  labs(title = paste0("Effect of spring SPEI on annual abundance"))+
  theme_bw()

spei_map_q5 <- ggplot()+
  geom_sf(data = strata_map,
          fill = "white")+
  geom_sf(data = map_spei,
          aes(fill = q5))+
  colorspace::scale_fill_continuous_diverging(rev = TRUE,
                                              palette = "Blue-Red 3")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])+
  labs(title = paste0("Lower Credible limit (90% CI)"))+
  theme_bw()

print(spei_map / spei_map_q5)


# NAO effect in space -----------------------------------------------------


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
  colorspace::scale_fill_continuous_diverging(rev = TRUE,
                                              palette = "Blue-Red 3")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])+
  labs(title = paste0("Effect of Jan-June NAO (1-year lag) \n on annual abundance"))+
  theme_bw()

nao_map_q5 <- ggplot()+
  geom_sf(data = strata_map,
          fill = "white")+
  geom_sf(data = map_nao,
          aes(fill = q95))+
  colorspace::scale_fill_continuous_diverging(rev = TRUE,
                                              palette = "Blue-Red 3")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])+
  labs(title = paste0("Upper Credible limit (90% CI)"))+
  theme_bw()

print(nao_map / nao_map_q5)

pdf(paste0("Figures/Spatial variation in SPEI effect.pdf"),
    width = 9,
    height = 9)
print(spei_map / spei_map_q5)
dev.off()

pdf(paste0("Figures/Spatial variation in NAO effect.pdf"),
    width = 9,
    height = 9)
print(nao_map / nao_map_q5)
dev.off()

cov_hypers <- get_summary(fit_cov,variables = c("BETA_cov","BETA_ann_cov")) %>%
  mutate(effect = ifelse(variable == "BETA_cov","Spring SPEI","NAO_lag1"))
write.csv(cov_hypers,"output/Moisture hyperparameter estimates.csv")

