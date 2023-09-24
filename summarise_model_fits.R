
# summarise base and cov fit ----------------------------------------------
library(bbsBayes2)
library(tidyverse)
library(patchwork)

BLTE_gen = 5.682 # generation time for Black Tern - Bird et al. 2020
BLTE_3Gen = round(BLTE_gen*3) # Three generations to calculate COSEWIC and IUCN trend thresholds
yr_pairs <- data.frame(sy = c(1970,1995,1970),
                       ey = c(1995,2022,2022))


#strata_sel <- readRDS("output/custom_latlong_bcr_stratification.rds")

# load the fitted models --------------------------------------------------
for(j in 1:2){#nrow(yr_pairs)){
  ey <-yr_pairs[j,"ey"]
  sy <- yr_pairs[j,"sy"]



fit <- readRDS(paste0("output/",sy,"_",ey,"_base.rds")) # read in the base model fit
fit_cov <- readRDS(paste0("output/",sy,"_",ey,"_2covariate_varying.rds")) # read in the covariate model fit

# summ <- readRDS("output/convergence_parameter_summaries.rds")
# summ %>% filter(variable == "beta_cov")

# trajectories trends and maps --------------------------------------------

inds_cov <- generate_indices(fit_cov,alternate_n = "n_smooth")
inds <- generate_indices(fit,alternate_n = "n_smooth")



#inds <- generate_indices(fit,alternate_n = "n_smooth")
trends_cov <- generate_trends(inds_cov, min_year = sy,
                              prob_decrease = c(0,30,50))
trends_cov$trends[1,c("percent_change","percent_change_q_0.05","percent_change_q_0.95")]
trends_cov$trends[1,c("trend","width_of_95_percent_credible_interval")]

trajs_cov <- plot_indices(inds_cov)
print(trajs_cov[[1]])


trends_cov_3gen <- generate_trends(inds_cov, min_year = ey-BLTE_3Gen,
                                   prob_decrease = c(0,30,50))
trends_cov_3gen$trends[1,c("percent_change","percent_change_q_0.05","percent_change_q_0.95")]
trends_cov_3gen$trends[1,c("trend","width_of_95_percent_credible_interval")]




trends <- generate_trends(inds, min_year = sy,
                          prob_decrease = c(0,30,50))
trends$trends[1,c("percent_change","percent_change_q_0.05","percent_change_q_0.95")]
trends$trends[1,c("trend","width_of_95_percent_credible_interval")]

trajs <- plot_indices(inds, add_observed_means = TRUE, add_number_routes = TRUE)
print(trajs[[1]])


trends_3gen <- generate_trends(inds, min_year = ey-BLTE_3Gen,
                               prob_decrease = c(0,30,50))
trends_3gen$trends[1,c("percent_change","percent_change_q_0.05","percent_change_q_0.95")]
trends_3gen$trends[1,c("trend","width_of_95_percent_credible_interval")]


trends_3gen$trends[1,c("prob_decrease_0_percent","prob_decrease_30_percent","prob_decrease_50_percent")]
trends_cov_3gen$trends[1,c("prob_decrease_0_percent","prob_decrease_30_percent","prob_decrease_50_percent")]


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

pdf(paste0("figures/trend_maps_varying_covariate",sy,"_",ey,".pdf"),
    width = 11,height = 8.5)
print(all_maps)
dev.off()



# mapping covariates ------------------------------------------------------

cov_spei <- get_summary(fit_cov,variables = "beta_cov")%>%
  mutate(strata = row_number())

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
          aes(fill = q5))+
  colorspace::scale_fill_continuous_diverging(rev = TRUE)+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])+
  labs(title = paste0("Effect of spring SPEI on annual abundance",sy,"-",ey))+
  theme_bw()

print(spei_map)


cov_nao <- get_summary(fit_cov,variables = "beta_ann_cov") %>%
  mutate(strata = row_number())

map_nao <- base_map %>%
  inner_join(.,cov_nao,
             by = "strata")


nao_map <- ggplot()+
  geom_sf(data = strata_map,
          fill = "white")+
  geom_sf(data = map_nao,
          aes(fill = q5))+
  colorspace::scale_fill_continuous_diverging(rev = TRUE)+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])+
  labs(title = paste0("Effect of NAO on annual abundance",sy,"-",ey))+
  theme_bw()

print(nao_map)

pdf(paste0("Figures/Spatial variation in covariate effects ",sy,"-",ey,".pdf"),
    width = 11,
    height = 8.5)
print(spei_map + nao_map)
dev.off()

}
