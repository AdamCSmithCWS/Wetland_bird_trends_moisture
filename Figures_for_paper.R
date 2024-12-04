#setwd("C:/GitHub/Wetland_bird_trends_moisture")
# summarise base and cov fit ----------------------------------------------
library(bbsBayes2)
library(tidyverse)
library(patchwork)
library(scico) # good cbf palettes
library(sf)


ey <-2022
sy <- 1970

BLTE_gen = 5.682 # generation time for Black Tern - Bird et al. 2020
BLTE_3Gen = round(BLTE_gen*3) # Three generations to calculate COSEWIC and IUCN trend thresholds


# base maps of core BCR and political jurisdictions
bcr11_core <- bbsBayes2::load_map("bcr") %>%
  filter(strata_name == "BCR11")
political <- bbsBayes2::load_map("prov_state")
strata_map <- bbsBayes2::load_map("latlong")

map_theme <- ggplot2::theme(line = ggplot2::element_line(linewidth = 0.4),
               rect = ggplot2::element_rect(linewidth = 0.1),
               text = element_text(family = "serif",
                                   size = 7))
# trend map breaks
breaks <- c(-7, -4, -2, -1, -0.5, 0.5, 1, 2, 4, 7)
labls <- c(paste0("< ", breaks[1]),
           paste0(breaks[-c(length(breaks))],":", breaks[-c(1)]),
           paste0("> ",breaks[length(breaks)]))
labls <- paste0(labls, " %")

## colours below come from scico::scico(n = 11,
#              palette = "roma")
trend_map_colours <- c("#7E1700","#974D13", "#AC7726", "#C1A443", "#D2D384", "#C0E9C2", "#88D9D7",
                       "#4BB2CE","#2C87BE", "#1E5FAC", "#023198")


# Figure 1 showing trends from base model and regions ---------------------
calc_indices <- FALSE
strata_core <- readRDS("data/strata_w_core_indicator.rds") %>%
  mutate(core = ifelse(periphery == 1, "periphery","core"))


model <- "base"


fit <- readRDS(paste0("output/gamye_",sy,"_",ey,"_",model,".rds")) # read in the base model fit







if(calc_indices){

  inds_plotting <- NULL

  inds <- generate_indices(fit,alternate_n = "n_smooth",
                           hpdi = TRUE)
  saveRDS(inds,paste0("output/inds_",model,"_smooth_gamye_",sy,"_",ey,".rds"))
  inds_temp <- inds$indices %>%
    mutate(model_type = model,
           index_type = "smooth")
  inds_plotting <- bind_rows(inds_plotting,
                         inds_temp)


  indsf <- generate_indices(fit,
                            hpdi = TRUE)
  saveRDS(indsf,paste0("output/inds_",model,"_full_gamye_",sy,"_",ey,".rds"))
  inds_temp <- indsf$indices %>%
    mutate(model_type = model,
           index_type = "full")
  inds_plotting <- bind_rows(inds_plotting,
                         inds_temp)


  #core and periphery trajectories
 inds_core <- generate_indices(fit,alternate_n = "n_smooth",
                               regions = "core",
                               regions_index = strata_core,
                               hpdi = TRUE)
 saveRDS(inds_core,paste0("output/inds_",model,"_smooth_core_gamye_",sy,"_",ey,".rds"))
 inds_temp <- inds_core$indices %>%
   mutate(model_type = model,
          index_type = "smooth")
 inds_plotting <- bind_rows(inds_plotting,
                        inds_temp)


 indsf_core <- generate_indices(fit,alternate_n = "n",
                               regions = "core",
                               regions_index = strata_core,
                               hpdi = TRUE)
   saveRDS(indsf_core,paste0("output/inds_",model,"_full_core_gamye_",sy,"_",ey,".rds"))
   inds_temp <- indsf_core$indices %>%
     mutate(model_type = model,
            index_type = "full")
   inds_plotting <- bind_rows(inds_plotting,
                          inds_temp)

   saveRDS(inds_plotting,
           paste0("output/indices_df_",model,"_",sy,"_",ey,".rds"))

}else{
  inds <- readRDS(paste0("output/inds_",model,"_smooth_gamye_",sy,"_",ey,".rds"))
 # indsf <- readRDS(paste0("output/inds_",model,"_full_gamye_",sy,"_",ey,".rds"))
inds_core <- readRDS(paste0("output/inds_",model,"_smooth_core_gamye_",sy,"_",ey,".rds"))
#indsf_core <- readRDS(paste0("output/inds_",model,"_full_core_gamye_",sy,"_",ey,".rds"))
inds_plotting <- readRDS(paste0("output/indices_df_",model,"_",sy,"_",ey,".rds"))
}


 # trends
trends_save <- NULL
for(sy1 in c(sy,(ey-BLTE_3Gen))){
t_long <- generate_trends(inds,
                          min_year = sy1,
                          prob_decrease = c(0,30,50),
                          hpdi = TRUE)

trends_out_t <- t_long$trends %>%
  mutate(model_type = model)
trends_save <- bind_rows(trends_save,trends_out_t)

t_long_core <- generate_trends(inds_core,
                          min_year = sy1,
                          prob_decrease = c(0,30,50),
                          hpdi = TRUE)

trends_out_t <- t_long_core$trends %>%
  mutate(model_type = model)
trends_save <- bind_rows(trends_save,trends_out_t)

}


trends_save$t_plot <- cut(trends_save$trend,
                          breaks = c(-Inf, breaks, Inf),
                     labels = labls)

pal <- stats::setNames(
  trend_map_colours,
  levels(trends_save$t_plot))

map_plot <- dplyr::inner_join(x = strata_map,
                              y = trends_save,
                              by = c("strata_name" = "region")) %>%
  mutate(span = paste0(start_year,"-",end_year))

bb <- sf::st_bbox(map_plot)


m <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = political,
                   colour = "grey70", size = 0.1,
                   fill = NA)  +
  ggplot2::geom_sf(data = map_plot, ggplot2::aes(fill = .data$t_plot),
                   colour = NA, size = 0.1) +
  ggplot2::theme_bw() +
  ggplot2::labs(fill = paste0("Trend")) +
  map_theme +
  ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))+
  ggplot2::scale_fill_manual(values = pal,
                             na.value = "white")+
  ggplot2::geom_sf(data = political,
                   colour = "grey70", size = 0.1,
                   fill = NA) +
  ggplot2::geom_sf(data = bcr11_core, fill = NA, linewidth = 0.5)+
  ggplot2::coord_sf(xlim = bb[c("xmin","xmax")],
                    ylim = bb[c("ymin","ymax")])+
  facet_grid(cols = vars(span))

m

#
# inds_traj <- inds_plotting %>%
#   filter(region_type %in% c("continent",
#                        "core"),
#          index_type == "full")
#
#
# traj_plot <- ggplot(data = inds_traj,
#                     aes(x = year, y = index,
#                         colour = region,
#                         fill = region))+
#   geom_ribbon(data = inds_traj,
#               colour = NA,
#               alpha = 0.2,
#               aes(ymin = index_q_0.05, ymax = index_q_0.95))+
#   geom_line()+
#   scale_colour_viridis_d(aesthetics = c("fill","colour"))+
#   theme_bw()
# traj_plot

trends_highlevel <- trends_save %>%
  filter(region_type %in% c("continent","core"))%>%
  mutate(span = paste0(start_year,"-",end_year),
         Region = str_to_title(region))

trends_plot <- ggplot(data = trends_highlevel,
                      aes(x = span,y = trend,
                          colour = Region))+
  geom_hline(yintercept = 0, colour = "grey60")+
  geom_errorbar(aes(ymin = trend_q_0.05, ymax = trend_q_0.95),
                width = 0, alpha = 0.6,
                position = position_dodge(width = 0.3))+
  geom_point(position = position_dodge(width = 0.3))+
  theme_bw()+
  map_theme +
  xlab("")+
  ylab("Trend (%/year)")+
  scale_colour_viridis_d(aesthetics = c("colour","fill"),
                         end = 0.8,
                         option = "inferno")
trends_plot

comb <- trends_plot + m + plot_layout(design = "11
                                      22
                                      22")



pdf("final_figures/Figure1.pdf",
    width = 7,height = 4.5)
print(comb)
dev.off()




# Figure 3 ----------------------------------------------------------------

# spatial effects of covariates

# error manual breaks
breaks_cov <- c(-0.5, -0.25, -0.1, 0, 0.1, 0.25, 0.5)
labls_cov <- c(paste0("< ", breaks_cov[1]),
           paste0(breaks_cov[-c(length(breaks_cov))],":", breaks_cov[-c(1)]),
           paste0("> ",breaks_cov[length(breaks_cov)]))
cov_effect_colours_manual <- rev(colorspace::diverge_hcl(11,
                        "Blue-Red")[-c(5,6,7)])
#"#023FA5" "#5D6CAE" "#8C94BF" "#B3B7CF" "#D2B0B6" "#C18692" "#AB5468" "#8E063B"


fit_cov_core <- readRDS(paste0("output/gamye_",sy,"_",ey,"_2covariate_varying_core.rds")) # read in the covariate model fit

cov_spei <- get_summary(fit_cov_core,variables = "beta_cov") %>%
  mutate(strata = row_number(),
         model = "covariate",
         predictor = "SPEI")


cov_spei_se <- cov_spei %>%
  pivot_longer(cols = c(q5,q95),
               names_to = "parameter",
               values_to = "estimate") %>%
  mutate(param = ifelse((parameter == "q5"),
                        "Lower 90% CI",
                        "Upper 90% CI"),
         qual_estimate = cut(estimate,
                             breaks = c(-Inf, breaks_cov, Inf),
                             labels = labls_cov))




pal_cov_se <- stats::setNames(
  cov_effect_colours_manual,
  levels(cov_spei_se$qual_estimate))

# load original data
ps <- readRDS(paste0("data/prepared_data_",sy,"-",ey,".rds"))

base_map <- load_map(ps$meta_data$stratify_by) %>%
  inner_join(ps$meta_strata,
             by = "strata_name")

bbox <- sf::st_bbox(base_map)


map_spei_se <- base_map %>%
  inner_join(.,cov_spei_se)

map_spei <- base_map %>%
  inner_join(.,cov_spei)



spei_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = political,
                   colour = "grey70", size = 0.1,
                   fill = NA)  +
  ggplot2::geom_sf(data = map_spei,
                   ggplot2::aes(fill = .data$mean),
                   colour = "grey50", size = 0.1) +
  ggplot2::theme_bw() +
  ggplot2::labs(fill = paste0("Effect of SPEI")) +
  map_theme +
  colorspace::scale_fill_continuous_diverging(rev = TRUE,
                                              palette = "Blue-Red")+
  ggplot2::guides(fill = ggplot2::guide_colorbar())+
  ggplot2::geom_sf(data = political,
                   colour = "grey70", size = 0.1,
                   fill = NA) +
  ggplot2::geom_sf(data = bcr11_core, fill = NA, linewidth = 0.5)+
  ggplot2::coord_sf(xlim = bb[c("xmin","xmax")],
                    ylim = bb[c("ymin","ymax")])


spei_map_se <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = political,
                   colour = "grey70", size = 0.1,
                   fill = NA)  +
  ggplot2::geom_sf(data = map_spei_se,
                   ggplot2::aes(fill = .data$qual_estimate),
                   colour = "grey50", size = 0.1) +
  ggplot2::theme_bw() +
  ggplot2::labs(fill = paste0("CI limits")) +
  map_theme +
  ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))+
  ggplot2::scale_fill_manual(values = pal_cov_se,
                             na.value = "white")+
  ggplot2::geom_sf(data = political,
                   colour = "grey70", size = 0.1,
                   fill = NA) +
  ggplot2::geom_sf(data = bcr11_core, fill = NA, linewidth = 0.5)+
  ggplot2::coord_sf(xlim = bb[c("xmin","xmax")],
                    ylim = bb[c("ymin","ymax")])+
  facet_wrap(vars(param),
             nrow = 2)


both <- spei_map / spei_map_se

both


pdf("final_figures/Figure3.pdf",
    width = 3.5,height = 5.5)
print(both)
dev.off()




# Figure 4 Spatial effects of NAOI ----------------------------------------


# fit_cov_core <- readRDS(paste0("output/gamye_",sy,"_",ey,"_2covariate_varying_core.rds")) # read in the covariate model fit

cov_naoi <- get_summary(fit_cov_core,variables = "beta_ann_cov") %>%
  mutate(strata = row_number(),
         model = "covariate",
         predictor = "naoi")


cov_naoi_se <- cov_naoi %>%
  pivot_longer(cols = c(q5,q95),
               names_to = "parameter",
               values_to = "estimate") %>%
  mutate(param = ifelse((parameter == "q5"),
                        "Lower 90% CI",
                        "Upper 90% CI"),
         qual_estimate = cut(estimate,
                             breaks = c(-Inf, breaks_cov, Inf),
                             labels = labls_cov))




pal_cov_se <- stats::setNames(
  cov_effect_colours_manual,
  levels(cov_naoi_se$qual_estimate))

# load original data
ps <- readRDS(paste0("data/prepared_data_",sy,"-",ey,".rds"))

base_map <- load_map(ps$meta_data$stratify_by) %>%
  inner_join(ps$meta_strata,
             by = "strata_name")

bb <- sf::st_bbox(base_map)


map_naoi_se <- base_map %>%
  inner_join(.,cov_naoi_se)

map_naoi <- base_map %>%
  inner_join(.,cov_naoi)



naoi_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = political,
                   colour = "grey70", size = 0.1,
                   fill = NA)  +
  ggplot2::geom_sf(data = map_naoi,
                   ggplot2::aes(fill = .data$mean),
                   colour = "grey50", size = 0.1) +
  ggplot2::theme_bw() +
  ggplot2::labs(fill = paste0("Effect of NAOI")) +
  map_theme +
  colorspace::scale_fill_continuous_diverging(rev = TRUE,
                                              palette = "Blue-Red")+
  ggplot2::guides(fill = ggplot2::guide_colorbar())+
  ggplot2::geom_sf(data = political,
                   colour = "grey70", size = 0.1,
                   fill = NA) +
  ggplot2::geom_sf(data = bcr11_core, fill = NA, linewidth = 0.5)+
  ggplot2::coord_sf(xlim = bb[c("xmin","xmax")],
                    ylim = bb[c("ymin","ymax")])


naoi_map_se <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = political,
                   colour = "grey70", size = 0.1,
                   fill = NA)  +
  ggplot2::geom_sf(data = map_naoi_se,
                   ggplot2::aes(fill = .data$qual_estimate),
                   colour = "grey50", size = 0.1) +
  ggplot2::theme_bw() +
  ggplot2::labs(fill = paste0("CI limits")) +
  map_theme +
  ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))+
  ggplot2::scale_fill_manual(values = pal_cov_se,
                             na.value = "white")+
  ggplot2::geom_sf(data = political,
                   colour = "grey70", size = 0.1,
                   fill = NA) +
  ggplot2::geom_sf(data = bcr11_core, fill = NA, linewidth = 0.5)+
  ggplot2::coord_sf(xlim = bb[c("xmin","xmax")],
                    ylim = bb[c("ymin","ymax")])+
  facet_wrap(vars(param),
             nrow = 2)


both <- naoi_map / naoi_map_se

both


pdf("final_figures/Figure4_alt.pdf",
    width = 3.5,height = 5.5)
print(both)
dev.off()




# Figure 2 Covariate hyperparameters --------------------------------------

fit_cov <- readRDS(paste0("output/gamye_",sy,"_",ey,"_2covariate_varying_15.rds")) # read in the covariate model fit

cov_hypers <- get_summary(fit_cov_core,variables = c("BETA_cov","BETA_ann_cov",
                                                     "BETA_cor_cov")) %>%
  mutate(effect = ifelse(variable == "BETA_cov","SPEI-local","NAOI"),
         effect = ifelse(variable == "BETA_cor_cov",
                         "SPEI-core",
                         effect)) %>%
  mutate(model = "Climate plus core")

cov_hypers2 <- get_summary(fit_cov,variables = c("BETA_cov","BETA_ann_cov")) %>%
  mutate(effect = ifelse(variable == "BETA_cov","SPEI-local","NAOI"),
         effect = ifelse(variable == "BETA_cor_cov",
                         "SPEI-core",
                         effect)) %>%
  mutate(model = "Climate")

cov_hypers <- bind_rows(cov_hypers,cov_hypers2)
cov_effect_plot <- ggplot(data = cov_hypers,
                          aes(x = effect, y = mean,
                              colour = model))+
  geom_hline(yintercept = 0, colour = "grey60")+
  geom_errorbar(aes(ymin = q5, ymax = q95),
                width = 0, alpha = 0.6,
                position = position_dodge(width = 0.3))+
  geom_point(position = position_dodge(width = 0.3))+
  theme_bw()+
  map_theme +
  xlab("")+
  ylab("Estimated effect")+
  scale_colour_viridis_d(aesthetics = c("colour","fill"),
                         end = 0.8)

pdf("final_figures/Figure2.pdf",
    width = 3.5,height = 2.5)
print(cov_effect_plot)
dev.off()




write.csv(cov_hypers,"output/Moisture hyperparameter estimates.csv")




# Figure 5 Trend comparison with covariates -------------------------------
inds_plotting <- NULL
trends_save <- NULL
for(model in c("base","2covariate_varying_15",
               "2covariate_varying_core")){

  fit_temp <- readRDS(paste0("output/gamye_",sy,"_",ey,"_",model,".rds")) # read in the base model fit


  inds <- generate_indices(fit_temp,alternate_n = "n_smooth",
                           regions = "continent",
                           hpdi = TRUE)
  inds_temp <- inds$indices %>%
    mutate(model_type = model,
           index_type = "smooth")
  inds_plotting <- bind_rows(inds_plotting,
                             inds_temp)



  #core and periphery trajectories
  inds_core <- generate_indices(fit_temp,
                                alternate_n = "n_smooth",
                                regions = "core",
                                regions_index = strata_core,
                                hpdi = TRUE)
  inds_temp <- inds_core$indices %>%
    mutate(model_type = model,
           index_type = "smooth")
  inds_plotting <- bind_rows(inds_plotting,
                             inds_temp)


  for(sy1 in c(sy,(ey-BLTE_3Gen))){
    t_long <- generate_trends(inds,
                              min_year = sy1,
                              prob_decrease = c(0,30,50),
                              hpdi = TRUE)

    trends_out_t <- t_long$trends %>%
      mutate(model_type = model)
    trends_save <- bind_rows(trends_save,trends_out_t)

    t_long_core <- generate_trends(inds_core,
                                   min_year = sy1,
                                   prob_decrease = c(0,30,50),
                                   hpdi = TRUE)

    trends_out_t <- t_long_core$trends %>%
      mutate(model_type = model)
    trends_save <- bind_rows(trends_save,trends_out_t)

  }



}
  saveRDS(inds_plotting,
          paste0("output/indices_df_compare_",sy,"_",ey,".rds"))

  saveRDS(trends_save,
          paste0("output/trends_df_compare_",sy,"_",ey,".rds"))



  inds_plotting <- readRDS(paste0("output/indices_df_compare_",sy,"_",ey,".rds"))
  trends_save <- readRDS(paste0("output/trends_df_compare_",sy,"_",ey,".rds"))


  trends_highlevel <- trends_save %>%
    filter(region_type %in% c("continent","core"))%>%
    mutate(span = paste0(start_year,"-",end_year),
           Region = str_to_title(region),
           model_name = ifelse(model_type == "base",
                               "Base",
                               "Climate plus core"),
           model_name = ifelse(model_type == "2covariate_varying_15",
                               "Climate",
                               model_name))

  trends_plot <- ggplot(data = trends_highlevel,
                        aes(x = span,y = trend,
                            colour = model_name))+
    geom_hline(yintercept = 0, colour = "grey60")+
    geom_errorbar(aes(ymin = trend_q_0.05, ymax = trend_q_0.95),
                  width = 0, alpha = 0.6,
                  position = position_dodge(width = 0.3))+
    geom_point(position = position_dodge(width = 0.3))+
    theme_bw()+
    #map_theme +
    theme(legend.position = "bottom",
          line = ggplot2::element_line(linewidth = 0.4),
          rect = ggplot2::element_rect(linewidth = 0.1),
          text = element_text(family = "serif",
                              size = 7))+
    xlab("")+
    ylab("Trend (%/year)")+
    scale_colour_viridis_d(aesthetics = c("colour","fill"),
                           end = 0.9,
                           name = "Model")+
    facet_grid(rows = vars(Region))


  trajs_highlevel1 <- inds_plotting %>%
    filter(region_type %in% c("continent"),
           year > 2004)%>%
    mutate(Region = str_to_title(region),
           model_name = ifelse(model_type == "base",
                               "Base",
                               "Climate plus core"),
           model_name = ifelse(model_type == "2covariate_varying_15",
                               "Climate",
                               model_name),
           time_period = "Short-term")

  trajs_highlevel <- inds_plotting %>%
    filter(region_type %in% c("continent"))%>%
    mutate(Region = str_to_title(region),
           model_name = ifelse(model_type == "base",
                               "Base",
                               "Climate plus core"),
           model_name = ifelse(model_type == "2covariate_varying_15",
                               "Climate",
                               model_name),
           time_period = "Long-term")


  traj_comp <- ggplot(data = trajs_highlevel,
                      aes(x = year,y = index))+
    geom_ribbon(aes(ymin = index_q_0.05,ymax = index_q_0.95,
                    fill = model_name),
                alpha = 0.3)+
    geom_line(aes(colour = model_name))+
    scale_colour_viridis_d(aesthetics = c("colour","fill"),
                           end = 0.9,
                           name = "Model")+
    scale_y_continuous(transform = "log10")+
    ylab("Index of abundance")+
    theme_bw()+
    theme(legend.position = "none",
          line = ggplot2::element_line(linewidth = 0.4),
          rect = ggplot2::element_rect(linewidth = 0.1),
          text = element_text(family = "serif",
                              size = 7))+
    facet_grid(rows = vars(Region))

  stack_plot <- traj_comp / trends_plot + plot_layout(heights = c(1,3))

  pdf("final_figures/Figure5.pdf",
      width = 3.5,height = 5.5)
  print(stack_plot)
  dev.off()


#















#strata_sel <- readRDS("output/custom_latlong_bcr_stratification.rds")
inds_save <- NULL
trends_save <- NULL
cov_spei_out <- NULL
cov_nao_out <- NULL



fit <- readRDS(paste0("output/",sy,"_",ey,"_base.rds")) # read in the base model fit

fit_cov <- readRDS(paste0("output/",sy,"_",ey,"_2covariate_varying.rds")) # read in the covariate model fit



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
                                                    11,17,6,22,18,19,
                                                    10,9,15,32)),
                                    BLTE_region = c(rep("Great Lakes and East",4),
                                                    rep("Prairies and Boreal",6),
                                                    rep("West",4)))


# compile BCR polygons into composite regions spatial polygons
  bcrs <- load_map("bcr") %>%
    inner_join(.,BLTE_BCR_composites,
               by = "strata_name") %>%
    group_by(BLTE_region) %>%
    summarise()
plot(bcrs)
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
  theme(legend.position = "bottom",
        text = element_text(size = 14))+
  guides(colour = guide_legend(reverse = TRUE,
                               title = ""))

pdf("figures/trends_plot.pdf",
    width = 9,
    height = 3)
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


trends_cov_t3 <- generate_trends(inds_cov, min_year = 2005,
                                max_year = 2022,
                                prob_decrease = c(0,30,50),
                                hpdi = TRUE)
map_3g <- plot_map(trends_cov_t3)+
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
                                              palette = "Blue-Red 2")+
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
                                              palette = "Blue-Red 2")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])+
  labs(title = paste0("Lower limit (90% CI)"))+
  theme_bw()
spei_map_q95 <- ggplot()+
  geom_sf(data = strata_map,
          fill = "white")+
  geom_sf(data = map_spei,
          aes(fill = q95))+
  colorspace::scale_fill_continuous_diverging(rev = TRUE,
                                              palette = "Blue-Red 2")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])+
  labs(title = paste0("Upper limit (90% CI)"))+
  theme_bw()



spei_all <- spei_map
spei2 <- spei_map_q5 / spei_map_q95 #+ plot_layout(design = design)
print(spei_all / spei2)


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
                                              palette = "Blue-Red 2")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])+
  labs(title = paste0("Effect of Jan-June NAO (1-year lag) \n on annual abundance"))+
  theme_bw()


nao_map_q5 <- ggplot()+
  geom_sf(data = strata_map,
          fill = "white")+
  geom_sf(data = map_nao,
          aes(fill = q5))+
  colorspace::scale_fill_continuous_diverging(rev = TRUE,
                                              palette = "Blue-Red 2")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])+
  labs(title = paste0("Lower limit (90% CI)"))+
  theme_bw()
nao_map_q95 <- ggplot()+
  geom_sf(data = strata_map,
          fill = "white")+
  geom_sf(data = map_nao,
          aes(fill = q95))+
  colorspace::scale_fill_continuous_diverging(rev = TRUE,
                                              palette = "Blue-Red 2")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])+
  labs(title = paste0("Upper limit (90% CI)"))+
  theme_bw()

nao_all <- nao_map
nao2 <- nao_map_q5 / nao_map_q95
print(nao_all)


pdf(paste0("Figures/Spatial variation in SPEI effect.pdf"),
    width = 9,
    height = 9)
print(spei_all)
print(spei2)
dev.off()

pdf(paste0("Figures/Spatial variation in NAO effect.pdf"),
    width = 9,
    height = 9)
print(nao_map)
print(nao2)
dev.off()

cov_hypers <- get_summary(fit_cov,variables = c("BETA_cov","BETA_ann_cov")) %>%
  mutate(effect = ifelse(variable == "BETA_cov","Spring SPEI","NAO_lag1"))
write.csv(cov_hypers,"output/Moisture hyperparameter estimates.csv")

