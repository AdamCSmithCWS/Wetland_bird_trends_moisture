
# summarise base and cov fit ----------------------------------------------
library(bbsBayes2)
library(tidyverse)
library(patchwork)

BLTE_gen = 5.682 # generation time for Black Tern - Bird et al. 2020
BLTE_3Gen = round(BLTE_gen*3) # Three generations to calculate COSEWIC and IUCN trend thresholds

#strata_sel <- readRDS("output/custom_latlong_bcr_stratification.rds")

# load the fitted models --------------------------------------------------


fit <- readRDS("output/base.rds") # read in the base model fit
fit_cov <- readRDS("output/covariate.rds") # read in the covariate model fit

summ <- readRDS("output/convergence_parameter_summaries.rds")
summ %>% filter(variable == "beta_cov")

# trajectories trends and maps --------------------------------------------

inds_cov <- generate_indices(fit_cov,alternate_n = "n_smooth")
inds <- generate_indices(fit,alternate_n = "n_smooth")



#inds <- generate_indices(fit,alternate_n = "n_smooth")
trends_cov <- generate_trends(inds_cov, min_year = 1970,
                              prob_decrease = c(0,30,50))
trends_cov$trends[1,c("percent_change","percent_change_q_0.05","percent_change_q_0.95")]
trends_cov$trends[1,c("trend","width_of_95_percent_credible_interval")]

trajs_cov <- plot_indices(inds_cov)
print(trajs_cov[[1]])


trends_cov_3gen <- generate_trends(inds_cov, min_year = 2021-BLTE_3Gen,
                                   prob_decrease = c(0,30,50))
trends_cov_3gen$trends[1,c("percent_change","percent_change_q_0.05","percent_change_q_0.95")]
trends_cov_3gen$trends[1,c("trend","width_of_95_percent_credible_interval")]




trends <- generate_trends(inds, min_year = 1970,
                          prob_decrease = c(0,30,50))
trends$trends[1,c("percent_change","percent_change_q_0.05","percent_change_q_0.95")]
trends$trends[1,c("trend","width_of_95_percent_credible_interval")]

trajs <- plot_indices(inds)
print(trajs[[1]])


trends_3gen <- generate_trends(inds, min_year = 2021-BLTE_3Gen,
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



map_cov_long <- plot_map(trends)
map_cov_3gen <- plot_map(trends_cov_3gen)
map_long <- plot_map(trends)
map_3gen <- plot_map(trends_3gen)



all_maps <- map_long + map_cov_long + map_3gen + map_cov_3gen + plot_layout(ncol = 2,
                                                                            nrow = 2,
                                                                            byrow = TRUE,
                                                                            guides = "collect")
print(all_maps)
