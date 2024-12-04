### running bbsBayes spatial GAMYE model


#setwd("C:/Users/SmithAC/Documents/GitHub/Wetland_bird_trends_moisture")
#setwd("C:/GitHub/Wetland_bird_trends_moisture")
library(bbsBayes2)
library(tidyverse)



# Data setup --------------------------------------------------------------


species <- "Black Tern"
stratification <- "latlong"

strata_map <- load_map(stratification)
# bcr_11 <- load_map("bcr") %>%
#   filter(strata_name == "BCR11") %>%
#   sf::st_buffer(.,50000) %>% # 50 km buffer to catch all possible BBS routes
#   rename(bcr = strata_name) %>%
#   select(bcr)
# strata_sel <- strata_map %>%
#   sf::st_intersection(.,bcr_11)

# saveRDS(strata_sel,"output/custom_latlong_bcr_stratification.rds")
#



model = "gamye"

model_variant <- "spatial"



s <- stratify(by = "latlong",
              species = species,
              release = 2023) #

ey <-2022
sy <-1970
#
# p <- prepare_data(s,
#                   min_n_routes = 1,
#                   min_max_route_years = 6,
#                   max_year = ey,
#                   min_year = sy)
#
# ps <- prepare_spatial(p,
#                       strata_map = strata_map)
#
# print(ps$spatial_data$map)
# saveRDS(ps,paste0("data/prepared_data_",sy,"-",ey,".rds"))
#


run_base <- TRUE
run_alt_base <- TRUE

  ps <- readRDS(paste0("data/prepared_data_",sy,"-",ey,".rds"))


# SPEI --------------------------------------------------------------------

  lag_time_spei <- 1 # number of years for moisture covariate lag



  n_months <- 15

  cov_all <- readRDS(paste0("data/annual_latlong_june_spei",n_months,".rds"))

  strata_incl <- ps$meta_strata
  years_incl <- min(ps$raw_data$year) : max(ps$raw_data$year)
  years_incl_lag <- c(min(ps$raw_data$year) : max(ps$raw_data$year))-lag_time_spei

  cov_incl <-  strata_incl %>%
    inner_join(.,cov_all,
               by = "strata_name") %>%
    select(matches(as.character(years_incl)),
           strata) %>%
    arrange(strata) %>%
    select(-strata) %>%
    as.matrix()


  cov_lag_incl <-  strata_incl %>%
    inner_join(.,cov_all,
               by = "strata_name") %>%
    select(matches(as.character(years_incl_lag)),
           strata) %>%
    arrange(strata) %>%
    select(-strata) %>%
    as.matrix()


  ## mean moisture in strata within core of species' range

  # ID BCR 11 strata
  #

  bcrs <- bbsBayes2::load_map("bcr") %>%
    rename(bcr = strata_name)

  core_strata <- strata_map %>%
    sf::st_join(.,bcrs,
                largest = TRUE,
                join = sf::st_covered_by) %>%
    filter(bcr == "BCR11")


  core_strata_incl <- strata_incl %>%
    filter(strata_name %in% core_strata$strata_name)

  periphery_incl <- strata_incl %>%
    filter(!strata_name %in% core_strata$strata_name)

  # strata_incl <- strata_incl %>%
  #   mutate(periphery = ifelse(strata_name %in% core_strata$strata_name,
  #                             0,
  #                             1))
  #
#
#   saveRDS(strata_incl,"data/strata_w_core_indicator.rds")

  # tst <- ggplot()+
  #   geom_sf(data = core_strata)+
  #   geom_sf(data = bcrs,
  #           fill = NA)
  #
  # tst

  strata_incl <- readRDS("data/strata_w_core_indicator.rds")

  periphery <- as.integer(strata_incl$periphery)
  core = which(periphery == 0)
  mean_cov_core <- colMeans(cov_incl[core,])
  cov_core <- matrix(as.numeric(mean_cov_core),
                     nrow = 1)



# NAOI --------------------------------------------------------------------

  ## global annual covariate
  lag_nao <- 0 #1-year lag for NAO data
  nao <- readRDS("data/nao.rds")
  nao <- nao %>%
    rowwise() %>%
    mutate(.,winter = mean(c(January:May))) %>%
    filter(year %in% c(years_incl-lag_nao)) %>%
    arrange(year)

  cov_ann <- matrix(as.numeric(nao$winter),
                    nrow = 1)




# alternate spei covariates using 3-month spei ---------------------------------




  n_months <- "03"

  cov_all3 <- readRDS(paste0("data/annual_latlong_june_spei",n_months,".rds"))


  cov_incl3 <-  strata_incl %>%
    inner_join(.,cov_all3,
               by = "strata_name") %>%
    select(matches(as.character(years_incl)),
           strata) %>%
    arrange(strata) %>%
    select(-strata) %>%
    as.matrix()


  cov_lag_incl3 <-  strata_incl %>%
    inner_join(.,cov_all3,
               by = "strata_name") %>%
    select(matches(as.character(years_incl_lag)),
           strata) %>%
    arrange(strata) %>%
    select(-strata) %>%
    as.matrix()


  ## mean moisture in strata within core of species' range

  # ID BCR 11 strata
  #


  mean_cov_core3 <- colMeans(cov_incl3[core,])
  cov_core3 <- matrix(as.numeric(mean_cov_core3),
                     nrow = 1)



# Run Base model ----------------------------------------------------------


if(run_base){
pm <- prepare_model(ps,
                    model = model,
                    model_variant = model_variant,
                    calculate_log_lik = TRUE)


fit <- run_model(pm,
                 refresh = 400,
                 iter_warmup = 2000,
                 iter_sampling = 4000,
                 thin = 2,
                 max_treedepth = 11,
                 adapt_delta = 0.8,
                 output_dir = "output",
                 output_basename = paste0(model,"_",sy,"_",ey,"_base"))
summ <- get_summary(fit)
saveRDS(summ, paste0("summary_",model,"_",sy,"_",ey,"_base.rds"))


}



  if(run_alt_base){
    pm_fd <- prepare_model(ps,
                        model = "first_diff",
                        model_variant = model_variant,
                        calculate_log_lik = TRUE)


    fit_fd <- run_model(pm_fd,
                     refresh = 400,
                     iter_warmup = 2000,
                     iter_sampling = 4000,
                     thin = 2,
                     max_treedepth = 11,
                     adapt_delta = 0.8,
                     output_dir = "output",
                     output_basename = paste0("first_diff","_",sy,"_",ey,"_base"))
    summ <- get_summary(fit_fd)
    saveRDS(summ, paste0("summary_","first_diff","_",sy,"_",ey,"_base.rds"))


  }


  # Covariate version -------------------------------------------------------

# this was run once to create a base file to modify
# bbsBayes2::copy_model_file(model,model_variant,
#                            dir = "models")
# the modifiations below were made to the .stan file and then saved as
# "models/gamye_spatial_bbs_CV_year_effect_covariate.stan"


## re-write model to add the following bits
# data {
#   ...
#   // covariate data
#   array[n_strata,n_years] real cov;   // covariate data annual moisture by strata
#   ...
# }
# parameters {
#   ...
# // covariate
# real BETA_cov; //mean coefficient of covariate effect on annual fluctuations
# vector[n_strata] beta_cov_raw; //coefficient of covariate effect on annual fluctuations
# real<lower=0> sd_beta_cov; //variance of covariate effect (among strata)
#   ...
# }
# transformed parameters {
#   ...
# vector[n_strata] beta_cov; //coefficient of covariate effect on annual fluctuations
#
# // covariate uncentered parameterization
# beta_cov = sd_beta_cov*beta_cov_raw + BETA_cov;
#
# // yeareffects as an additive combination of a random fluctuation
# // and the effect of the annual moisture covariate
# // may not be sufficient data to estimate both, in which case
# // remove yeareffect_raw and sdyear
# for(s in 1:n_strata){
#   yeareffect[s,] = sdyear[s]*yeareffect_raw[s,] + beta_cov[s]*cov[s,];
# }

#   ...
# }
# model {
#   ...
#   // covariate effect
# beta_cov_raw ~ normal(0,1); //prior for non-centered covariate effect
# BETA_cov ~ normal(0,1); //prior for mean covariate effect
# sd_beta_cov ~ normal(0,1); //prior on variance of covariate effect
#
#   ...
# }


  ps <- readRDS(paste0("data/prepared_data_",sy,"-",ey,".rds"))



# Fit first covariate model -----------------------------------------------

cov_mod <- paste0("models/",model,"_spatial_bbs_CV_year_effect_2covariate_varying.stan")

pm_cov <- prepare_model(ps,
                        model = model,
                        model_variant = model_variant,
                        model_file = cov_mod,
                        calculate_log_lik = TRUE)

pm_cov$model_data[["cov"]] <- cov_incl
pm_cov$model_data[["cov_ann"]] <- cov_ann

fit_cov <- run_model(pm_cov,
                 refresh = 200,
                 iter_warmup = 2000,
                 iter_sampling = 4000,
                 thin = 2,
                 max_treedepth = 11,
                 adapt_delta = 0.8,
                 output_dir = "output",
                 output_basename = paste0(model,"_",sy,"_",ey,"_2covariate_varying_15"))

summ <- get_summary(fit_cov)
saveRDS(summ, paste0("summary_",model,"_",sy,"_",ey,"_2covariate_varying_15.rds"))





# Three covariate model ---------------------------------------------------





cov_mod2 <- paste0("models/",model,"_spatial_bbs_CV_year_effect_3covariate_varying.stan")


pm_cov2 <- prepare_model(ps,
                        model = model,
                        model_variant = model_variant,
                        model_file = cov_mod2,
                        calculate_log_lik = TRUE)

pm_cov2$model_data[["cov"]] <- cov_incl3
pm_cov2$model_data[["cov_ann"]] <- cov_ann

pm_cov2$model_data[["cov_lag"]] <- cov_lag_incl3



fit_cov2 <- run_model(pm_cov2,
                     refresh = 200,
                     iter_warmup = 2000,
                     iter_sampling = 4000,
                     thin = 2,
                     max_treedepth = 11,
                     adapt_delta = 0.8,
                     output_dir = "output",
                     output_basename = paste0(model,"_",sy,"_",ey,"_3covariate_varying"))

summ <- get_summary(fit_cov2)
saveRDS(summ, paste0("summary_",model,"_",sy,"_",ey,"_3covariate_varying.rds"))







# Fit core moisture effect model ------------------------------------------


cov_mod3 <- paste0("models/",model,"_spatial_bbs_CV_year_effect_2covariate_varying_core.stan")


pm_cov3 <- prepare_model(ps,
                         model = model,
                         model_variant = model_variant,
                         model_file = cov_mod3,
                         calculate_log_lik = TRUE)

pm_cov3$model_data[["cov"]] <- cov_incl
pm_cov3$model_data[["cov_ann"]] <- cov_ann

pm_cov3$model_data[["cov_core"]] <- cov_core
pm_cov3$model_data[["periphery"]] <- periphery



fit_cov3 <- run_model(pm_cov3,
                      refresh = 200,
                      iter_warmup = 2000,
                      iter_sampling = 4000,
                      thin = 2,
                      max_treedepth = 11,
                      adapt_delta = 0.8,
                      output_dir = "output",
                      output_basename = paste0(model,"_",sy,"_",ey,"_2covariate_varying_core"))

summ <- get_summary(fit_cov3)
saveRDS(summ, paste0("summary_",model,"_",sy,"_",ey,"_2covariate_varying_core.rds"))





# Fit core moisture effect model ------------------------------------------


cov_mod4 <- paste0("models/",model,"_spatial_bbs_CV_year_effect_3covariate_varying_core.stan")


pm_cov4 <- prepare_model(ps,
                         model = model,
                         model_variant = model_variant,
                         model_file = cov_mod4,
                         calculate_log_lik = TRUE)

pm_cov4$model_data[["cov"]] <- cov_incl
pm_cov4$model_data[["cov_ann"]] <- cov_ann

pm_cov4$model_data[["cov_core"]] <- cov_core
pm_cov4$model_data[["periphery"]] <- periphery



fit_cov4 <- run_model(pm_cov4,
                      refresh = 200,
                      iter_warmup = 2000,
                      iter_sampling = 4000,
                      thin = 2,
                      max_treedepth = 11,
                      adapt_delta = 0.8,
                      output_dir = "output",
                      output_basename = paste0(model,"_",sy,"_",ey,"_3covariate_varying_core"))

summ <- get_summary(fit_cov4)
saveRDS(summ, paste0("summary_",model,"_",sy,"_",ey,"_3covariate_varying_core.rds"))






