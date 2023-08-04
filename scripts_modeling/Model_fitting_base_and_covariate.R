### running bbsBayes spatial GAMYE model


setwd("C:/Users/SmithAC/Documents/GitHub/Wetland_bird_trends_moisture")

library(bbsBayes2)
library(tidyverse)

species <- "Black Tern"
stratification <- "latlong"

strata_map <- load_map(stratification)
bcr_11 <- load_map("bcr") %>%
  filter(strata_name == "BCR11") %>%
  sf::st_buffer(.,50000) %>% # 50 km buffer to catch all possible BBS routes
  rename(bcr = strata_name) %>%
  select(bcr)
strata_sel <- strata_map %>%
  sf::st_intersection(.,bcr_11)

saveRDS(strata_sel,"output/custom_latlong_bcr_stratification.rds")
model = "gamye"

model_variant <- "spatial"



s <- stratify(by = "latlong_bcr11",
              strata_custom = strata_sel,
              species = species)


p <- prepare_data(s,
                  min_n_routes = 1,
                  min_max_route_years = 6)

ps <- prepare_spatial(p,
                      strata_map = strata_sel)

print(ps$spatial_data$map)

pm <- prepare_model(ps,
                    model = model,
                    model_variant = model_variant)


fit <- run_model(pm,
                 refresh = 200,
                 iter_warmup = 2000,
                 iter_sampling = 4000,
                 thin = 2,
                 max_treedepth = 11,
                 adapt_delta = 0.8,
                 output_dir = "output",
                 output_basename = "base")



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
# // covariate data
# matrix[n_strata,n_years] cov; // strata by year covariate matrix
#   ...
# }
# transformed parameters {
#   ...
#   // yeareffects as an additive combination of a random fluctuation
#   // and the simple linear effect of the annual moisture covariate
#   // may not be sufficient data to estimate both, in which case
#   // consider removing yeareffect_raw and sdyear
#   for(s in 1:n_strata){
#     yeareffect[s,] = sdyear[s]*yeareffect_raw[s,] + beta_cov*cov[s,];
#   }
#   ...
# }
# model {
#   ...
#   // covariate effect
#   beta_cov ~ normal(0,1); //prior for covariate effect
#   ...
# }


 cov_mod <- "models/gamye_spatial_bbs_CV_year_effect_covariate.stan"

 cov_all <- readRDS("data/annual_latlong_june_spei03.rds")

strata_incl <- ps$meta_strata
years_incl <- min(ps$raw_data$year) : max(ps$raw_data$year)

cov_incl <-  strata_incl %>%
  inner_join(.,cov_all,
             by = "strata_name") %>%
  select(matches(as.character(years_incl)),
         strata) %>%
  arrange(strata) %>%
  select(-strata) %>%
  as.matrix()

pm_cov <- prepare_model(ps,
                        model = model,
                        model_variant = model_variant,
                        model_file = cov_mod)

pm_cov$model_data[["cov"]] <- cov_incl


fit_cov <- run_model(pm_cov,
                 refresh = 200,
                 iter_warmup = 2000,
                 iter_sampling = 4000,
                 thin = 2,
                 max_treedepth = 11,
                 adapt_delta = 0.8,
                 output_dir = "output",
                 output_basename = "covariate")










