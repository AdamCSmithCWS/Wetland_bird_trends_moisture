### running bbsBayes spatial GAMYE model



library(bbsBayes2)
library(tidyverse)

species <- "Black Tern"
stratification <- "latlong"

model = "gamye"

model_variant <- "spatial"


s <- stratify(by = stratification,
              species = species)


p <- prepare_data(s,
                  min_n_routes = 1,
                  min_max_route_years = 6)

ps <- prepare_spatial(p,
                      strata_map = load_map(stratification))

print(ps$spatial_data$map)

pm <- prepare_model(ps,
                    model = model,
                    model_variant = model_variant)

fit <- run_model(pm,
                 refresh = 200,
                 iter_warmup = 2000,
                 iter_sampling = 4000,
                 thin = 2,
                 adapt_delta = 0.8,
                 output_dir = "output",
                 output_basename = paste(species,stratification,model,model_variant,sep = "_"))

summ <- get_summary(fit)


summ <- summ %>%
  mutate(variable_type = stringr::str_extract(variable, "^\\w+"))

rhat_ess_summ <- summ %>%
  group_by(variable_type) %>%
  summarise(n = n(),
            min_ess = min(ess_bulk),
            max_rhat = max(rhat),
            med_ess = median(ess_bulk),
            med_rhat = median(rhat))

sds <- summ %>% filter(grepl("sd",variable))

beta_raw <- summ %>% filter(variable_type == "beta_raw")

ess_fail <- summ %>% filter(ess_bulk < 1000)

ess_fail_summ <- ess_fail %>%
  group_by(variable_type) %>%
  summarise(n = n(),
            min_ess = min(ess_bulk),
            max_rhat = max(rhat))

rhat_fail <- summ %>% filter(rhat > 1.03)

rhat_fail_summ <- rhat_fail %>%
  group_by(variable_type) %>%
  summarise(n = n(),
            min_ess = min(ess_bulk),
            max_rhat = max(rhat))



# indices and trends ------------------------------------------------------


inds <- generate_indices(fit)

inds_smooth <- generate_indices(fit,
                                alternate_n = "n_smooth")


inds <- generate_indices(fit)
inds_smooth <- generate_indices(fit,
                                alternate_n = "n_smooth")


trends <- generate_trends(inds_smooth)

pdf("figures/rolling_shorttrends_spatial.pdf")
map <- plot_map(trends)
print(map)

for(ys in seq(1970,2011,by = 5)){
  tt <- generate_trends(inds_smooth,
                        min_year = ys,
                        max_year = ys + 10)
  print(plot_map(tt))
}
dev.off()










