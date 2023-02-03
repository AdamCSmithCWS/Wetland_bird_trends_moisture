### running bbsBayes spatial GAMYE model



library(bbsBayes2)

species <- "Black Tern"
stratification <- "latlong"

model = "gamye"

model_variant <- "spatial"


s <- stratify(by = stratification,
              species = species)


p <- prepare_data(s)

ps <- prepare_spatial(p,
                      strata_map = load_map(stratification))

print(ps$spatial_data$map)

pm <- prepare_model(ps,
                    model = model,
                    model_variant = model_variant)

fit <- run_model(pm,
                 refresh = 200,
                 #adapt_delta = 0.8,
                 output_dir = "output",
                 output_basename = paste(species,stratification,model,model_variant,sep = "_"))







