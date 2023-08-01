
# Script to overlay the latlong stratification map with
# SPEI moisture covariates

library(terra)
library(bbsBayes2)
library(sf)
library(tidyverse)
library(exactextractr)

#load the strata map
strata_map <- bbsBayes2::load_map("latlong")
strata_crs <- st_crs(strata_map)

# load moisture data - SPEI 3 month values at monthly scale
# downloaded from https://doi.org/10.20350/digitalCSIC/15470

## S.M. Vicente-Serrano, S. Beguería, J.I. López-Moreno. 2010. A Multi-scalar drought index sensitive to global warming: The Standardized Precipitation Evapotranspiration Index – SPEI. Journal of Climate 23: 1696, DOI: 10.1175/2009JCLI2909.1


moisture_full = terra::rast("data/spei03.nc")

strata_map = st_transform(strata_map, st_crs(moisture_full))
moisture = terra::crop(moisture_full, strata_map)
#remove large moisture object
rm(moisture_full)

strata_moisture <- exact_extract(moisture,strata_map,"mean")

#spei data are monthly values from Jan 1901 - December 2022
spei_dates <- data.frame(col_names = names(strata_moisture),
                          month = rep(1:12,times = 122),
                          year = rep(1901:2022,each = 12))


june_spei <- spei_dates %>%
  filter(month == 6,
         year %in% 1966:2021)


strata_june_moisture <- strata_moisture %>%
  select(., matches(june_spei$col_names))


names(strata_june_moisture) <- paste0(june_spei$year)

strata_names <- strata_map %>%
  sf::st_drop_geometry() %>%
  select(strata_name)


strata_june_moisture <- bind_cols(strata_names,strata_june_moisture)


strata_june_moisture <- strata_june_moisture %>%
  pivot_longer(.,all_of(as.character(1966:2021)),
               names_to = "year")

saveRDS(strata_june_moisture, "annual_latlong_june_spei03.rds")
strata_map_moisture <- strata_map %>%
  left_join(strata_june_moisture,
            by = "strata_name")

vis_tmp <- ggplot() +
  geom_sf(data = strata_map_moisture,
          aes(fill = value))+
  facet_wrap(vars(year))

vis_tmp


