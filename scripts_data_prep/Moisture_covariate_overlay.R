
# Script to overlay the latlong stratification map with
# SPEI moisture covariates

library(terra)
library(bbsBayes2)
library(sf)
library(tidyverse)
library(exactextractr)
# Download the Nroth Atlantic Oscilation data and save to rds
# nao <- read.fwf(file = "https://www.cpc.ncep.noaa.gov/products/precip/CWlink/pna/norm.nao.monthly.b5001.current.ascii.table",
#                   header = FALSE,
#                 skip = 1,
#                 widths = c(5, rep(7,12)),
#                 strip.white = TRUE,
#                 col.names = c("year",month.name)) # accessed on September 18, 2023
#saveRDS(nao,file = "data/nao.rds")

# In terms of what values to extract, the authors of that paper found an effect of mean monthly NAOI values
 # from January to June on subsequent-year survival, but no current-year effect of Sept-Dec/Jan-Feb values
 # on survival the current year. Honestly though I just re-read that section of the paper several times
 # (top right paragraph of p. 4) and I’m quite confused about the way they are describing the lagged
 # effect, and as a result I may be interpreting this wrong. I know the authors and I’m going to send
 # a query to try to sort that out, so stay tuned. It almost seems like from the way they describe it,
 # they looked at a 1-year lag effect and a 1-year future effect, but no current-year effect which seems odd…
 #
 # I believe for the time series we had talked about a simple early-late split, to separate out the time
 # of the steep decline to the greater stability.


# saveRDS(pdo,file = "data/pdo.rds")
#load the strata map
strata_map <- bbsBayes2::load_map("latlong")
strata_crs <- st_crs(strata_map)

# load moisture data - SPEI 3 month values at monthly scale
# downloaded from https://doi.org/10.20350/digitalCSIC/15470

## S.M. Vicente-Serrano, S. Beguería, J.I. López-Moreno. 2010. A Multi-scalar drought index sensitive to global warming: The Standardized Precipitation Evapotranspiration Index – SPEI. Journal of Climate 23: 1696, DOI: 10.1175/2009JCLI2909.1

# These are the means for the preceeding 3 months "spei03"
moisture_full = terra::rast("data/too_large/spei03.nc")

strata_map = st_transform(strata_map, st_crs(moisture_full))
moisture = terra::crop(moisture_full, strata_map)
#remove large moisture object
rm(moisture_full)

strata_moisture <- exact_extract(moisture,strata_map,"mean")

#spei data are monthly values from Jan 1901 - December 2022
spei_dates <- data.frame(col_names = names(strata_moisture),
                          month = rep(1:12,times = 122),
                          year = rep(1901:2022,each = 12))

# june 3-month spei values (april, May, June)
june_spei <- spei_dates %>%
  filter(month == 6,
         year %in% 1966:2022)


strata_june_moisture <- strata_moisture %>%
  select(., matches(june_spei$col_names))


names(strata_june_moisture) <- paste0(june_spei$year)

strata_names <- strata_map %>%
  sf::st_drop_geometry() %>%
  select(strata_name)


strata_june_moisture <- bind_cols(strata_names,strata_june_moisture)


strata_june_moisture_df <- strata_june_moisture %>%
  pivot_longer(.,all_of(as.character(1966:2022)),
               names_to = "year")

saveRDS(strata_june_moisture, "data/annual_latlong_june_spei03.rds")

saveRDS(strata_june_moisture_df, "data/annual_latlong_june_spei03_df.rds")




#
# strata_map_moisture <- strata_map %>%
#   left_join(strata_june_moisture,
#             by = "strata_name")
#
# vis_tmp <- ggplot() +
#   geom_sf(data = strata_map_moisture,
#           aes(fill = value))+
#   facet_wrap(vars(year))
#
# vis_tmp
#

