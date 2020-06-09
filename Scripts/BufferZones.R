## ---------------------------
##
## Script name: BufferZones
##
## Purpose of script: Extract land use values around sites for modelling inference
##
## Author: Daniel Smith
##
## Date Created: 2020-06-08
##
## Email: dansmi@ceh.ac.uk
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## load up the packages we will need:

library(raster)
library(rgdal)
library(rgeos)

## ---------------------------

# Land use map 2007: https://www.ceh.ac.uk/services/land-cover-map-2007
land2007 <- raster("Spatial/lcm2007gb25m_.tif")
print(land2007)
OBGS <- projection(land2007)
OBGS <- crs(OBGS)

# Data
hmscdata <- read_rds("Data/hmscdata.rds")

# Get the site and plot spatial points
sites <- hmscdata %>%  dplyr::select(plot_id, eastings, northings)

coords <- sites %>%
  dplyr::select(x = eastings, y = northings) %>%
  data.matrix()

# Turn into coordinates
points <- SpatialPointsDataFrame(coords = coords,
                                 data = as.data.frame(sites),
                                 proj4string = OBGS)

# Make 1km Buffers
buff1km <-
  gBuffer(
    points,
    byid = TRUE,
    width = 1000,
    capStyle = "ROUND",
    joinStyle = "ROUND"
  )

# What is under 1km buffers?
luse_1km <- raster::extract(land2007, buff1km)
names(luse_1km) <- points$plot_id

# CEH Land Use 2007 Classes
land_types <- c(
  "broadleaved_woodland",
  "coniferous_woodland",
  "arable_horticulture",
  "improved_grassland",
  "rough_grassland",
  "neutral_grassland",
  "calcareous_grassland",
  "acid_grassland",
  "fen_marsh_swamp",
  "heather",
  "heather_grassland",
  "bog",
  "montane",
  "inland_rock",
  "salt_water",
  "freshwater",
  "supralittoral_rock",
  "supralittoral_sediment",
  "littoral_rock",
  "littoral_sediment",
  "saltmarsh",
  "urban",
  "suburban"
)


library(tidyverse)

# Key of land values
land_key = 1:23 %>% as.character()

# Lookup table for easy replacing of variables
lookup <- data.frame(old_val = land_key,
                     new_val = land_types)

# Calculate whats in each plot_id area by %?
dict <- function(x) {
  y = lookup$new_val[match(unlist(x), lookup$old_val)]
  y = table(y)
  enframe(y)
}

luse_1km %>% map(dict) %>% bind_rows(.id = "plot_id")


# Create dataframes with this new function
df1km <-
  luse_1km %>%
  map(dict) %>%
  bind_rows(.id = "plot_id") %>%
  group_by(plot_id) %>%
  # round to 2 digits
  mutate(freq = round(value / sum(value), digits = 2)) %>%
  select(-value) %>% 
  pivot_wider(id_cols = plot_id, names_from = name, values_from = freq, values_fn = list(freq = mean)) %>% 
  # remove any rows that have no variation (AKA are always 0)
  janitor::remove_constant() %>% 
  # Ungroup
  ungroup() %>% mutate(plot_id = as.factor(plot_id))


# Save these  as .csv
write_csv(df1km, "Data/buffer1km.csv")
# # Save easy formats as RDS
# library(gdata)
# gdata::keep(df100m, df300m, sure = T)
# save.image("data/buffers.RData")
