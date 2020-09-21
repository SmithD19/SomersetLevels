##################################################
## Project: SLM
## Script purpose: Creating functional plants
## year, season: 2020-08-03
## Author: Daniel Smith
## Contact: dansmi@ceh.ac.uk
## Licence: MIT
##################################################

library(tidyverse)
library(readxl)

# Mon Aug  3 13:05:50 2020 ------------------------------
dfraw <- read_xlsx("Data/RawCovariateData.xlsx") %>% janitor::clean_names()


# Vegetation Data -----

# vertical group names based on steffi's document
vertvar <- c("carex", "equisetum", "glyceria", "typha", "juncus", "yellow", "phragmite")
vertical_cover <- dfraw %>% select(contains(vertvar) & contains("_cover")) %>% colnames()
vertical_height <- dfraw %>% select(contains(vertvar) & contains("height")) %>% colnames()

# emergent group names
emergentvar <- c("parsnip", "plantain", "arrow")
emergent_cover <- dfraw %>% select(contains(emergentvar)& contains("_cover")) %>% colnames()
emergent_height <- dfraw %>% select(contains(emergentvar) & contains("height")) %>% colnames()

# surface group names
surfvar <- c("duckweed", "ivy", "frog", "blanket")
surface_cover <- dfraw %>% select(contains(surfvar) & contains("_cover")) %>% colnames()
surface_height <- dfraw %>% select(contains(surfvar) & contains("height")) %>% colnames()

# proximal
prox <- c(
  vertical_proximal <- dfraw %>% select(contains(vertvar) & contains("proximal")) %>% colnames(),
  emergent_proximal <- dfraw %>% select(contains(emergentvar) & contains("proximal")) %>% colnames(),
  surface_proximal <- dfraw %>% select(contains(surfvar) & contains("proximal")) %>% colnames()
)

# central 
centr <- c(
  vertical_central <- dfraw %>% select(contains(vertvar) & contains("central")) %>% colnames(),
  emergent_central <- dfraw %>% select(contains(emergentvar) & contains("central")) %>% colnames(),
  surface_central <- dfraw %>% select(contains(surfvar) & contains("central")) %>% colnames()
)

# distal
dist <- c(
  vertical_distal <- dfraw %>% select(contains(vertvar) & contains("distal")) %>% colnames(),
  emergent_distal <- dfraw %>% select(contains(emergentvar) & contains("distal")) %>% colnames(),
  surface_distal <- dfraw %>% select(contains(surfvar) & contains("distal")) %>% colnames()
)

# stripping uneeded text from plant_name function convenience
vegrem <- function(x) {
  y = x %>% 
    str_remove("central_cover_") %>% 
    str_remove("proximal_cover_") %>% 
    str_remove("distal_cover_") %>%
    str_remove("central_height_") %>% 
    str_remove("proximal_height_") %>% 
    str_remove("distal_height_") %>% 
    str_remove("_percent") %>% 
    str_remove("_cm")
  return(y)
}

foo <- dfraw %>% 
  select(plot_id, year, season, contains("_cover"), contains("height")) %>%
  mutate_at(.vars = vars(contains("height"), contains("_cover")), ~ as.numeric(.)) %>% 
  pivot_longer(c(-plot_id, -year, -season), names_to = "plant") %>% 
  # add functional groups based on plant name
  mutate(functional_group = 
           # case_when for multiple if else conditionals
           case_when(
             # vertical groups
             plant %in% vertical_cover ~ "vertical",
             plant %in% vertical_height ~ "vertical",
             # surface groups
             plant %in% surface_cover ~ "surface",
             plant %in% surface_height ~ "surface",
             # emergent groups
             plant %in% emergent_cover ~ "emergent",
             plant %in% emergent_height ~ "emergent",
           ),
         plant_measurement =
           # height and cover types
           case_when(
             # vertical groups
             plant %in% vertical_cover ~ "cover",
             plant %in% vertical_height ~ "height",
             # surface groups
             plant %in% surface_cover ~ "cover",
             plant %in% surface_height ~ "height",
             # emergent groups
             plant %in% emergent_cover ~ "cover",
             plant %in% emergent_height ~ "height",),
         position = 
           # proximal, central or distal?
           case_when(
             plant %in% prox ~ "proximal",
             plant %in% centr ~ "central",
             plant %in% dist ~ "distal"
           )
  ) %>% 
  # This drop NA gets rid of species we havent selected or care about 
  drop_na(functional_group) %>% 
  # strip plant names
  mutate(plant = vegrem(plant)) 

# join mean height and mean total cover for each functional group per plot
vegdat <- full_join(
  # calculate the total cover, averaged by functional group and plot
  foo %>% group_by(plot_id, year, season, plant, plant_measurement, functional_group, position) %>% 
    summarise(mean = mean(value, na.rm = T)) %>% 
    group_by(plot_id, year, season, functional_group) %>% 
    filter(plant_measurement == "cover") %>% 
    # have to divide by 300 because it's 300% inital value:
    # (100% prox + 100% cent + 100% dist) / 300 gives overall proportion of rhyne covered
    summarise(mean_cover = sum(mean, na.rm = T)/300),
  # calculate the mean height of each functional groupings plants
  foo %>% group_by(plot_id, year, season, plant, plant_measurement, functional_group, position) %>% 
    summarise(mean = mean(value, na.rm = T)) %>% 
    group_by(plot_id, year, season, functional_group) %>% 
    filter(plant_measurement == "height") %>% 
    summarise(mean_height = mean(mean, na.rm = T))
) %>% 
  # ungroup so plays nicely elsewhere
  ungroup() %>% 
  # Add total cover column
  group_by(year, season, plot_id) %>% 
  mutate(total_cover = sum(mean_cover, na.rm = T),
         mean_height = mean(mean_height, na.rm = T))

# aggregate and pivot the veg data 
vegdat_aggregated <- left_join(
  # pivot around total cover
  vegdat %>% 
    select(-mean_height) %>% 
    pivot_wider(names_from = functional_group, values_from = mean_cover, names_prefix = "cover_"), 
  # pivot for mean height
  vegdat %>% 
    select(-total_cover) %>% 
    pivot_wider(names_from = functional_group, values_from = mean_cover, names_prefix = "height_")
)

write_csv(vegdat_aggregated, "Data/FunctionalVeg.csv")
