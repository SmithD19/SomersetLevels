## ---------------------------
##
## Script name:
##
## Purpose of script:
##
## Author: Daniel Smith
##
## Date Created: 2020-07-10
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

library(tidyverse)

## ---------------------------

# Fertility(lowest value = lowest fertility)
# pH(lowest value = lowest pH)
# Wetness(lowest value = driest conditions)
# Light(lowest scores = most shade tolerant)

# E = Ellenberg score for each species
# c = cover value for each species
# n = number of species having Ellenbergscores in the quadrat
# Cover-weighted score = Sum (E. c)/ Sum (c)

# coverweighted <- function(E, C) {
#   sum(E * C, na.rm = T) / sum()
# }

ellenbergs <- read_csv("Data/FinalEllenbergs.csv")
data <- readxl::read_xlsx("Data/RawCovariateData.xlsx")


vegdata <-
  data %>% select(Plot_ID, Year, Season, ellenbergs$VariableName)


cweight <- vegdata %>%
  # Data into longer format
  pivot_longer(
    cols = c(-Plot_ID, -Year, -Season),
    names_to = "VariableName",
    values_to = "Cover"
  ) %>%
  # Change cover values to numeric
  mutate(Cover = as.double(Cover)) %>%
  # Join with gathered Ellenberg traits
  left_join(ellenbergs) %>%
  # Split into cover variables
  mutate(VarType = if_else(str_detect(VariableName, "cover"), "cover", "height")) %>%
  # Only take values with Cover values over 0 for performance
  filter(VarType == "cover") %>%
  # Categorise variables
  mutate(Position = case_when(
    str_detect(VariableName, "Proximal") ~ "Proximal",
    str_detect(VariableName, "Central") ~ "Central",
    str_detect(VariableName, "Distal") ~ "Distal"
  )) %>%
  
  # EDITS here ---------

# We have to move from plot_Id up to a site level average for ellenberg values
# this prevents the ellenberg values becoming 0 because there is no cover at a particular plot
mutate(Plot_ID = as.character(Plot_ID)) %>%
  # Seperation into the site grouping
  separate(
    col = Plot_ID,
    into = c("Site", "Plot"),
    sep = "0",
    remove = F
  ) %>%
  
  # End EDITS ---------

# Grouping

group_by(Site, Year, Season) %>%
  
  
  # Calculate the cover weighted values
  summarise(
    CW_Light = round(sum(light * Cover, na.rm = T) / sum(Cover, na.rm = T)),
    CW_Moisture = round(sum(moisture * Cover, na.rm = T) / sum(Cover, na.rm = T)),
    CW_pH = round(sum(ph * Cover, na.rm = T) / sum(Cover, na.rm = T)),
    CW_Nitrogen = round(sum(nitrogen * Cover, na.rm = T) / sum(Cover, na.rm = T)),
    CW_Salt = round(sum(salt * Cover, na.rm = T) / sum(Cover, na.rm = T))
  )

cweight %>% write_csv("Data/CoverWeightedEllenbergs2.csv")
