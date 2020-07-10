## ---------------------------
##
## Script name:
##
## Purpose of script:
##
## Author: Daniel Smith
##
## Date Created: 2020-07-09
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
library(readxl)

## ---------------------------

plantnames <- read_xlsx("Data/PlantNames.xlsx")

# Testing string
string <- "Proximal_cover_Duckweed_[%]"

cleanveg <- function(x) {
  str_remove_all(x, "Proximal_") %>%
    str_remove_all("Distal_") %>%
    str_remove_all("Central_") %>%
    str_remove_all("cover_") %>%
    str_remove_all("height_") %>%
    str_remove_all("cm") %>%
    str_remove_all("[[:punct:]]")
}

# Test
cleanveg(string)

# Create cleaned variable names
plantnames <-
  plantnames %>% mutate(CleanName = cleanveg(VariableName))

# Now manually go through the BRC database and look for anlogues to these species
# Use the helper script BRC_scraper_function.R

# -------------------------------------------------------------------------

# read in the file full of URLS linking to the BRC database
csv <- read_csv("Data/EllenbergURLs.csv")
csv <- csv %>% mutate(
  BRCName = NA,
  light = NA,
  moisture = NA,
  ph = NA,
  nitrogen = NA,
  salt = NA
)


for (i in seq_along(csv$URL)) {
  #print(cat(crayon::bgYellow(paste("checking URL:", csv$URL[[i]]))))
  URL = csv$URL[[i]]
  if (is.na(URL)) {
    csv$BRCName[[i]] = NA
    #print(cat(crayon::bgRed(paste("BRC has no data for:", csv$URL[[i]]))))
    next()
  }
  else {
    htmlplant = read_html(URL)
    csv$BRCName[[i]] = html_text(html_nodes(htmlplant, ".page-header"), trim = TRUE)
    ellenbergs = html_text(html_nodes(htmlplant, "strong"), trim = TRUE)
    if (length(ellenbergs) > 4) {
      csv$light[[i]] = as.integer(ellenbergs[[1]])
      csv$moisture[[i]] = as.integer(ellenbergs[[2]])
      csv$ph[[i]] = as.integer(ellenbergs[[3]])
      csv$nitrogen[[i]] = as.integer(ellenbergs[[4]])
      csv$salt[[i]] = as.integer(ellenbergs[[5]])
    }
  }
}



# This was the old way ----------------------------------------------------

# # Create tibble
# plants <-
#   enframe(name, name = "ID", value = "Name") %>% unnest(Name)
# # Create ellenberg values
# ellenbergs <- enframe(ell, name = "ID", value = "Ellenbergs") %>%
#   # Take only lists over 4 returns. Prevents incorrect habitats being returned
#   filter(lengths(Ellenbergs) > 4) %>%
#   # Take these out of the enframed list columns and name
#   mutate(
#     light = as.double(map(Ellenbergs, `[`, 1)),
#     moisture = as.double(map(Ellenbergs, `[`, 2)),
#     ph = as.double(map(Ellenbergs, `[`, 3)),
#     nitrogen = as.double(map(Ellenbergs, `[`, 4)),
#     salt = as.double(map(Ellenbergs, `[`, 5))
#   ) %>%
#   # Get rid of list column and unnest the useful ellenberg list columns
#   select(-Ellenbergs) %>% unnest(cols = everything())

# A bunch of data processing and cleaning
# csv %>% rowid_to_column("ID")

# -------------------------------------------------------------------------

# Averages for given names if there is more than one species for each name
givennamedellenbergs <-
  csv %>% 
  mutate_at(.vars = vars(light, moisture, ph, nitrogen, salt), .funs = ~ as.integer(.)) %>% 
  group_by(CleanName) %>%
  summarise_at(
    .vars = vars(light, moisture, ph, nitrogen, salt),
    .funs = ~ as.integer(mean(., na.rm = T))
  )

write.csv(givennamedellenbergs, "Data/BRC_ellenbergs.csv")

fuzzyjoin::stringdist_left_join(plantnames, givennamedellenbergs) %>% write.csv("Data/FinalEllenbergs.csv")



