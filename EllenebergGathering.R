## ---------------------------
##
## Script name: Ellenberg Name Resolution
##
## Purpose of script: To create a dictionary of species names
## resolve them and then extract Ellenberg values from trait databases
###
## Author: Daniel Smith
##
## Date Created: 2020-06-03
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
library(taxize)
library(readxl)
library(TR8)

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
plantnames <- plantnames %>% mutate(CleanName = cleanveg(VariableName))

# Go through both provided lsit of names and check if any are common names that can be converted to scientific
uniqueplantnames <- comm2sci(unique(plantnames$NameProvided))
uniqueplantnames2 <- comm2sci(unique(plantnames$CleanName))

# Manipulate this data
uniqueplantnames <- 
  uniqueplantnames %>%
  enframe(name = "NameProvided", value = "ReturnedNameProvided") %>% 
  unnest(cols = ReturnedNameProvided, keep_empty = T)

uniqueplantnames2 <-
  uniqueplantnames2 %>% 
  enframe(name = "CleanName", value = "ReturnedClean") %>% 
  unnest(cols = ReturnedClean, keep_empty = T)

# Join into a master data frame
data <- left_join(plantnames, uniqueplantnames) %>% left_join(uniqueplantnames2)

# Coalesce the columns for the two names depending onwhether it returned null or not
data2 <- data %>% 
  mutate(Coalesced1 = if_else(is.na(ReturnedClean), CleanName, ReturnedClean),
         Coalesced2 = if_else(is.na(ReturnedNameProvided), NameProvided, ReturnedNameProvided))

# Resolve these names using taxize and then take the accepted name for them
coalesced1resolved <- tnrs(unique(data2$Coalesced1), source = "iPlant_TNRS")

coalesced2resolved <- tnrs(na.omit(unique(data2$Coalesced2)), source = "iPlant_TNRS")

# Bind these back to the data frame but filter for accuracy
bind1 <- coalesced1resolved %>% 
  filter(score > 0.9) %>% 
  select(Coalesced1 = submittedname,
         Coalesced1Accepted = acceptedname) %>% 
  full_join(data2)

# Bind these back to the data frame but filter for accuracy
bind2 <- coalesced2resolved %>% 
  filter(score > 0.9) %>% 
  select(Coalesced2 = submittedname,
         Coalesced2Accepted = acceptedname) %>% 
  full_join(data2)

# Bind them together finally
data3 <- full_join(bind1, bind2)

# Search TR8 for these traits
ellenberguk <- c("ell_light_uk",
                 "ell_moist_uk",
                 "ell_pH_uk",
                 "ell_N",
                 "ell_S")

# Functions to fix TR8 - Silently stops when hits an error
# Thats no good to me - these work better don't silently stop
searchtr8 <- function(x){
  traitlist = list()
  traitlist = lapply(x, tr8, ellenberguk)
}

bindtr8 <- function(y, colname){
  colname = colname
  z = map(y, extract_traits)
  z = map(z, rownames_to_column, var = colname)
  z = bind_rows(z)
}

# Species list 1 - No blank values and no NA values
specieslist1 <- data3$Coalesced1Accepted %>% na.omit %>% discard((.) == "") %>% unique()
# Ellenbergs for species list 1
trait1df <- specieslist1 %>% searchtr8() %>% bindtr8(colname = "Coalesced1")

# Species list 2 - No blank values and no NA values
specieslist2 <- data3$Coalesced2Accepted %>% na.omit %>% discard((.) == "") %>% unique()
# Ellenbergs for species list 2
trait2df <- specieslist2 %>% searchtr8() %>% bindtr8(colname = "Coalesced2")

# Prepare these for merging together
test <- inner_join(data3, trait1df) %>% as.tibble() %>% select(NameProvided, VariableName, CleanName, ell_light_uk:ell_S)
test2 <- inner_join(data3, trait2df) %>% as.tibble() %>% select(NameProvided, VariableName, CleanName, ell_light_uk:ell_S)

# Join together
ellenbergmatching <- full_join(test, test2) %>% distinct

# Parse the numbers to tkae the first value
ellenbergfinal <- ellenbergmatching %>% mutate_at(ellenberguk, parse_number) %>% select(-VariableName) %>% distinct()

write_csv(ellenbergfinal, "data/KnownEllenbergValues2.csv")



