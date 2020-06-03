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
  left_join(data2)

# Bind these back to the data frame but filter for accuracy
bind2 <- coalesced2resolved %>% 
  filter(score > 0.9) %>% 
  select(Coalesced2 = submittedname,
         Coalesced2Accepted = acceptedname) %>% 
  left_join(data2)

# Bind them together finally
data3 <- full_join(bind1, bind2)

# Search TR8 for these traits
ellenberguk <- c("ell_light_uk",
                 "ell_moist_uk",
                 "ell_pH_uk",
                 "ell_N",
                 "ell_S")

# Searching for first name column

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


# Species list 1
specieslist1 <- data3$Coalesced1Accepted %>% na.omit %>% discard((.) == "") %>% unique()
# Ellenbergs for species list 1
trait1df <- specieslist1 %>% searchtr8() %>% bindtr8(colname = "Coalesced1")

# Species list 2
specieslist2 <- data3$Coalesced2Accepted %>% na.omit %>% discard((.) == "") %>% unique()
# Ellenbergs for species list 2
trait2df <- specieslist2 %>% searchtr8() %>% bindtr8(colname = "Coalesced2")


### STOP HERE DEBUG AFTER

stop()

# Join these to the master DF?
data4 <- data3 %>% full_join(trait1df)
data5<- data3 %>% full_join(trait2df)

data6 <- bind_cols(data4, data5) %>% mutate_all(as.character)

ellenbergmatching <- data6 %>% mutate(ell_Light = coalesce(ell_light_uk, ell_light_uk1),
                 ell_Moist = coalesce(ell_moist_uk, ell_moist_uk1),
                 ell_pH = coalesce(ell_pH_uk, ell_pH_uk1),
                 ell_Nitro = coalesce(ell_N, ell_N1),
                 ell_Salt = coalesce(ell_S, ell_S1)) %>% 
  select(VariableName, NameProvided,
         ell_Light,
         ell_Moist,
         ell_pH,
         ell_Nitro,
         ell_Salt)

final <- plantnames %>% left_join(ellenbergmatching)

################ !!!! FOR SOME REASON !!!! ####################
# traits arent gathered properly when supplied with a list of traits.
# will have to do it the old way first in a lapply and then bind after. VERY STRANGE

tr8("Iris pseudacorus", ellenberguk)

specieslisttemp <- na.omit(unique(data3$Coalesced2Accepted)) %>% discard((.) == "")
length(specieslisttemp)

testtr8 <- lapply(specieslisttemp, tr8, ellenberguk)


  testtr8 %>% map(extract_traits) %>% map(rownames_to_column) %>% bind_rows()
