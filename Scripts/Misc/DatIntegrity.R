## ---------------------------
##
## Script name: Raw vs Clean
##
## Purpose of script: Just to check that the raw data matches the clean stuff
##
## Author: Daniel Smith
##
## Date Created: 2020-09-10
##
## Email: dansmi@ceh.ac.uk
##
## ---------------------------

library(tidyverse)
library(readxl)

raw <- read_xlsx("Data/RawDipData.xlsx")

clean_dip <- read_csv("Data/DipPointLevelData.csv")
clean <- read_csv("Data/hmscdata.csv")

tot_clean <- sum(
  clean$an_maculipennis,
  clean$an_claviger,
  clean$cx_pipiens,
  clean$cs_annulata,
  clean$cs_morsitans,
  clean$oc_cantans,
  clean$oc_caspius
)


tot_dipclean <- sum(
  clean_dip$an_maculipennis,
  clean_dip$an_claviger,
  clean_dip$cx_pipiens,
  clean_dip$cs_annulata,
  clean_dip$cs_morsitans,
  clean_dip$oc_cantans,
  clean_dip$oc_caspius,
  na.rm = T
)

tot_raw <- raw$Totals_N_larvae %>% as.numeric() %>% sum(na.rm = T)

clean_dip <- clean_dip %>% mutate(total = 
                         an_maculipennis +
                         an_claviger +
                         cx_pipiens +
                         cs_annulata +
                         cs_morsitans +
                         oc_cantans +
                         oc_caspius)


bar <- ifelse(clean_dip$total > 1, 1, 0) %>% table()
foo <- ifelse(raw$Totals_N_larvae > 1, 1, 0) %>% table()

# Missing 10% of the larave? But where?
bar[2]/nrow(raw) * 100
foo[2]/nrow(raw) * 100

