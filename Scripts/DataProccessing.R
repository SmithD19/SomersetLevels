## ------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(janitor)


## ------------------------------------------------------------------------------------------------------------------
dip <- read_xlsx("Data/RawDipData.xlsx", na = c("NA", "nd"))
cov <- read_xlsx("Data/RawCovariateData.xlsx", na = c("NA", "nd"))
ellen <- read_csv("Data/KnownEllenbergValues2.csv")


## ------------------------------------------------------------------------------------------------------------------
data <- left_join(dip, cov) %>% clean_names() %>% remove_empty("cols")


## ------------------------------------------------------------------------------------------------------------------
mosquitoes <- data %>% select(totals_n_larvae:totals_n_oc_caspius) %>% colnames()


## ------------------------------------------------------------------------------------------------------------------
predators <- data %>% select(fish:gammarus) %>% colnames()


## ------------------------------------------------------------------------------------------------------------------
structural <-
  data %>% 
  select(rhyne_dry,
         rhyne_cleared,
         average_width_m,
         av_depth_centre_cm,
         av_depth_proximal_edge_cm, 
         waterboard_water_level_below_land_m, 
         percentage_water_shaded) %>% 
  colnames()


## ------------------------------------------------------------------------------------------------------------------
study <- data %>% select(site, plot, plot_id, season, year, dip_point) %>% colnames()


## ------------------------------------------------------------------------------------------------------------------
plot_id_xy <- data %>% select(plot_id, new_averaged_gps_readings_st, new_averaged_gps_readings_eastings, new_averaged_gps_readings_northings) %>% 
  group_by(plot_id) %>% 
  # Give average coords for each plot_ID
  summarise_if(is.numeric,  ~ round(mean(., na.rm = T))) %>% 
  rename(eastings = new_averaged_gps_readings_eastings,
         northings = new_averaged_gps_readings_northings) %>% 
  # PLotID as factor for future grouping
  mutate(plot_id = as.factor(plot_id),
         # Easting and Northing have wrong grid ref (From ST)
         eastings = eastings + 300000,
         northings = northings + 100000)


## ------------------------------------------------------------------------------------------------------------------
clean <- data %>% 
  # Abundance variables are count
  mutate_at(.vars = vars(mosquitoes, predators), as.integer) %>% 
  # Structural variables are numeric
  mutate_at(.vars = vars(structural), parse_number) # parse_number() selects the first number in a string then discards the rest
  # Site and seasonal variables should be factoral


## ------------------------------------------------------------------------------------------------------------------
plantnames <- data %>% select(contains("_cover_")) %>% colnames()

# What plant species do we actually have?
plantnames

# Ellenberg variables
ellenval <- c("ell_light_uk", "ell_moist_uk", "ell_pH_uk", "ell_N")

# Ellenberg values ready for binding
ellen <- ellen %>% 
  mutate(CleanNames = tolower(CleanName)) %>% 
  select(-CleanName)

# Use this later
cleanveg <- function(x) {
  str_remove_all(x, "proximal_") %>%
    str_remove_all("distal_") %>%
    str_remove_all("central_") %>%
    str_remove_all("cover_") %>%
    str_remove_all("_percent") %>% 
    str_replace_all("_", " ")
    # str_remove_all("height_") %>%
    # str_remove_all("cm") %>%
    # str_remove_all("[[:punct:]]")
}

# Newdata
plantdata <- data %>% 
  select(interlink_larva_dippin_with_metadata, plantnames) %>% 
  mutate_if(is.character, parse_number) %>% 
  pivot_longer(-interlink_larva_dippin_with_metadata) %>% 
  mutate(CleanNames = cleanveg(name))


## ------------------------------------------------------------------------------------------------------------------
# Manually search and fill in remaining ellenberg values?
ellenref <- plantdata %>% distinct(CleanNames) %>% left_join(ellen) %>% distinct()

# write_csv(ellenref, "Data/MissingEllenbergValues.csv")



## ------------------------------------------------------------------------------------------------------------------
covervalue <- plantdata %>% 
  group_by(CleanNames, interlink_larva_dippin_with_metadata) %>% 
  summarise(
    # This gives the % cover value for each dip point sampled
    covervalue = sum(value)/300
  ) %>% ungroup() %>% 
  left_join(ellen) %>% 
  distinct()


## ------------------------------------------------------------------------------------------------------------------
# So how many samples do we have?
covervalue$interlink_larva_dippin_with_metadata %>% unique() %>% length()
# 2383

# So how many out of those are each species actually present?
desc_cover <- covervalue %>% 
  mutate(coverpres = ifelse(covervalue > 0, 1, 0)) %>% 
  group_by(CleanNames) %>% 
  summarise(
    #n = n(),
    perc_pres = (sum(coverpres, na.rm = T) / n()),
    mean_covervalue = mean(covervalue, na.rm = T)
  ) %>% 
  arrange(desc(perc_pres))

ellenbergcsv <- covervalue %>% 
  select(CleanNames, NameProvided, ell_light_uk:ell_S) %>% 
  distinct() %>% 
  left_join(desc_cover)


#write_csv(ellenbergcsv, "Data/MissingEllenbergValues.csv")


## ------------------------------------------------------------------------------------------------------------------
weightedellen <- covervalue %>% filter(covervalue != 0) %>% 
  group_by(interlink_larva_dippin_with_metadata) %>% 
  summarise(
    # Light values for each plot
    L = (sum(ell_light_uk * covervalue, na.rm = T) / sum(covervalue, na.rm = T)),
     # Moisture values for each plot
    M = (sum(ell_moist_uk * covervalue, na.rm = T) / sum(covervalue, na.rm = T)),
     # pH values for each plot
    pH = (sum(ell_pH_uk * covervalue, na.rm = T) / sum(covervalue, na.rm = T)),
     # Nitrogen values for each plot
    N = (sum(ell_N * covervalue, na.rm = T) / sum(covervalue, na.rm = T))
    )

# Unweighted scores = Sum (E) /n

unweightedellen <- covervalue %>% filter(ell_light_uk > 0 | covervalue > 0) %>% 
  group_by(interlink_larva_dippin_with_metadata) %>%
  summarise(
    L = sum(ell_light_uk) / n()
  )

listed <- covervalue %>% 
  filter(covervalue > 0) %>%
  filter(ell_light_uk > 0) %>% 
  mutate(present = if_else(covervalue > 0, 1, 0)) %>% drop_na() %>%
  group_split(interlink_larva_dippin_with_metadata)



## ------------------------------------------------------------------------------------------------------------------
dfabundance <- clean %>% 
  select(mosquitoes, predators) %>%
  # We dont need these vars for the model
  select(-totals_n_larvae, -totals_not_ided) %>% 
  # Rename all of them
  rename(
    # Mosquitoes
    an_maculipennis = totals_n_an_maculipennis,
    an_claviger = totals_n_an_claviger,
    cx_pipiens = totals_n_cx_pipiens,
    cs_annulata = totals_n_cs_annulata,
    cs_morsitans = totals_n_cs_morsitans,
    oc_cantans = totals_n_oc_annulipes_cantans,
    oc_caspius = totals_n_oc_caspius,
    # Predators
    water_beetle_larva = wbl_water_beetle_larva
  )


## ------------------------------------------------------------------------------------------------------------------
dfstruc <- clean %>% 
  # These are structural characteristics of the environment
  select(dry = rhyne_dry,
         cleared = rhyne_cleared,
         width = average_width_m,
         freeboard = waterboard_water_level_below_land_m,
         # length = approx_length_m, # Why is this such a long name.....
         exposure = exposure_1_sun_3_shade_2_partial_shade_4_open_but_emergent_vegetation_within_rhyne_5_open_but_shaded_by_very_high_bank_or_dense_overhanging_bank_vegetation,
         management = water_tier_management,
         shaded = percentage_water_shaded,
         edgedepth = av_depth_proximal_edge_cm,
         centredepth = av_depth_centre_cm) %>% 
  # Mutate them to replace bad variables that are clearly outliers and modify data type
  mutate(
    # Anything above 100 shade can't exist
    shaded = ifelse(shaded > 100, 100, shaded),
    # Make a proportion
    shaded = shaded/100,
    # An exposure level over 5 also cant exist
    exposure = ifelse(exposure > 5, 5, exposure),
    # Factor variables
    dry = as.factor(dry),
    cleared = as.factor(cleared),
    exposure = as.factor(exposure),
    management = as.factor(management)
  )
  
  


## ------------------------------------------------------------------------------------------------------------------
dfstudy <- clean %>% 
  select(study) %>%
  mutate_all(as.factor)


## ------------------------------------------------------------------------------------------------------------------
df <- bind_cols(dfstudy, dfabundance, dfstruc)


## ------------------------------------------------------------------------------------------------------------------
agg1 <- df %>% 
  group_by(site, plot, season, year, plot_id) %>% 
  summarise_at(colnames(dfabundance),
               .funs = ~ sum(. , na.rm = T)) %>% ungroup()
  
agg2 <- df %>% 
  select(-colnames(dfabundance)) %>% 
  group_by(site, plot, season, year, plot_id) %>% 
  summarise_if(is.numeric, .funs = ~ mean(. , na.rm = T)) %>% ungroup()

agg3 <- df %>% 
  select(-colnames(dfabundance), -dip_point) %>% 
  group_by(site, plot, season, year, plot_id) %>% 
    # Aggregate to most common factor level
  summarise_if(is.factor,
    # This function chooses the most common factor level for each grouping
    .funs = function(x) names(table(x))[which.max(table(x))]
  ) %>% ungroup()

hmscdata <- left_join(agg1, agg2) %>% left_join(agg3) %>% left_join(plot_id_xy)

write_csv(hmscdata, "Data/hmscdata.csv")
write_rds(hmscdata, "Data/hmscdata.rds")

