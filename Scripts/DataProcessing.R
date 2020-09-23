## ---------------------------
##
## Script name:
##
## Purpose of script:
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
library(janitor)

dip <-
  read_xlsx("Data/Copy_RawDipData.xlsx", na = c("NA", "nd", "na"))
cov <-
  read_xlsx("Data/RawCovariateData.xlsx", na = c("NA", "nd", "na"))

# Do these match up? Dimensions and linking data?
length(dip$Interlink_larva_dippin_with_metadata)
length(cov$Interlink_larva_dippin_with_metadata)

interlink <- cbind(
  dip$Interlink_larva_dippin_with_metadata,
  cov$Interlink_larva_dippin_with_metadata
)

# These are the indices that dont match up. Take a look manually
check <- which(interlink[, 1] != interlink[, 2])

cov[check, ]

dip[check, ]

# Grouping vars
grouping_vars <-
  c("Site", "Plot_ID", "Year", "Season", "Dip_point")
nodip_group <- c("Site", "Plot_ID", "Year", "Season")

# Join all the data grouping by these variables:
raw_data <- left_join(dip, cov)

# We also don't want any rows where the rhyne wasn't wet
# raw_data <- raw_data %>% filter(Rhyne_dry == "0")

# Also spatial variables are wrong number of digits fro grid reference
raw_data$NEW_averaged_GPS_readings_Eastings = raw_data$NEW_averaged_GPS_readings_Eastings + 300000
raw_data$NEW_averaged_GPS_readings_Northings = raw_data$NEW_averaged_GPS_readings_Northings + 100000


# Check that after the join the dimensions are correct. And the sums are correct for larvae
n_larvae_og <- sum(as.numeric(dip$Totals_N_larvae), na.rm = TRUE)
n_larave_left <- sum(as.numeric(raw_data$Totals_N_larvae), na.rm = TRUE)
if (n_larave_left != n_larvae_og) {
  stop()
}

# Mosquito Dip Data -------------------------------------------------------

# These are the names of the columns with mosquitoes in them
mosquito_cols <- colnames(select(dip, starts_with("Totals")))

mosquito_wrangle <- raw_data %>%
  # Select the mosquitoes and their grouping variables
  select(all_of(nodip_group), all_of(mosquito_cols)) %>%
  # Now turn the mosquito columns into numeric variables
  mutate_at(.vars = all_of(mosquito_cols),
            .funs = as.numeric) %>%
  # # Now we want to summarise everything by the grouping vars except dip point
  group_by(across(nodip_group)) %>%
  # Summarise = sum the values
  summarise(across(mosquito_cols, sum, na.rm = T)) %>% 
  clean_names()

# Predator Data -----------------------------------------------------------

# These are the names of the columns with mosquitoes in them
predator_cols <- cov %>% select(Fish:Gammarus) %>% colnames()

predator_wrangle <- raw_data %>%
  # Select the mosquitoes and their grouping variables
  select(all_of(nodip_group), all_of(predator_cols)) %>%
  # Now turn the mosquito columns into numeric variables
  mutate_at(.vars = all_of(predator_cols),
            .funs = as.numeric) %>%
  # # Now we want to summarise everything by the grouping vars except dip point
  group_by(across(nodip_group)) %>%
  # Summarise = sum the values
  summarise(across(predator_cols, sum, na.rm = T)) %>% 
  clean_names()

# Water Chemistry ---------------------------------------------------------

###  These water variables are very strange. by joining the data frame I'm        
###  missing a lot of variables between the raw_data and the cov data, but        
###  can't tell why. Joining is working fine but lots of values turn to NA for    
###  apparently no reason. When comparing the vector outputs of the two values    
###  for a water chem variable we get no false values? I may just drag and drop   
###  these into the raw_data, after a few sanity checks below. Very puzzling!     

# Are the vectors of the water vars the same between .xlsx file and joined data frame? 
# There are no false readings only NA? If any false values this can't work
table(cov$`Water_temperature_[˚C]` == raw_data$`Water_temperature_[˚C]`)
table(cov$Turbidity == raw_data$Turbidity)
table(cov$Salinity == raw_data$Salinity)
table(cov$Dissolved_oxygen == raw_data$Dissolved_oxygen)
table(cov$pH_probe == raw_data$pH_probe)
table(cov$`pH_-_new_pH_probe` == raw_data$`pH_-_new_pH_probe`)

# No false values for any so lets drag and drop them in
raw_data$`Water_temperature_[˚C]` <- cov$`Water_temperature_[˚C]`
raw_data$Turbidity <- cov$Turbidity
raw_data$Salinity <- cov$Salinity
raw_data$Dissolved_oxygen <- cov$Dissolved_oxygen
raw_data$pH_probe <- cov$pH_probe
raw_data$`pH_-_new_pH_probe` <- cov$`pH_-_new_pH_probe`

# Two pH probes were used, we should take the mean of these two values?
raw_data$pH_mean <-
  rowMeans(cbind(
    as.numeric(raw_data$pH_probe),
    as.numeric(raw_data$`pH_-_new_pH_probe`)
  ), na.rm = T)

# Water chem values
water_cols <-
  raw_data %>% select(`Water_temperature_[˚C]`:Dissolved_oxygen, pH_mean) %>% colnames()

water_wrangle <- raw_data %>%
  # Select the mosquitoes and their grouping variables
  select(all_of(nodip_group), all_of(water_cols)) %>%
  # Now turn the mosquito columns into numeric variables
  mutate(across(water_cols, as.numeric)) %>%
  # # Now we want to summarise everything by the grouping vars except dip point
  group_by(across(nodip_group)) %>%
  # Summarise = mean the values
  summarise(across(water_cols, mean, na.rm = T)) %>% 
  clean_names()

# PLant Cover Variables ---------------------------------------------------

plantnames <- raw_data %>% janitor::clean_names() %>% select(contains("_cover_")) %>% colnames()

# What plant species do we actually have?
plantnames

# Use this later to clean the plant column names
cleanveg <- function(x) {
  str_remove_all(x, "proximal_") %>%
    str_remove_all("distal_") %>%
    str_remove_all("central_") %>%
    str_remove_all("cover_") %>%
    str_remove_all("_percent") # %>%
    # str_replace_all("_", " ")
  # str_remove_all("height_") %>%
  # str_remove_all("cm") %>%
  # str_remove_all("[[:punct:]]")
}

# Newdata
plantdata <- raw_data %>%
  janitor::clean_names() %>%
  select(# Grouping variables
    site, plot_id, location, year, season,
    # The plantvars
    plantnames)

# Use the clean name function on the plant columns in a pivoted table
plantdata2 <- plantdata %>% 
  mutate(across(plantnames, as.numeric)) %>%
  pivot_longer(cols = plantnames) %>% 
  mutate(name = cleanveg(name))

# Names of plant variables
plants <- plantdata2$name %>% unique()

# This is the overall cover value for each rhyne without dip point
plant_wrangle <- plantdata2 %>% 
  group_by(plot_id, year, season, name) %>% 
  summarise(cover = mean(value)) %>% 
  pivot_wider(names_from = name, values_from = cover) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(
    cover = sum(c_across(plants), na.rm = T) / 100
  ) %>% 
  select(-plants) %>% 
  clean_names()


# Structural variables ----------------------------------------------------

###  I think the same thing happening to the water vars is happening to this      
###  shaded water variable. Lets run those checks like earlier to see if it is,   
###  and if so, then use the same method of drag and drop to replace them. I'm    
###  wondering if this is an artifact of the .XLSX file format or something.      

# Sanity check. No false values so go ahead again
table(as.numeric(cov$Percentage_water_shaded) == as.numeric(raw_data$Percentage_water_shaded))
raw_data$Percentage_water_shaded <- cov$Percentage_water_shaded
table(as.numeric(cov$`av.depth_centre_[cm]`) == as.numeric(raw_data$`av.depth_centre_[cm]`))
raw_data$`av.depth_centre_[cm]` <- cov$`av.depth_centre_[cm]`

# These are the names of the columns with mosquitoes in them
struct_cols <- colnames(select(raw_data, 
                               Percentage_water_shaded, 
                               Water_tier_management,
                               `av.depth_centre_[cm]`, ))

struct_wrangle <- raw_data %>% 
  # Select the mosquitoes and their grouping variables
  select(all_of(nodip_group), all_of(struct_cols)) %>%
  # Now turn the mosquito columns into numeric variables
  mutate(across(struct_cols, as.numeric)) %>%
  # # Now we want to summarise everything by the grouping vars except dip point
  group_by(across(nodip_group)) %>%
  # Summarise = mean the values
  summarise(across(struct_cols, mean, na.rm = T)) %>% 
  clean_names()


# Spatial variables -------------------------------------------------------

spatial_cols <- raw_data %>% select(NEW_averaged_GPS_readings_Eastings, NEW_averaged_GPS_readings_Northings) %>% colnames()

spatial_wrangle <- raw_data %>% 
  select(Plot_ID, spatial_cols) %>% 
  group_by(Plot_ID) %>% 
summarise(
  across(spatial_cols, function(x) mean(x, na.rm = T))
) %>% 
  rename(
    X = NEW_averaged_GPS_readings_Eastings, 
    y = NEW_averaged_GPS_readings_Northings
  ) %>% 
  clean_names()


# Join all together -------------------------------------------------------

# For Inner Join on multiple dataframes in a list use Reduce()
data <- Reduce(
  # This function
  inner_join,
  # Reduces this list to a single DF
  list(
    mosquito_wrangle,
    predator_wrangle,
    plant_wrangle,
    struct_wrangle,
    water_wrangle,
    spatial_wrangle
  )
)

glimpse(data)

###  Because of the weird missing data, implementing the drag and drop sanity   
###  check has reduced the missing data from ~30% to just 1%, much more data    
###  for our models to play with. This equates to roughly 70 rows with missing
###  data

library(naniar)
vis_miss(data)

# Reformatting names of data ----------------------------------------------

data <- data %>%
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
    water_beetle_larva = wbl_water_beetle_larva,
    # Structural 
    width = av_depth_centre_cm,
    management = water_tier_management,
    # Water Chem
    water_temp = water_temperature_c,
    ph = p_h_mean
  )

data %>% write_csv("Data/hmscdata.csv")
data %>% write_rds("Data/hmscdata.rds")



