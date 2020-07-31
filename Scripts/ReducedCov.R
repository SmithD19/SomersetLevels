## ---------------------------
##
## Script name: ReducedCov
##
## Purpose of script: Reducing the number of covariates in the HMSC models
##
## Author: Daniel Smith
##
## Date Created: 2020-07-31
##
## Email: dansmi@ceh.ac.uk
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------

library(tidyverse)
library(janitor)

# Load data

hmscdata <- read_rds("Data/hmscdata.rds")

# Dry rows have no data - because there was no water to sample
hmscdata <- hmscdata %>% filter(dry == 0)

# Reduce covariates -------------------------------------------------------

# We decided that Exposure wasnt important as it correlated with shade
# Also decided that Newts and Fish arent best represented by the sampling procedure
# Also need to combine edge and centre depth into a water volume variable
# Also remove the cleared variable - hasnt been significant for a long time 
# Caspius and cantans are also very small smaple sizes - remove

reducecov <- hmscdata %>% select(exposure, newt, fish, cleared, oc_cantans, oc_caspius) %>% colnames()
hmscdata <- hmscdata %>% select(-reducecov)


# Data manipulation -------------------------------------------------------

# As factors for those that didnt convert for some reason
data <- hmscdata %>% mutate(
  management = as.factor(management),
)

# Split data into seasonal groups
seasons <- data %>% drop_na() %>% group_by(season) %>% group_split()

# Remove columns with no variation
explore <- map(seasons, remove_constant)

# Make sure all data frames have the same columns/predictors
columns <- map(explore, colnames)

to_remove1 <- setdiff(columns[[1]], columns[[2]])
to_remove2 <- setdiff(columns[[1]], columns[[3]])
to_remove3 <- setdiff(columns[[2]], columns[[3]])

to_remove <- c(to_remove1, to_remove2, to_remove3) %>% unique()

# Now create seasonal presence absence data
pa_mutate <-
  function(x) {
    if_else(x > 0, as.integer(1), as.integer(0))
  }

seasonal_pa <- seasons %>%
  bind_rows() %>%
  # Convert abundance to presence absence
  mutate_if(is.integer, pa_mutate) %>%
  # Remove variables not consistent across all seasons
  select(-to_remove) %>%
  # Remove rows with no variation across combined data
  remove_constant() %>%
  # Group and split into seasonal data
  group_by(season) %>% group_split(season) %>%
  # As dataframe
  map(as.data.frame)

# Presence Absence data - Only include Mosquito species
Y <- seasonal_pa %>% map(select, an_maculipennis:gammarus)

# Covariates
covariates <- seasonal_pa %>% map(select, width:management, -eastings, -northings)

XFormula <-
  as.formula(paste("~" , paste(colnames(covariates[[1]]), collapse = "+")))

X <-
  lapply(covariates, function(x)
    model.matrix(XFormula, data = x))

# Contrasts for ordinal in future?
# contrasts.arg = list(cw_light = "contr.poly",
#                      cw_nitrogen = "contr.poly",
#                      cw_p_h = "contr.poly",
#                      cw_moisture = "contr.poly"))

# Geographical co-ordinates for each site
xy <-
  seasonal_pa %>% map(select, eastings, northings) %>% map(data.matrix)

# Study Design structure
studydesign <-  seasonal_pa %>%
  # study vars
  map(select, site, plot_id, year) %>%
  # Change to useful names
  map(
    mutate,
    site = paste0("site_", as.character(site)),
    plot_id = paste0("plot_id_", as.character(plot_id)),
    year = paste0("year_", as.character(year))
  ) %>%
  # As factor
  map(mutate_all, as.factor) %>%
  # As dataframe
  map(as.data.frame)

library(Hmsc)
# Spatial latent random variables and structure for each data set
randomlevels <- list()
for (i in 1:3) {
  # Site random level
  rL1 <-
    HmscRandomLevel(units = unique(studydesign[[i]][, 1]), sData = xy[[i]])
  # Plot random level - nested within site
  rL2 <-
    HmscRandomLevel(units = unique(studydesign[[i]][, 2]), sData = xy[[i]])
  # Year random level
  rL3 <-
    HmscRandomLevel(units = unique(studydesign[[i]][, 3]), sData = xy[[i]])
  # Combine into a list to be easily called
  randomlevels[[i]] <- list(site = rL1, plot_id = rL2, year = rL3)
}



# Models ------------------------------------------------------------------

models <- list()

for (i in 1:3) {
  # This is a full model with co-occuring predators
  models[[i]] <- Hmsc(
    # A matrix of species occurence/abundance records
    Y = as.matrix(Y[[i]]),
    # A formula object for linear regression
    XFormula = XFormula,
    # A matrix of measured covariates
    X = X[[i]],
    # Distribution
    distr = "probit",
    # Study Design
    studyDesign = studydesign[[i]],
    # Random Levels
    ranLevels = randomlevels[[i]]
  )
}

names(models) <- c("Spring", "Summer", "Autumn")

# Testing Params ----------------------------------------------------------

# Create File Path if doesnt exist
ifelse(!dir.exists(file.path("Models", "ReducedCov")), dir.create(file.path("Models", "ReducedCov")), FALSE)

# Test run or not?
test.run = F

nChains = 4

if (test.run) {
  # with this option mcmc runs fast for checking
  thin = 1
  samples = 10
  transient = 5
  verbose = 5
} else {
  # with this option mcmc runs slow for analysis
  thin = 100
  samples = 1000
  adaptNf = rep(ceiling(0.4 * samples * thin), 1)
  transient = ceiling(0.5 * samples * thin)
  verbose = 500 * thin
}

# Run models in loop

for (i in seq_along(models)) {
  # Timings
  ptm = proc.time()
  
  output = sampleMcmc(
    models[[i]],
    thin = thin,
    samples = samples,
    transient = transient,
    #adaptNf = adaptNf,
    nChains = nChains,
    verbose = verbose,
    nParallel = nChains
  )
  # Timings
  computationtime = proc.time() - ptm
  
  # Filename Saving
  filename = file.path("Models", "ReducedCov", paste0(names(models)[[i]], ".RData"))
  # Save file
  save(output, file = filename, computationtime)
}













