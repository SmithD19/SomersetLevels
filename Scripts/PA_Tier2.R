##################################################
## Project: SLM
## Script purpose: Final Presence/Absence model not stratified by season
## Date: 2020-08-03
## Author: Daniel Smith
## Contact: dansmi@ceh.ac.uk
## Licence: MIT
##################################################
## Libraries:

library(tidyverse)
library(Hmsc)

##################################################
## Data:

data <- read_rds("Data/hmscdata.rds") %>% na.omit() %>% 
  # THIS IS IMPORTANT - Need to drop levels not used after omitting NA
  # Factors are never fun :-(
  droplevels() %>% 
  ungroup()

tiered <- data %>% group_split(management)

data <- tiered[[2]] %>% dplyr::select(-management) %>% as.data.frame()


##################################################
## Functions:

pa_mutate <-
  function(x) {
    if_else(x > 0, as.integer(1), as.integer(0))
  }

##################################################
## Y Matrix: Species Matrix

Y <- data %>% select(an_maculipennis:gammarus) %>% 
  # drop fish and newts, as well as the two infrequent mosquito species
  select(-fish, -newt, -cs_morsitans, -oc_cantans, -oc_caspius) %>% 
  # presence/absence
  mutate_all(~ pa_mutate(.)) %>% 
  as.data.frame()

# Change all 0 values to NA
# Model the abundance conditional on presence AKA hurdle model
# Y <- replace(Y, Y == 0, NA)

# Change Y matrix to ones and zeroes for easy counting
Yt <- replace(Y, Y == 0, NA) %>% replace(. > 0, 1)

# This is how many sites that the models actually have occurance data
tabled <- Yt %>% colSums(na.rm = T)

# Which columns have less than 10 instances of occurrence overall?
removeme <- names(tabled[tabled < 10])

# Removing rare species
Y <- Y %>% select(-removeme)

##################################################
## X Matrix: Covariance Matrix

# These covariates
Xcovariates <- data %>% select(cover:ph, -salinity, -turbidity)

#Xcovariates$management <- as.factor(Xcovariates$management)

# This is the model formula
XFormula <- as.formula(paste("~", paste(colnames(Xcovariates), collapse = "+")))

# The covariates in model.matrix format
X <- model.matrix(XFormula, data = Xcovariates)

# Use XData? For gradient contruction?
XData <- Xcovariates %>% as.data.frame()
#rownames(XData) <- data$plot_id

##################################################
## Study Design

plot_id <- data$plot_id
season <- data$season
year <- data$year

StudyDesign <- data.frame(plot_id = plot_id,
                          season = paste0("season_", season),
                          year = paste0("year_", year)) %>% 
  mutate_all(~ as.factor(.)) %>% 
  as.data.frame()


##################################################
## Spatial Coords:

XY <- data %>% select(x, y)

colnames(XY) <- c("X", "Y")

sRL <- XY %>% as.data.frame() %>% distinct()

# Give rownames of study design for HMSC
rownames(sRL) <- unique(StudyDesign$plot_id)


##################################################
## Assign Random levels:

SpatialRandomLevel <- HmscRandomLevel(sData = sRL)

SeasonRandomLevel <- HmscRandomLevel(units = levels(StudyDesign$season))

YearRandomLevel <- HmscRandomLevel(units = levels(StudyDesign$year))

# RL needs to be in named list format
rl <- list(
  plot_id = SpatialRandomLevel,
  season = SeasonRandomLevel,
  year = YearRandomLevel
)

##################################################
## Modelling:

# This is a full model with co-occurring predators
model <- Hmsc(
  # A matrix of species occurrence/abundance records
  Y = as.matrix(Y),
  # A formula object for linear regression
  XFormula = XFormula,
  # A matrix of measured covariates
  XData = XData,
  # Distribution
  distr = "probit",
  # Study Design
  studyDesign = StudyDesign,
  # Random Levels
  ranLevels = rl
)

##################################################
## Params:

# Test run or not?
test.run = F

nChains = 4

if (test.run) {
  # with this option MCMC runs fast for checking
  thin = 1
  samples = 10
  transient = 5
  verbose = 5
} else {
  # with this option MCMC runs slow for analysis
  thin = 200
  samples = 4000
  #adaptNf = rep(ceiling(0.4 * samples * thin), 1)
  transient = ceiling(0.5 * samples * thin)
  verbose = 10 * thin
}

##################################################
## Saving:

# Timings
ptm = proc.time()

output = sampleMcmc(
  model,
  thin = thin,
  samples = samples,
  transient = transient,
  nChains = nChains,
  verbose = verbose,
  nParallel = nChains
)

# Timings
computationtime = proc.time() - ptm

##################################################
## Dir checking:

# Create File Path if doesn't exist

filepath = file.path("Models", "Tier2_PresenceAbsence")

ifelse(!dir.exists(filepath), dir.create(filepath), FALSE)

save.image(paste0(filepath, "/Model.RData"))