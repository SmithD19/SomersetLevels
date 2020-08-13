##################################################
## Project: SLM
## Script purpose: Full seasonal Models (Abundance)
## Date: 2020-08-10
## Author: Daniel Smith
## Contact: dansmi@ceh.ac.uk
## Licence: MIT
##################################################
## Libraries:

library(tidyverse)
library(janitor)
library(Hmsc)

##################################################
## Data:

rawdata <- read_rds("Data/hmscdata.rds") %>% na.omit() %>%
  # THIS IS IMPORTANT - Need to drop levels not used after omitting NA
  # Factors are never fun :-(
  droplevels()


## Drop covariates we dont want:
data <- rawdata %>%
  # drop fish, frogs & newts, as well as the two infrequent mosquito species
  select(-fish, -newt, -cs_morsitans, -oc_cantans, -oc_caspius, -frog) %>%
  # drop the unused structural levels
  select(-dry, -cleared, -exposure)

##################################################
## Split into seasonal data

seasonal_data <- group_split(data, season)

## Remove factors that arent the same throughout all seasons

# Make sure all data frames have the same columns/predictors
columns <- map(seasonal_data, colnames)

to_remove1 <- setdiff(columns[[1]], columns[[2]])
to_remove2 <- setdiff(columns[[1]], columns[[3]])
to_remove3 <- setdiff(columns[[2]], columns[[3]])

to_remove <- c(to_remove1, to_remove2, to_remove3) %>% unique()

seasonal_pa <- seasonal_data %>%
  bind_rows() %>%
  # Remove variables not consistent across all seasons
  select(-to_remove) %>%
  # Remove rows with no variation across combined data
  remove_constant() %>%
  # Group and split into seasonal data
  group_by(season) %>% group_split() %>%
  # As dataframe
  map(as.data.frame) %>%
  # Drop any levels
  map(droplevels)


##################################################

Y <- seasonal_pa %>% map(select, an_maculipennis:gammarus) %>%
  # as data frame
  map(as.data.frame)

# Change all 0 values to NA
# Model the abundance conditonal on presence AKA hurdle model
Y <- lapply(Y, function(e) replace(e, e == 0, NA))

##################################################

Xcovariates <-
  seasonal_pa %>% map(select, width:cover_vertical, -total_cover)

XFormula <-
  as.formula(paste("~", paste(colnames(Xcovariates[[1]]), collapse = "+")))

X <-
  lapply(Xcovariates, function(x)
    model.matrix(XFormula, data = x))

##################################################

plot_id <- map(seasonal_pa, select, plot_id)
year <- map(seasonal_pa, select, year)

plot_id %>% bind_rows(.id = "season")

StudyDesign = list()

for (i in 1:3) {
  StudyDesign[[i]] <- data.frame(plot_id = plot_id[[i]],
                                 year = year[[i]]) %>%
    mutate_all(~ as.factor(.)) %>%
    as.data.frame()
}

##################################################

XY <- seasonal_data %>% map(select, X = eastings, Y = northings)

sRL <- XY %>% map(as.data.frame) %>% map(distinct)

for (i in 1:3) {
  rownames(sRL[[i]]) <- unique(StudyDesign[[i]]$plot_id)
}

##################################################

SpatialRandomLevel <-
  lapply(sRL, function(x)
    HmscRandomLevel(sData = x))

YearRandomLevel = list()

for (i in 1:3) {
  YearRandomLevel[[i]] <-
    HmscRandomLevel(units = levels(StudyDesign[[i]]$year))
}

## Random levels:
rl = list(
  list("plot_id" = SpatialRandomLevel[[1]], "year" = YearRandomLevel[[1]]),
  list("plot_id" = SpatialRandomLevel[[2]], "year" = YearRandomLevel[[2]]),
  list("plot_id" = SpatialRandomLevel[[3]], "year" = YearRandomLevel[[3]])
)


##################################################

models = list()

# This is a full model with co-occuring predators
for (i in 1:3) {
  models[[i]] <- Hmsc(
    # A matrix of species occurence/abundance records
    Y = as.matrix(Y[[i]]),
    # A formula object for linear regression
    XFormula = XFormula,
    # A matrix of measured covariates
    X = X[[i]],
    # Distribution
    distr = "lognormal poisson",
    # Study Design
    studyDesign = StudyDesign[[i]],
    # Random Levels
    ranLevels = rl[[i]]
  )
}

names(models) <- c("Spring", "Summer", "Autumn")

##################################################
# Create File Path if doesnt exist
ifelse(!dir.exists(file.path("Models", "Seasonal_Abundance")), dir.create(file.path("Models", "Seasonal_Abundance")), FALSE)

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
  thin = 100
  samples = 1000
  adaptNf = rep(ceiling(0.4 * samples * thin), 1)
  transient = ceiling(0.5 * samples * thin)
  verbose = 500 * thin
}

##################################################
## Run models in loop:

for (i in seq_along(models)) {
  # Timings
  ptm = proc.time()
  
  output = try(sampleMcmc(
    models[[i]],
    thin = thin,
    samples = samples,
    transient = transient,
    #adaptNf = adaptNf,
    nChains = nChains,
    verbose = verbose,
    nParallel = nChains
  ))
  # Timings
  computationtime = proc.time() - ptm
  
  # Filename Saving
  filename = file.path("Models", "Seasonal_Abundance", paste0(names(models)[[i]], ".RData"))
  # Save file
  save(output, file = filename, computationtime)
}

##################################################
