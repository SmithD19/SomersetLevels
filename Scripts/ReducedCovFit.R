## load up the packages we will need:

library(tidyverse)
library(Hmsc)

## ---------------------------

assign('Spring', get(load("Models/ReducedCov/Spring.RData")))
assign('Summer', get(load("Models/ReducedCov/Summer.RData")))
assign('Autumn', get(load("Models/ReducedCov/Autumn.RData")))

model_list = list(Spring = Spring,
                  Summer = Summer,
                  Autumn = Autumn)

# -------------------------------------------------------------------------

partition = lapply(model_list, function(x) createPartition(x, nfolds = 5, column = "site"))

MF = list()
MFCV = list()

for (i in 1:3) {
  # Explanatory Power - Not Cross Validated
  preds = computePredictedValues(model_list[[i]])
  MF[[i]] = evaluateModelFit(hM = model_list[[i]], predY = preds)  
  # Predictive Power - Cross Validated
  preds = computePredictedValues(model_list[[i]], partition = partition[[i]], nParallel = 4)
  MFCV[[i]] = evaluateModelFit(hM = model_list[[i]], predY = preds)
}


save(MF, MFCV, file = "Models/ReducedCov/PredictExplanPower.RData")