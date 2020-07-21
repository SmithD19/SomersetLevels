## ---------------------------
##
## Script name: Model Fit Evaluation
##
## Purpose of script: evaluate predictive and explanatory power of our models
##
## Author: Daniel Smith
##
## Date Created: 2020-06-25
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
library(Hmsc)

## ---------------------------

assign('Spring', get(load("Models/SpikeSlab/Spring.RData")))
assign('Summer', get(load("Models/SpikeSlab/Summer.RData")))
assign('Autumn', get(load("Models/SpikeSlab/Autumn.RData")))

model_list = list(Spring = Spring,
                  Summer = Summer,
                  Autumn = Autumn)

# -------------------------------------------------------------------------

partition = lapply(model_list, function(x) createPartition(x, nfolds = 10, column = "site"))

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


save(MF, MFCV, file = "Models/SpikeSlab/PredictExplanPower.RData")

# AUC ---------------------------------------------------------------------

# load("Models/SpikeSlab/PredictExplanPower.RData")
# # AUC value of 0.5 indicates as good as just chance. 1 indicates perfect discrimination of presence between sites
# 
# # Explanatory power
# explanAUC <- MF %>% map_depth(.depth = 0, pluck, "AUC")
# names(explanAUC) <- names(model_list)
# explanAUC %>% map(mean, na.rm = T)
# 
# # Predictive power
# predictAUC <- MFCV %>% map_depth(.depth = 0, pluck, "AUC")
# names(predictAUC) <- names(model_list)
# predictAUC %>% map(mean, na.rm = T)
# 
# # For each individual mosquito
# # Explanatory
# eAUCframe <- explanAUC %>% bind_rows() %>% as.data.frame()
# rownames(eAUCframe) <- colnames(model_list$Spring$Y)
# eAUCframe[1:5, ] %>% t()
# 
# # Predictive
# pAUCframe <- predictAUC %>% bind_rows() %>% as.data.frame()
# rownames(pAUCframe) <- colnames(model_list$Spring$Y)
# pAUCframe[1:5, ] %>% t()
# 
# # We are losing predictive power in summer for a lot of species
# # Must make a graph denoting explantory vs predictive power
# 
# # TjurR2 ---------------------------------------------------------------------
# 
# # TjurR2 value of 0 indicates as good as just chance. 1 indicates perfect discrimination of presence between sites
# 
# # Overall Explanatory power
# explanTjurR2 <- MF %>% map_depth(.depth = 0, pluck, "TjurR2")
# names(explanTjurR2) <- names(model_list)
# explanTjurR2 %>% map(mean, na.rm = T) %>% t()
# 
# # Overall Predictive power
# predictTjurR2 <- MFCV %>% map_depth(.depth = 0, pluck, "TjurR2")
# names(predictTjurR2) <- names(model_list)
# predictTjurR2 %>% map(mean, na.rm = T) %>% t()
# 
# # For each individual mosquito
# 
# # Explanatory
# eTjurR2frame <- explanTjurR2 %>% bind_rows() %>% as.data.frame()
# rownames(eTjurR2frame) <- colnames(model_list$Spring$Y)
# eTjurR2frame[1:5, ]
# 
# # Predictive
# pTjurR2frame <- predictTjurR2 %>% bind_rows() %>% as.data.frame()
# rownames(pTjurR2frame) <- colnames(model_list$Spring$Y)
# pTjurR2frame[1:5, ]
# 
# 
# # WAIC --------------------------------------------------------------------
# 
# WAIC <- lapply(model_list, computeWAIC)
# 
# 
# # Plot --------------------------------------------------------------------
# 
# # AUC Plotting
# AUC <- list(eAUCframe, pAUCframe) %>% 
#   map(rownames_to_column, "Species") %>%
#   map(slice, 1:5) %>% 
#   bind_rows(.id = "Type") %>% 
#   mutate(Type = if_else(Type == 1, "Explanatory", "Predictive")) %>% 
#   pivot_longer(cols = c(-Species, -Type), names_to = "Season", values_to = "AUC")
# 
# AUC %>% 
#   ggplot(aes(y = AUC, x = AUC, col = Species)) +
#   geom_line()
# 
# # AUC2
# AUC.e <- eAUCframe %>% 
#   rownames_to_column("Species") %>% 
#   slice(1:5) %>% 
#   pivot_longer(cols = -Species, names_to = "Season", values_to = "AUC.e")
# 
# AUC.p <- pAUCframe %>% 
#   rownames_to_column("Species") %>% 
#   slice(1:5) %>% 
#   pivot_longer(cols = -Species, names_to = "Season", values_to = "AUC.p")
# 
# AUC <- left_join(AUC.e, AUC.p)
# 
# library(patchwork)
# 
# p.e <- AUC %>% 
#   ggplot(aes(x = Species, fill = Season)) +
#   geom_col(aes(y = AUC.e), position = position_dodge())
# 
# p.p <- AUC %>% 
#   ggplot(aes(x = Species, fill = Season)) +
#   geom_col(aes(y = AUC.p), position = position_dodge())
# 
# p.e + p.p  


