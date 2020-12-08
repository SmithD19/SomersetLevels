# CrossValidation Results -------------------------------------------------
library(tidyverse)

# These are the cross validation results for the Presence Absence modeling
load("Models/CV-PaExtended.RData")

load("Models/PA_Thin300/ModelExtended.RData")

# Species names
sppname <- output$Y %>% colnames()

ExplanPred <- data.frame(
  ## AUC
  # ------
  # Explanatory power of PA
  enframe(MF[[1]]$AUC, name = NULL, value = "ExplanAUC"),
  # Predictive power of PA
  enframe(MFCV[[1]]$AUC, name = NULL, value = "PredAUC"),  
  # # Explanatory power of PA
  # enframe(MF[[1]]$TjurR2, name = NULL, value = "ExplanTjurR2"),
  # # Predictive power of PA
  # enframe(MFCV[[1]]$TjurR2, name = NULL, value = "PredTjurR2"),
  # # Explanatory power of PA
  # enframe(MF[[1]]$TjurR2, name = NULL, value = "ExplanRMSE"),
  # # Predictive power of PA
  # enframe(MFCV[[1]]$TjurR2, name = NULL, value = "PredRMSE"),
  # Spp names
  Species = sppname
)

source("../pub_themes.r")

# Plot
CVPlot <- ExplanPred %>% 
  pivot_longer(-Species, names_to = "Model", values_to = "AUC") %>% 
  ggplot(aes(x = AUC, y = Species, col = Model)) +
  geom_point(size = 5) + theme_Publication() +
  labs(
    col = NULL
  )

ggsave(CVPlot, filename = "Plots/CV-AUC-PaExtended.png", dpi = 300, device = "png")
