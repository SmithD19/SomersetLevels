##################################################
## Project:
## Script purpose:
## Date: 2020-08-17
## Author: Daniel Smith
## Contact: dansmi@ceh.ac.uk
## Licence: MIT
##################################################

## load up the packages we will need:
library(tidyverse)
library(Hmsc)

## Plotting tools
library(ggcorrplot)
library(igraph)
library(ggraph)
library(tidygraph)
library(RColorBrewer)
library(ggridges)

## Assign
load("Models/Abundance/Model-250000.RData")
abu25000 <- output
## Assign
load("Models/Abundance/Model.RData")
abundance <- output
## Assign
load("Models/AbundanceHurdle/Model.RData")
abuhur <- output
## Assign
load("Models/PA/Model.RData")
pa <- output
## List
# model_list <- list(AbundanceShort = abundance, AbundanceLong = abu25000, AbundanceHurdle = abuhur, PresenceAbsence = pa)
model_list <- list(Abundance = abu25000, PresenceAbsence = pa)

## Species ~ Covariates ----------------------------------------------------

cormat <- list()

for (i in seq_along(model_list)) {
  post <- getPostEstimate(model_list[[i]], parName = "Beta")
  
  plotCustomBeta <- function(post, model, sl) {
    ## the support levels and values from the post values calculated by
    ## getPostEstimate()
    mbeta = post$mean
    betaP = post$support
    ## Support level to plot
    ## Specify inside or outside loop?
    supportLevel = sl
    ## Matrix to plot
    toPlot = 2 * betaP - 1
    toPlot = toPlot * ((betaP > supportLevel) + (betaP < (1 - supportLevel)) > 0.7)
    ## Store as tibble/df
    toPlot <- as.data.frame(toPlot)
    ## Give rownames
    rownames(toPlot) <- colnames(model_list[[i]]$X)
    ## Return the matrix or tibble
    return(toPlot)
  }
  
  
  cormat[[i]] <-
    plotCustomBeta(post, model = model_list[[i]], 0) %>%
    select(1:4) %>% 
    rownames_to_column() %>% 
    pivot_longer(-rowname)
  
}


names(cormat) <- names(model_list)

corplot <- bind_rows(cormat, .id = "Model")

## defaults
ggtheme = ggplot2::theme_minimal
colors = c("blue", "white", "red")
outline.color = "gray"
legend.title = "Corr"
tl.cex = 12
tl.srt = 45
pch = 4

corplotpmat <- corplot %>% mutate(value = ifelse(abs(value) > 0.9, value, 0))

## ggplot
p <- corplot %>% 
  ggplot(aes(rowname, name, fill = value)) + 
  geom_tile(color = outline.color) + 
  geom_point(data = corplotpmat, mapping = aes(x = rowname, y = name), shape = ifelse(corplotpmat$value == 0, 26, pch), size = 5) +
  # geom_text(aes(label = round(value, digits = 2)), size = 3) +
  scale_fill_gradient2(low = colors[1], 
                       high = colors[3], 
                       mid = colors[2], 
                       midpoint = 0,
                       limit = c(-1, 1), 
                       space = "Lab", 
                       name = legend.title) + 
  ggtheme() + theme(axis.text.x = element_text(angle = tl.srt,
                                               vjust = 1, 
                                               size = tl.cex, hjust = 1), 
                    axis.text.y = ggplot2::element_text(size = tl.cex)) + 
  ylab("Species") +
  xlab("Covariates") +
  coord_fixed() +
  facet_wrap("Model", ncol = 1) +
  labs(title = NULL,
       subtitle = NULL)

p1 <- p + theme(plot.caption = element_text(hjust = 0.5))

p1

ggsave("Plots/CovCorrelationValues.png", p1)

## Species Residiual Covariance --------------------------------------------

rhynemat = list()
rhynepmat = list()

for (i in seq_along(model_list)) {
  
  OmegaCor <- computeAssociations(model_list[[i]])
  supportLevel = 0.9
  
  ## These are the rhyne level residual correlations
  toplotrhyne <-
    ((OmegaCor[[1]]$support > supportLevel) +
       (OmegaCor[[1]]$support < (1 - supportLevel)) > 0) * OmegaCor[[1]]$mean
  
  rhynemat[[i]] <- OmegaCor[[1]]$mean
  rhynepmat[[i]] <- OmegaCor[[1]]$support
  
}

names(rhynemat) <- names(model_list)
names(rhynepmat) <- names(model_list)

rhynemat <- rhynemat %>% 
  map(as.data.frame) %>%
  map(select, 1:4) %>% 
  map(rownames_to_column) %>% 
  map(pivot_longer, -rowname) %>% 
  bind_rows(.id = "Model")

rhynepmat <- rhynepmat %>% 
  map(as.data.frame) %>% 
  map(select, 1:4) %>% 
  map(rownames_to_column) %>% 
  map(pivot_longer, -rowname) %>% 
  bind_rows(.id = "Model")

p.mat <- rhynepmat %>% mutate(value = ifelse(abs(value) > 0.9, value, 0))

p2 <- ggplot(data = rhynemat, aes(x = rowname, y = name, fill = value)) +
  geom_tile(color = outline.color) + 
  geom_point(data = p.mat, mapping = aes(x = rowname, y = name), shape = ifelse(p.mat$value == 0, 26, pch), size = 5) +
  # geom_text(aes(label = round(value, digits = 2)), size = 3) +
  scale_fill_gradient2(low = colors[1], 
                       high = colors[3], 
                       mid = colors[2], 
                       midpoint = 0,
                       limit = c(-1, 1), 
                       space = "Lab", 
                       name = legend.title) + 
  ggtheme() + theme(axis.text.x = element_text(angle = tl.srt,
                                               vjust = 1, 
                                               size = tl.cex, hjust = 1), 
                    axis.text.y = ggplot2::element_text(size = tl.cex)) +
  coord_fixed() +
  xlab(NULL) + ylab(NULL) +
  facet_wrap("Model", ncol = 1)

p2

ggsave("Plots/SppCorrelationValues.png", p2)

library(ggraph)

# Network figures
rhynemat %>%
  group_split(Model) %>%
  map(select, -Model) %>% 
  map(as_tbl_graph) %>%
  lapply(., function(plot, ...) {
  ggraph(graph = plot, layout = "linear", circular = TRUE) +
  geom_node_point() +
  geom_edge_arc(aes(color = value), edge_width = 1) +
  geom_node_label(aes(label = name)) +
  scale_edge_color_gradient2(name = "Correlation", low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_graph() +
  theme(legend.position = "bottom")
  }
)

# Comparative plotting ----------------------------------------------------

# Because they have different response matrix sizes we need a function to combine
# uneven vectors:

# A function to bind a list of uneven vectors together into a data frame
bind_uneven <- function(x = list) {
  vectorlist <- map(x, as.vector)
  lengthened <-
    map(vectorlist, `length<-`, max(lengths(vectorlist)))
  bind_cols(lengthened)
}

# Directory to store comparative plots
comparisondir <- file.path("Plots/")

# Effective samples comparison - Beta
effective_samples_Beta <- lapply(model_list, function(x) {
  mpost <- convertToCodaObject(x)
  es.beta <- effectiveSize(mpost$Beta)
  return(es.beta)
})

# Plot - Beta
bind_uneven(effective_samples_Beta) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = name, y = value)) +
  geom_boxplot() +
  xlab("Model Type") +
  ylab("Effective Sample Size Beta") +
  theme_minimal()

ggsave(
  paste0(comparisondir, "/ess_Beta.png"),
  width = 7,
  height = 5,
  dpi = 300
)

# Effective samples comparison - V
effective_samples_V <- lapply(model_list, function(x) {
  mpost <- convertToCodaObject(x)
  es.V <- effectiveSize(mpost$V)
  return(es.V)
})

# Plot - V
V <- bind_uneven(effective_samples_V) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = name, y = value)) +
  geom_boxplot() +
  xlab("Model Type") +
  ylab("Effective Sample Size V-Cov") +
  theme_Publication()

ggsave(plot = V,
  paste0(comparisondir, "/ess_V.png"),
  width = 7,
  height = 5,
  dpi = 300
)

# Effective samples comparison - Gamma
effective_samples_Gamma <- lapply(model_list, function(x) {
  mpost <- convertToCodaObject(x)
  es.Gamma <- effectiveSize(mpost$Gamma)
  return(es.Gamma)
})

# Plot - Gamma
bind_uneven(effective_samples_Gamma) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = name, y = value)) +
  geom_boxplot() +
  xlab("Model Type") +
  ylab("Effective Sample Size Gamma") +
  theme_Publication()

ggsave(
  paste0(comparisondir, "/ess_Gamma.png"),
  width = 7,
  height = 5,
  dpi = 300
)

# Model mixing and convergence
convergence <- lapply(model_list, function(x) {
  mpost <- convertToCodaObject(x)
  ge.beta <- gelman.diag(mpost$Beta, multivariate = FALSE)$psrf
  return(ge.beta)
})

# Plot - mixing
bind_uneven(convergence) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value, y = name, fill = name)) +
  geom_density_ridges() +
  # scale_fill_viridis_d() +
  geom_vline(col = "red", xintercept = 1.01) +
  ylab("Model Type") +
  xlab("R Hat value of mixing chains") +
  xlim(0.99, 1.1) +
  theme_Publication() +
  theme(legend.position = "none")
  

ggsave(
  paste0(comparisondir, "/convergence.png"),
  width = 7,
  height = 5,
  dpi = 300
)


# CrossValidation Results -------------------------------------------------

# These are the cross validation results for the Presence Absence modeling
MFCV <- R.utils::loadToEnv("Models/CV-PA.RData")[["MFCV"]]
MF <- R.utils::loadToEnv("Models/CV-PA.RData")[["MF"]]

# Species names
sppname <- model_list$PresenceAbsence$Y %>% colnames()

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

ggsave(CVPlot, filename = "Plots/CV-AUC-PA.png", dpi = 300, device = "png")



# Variance Partitioning ---------------------------------------------------


# VP 

vp2df <- function(vp) {
  # A function to take a variance partition generated by Hmsc
  # and turn it into a dataframe
  x <- as.data.frame(vp$vals)
  x <- rownames_to_column(x, "fixeff")
  return(x)
}

## Load
AbundanceLong <- R.utils::loadToEnv("Models/Abundance/Model-250000.RData")[["output"]]
AbundanceShort <- R.utils::loadToEnv("Models/Abundance/Model.RData")[["output"]]
AbundanceHurdle <- R.utils::loadToEnv("Models/AbundanceHurdle/Model.RData")[["output"]]
PresenceAbsence <- R.utils::loadToEnv("Models/PA/Model.RData")[["output"]]

# List
model_list <- list(AbundanceShort = AbundanceShort, AbundanceLong = AbundanceLong, AbundanceHurdle = AbundanceHurdle, PresenceAbsence = PresenceAbsence)
model_list <- list(Abundance = AbundanceLong, PresenceAbsence = PresenceAbsence)

# VP
vp <- lapply(model_list, computeVariancePartitioning)

# convert to plotting DF
vpdf <- lapply(vp, vp2df) %>% 
  bind_rows(.id = "Model") %>% 
  pivot_longer(c(-Model, -fixeff))


# Plot

vp <- vpdf %>% 
  ggplot(aes(x = name, y = value, fill = fixeff)) +
  geom_col() +
  xlab("Species") +
  ylab("Variance Proportion") +
  labs(fill = "Effects") +
  scale_fill_brewer(palette ="Paired") +
  facet_wrap(~ Model, ncol = 1) +
  #theme_Publication() +
  theme(axis.text.x = element_text(angle = tl.srt,
                                   vjust = 1, 
                                   size = tl.cex, hjust = 1), 
        axis.text.y = ggplot2::element_text(size = tl.cex))


ggsave(plot = vp, "Plots/VariancePartition.png", dpi = 300, width = 7, height = 5)






