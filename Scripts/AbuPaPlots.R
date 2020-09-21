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
load("Models/Abundance_Final/Model.RData")
abundance <- output
## Assign
load("Models/PA_Final/Model.RData")
pa <- output
## List
model_list <- list(abu = abundance, pa = pa)

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

corplotpmat <- corplot %>% mutate(value = ifelse(abs(value) > 0.95, value, 0))

## ggplot
p <- corplot %>% 
  ggplot(aes(rowname, name, fill = value)) + 
  geom_tile(color = outline.color) + 
  geom_point(data = corplotpmat, mapping = aes(x = rowname, y = name), shape = ifelse(corplotpmat$value == 0, 26, pch), size = 5) +
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
  facet_wrap("Model", ncol=1) +
  labs(title = NULL,
       subtitle = NULL,
       caption = "Correlative interactions between mosquito species and covaraites by model type
       Only correlative values above 70% are plotted and significant values (p < 0.05) are marked with an X")

p1 <- p + theme(plot.caption = element_text(hjust = 0.5))

## Species Residiual Covariance --------------------------------------------

rhynemat = list()
rhynepmat = list()

for (i in seq_along(model_list)) {
  
  OmegaCor <- computeAssociations(model_list[[i]])
  supportLevel = 0.7
  
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

p.mat <- rhynepmat %>% mutate(value = ifelse(abs(value) > 0.95, value, 0))

p2 <- ggplot(data = rhynemat, aes(x = rowname, y = name, fill = value)) +
  geom_tile(color = outline.color) + 
  geom_point(data = p.mat, mapping = aes(x = rowname, y = name), shape = ifelse(p.mat$value == 0, 26, pch), size = 5) +
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
  facet_wrap("Model", ncol = 1) +
  labs(
    caption = "Residual correlative interactions at the rhyne level between mosquito and predator species by model type. 
    Only correlative values above 70% are plotted and significant values (p < 0.05) are marked with an X"
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
  ylab("Effective Sample Size") +
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
bind_uneven(effective_samples_V) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = name, y = value)) +
  geom_boxplot() +
  xlab("Model Type") +
  ylab("Effective Sample Size") +
  theme_minimal()

ggsave(
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
  ylab("Effective Sample Size") +
  theme_minimal()

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
  scale_fill_viridis_d() +
  geom_vline(col = "red", xintercept = 1.01) +
  ylab("Model Type") +
  xlab("R Hat value of mixing chains") +
  xlim(0.99, 1.1) +
  theme_minimal()

ggsave(
  paste0(comparisondir, "/convergence.png"),
  width = 7,
  height = 5,
  dpi = 300
)







