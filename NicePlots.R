## ---------------------------
##
## Script name:
##
## Purpose of script:
##
## Author: Daniel Smith
##
## Date Created: 2020-07-29
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

## Plotting tools

library(ggcorrplot)
library(igraph)
library(ggraph)
library(tidygraph)
library(RColorBrewer)

## ---------------------------

assign('Spring', get(load(
  "Models/Seasonal_Full/Spring.RData"
)))
assign('Summer', get(load(
  "Models/Seasonal_Full/Summer.RData"
)))
assign('Autumn', get(load(
  "Models/Seasonal_Full/Autumn.RData"
)))

model_list = list(Spring = Spring,
                  Summer = Summer,
                  Autumn = Autumn)

cormat <- list()

for (i in seq_along(model_list)) {
  post <- getPostEstimate(model_list[[i]], parName = "Beta")
  
  plotCustomBeta <- function(post, model, sl) {
    # the support levels and values from the post values calculated by
    # getPostEstimate()
    mbeta = post$mean
    betaP = post$support
    # Support level to plot
    # Specify inside or outside loop?
    supportLevel = sl
    # Matrix to plot
    toPlot = 2 * betaP - 1
    toPlot = toPlot * ((betaP > supportLevel) + (betaP < (1 - supportLevel)) > 0.7)
    # Store as tibble/df
    toPlot <- as.data.frame(toPlot)
    # Give rownames
    rownames(toPlot) <- colnames(model_list[[i]]$X)
    # Return the matrix or tibble
    return(toPlot)
  }
  
  
  cormat[[i]] <-
    plotCustomBeta(post, model = model_list[[i]], 0) %>%
    select(1:4) %>% 
    rownames_to_column() %>% 
    pivot_longer(-rowname)
  
}


names(cormat) <- names(model_list)

corplot <- bind_rows(cormat, .id = "Season")

# defaults
ggtheme = ggplot2::theme_minimal
colors = c("blue", "white", "red")
outline.color = "gray"
legend.title = "Corr"
tl.cex = 12
tl.srt = 45
pch = 4

corplot$Season_F <- factor(corplot$Season, levels=c("Spring", "Summer", "Autumn"))

corplotpmat <- corplot %>% mutate(value = ifelse(abs(value) > 0.95, value, 0))

# ggplot
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
  facet_wrap("Season_F", ncol=1) +
  labs(title = NULL,
       subtitle = NULL,
       caption = "Correlative interactions between mosquito species and covaraites by season. 
       Only correlative values above 70% are plotted and significant values (p < 0.05) are marked with an X")

p1 <- p + theme(plot.caption = element_text(hjust = 0.5))

ggsave("Models/Seasonal_Full/CorrelativeCov.png", plot = p1, width = 8.3, height = 11.7 ,dpi = 320)

# -------------------------------------------------------------------------
rhynemat = list()
rhynepmat = list()

for (i in seq_along(model_list)) {
  
OmegaCor <- computeAssociations(model_list[[i]])
supportLevel = 0.7

# These are the rhyne level residual correlations
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
  bind_rows(.id = "Season")

rhynepmat <- rhynepmat %>% 
  map(as.data.frame) %>% 
  map(select, 1:4) %>% 
  map(rownames_to_column) %>% 
  map(pivot_longer, -rowname) %>% 
  bind_rows(.id = "Season")


p.mat <- rhynepmat %>% mutate(value = ifelse(abs(value) > 0.95, value, 0))

rhynemat$Season_F = factor(rhynemat$Season, levels=c("Spring", "Summer", "Autumn"))
p.mat$Season_F = factor(p.mat$Season, levels=c("Spring", "Summer", "Autumn"))


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
  facet_wrap("Season_F", ncol = 1) +
  labs(
    caption = "Residual correlative interactions at the rhyne level between mosquito and predator species stratified by season. 
    Only correlative values above 70% are plotted and significant values (p < 0.05) are marked with an X"
  )

ggsave("Models/Seasonal_Full/CorrelativeSpecies.png", plot = p2, width = 8.3, height = 11.7 ,dpi = 320)





