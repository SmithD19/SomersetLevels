## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Daniel Smith
##
## Date Created: 2020-06-24
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

# More plotting libraries

library(ggridges)

## ---------------------------

load("Models/Full/Model.RData")

model_list = list(Full = output)


# Directory Creating for storing plots - If doesn't exist then create it in the current root
ifelse(!dir.exists(file.path("Models", "Full_Plots")), dir.create(file.path("Models", "Full_Plots")), FALSE)


for (i in seq_along(model_list)) {
  plotdirectory <-
    file.path("Models", "Full_Plots", names(model_list)[[i]])
  
  # Create directory to store plots if isn't present
  if (!dir.exists(file.path(plotdirectory))) {
    print(paste(
      "Creating directory to store plots of model",
      names(model_list)[[i]]
    ))
    dir.create(file.path(plotdirectory))
  } else {
    print("Directory already exists")
  }
  
  # Plot Beta Correlations ----
  post <- getPostEstimate(model_list[[i]], parName = "Beta")
  
  plotCustomBeta <- function(post, model){
    # the support levels and values from the post values calculated by 
    # getPostEstimate()
    mbeta = post$mean
    betaP = post$support
    # Support level to plot
    # Specify inside or outside loop?
    supportLevel = 0.75
    # Matrix to plot 
    toPlot = 2 * betaP - 1
    toPlot = toPlot * ((betaP > supportLevel) + (betaP < (1 - supportLevel)) > 0)
    # Store as tibble/df
    toPlot <- as.data.frame(toPlot)
    # Give rownames
    rownames(toPlot) <- colnames(model_list[[i]]$X)
    # Return the matrix or tibble
    return(toPlot)
  }
  
  plotCustomBeta(post = post, model = model_list[[i]]) %>% 
    ggcorrplot(legend.title = "Correlation",
               title = paste("Beta Correlation 75%", names(model_list)[[i]]), tl.cex = 15, type = "full") +
    xlab("Covariates") +
    ylab("Species")
  
  # Save this in the correct spot and title it properly
  ggsave(filename = paste0(plotdirectory, "/Beta_", names(model_list)[[i]], ".png"),
         dpi = 300, height = 7, width = 7)
  
  
  # Does the model have random levels?
  if (length(model_list[[i]]$rL) == 0) {
    print(paste(
      "No random levels to check correlations for in model:",
      names(model_list)[[i]]
    ))
    
  } else {
    print(paste(
      "Plotting residual correlations in random levels for model:",
      names(model_list)[[i]]
    ))
    
    # Correlations ----
    OmegaCor <- computeAssociations(model_list[[i]])
    supportLevel = c(0.95, 0.75, 0.5)
    
    
    # These are the site level residual correlations. Goes through three levels of support and plots all
    # both seperately and together
    
    OmegaCor <- computeAssociations(model_list[[i]])
    supportLevel = c(0.95, 0.75, 0.5)
    
    
    # These are the site level residual correlations. Goes through three levels of support and plots all
    # both seperately and together
    
    # Plotting loop for multiple support levels
    for (j in seq_along(supportLevel)) {
      # These are the site level correlations
      toplotsite <-
        ((OmegaCor[[1]]$support > supportLevel[[j]]) +
           (OmegaCor[[1]]$support < (1 - supportLevel[[j]])) > 0) * OmegaCor[[1]]$mean
      
      # These are the rhyne level residual correlations
      toplotrhyne <-
        ((OmegaCor[[2]]$support > supportLevel[[j]]) +
           (OmegaCor[[2]]$support < (1 - supportLevel[[j]])) > 0) * OmegaCor[[2]]$mean
      
      # These are the year level residual correlations
      toplotyear <-
        ((OmegaCor[[3]]$support > supportLevel[[j]]) +
           (OmegaCor[[3]]$support < (1 - supportLevel[[j]])) > 0) * OmegaCor[[3]]$mean
      
      # Plot site level stuff -----
      
      # Correlation figures
      ggcorrplot(
        toplotsite,
        method = "square",
        typ = "full",
        title = paste0(
          "Spatial Level Residual Correlations ",
          supportLevel[[j]],
          " Support"
        ),
        tl.cex = 15
      )
      
      # Save correlation figures here
      ggsave(
        filename = paste0(
          plotdirectory,
          "/SiteCor_",
          names(model_list)[[i]],
          "_",
          supportLevel[[j]],
          ".png"
        ),
        dpi = 300
      )
      
      # Network figures
      toplotsite %>%
        as_tbl_graph() %>%
        ggraph(layout = "linear", circular = TRUE) +
        geom_node_point() +
        geom_edge_arc(aes(color = weight), edge_width = 1) +
        geom_node_label(aes(label = name)) +
        scale_edge_color_gradient2(name = "Correlation") +
        theme_graph() +
        theme(legend.position = "bottom")
      
      # Save network figures here
      ggsave(
        filename = paste0(
          plotdirectory,
          "/SiteNet_",
          names(model_list)[[i]],
          "_",
          supportLevel[[j]],
          ".png"
        ),
        dpi = 300
      )
      
      # Plot rhyne level stuff -----
      
      # Plot correlation figure
      ggcorrplot(
        toplotrhyne,
        method = "square",
        typ = "full",
        title = paste0(
          "Seasonal Level Residual Correlations ",
          supportLevel[[j]],
          "% Support"
        )
      )
      
      # Save
      ggsave(width = 12, height = 12,
             filename = paste0(
               plotdirectory,
               "/RhyneCor_",
               names(model_list)[[i]],
               "_",
               supportLevel[[j]],
               ".png"
             ),
             dpi = 300
      )
      
      # Plot network figure
      toplotrhyne %>%
        as_tbl_graph() %>%
        ggraph(layout = "linear", circular = TRUE) +
        geom_node_point() +
        geom_edge_arc(aes(color = weight), edge_width = 1) +
        geom_node_label(aes(label = name), size = 5.5) +
        scale_edge_color_gradient2(name = "Correlation", low = "blue", high = "red") +
        theme_graph() +
        theme(legend.position = "bottom") +
        ggtitle(paste0(
          "Rhyne Level Residual Correlations ",
          supportLevel[[j]],
          " Support"
        )) +
        # Expand labels for text to fit
        scale_x_continuous(expand = c(.1, .1))
      
      # Save
      ggsave(
        filename = paste0(
          plotdirectory,
          "/RhyneNet_",
          names(model_list)[[i]],
          "_",
          supportLevel[[j]],
          ".png"
        ),
        dpi = 300, width = 10, height = 10
      )
      
      # testing removing empty nodes and edges
      g <-  toplotrhyne %>% as_tbl_graph()
      
      g %>% activate(edges) %>% filter(weight != 0) %>% 
        ggraph(layout = "linear", circular = TRUE) +
        geom_node_point() +
        geom_edge_arc(aes(color = weight), edge_width = 1) +
        geom_node_label(aes(label = name), size = 5.5) +
        scale_edge_color_gradient2(name = "Correlation", low = "blue", high = "red") +
        theme_graph() +
        theme(legend.position = "bottom") +
        ggtitle(paste0(
          "Rhyne Level Residual Correlations ",
          supportLevel[[j]],
          " Support"
        )) +
        # Expand labels for text to fit
        scale_x_continuous(expand = c(.1, .1))
      
      # Save
      ggsave(
        filename = paste0(
          plotdirectory,
          "/TestEdges_",
          names(model_list)[[i]],
          "_",
          supportLevel[[j]],
          ".png"
        ),
        dpi = 300, width = 10, height = 10
      )
      
    }
    
  }
}


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
comparisondir <- file.path("Models", "Full_Plots")

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


