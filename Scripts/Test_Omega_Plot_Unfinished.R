## ---------------------------
##
## Script name: Custom plotting of Omega associations
##
## Purpose of script:
##
## Author: Daniel Smith
##
## Date Created: 2020-11-19
##
## Email: dansmi@ceh.ac.uk
##
## ---------------------------

library(readxl)
library(tidyverse)

read_excel_allsheets <- function(filename, tibble = TRUE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <-
    lapply(sheets, function(X)
      readxl::read_excel(filename, sheet = X))
  if (!tibble)
    x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

omega_pa <- read_excel_allsheets("Panels/parameter_estimates_Omega_PresenceAbsence_plot_id.xlsx")



clean_omega <- function(x) {
  
  # Assign params
  params = list(pos = x$`Pr(x>0)`, neg = x$`Pr(x<0)`)
  
  # Reshaping function
  omega_reshape = 
    function(x) {reshape2::melt(as.matrix(column_to_rownames(x, var = "...1")))}
   
  # Reshape both posotive and negative associations
  params = lapply(params, omega_reshape)
  
  # Make negative associations negative
  params$neg[,3] = params$neg[,3] * -1
  
  return(params)
  
  }

omega_tile <- omega_pa %>% 
  clean_omega() %>% 
  bind_rows() %>% 
  filter(abs(value) >= 0.9)

omega_tile %>% 
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  #scale_fill_gradient2(low = "#67001F", mid = "#FFFFFF", high = "#053061") +
  scale_fill_gradientn(colours = c("#67001F",
                             "#B2182B",
                             "#D6604D",
                             "#F4A582",
                             "#FDDBC7",
                             "#FFFFFF",
                             "#D1E5F0",
                             "#92C5DE",
                             "#4393C3",
                             "#2166AC",
                             "#053061"))
  coord_fixed()



# # igraphing it up? --------------------------------------------------------
# 
# library(igraph)
# 
# my_adj_list <- omega_tile
# 
# names(my_adj_list) <- c("from", "to", "weight")
# 
# my_adj_list <- my_adj_list %>% 
#   mutate(association = if_else(weight > 0, "Positive", "Negative"),
#                        weight = abs(weight))
# 
# 
# 
# # create igraph S3 object
# net <- graph.data.frame(my_adj_list, directed = FALSE) %>% simplify()
# 
# # community detection based on edge betweenness (Newman-Girvan)
# ceb <- cluster_fast_greedy(net)
# 
# plot(ceb, net)
# 
# # community membership for each node
# membership(ceb)
# 
# 
# 


