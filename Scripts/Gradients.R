library(Hmsc)

# How does water_temp impact species abundance?
Grad <-
  constructGradient(model_list$Abundance, focalVariable = "water_temp",)

# not exactly sure what the non-focal variables do
#non.focalVariables = list("management" = list(1), "dissolved_oxygen" = list(1), "ph" = list(1))

predY1 <-
  predict(model_list$Abundance,
          Gradient = Grad,
          expected = TRUE)

plotGradient(
  model_list$Abundance,
  Grad,
  pred = predY1,
  measure = "Y",
  showData = TRUE,
  index = 1
)



variables <- model_list$Abundance$XData %>% colnames()

for (i in seq_along(variables)) {

  print(variables[i])
  
  Grad <-
    constructGradient(model_list$Abundance, focalVariable = variables[i])
  
  predY1 <-
    predict(model_list$Abundance,
            Gradient = Grad,
            expected = TRUE)

  # Filepath to save too
  dir <- file.path("Plots","GradsALL/")
  
  # Start PNG
  png(filename = paste0(dir, variables[i], "_grad.png"))
  
  # Call 2x2 panel
  par(mfrow = c(4, 3))
  
  # Plot 4 mossie species
  plot <- for (j in 1:12) {
    plotGradient(
      model_list$Abundance,
      Grad,
      pred = predY1,
      measure = "Y",
      showData = TRUE,
      index = j
    )
  }
  
  # Save
  dev.off()
  
  # Set plotting back to normal
  par(mfrow = c(1,1))
  
}

# Presence Abscence -------------------------------------------------------

for (i in seq_along(variables)) {
  
  print(variables[i])
  
  Grad <-
    constructGradient(model_list$PresenceAbsence, focalVariable = variables[i])
  
  predY2 <-
    predict(model_list$PresenceAbsence,
            Gradient = Grad,
            expected = TRUE)
  
  # Filepath to save too
  dir <- file.path("Plots","GradsPA/")
  
  # Start PNG
  png(filename = paste0(dir, variables[i], "_grad_PA.png"))
  
  # Call 2x2 panel
  par(mfrow = c(2, 2))
  
  # Plot 4 mossie species
  plot <- for (j in 1:4) {
    plotGradient(
      model_list$Abundance,
      Grad,
      pred = predY2,
      measure = "Y",
      showData = TRUE,
      index = j
    )
  }
  
  # Save
  dev.off()
  
  # Set plotting back to normal
  par(mfrow = c(1,1))
  
}












