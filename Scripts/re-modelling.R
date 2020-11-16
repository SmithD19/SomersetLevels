library(Hmsc)
library(corrplot)
library(tidyverse)

# Any model previously ran in HMSC
model

# The output of this model after fitting
output

# initParList from this last model fitted?
initParList <- dplyr::last(output$postList[[1]])

# Rerun the model for an extra period of time
reoutput <-
  sampleMcmc(
    hM = model,
    samples = 1000,
    thin = 2,
    initPar = initParList,
    nChains = 8,
    nParallel = nChains
  )

# Results
results <- list(original = output, remodeled = reoutput)

# Check PSRF
for (i in seq_along(results)) {
  mpost = convertToCodaObject(results[[i]])
  
  par(mfrow = c(3, 2))
  
  ess.beta = effectiveSize(mpost$Beta)
  psrf.beta = gelman.diag(mpost$Beta, multivariate = FALSE)$psrf
  hist(ess.beta)
  hist(psrf.beta)
  ess.gamma = effectiveSize(mpost$Gamma)
  psrf.gamma = gelman.diag(mpost$Gamma, multivariate = FALSE)$psrf
  hist(ess.gamma)
  hist(psrf.gamma)
  sppairs = matrix(sample(x = 1:output$ns ^ 2, size = 100))
  tmp = mpost$Omega[[1]]
  for (chain in 1:length(tmp)) {
    tmp[[chain]] = tmp[[chain]][, sppairs]
  }
  ess.omega = effectiveSize(tmp)
  psrf.omega = gelman.diag(tmp, multivariate = FALSE)$psrf
  hist(ess.omega)
  hist(psrf.omega)
  
  par(mfrow = c(1, 1))
  
}

# Check Plots
for (i in seq_along(results)) {
  
  beta <- getPostEstimate(results[[i]], parName = "Beta")
  
  mar.default <- c(5, 4, 4, 2) + 0.1
  par(mar = mar.default + c(0, 4, 0, 0))
  
  plotBeta(
    results[[i]],
    beta,
    supportLevel = 0.90,
    # Cov spp names or labels?
    covNamesNumbers = c(F, T),
    # Colouring
    colors = colorRampPalette(
      c(
        "#67001F",
        "#B2182B",
        "#D6604D",
        "#F4A582",
        "#FDDBC7",
        "#FFFFFF",
        "#D1E5F0",
        "#92C5DE",
        "#4393C3",
        "#2166AC",
        "#053061"
      )
    ),
    colorLevels = 200,
  )
  
  # Omega
  OmegaCor <- computeAssociations(results[[i]])
  supportLevel = 0.90
  
  toplotrhyne <-
    ((OmegaCor[[1]]$support > supportLevel) + (OmegaCor[[1]]$support < (1 - supportLevel)) > 0) * OmegaCor[[1]]$mean
  
  corrplot(toplotrhyne, method = "circle", tl.col = "black", title = paste(names(results[i])))

    
}


# -------------------------------------------------------------------------

# What about mean parameters over all chains?

length(output$postList) # 8 chains here

# Use foo as testbed for pipeline
foo <- lapply(output$postList, last)

# Store mean paramaters
param <- list()

# Store temporary matrices
tmp <-  list()

for (i in seq_along(foo[[1]])) {
  
 
  # Pull paramters from all 8 chains into one list
  for (j in seq_along(foo)) {

    tmp[[j]] <- foo[[j]][[i]]
    
  }
  
  if (is.matrix(tmp[1])) {
  
    # Convert to an array 
    tmp.array <- array(unlist(tmp), dim = c(nrow(tmp[[1]]), ncol(tmp[[1]]), length(tmp)))
    
    # Get the means of this array to produce a single matrix
    param[[i]] <-  rowMeans(tmp.array, dim = 2)
    
    }

  # Name the list element with the parameter name used in the model
  names(param[[i]]) <- names(foo[i])
  
  
}

# Example
L <- list(matrix(rnorm(6), nrow = 3), matrix(rnorm(6), nrow = 3))

array(unlist(foo), dim = c(nrow(foo[[1]]), ncol(foo[[1]]), length(L)))





