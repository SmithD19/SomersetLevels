#setwd("") # set directory to the folder where the folders "data", "models" and "Panels" are
library(Hmsc)
library(colorspace)
library(corrplot)
library(writexl)

# Fri Nov 06 15:06:21 2020 ------------------------------

load("Models/PA_Thin300/ModelExtended.RData")
pa <- output

load("Models/Abundance_Thin300/ModelExtended.RData")
abu <- output

models = list(PresenceAbsence = pa, Abundance = abu)

modelnames = names(models)

nm = length(models)

# Mon Nov 09 16:44:27 2020 ------------------------------
# Changed to 90% intervals just to see.

filename = paste("Panels/parameter_estimates_90.pdf")

# -------------------------------------------------------------------------

## VAriance partitioning
custom_VP <- function (hM, VP, cols = NULL, mainTitle, ...)
{
  ng = dim(VP$vals)[1]
  if (is.null(cols)) {
    cols = heat.colors(ng, alpha = 1)
  }
  leg = VP$groupnames
  for (r in 1:hM$nr) {
    leg = c(leg, paste("Random: ", hM$rLNames[r], sep = ""))
  }
  means = round(100 * rowMeans(VP$vals), 1)
  for (i in 1:ng) {
    leg[i] = paste(leg[i], " (mean = ", toString(means[i]),
                   ")", sep = "")
  }
  
  mainTitle = mainTitle
  
  barplot(
    VP$vals,
    main = mainTitle,
    xlab = NULL,
    ylab = "Variance proportion",
    las = 1,
    legend = leg,
    col = cols,
    ...
  )
}


# Edit the names of the species in models to get better plots -------------

Tax_Names <- c(
  "An. maculipennis",
  "An. claviger",
  "Cx. pipiens",
  "Cs. annulata",
  "Corixidae",
  "Coleoptera larvae",
  "Coleoptera",
  "Zygoptera larvae",
  "Anisoptera larvae",
  "Ilyocoris",
  "Nepa cinerea",
  "Gammaridae"
)

colnames(models$PresenceAbsence$Y) <- Tax_Names
colnames(models$Abundance$Y) <- Tax_Names

models$PresenceAbsence$spNames <- Tax_Names
models$Abundance$spNames <- Tax_Names

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


pdf(file = filename)

for (i in seq_along(models)) {
  
  models[[i]][["covNames"]] = c(
    "Intercept",
    "Cover",
    "Shaded",
    "Manage",
    "Width",
    "Temp",
    #"Turbidity",
    #"Salinity",
    "Oxygen",
    "pH"
  )
  
  
  m = models[[i]]
  
  group = c(1, 1, 1, 1, 1, 2, 2, 2)
  
  groupnames = c("Structural", "Physiochemical")
  
  VP = computeVariancePartitioning(m, group = group, groupnames = groupnames)
  
  vals = VP$vals
  
  mycols = rainbow(nrow(VP$vals)) 
  
  mar.default <- c(5, 4, 4, 2) + 0.1
  par(mar = mar.default + c(4, 0, 0, 0))
  
  custom_VP(
    hM = m,
    VP = VP,
    cols = mycols,
    args.leg = list(bg = "white", cex = 0.7),
    cex.main = 0.8,
    las = 2, 
    mainTitle = paste0("Grouped variance partition, ",modelnames[[i]])
  )
  
  preds = computePredictedValues(m)
  MF = evaluateModelFit(hM = m, predY = preds)
  
  R2 = NULL
  if (!is.null(MF$TjurR2)) {
    TjurR2 = MF$TjurR2
    vals = rbind(vals, TjurR2)
    R2 = TjurR2
  }
  if (!is.null(MF$SR2)) {
    R2 = MF$SR2
    vals = rbind(vals, R2)
  }
  
  filename =  paste0("Panels/grouped_parameter_estimates_VP_", modelnames[[i]], ".csv")
  write.csv(vals, file = filename)
  
  if (!is.null(R2)) {
    VPr = VP
    for (k in 1:m$ns) {
      VPr$vals[, k] = R2[k] * VPr$vals[, k]
    }
    
    VPr$vals = VPr$vals[, order(-R2)]
    
    mar.default <- c(5, 4, 4, 2) + 0.1
    par(mar = mar.default + c(4, 0, 0, 0))
    
    custom_VP(
      hM = m,
      VP = VPr,
      cols = mycols,
      args.leg = list(bg = "white", cex = 0.7),
      ylim = c(0, 1),
      cex.main = 0.8,
      las = 2,
      mainTitle = paste0("Grouped raw variance partition ",modelnames[[i]])
    )
  }
}

for (i in seq_along(models)) {
  
  models[[i]][["covNames"]] = c(
    "Intercept",
    "Cover",
    "Shaded",
    "Manage",
    "Width",
    "Temp",
    #"Turbidity",
    #"Salinity",
    "Oxygen",
    "pH"
  )
  
  
  m = models[[i]]
  
  VP = computeVariancePartitioning(m)
  
  vals = VP$vals
  
  mycols = rainbow(nrow(VP$vals)) 
  
  mar.default <- c(5, 4, 4, 2) + 0.1
  par(mar = mar.default + c(4, 0, 0, 0))
  
  custom_VP(
    hM = m,
    VP = VP,
    cols = mycols,
    args.leg = list(bg = "white", cex = 0.7),
    cex.main = 0.8,
    las = 2, 
    mainTitle = paste0("Proportion of explained variance, ",modelnames[[i]])
  )
  
  preds = computePredictedValues(m)
  MF = evaluateModelFit(hM = m, predY = preds)
  
  R2 = NULL
  if (!is.null(MF$TjurR2)) {
    TjurR2 = MF$TjurR2
    vals = rbind(vals, TjurR2)
    R2 = TjurR2
  }
  if (!is.null(MF$SR2)) {
    R2 = MF$SR2
    vals = rbind(vals, R2)
  }
  
  filename =  paste0("Panels/parameter_estimates_VP_", modelnames[[i]], ".csv")
  write.csv(vals, file = filename)
  
  if (!is.null(R2)) {
    VPr = VP
    for (k in 1:m$ns) {
      VPr$vals[, k] = R2[k] * VPr$vals[, k]
    }
    
    VPr$vals = VPr$vals[, order(-R2)]
    
    mar.default <- c(5, 4, 4, 2) + 0.1
    par(mar = mar.default + c(4, 0, 0, 0))
    
    custom_VP(
      hM = m,
      VP = VPr,
      cols = mycols,
      args.leg = list(bg = "white", cex = 0.7),
      ylim = c(0, 1),
      cex.main = 0.8,
      las = 2,
      mainTitle = paste0("Proportion of raw variance, ",modelnames[[i]])
    )
  }
}
for (j in 1:nm) {
  m = models[[j]]
  
  mar.default <- c(5, 4, 4, 2) + 0.1
  par(mar = mar.default + c(0, 4, 0, 0))
  
  postBeta = getPostEstimate(m, parName = "Beta")
  show.sp.names = (is.null(m$phyloTree) && m$ns <= 20)
  plotBeta(
    m,
    post = postBeta,
    supportLevel = 0.900,
    param = "Sign",
    plotTree = !is.null(m$phyloTree),
    covNamesNumbers = c(TRUE, FALSE),
    spNamesNumbers = c(show.sp.names, FALSE),
    cex = c(1, 1, 0.8),
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
    colorLevels = 200
  )
  
  mymain = paste0("BetaPlot, ", modelnames[[j]])
  
  if (!is.null(m$phyloTree)) {
    mpost = convertToCodaObject(m)
    rhovals = unlist(poolMcmcChains(mpost$Rho))
    mymain = paste0(mymain,
                    ", E[rho] = ",
                    round(mean(rhovals), 2),
                    ", Pr[rho>0] = ",
                    round(mean(rhovals > 0), 2))
  }
  title(main = mymain,
        line = 2.5,
        cex.main = 0.8)
  
  me = as.data.frame(t(postBeta$mean))
  me = cbind(m$spNames, me)
  colnames(me) = c("Species", m$covNames)
  po = as.data.frame(t(postBeta$support))
  po = cbind(m$spNames, po)
  colnames(po) = c("Species", m$covNames)
  ne = as.data.frame(t(postBeta$supportNeg))
  ne = cbind(m$spNames, ne)
  colnames(ne) = c("Species", m$covNames)
  vals = list(
    "Posterior mean" = me,
    "Pr(x>0)" = po,
    "Pr(x<0)" = ne
  )
  filename = paste0("Panels/parameter_estimates_Beta_", modelnames[j], ".xlsx")
  writexl::write_xlsx(vals, path = filename)
}

for (j in 1:nm) {
  if (m$nt > 1) {
    m = models[[j]]
    postGamma = getPostEstimate(m, parName = "Gamma")
    plotGamma(
      m,
      post = postGamma,
      supportLevel = 0.900,
      param = "Sign",
      covNamesNumbers = c(TRUE, FALSE),
      trNamesNumbers = c(m$nt < 21, FALSE),
      cex = c(0.6, 0.6, 0.8)
    )
    title(
      main = paste0("GammaPlot ", modelnames[[j]]),
      line = 2.5,
      cex.main = 0.8
    )
  }
}


for (j in 1:nm) {
  m = models[[j]]
  OmegaCor = computeAssociations(m)
  supportLevel = 0.900
  for (r in 1:m$nr) {
    plotOrder = corrMatOrder(OmegaCor[[r]]$mean, order = "AOE")
    toPlot = ((OmegaCor[[r]]$support > supportLevel) + (OmegaCor[[r]]$support <
                                                          (1 - supportLevel)) > 0) * sign(OmegaCor[[r]]$mean)
    if (m$ns > 20) {
      colnames(toPlot) = rep("", m$ns)
      rownames(toPlot) = rep("", m$ns)
    }
    mymain = paste0("Associations, ", modelnames[[j]], ": ", names(m$ranLevels)[[r]])
    if (m$ranLevels[[r]]$sDim > 0) {
      mpost = convertToCodaObject(m)
      alphavals = unlist(poolMcmcChains(mpost$Alpha[[1]][, 1]))
      mymain = paste0(mymain,
                      ", E[alpha1] = ",
                      round(mean(alphavals), 2),
                      ", Pr[alpha1>0] = ",
                      round(mean(alphavals > 0), 2))
    }
    corrplot(
      toPlot[plotOrder, plotOrder],
      method = "circle",
      # col = colorRampPalette(c("blue", "white", "red"))(200),
      mar = c(0, 0, 1, 0),
      main = mymain,
      cex.main = 0.8, outline = F, tl.col = "black",
      type = "full"
    )
    
    me = as.data.frame(OmegaCor[[r]]$mean)
    me = cbind(m$spNames, me)
    colnames(me)[1] = ""
    po = as.data.frame(OmegaCor[[r]]$support)
    po = cbind(m$spNames, po)
    colnames(po)[1] = ""
    ne = as.data.frame(1 - OmegaCor[[r]]$support)
    ne = cbind(m$spNames, ne)
    colnames(ne)[1] = ""
    vals = list(
      "Posterior mean" = me,
      "Pr(x>0)" = po,
      "Pr(x<0)" = ne
    )
    filename = paste0(
      "Panels/parameter_estimates_Omega_",
      modelnames[[j]],
      "_",
      names(m$ranLevels)[[r]],
      ".xlsx"
    )
    writexl::write_xlsx(vals, path = filename)
  }
}

dev.off()

