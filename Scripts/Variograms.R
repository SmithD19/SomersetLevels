# Semivariogram
library(dplyr)
library(geoR)
library(gstat)
library(moments)
library(raster)
library(cowplot)

data <- readRDS("Data/hmscdata.rds")

data <- as.data.frame(data)

data <- na.omit(data)

# Columns 7, 8, 9, 10 are the mosquitoes we care about
cols = 7:10

# Assign coords using sp
spdata <- data
coordinates(spdata) <- ~ x + y

bubble.list = list()

for (i in seq_along(cols)) {
  bubble.list[[i]] = bubble(spdata,
                            zcol = cols[i],
                            xlab = "Eastings (m)",
                            ylab = "Northings (m)")
}

bubble.plots <-
  plot_grid(bubble.list[[1]], bubble.list[[2]], bubble.list[[3]], bubble.list[[4]])

# Experimental Variogram
## Shows all pairs of locations in a dataset and their semi-variance
vcloud.pip <- variogram(cx_pipiens ~ 1, data = spdata, cloud = T)
vcloud.ann <- variogram(cs_annulata ~ 1, data = spdata, cloud = T)
vcloud.mac <- variogram(an_maculipennis ~ 1, data = spdata, cloud = T)
vcloud.cla <- variogram(an_claviger ~ 1, data = spdata, cloud = T)

vcloud.variograms <- plot_grid(
  plot(vcloud.pip, main = "Variogram Cloud - Cx. pipiens", xlab = "Seperation Distance (m)"),
  plot(vcloud.ann, main = "Variogram Cloud - Cs. annulata", xlab = "Seperation Distance (m)"),
  plot(vcloud.mac, main = "Variogram Cloud - An. maculipennis", xlab = "Seperation Distance (m)"),
  plot(vcloud.cla, main = "Variogram Cloud - An. claviger", xlab = "Seperation Distance (m)")
)

# Isotropic Variogram
## When spatial dependence is the same in all directions
v.cx <- variogram(cx_pipiens ~ 1, data = spdata, cloud = F)
v.cs <- variogram(cs_annulata ~ 1, data = spdata, cloud = F)
v.ma <- variogram(an_maculipennis ~ 1, data = spdata, cloud = F)
v.cl <- variogram(an_claviger ~ 1, data = spdata, cloud = F)

iso.pip <-
  plot(v.cx, main = "Isotropic Variogram - Cx. pipiens", xlab = "Seperation Distance (m)")
iso.ann <-
  plot(v.cs, main = "Isotropic Variogram - Cs. annulata", xlab = "Seperation Distance (m)")
iso.mac <-
  plot(v.ma, main = "Isotropic Variogram - An. maculipennis", xlab = "Seperation Distance (m)")
iso.cla <-
  plot(v.cl, main = "Isotropic Variogram - An. claviger", xlab = "Seperation Distance (m)")

iso.variograms <- plot_grid(iso.pip, iso.ann, iso.cla, iso.mac)

# Ansiotropic Variogram
## Visualisation of the variogram surface
## (variogram map or directional variogram)

vmap.pip <-
  variogram(
    cx_pipiens ~ 1,
    data = spdata,
    map = TRUE,
    cutoff = 10000,
    width = 10000 / 20
  )
vmap.ann <-
  variogram(
    cs_annulata ~ 1,
    data = spdata,
    map = TRUE,
    cutoff = 10000,
    width = 10000 / 20
  )
vmap.mac <-
  variogram(
    an_maculipennis ~ 1,
    data = spdata,
    map = TRUE,
    cutoff = 10000,
    width = 10000 / 20
  )
vmap.cla <-
  variogram(
    an_claviger ~ 1,
    data = spdata,
    map = TRUE,
    cutoff = 10000,
    width = 10000 / 20
  )

vmaps.variograms <- plot_grid(
  plot(vmap.pip, col.regions = bpy.colors(64), main = "Cx. pipiens"),
  plot(vmap.ann, col.regions = bpy.colors(64), main = "Cs. annulata"),
  plot(vmap.mac, col.regions = bpy.colors(64), main = "An. maculipennis"),
  plot(vmap.cla, col.regions = bpy.colors(64), main = "An. claviger")
)

## NCF correlelograms
library(ncf)
clog.pip <- correlog(data$x, data$y, data$cx_pipiens, increment = 1000)
clog.ann <- correlog(data$x, data$y, data$cs_annulata, increment = 1000)
clog.mac <- correlog(data$x, data$y, data$an_maculipennis, increment = 1000)
clog.cla <- correlog(data$x, data$y, data$an_claviger, increment = 1000)

correlelograms <- plot_grid(
  plot(clog.pip, main = "Cx. pipiens"),
  plot(clog.ann, main = "Cs. annulata"),
  plot(clog.mac, main = "An. maculipennis"),
  plot(clog.cla, main = "An. claviger")
)


# PDF
pdf(file = "Panels/Variograms.pdf")
bubble.plots
vcloud.variograms
iso.variograms
vmaps.variograms
plot(clog.pip, main = "Cx. pipiens")
plot(clog.ann, main = "Cs. annulata")
plot(clog.mac, main = "An. maculipennis")
plot(clog.cla, main = "An. claviger")
dev.off()


