library(tidyverse)
library(gstat)
library(sp)
library(ncf)
library(raster)


# -------------------------------------------------------------------------


data <- read_rds("Data/hmscdata.rds")
data <- data %>% ungroup() %>% na.omit() %>% as.data.frame()
d <- data %>% select(plot_id, totals_n_larvae, 
                     an_maculipennis, an_claviger,
                     cx_pipiens, cs_annulata,
                     x, y)



plot(d$x, d$y)

coordinates(d) = ~x+y



# -------------------------------------------------------------------------


par(mfrow = c(2,2))
bubble(d, zcol = "an_maculipennis")
bubble(d, zcol = "an_claviger")
bubble(d, zcol = "cx_pipiens")
bubble(d, zcol = "cs_annulata")
par(mfrow = c(1,1))

V <- variogram(cx_pipiens ~ 1, data = d)

plot(V)

VM <- vgm(psill=0.015, model="Gau", nugget=0.001, range=1000)

plot(V, model=VM)

variogram()

# -------------------------------------------------------------------------

foo <- data %>% dplyr::select(x, y, totals_n_larvae)

rounded <- foo %>% round()

counts <- rasterFromXYZ(rounded, res = )

plot(counts, "Abundance Distribution (All)")


P.idw <- gstat::idw(Precip_in ~ 1, P, newdata=grd, idp=2.0)


