library(tidyverse)
library(virtualspecies)
library(raster)


r <- raster(xmn=0, xmx=1, ymn=0, ymx=1, ncol=100, nrow=100)
values(r) <- rnorm(ncell(r), 0, 2)
plot(r)

ra <- focal(r, w=matrix(1/9, nc=3, nr=3), na.rm=TRUE, pad=TRUE)
ra <- focal(ra, w=matrix(1/9, nc=3, nr=3), na.rm=TRUE, pad=TRUE)
plot(ra)



rr <- lapply(1:4, function(i) setValues(ra, runif(ncell(ra))))

rra <- rr %>% lapply(focal, w=matrix(1/9, nc=3, nr=3), na.rm=TRUE, pad=TRUE)

stacked <- raster::stack(rra)
plot(stacked)


generateRandomSp(stacked)



r <- raster(xmn=0, xmx=1, ymn=0, ymx=1, ncol=20, nrow=20)
values(r) <- rnorm(ncell(r), 0, 2)
plot(r)

ra <- focal(r, w = matrix(1/9, nc=3, nr=3), na.rm=TRUE, pad=TRUE)
ra <- focal(ra, w = matrix(1/9, nc=3, nr=3), na.rm=TRUE, pad=TRUE)
plot(ra, col = pal)

pal <- viridisLite::plasma(10)



ra <- focal(r, w=matrix(1/9, nc=3, nr=3), na.rm=TRUE, pad=TRUE)
ra <- focal(ra, w=matrix(1/9, nc=3, nr=3), na.rm=TRUE, pad=TRUE)
plot(ra)
