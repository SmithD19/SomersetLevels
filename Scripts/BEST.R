library(tidyverse)

data <- read_rds("Data/hmscdata.rds") %>% ungroup()

fulldata <- data %>% na.omit() %>% as.data.frame()

fulldata <- fulldata %>% mutate(management = as.factor(management), year = as.factor(year), season = as.factor(season))

physical <- data %>% select(management, cover:ph) %>% na.omit()

physical <- physical %>% mutate(management = as.factor(management))

pairs(physical)

split <- physical %>% group_split(management)

T1 <- split[[1]]

T3 <- split[[2]]

# -------------------------------------------------------------------------

library(BEST)

cover <- BEST::BESTmcmc(y1 = T1$cover, y2 = T3$cover)
plot(cover)
plotPostPred(cover)

percentage_water_shaded <- BEST::BESTmcmc(y1 = T1$percentage_water_shaded, y2 = T3$percentage_water_shaded)
# Is significantly difference - about 34% more shaded in T3 - correlates with not cutting grass
plotAll(percentage_water_shaded)
plotPostPred(percentage_water_shaded)

width <- BEST::BESTmcmc(y1 = T1$width, y2 = T3$width)
# Is significantly different
plotAll(width)
plotPostPred(width)

water_temp <- BEST::BESTmcmc(y1 = T1$water_temp, y2 = T3$water_temp)
# Not different? Cant see from plot very well, but it's close.
plotAll(water_temp)
plotPostPred(water_temp)

turbidity <- BEST::BESTmcmc(y1 = T1$turbidity, y2 = T3$turbidity)
plotAll(turbidity)
plotPostPred(turbidity)

salinity <- BEST::BESTmcmc(y1 = T1$salinity, y2 = T3$salinity)
plotAll(salinity)
plotPostPred(salinity)

t.test(T1$water_temp, T3$water_temp)

dissolved_oxygen <- BEST::BESTmcmc(y1 = T1$dissolved_oxygen, y2 = T3$dissolved_oxygen)
plotAll(dissolved_oxygen)
plotPostPred(dissolved_oxygen)

t.test(T1$dissolved_oxygen, T3$dissolved_oxygen)

ph <- BEST::BESTmcmc(y1 = T1$ph, y2 = T3$ph)
plotAll(ph)
plotPostPred(ph)


# -------------------------------------------------------------------------

library(rstanarm)

options(mc.cores = parallel::detectCores())

m1_width <- stan_lmer(formula = width ~ 1 + management + (1 | year/season), data = fulldata)

summary(m1_width)

plot(m1_width)

library(bayestestR)

bayestestR::hdi(m1_width, ci = .95)



