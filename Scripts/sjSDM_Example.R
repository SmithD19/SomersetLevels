data <- readRDS("Data/hmscdata.rds") %>% na.omit()

library(dplyr)

Env <- data %>% ungroup() %>% select(cover:ph) %>% data.matrix()

Occ <- data %>% ungroup() %>% select(an_claviger:gammarus) %>% data.matrix()

# PresenceAbsence
# Occ <- apply(Occ, 2, function(x) ifelse(x > 1, 1, 0))

SP <- data %>% ungroup() %>% select(x, y) %>% data.matrix() %>% round()

colnames(Env)


# My data
Y <- Y %>% data.matrix()
X <- XData %>% data.matrix()

library(sjSDM)

my_model <- sjSDM_cv(
  Y = Occ,
  env = linear(data = Env, formula = ~ .),
  spatial = linear(data = SP, formula = ~0+x:y),
  se = F, family=binomial(link = "probit"), sampling = 100L, device = "gpu",
  biotic = bioticStruct(lambda = 0.001)
)

coefficients(summary(my_model))

importance(my_model)

# simulate sparse community:
com = simulate_SDM(env = 5L, species = 25L, sites = 100L, sparse = 0.5)

# tune regularization:
tune_results = sjSDM_cv(Y = Occ,
                        env = Env, 
                        tune = "random", # random steps in tune-paramter space
                        CV = 3L, # 3-fold cross validation
                        tune_steps = 25L,
                        alpha_cov = seq(0, 1, 0.1),
                        alpha_coef = seq(0, 1, 0.1),
                        lambda_cov = seq(0, 0.1, 0.001), 
                        lambda_coef = seq(0, 0.1, 0.001),
                        #n_cores = 2L, # small models can be also run in parallel on the GPU
                        iter = 2L, 
                        device = "gpu" # we can pass arguments to sjSDM via ...
)

# print overall results:
tune_results

# summary (mean values over CV for each tuning step)
summary(tune_results)

# visualize tuning and best points:
best = plot(tune_results, perf = "logLik")

# fit model with new regularization paramter:
model = sjSDM(Y = Occ,
              env = linear(Env, 
                           lambda = best[["lambda_coef"]],
                           alpha = best[["alpha_coef"]]),
              biotic = bioticStruct(lambda = best[["lambda_cov"]],
                                    alpha = best[["alpha_cov"]])
)

summary(model)

importance(model)
