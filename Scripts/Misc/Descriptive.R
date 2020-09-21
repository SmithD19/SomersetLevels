#### Use this after laoding data from final model scripts ####

# Abundance per season
data %>% 
  select(season, an_maculipennis:oc_caspius) %>% 
  group_split(season) %>% map(select, -season) %>% 
  sapply(colSums, na.rm = T)

# presence in plots per season
pa_season <- data %>% 
  select(season, an_maculipennis:oc_caspius) %>%
  group_split(season) %>% map(select, -season) %>%
  # pa mutation
  map(mutate_all, .funs = function(x) {
          if_else(x > 0, as.integer(1), as.integer(0))
        }) %>% 
  sapply(colSums, na.rm = T)

# 318 sites for sampling
sites <- data %>% nrow()

# How many sites per season?
seasonalsites <- data %>% split(season) %>% sapply(nrow)

perc_pa_season <- pa_season %>% as.data.frame() %>% 
  mutate(V1 = round(V1 / 113 * 100, digits = 2),
         V2 = round(V2 / 113 * 100, digits = 2),
         V3 = round(V3 / 113 * 100, digits = 2)) %>% 
  rename_all(function(x) paste0("Season_", x))

rownames(perc_pa_season) <- rownames(pa_season)

colnames(pa_season) <- colnames(perc_pa_season)

# Not stratified by season
data %>% select(an_maculipennis:cs_annulata) %>% 
  mutate_all(function(x) {
    if_else(x > 0, as.integer(1), as.integer(0))
  }) %>% 
  colSums()

# Overlap -----------------------------------------------------------------

count <- data %>% select(an_maculipennis:cs_annulata) %>% 
  mutate_all(function(x) {
    if_else(x > 0, as.integer(1), as.integer(0))
  }) 

count %>% group_by_all() %>% count()


# Given two species vectors of occurence
# which indeces are both above 0?

# Need to iterate this over every unique combination
# count %>% filter(cs_annulata != 0 & cx_pipiens != 0) %>% nrow()

# # Unique pairings:
# library(arrangements)
# tx <- table(colnames(count))
# comb <- combinations(names(tx), k = 2, freq = tx) %>% as.data.frame()
# v1 <- comb$V1
# v2 <- comb$V2
# 


# # example data
# df <- tibble(animal_1 = rbinom(100, 1, 0.5),
#        animal_2 = rbinom(100, 1, 0.5),
#        animal_3 = rbinom(100, 1, 0.5))
# 
# # tedious way to get unique pairings
# df %>% filter(animal_1 != 0 & animal_2 != 0) %>% nrow()
# 
# # this gives a nice way to get all unique combinations of a vector
# library(arrangements)
# tx <- table(colnames(df))
# comb <- combinations(names(tx), k = 2, freq = tx) %>% as.data.frame()
# 
# # Two vectors with all unique pairings of animals
# v1 <- comb$V1
# v2 <- comb$V2
# 
# # Loop?
# results = list()
# 
# for (i in 1:nrow(comb)) {
#  results[[i]] <- nrow(filter(df, v1$x != 0 & v2$x != 0))
# }
# 
# 
# 
# df %>% mutate(variable = as.numeric(variable))
# 
# vars <- c("var1", "var2", "var3", "etc")
# mutate_at(vars, as.numeric)

count
count %>% mutate(across(all_of(names(count)), as.character))
                        
