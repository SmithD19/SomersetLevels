

# Change Y matrix to ones and zeroes for easy counting
Yt <- lapply(Y, function(e) replace(e, e == 0, NA)) %>% 
  lapply(function(e) replace(e, e > 0, 1))

# This is how many sites that the models actually have occurance data
Yt %>% sapply(colSums, na.rm = T)

# Looks like we need to find out if we can get better results than this -----

df <- read_rds("Data/hmscdata.rds")

# So dry rhynes had no data at all
df$dry %>% table()
df.dry <- df %>% filter(dry != 1)

df.missing <- select(df.dry, width:cover_vertical)

VIM::aggr(df.missing, labels = T)


# -------------------------------------------------------------------------

# So after I've finished cleaning half of the dip points are NA values...
rdip <- read_csv("Data/DipPointLevelData.csv")
rdip$fish %>% is.na %>% table()

# But the raw data only has 123 NAS
rawdip <- readxl::read_xlsx("Data/RawCovariateData.xlsx", na = "NA")
rawdip$Fish %>% is.na() %>% table()
