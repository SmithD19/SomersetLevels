library(tidyverse)
library(rvest)


# -------------------------------------------------------------------------

csv <- read_csv("Data/EllenbergURLs.csv")
planturls <- csv %>% filter(`Presence in Somerset` == "Y") %>% pull(URL)

# Loop through
name <- list()
ell <- list()

for (i in seq_along(planturls)) {
  URL = planturls[[i]]
  htmlplant = read_html(URL)
  name[[i]] = html_text(html_nodes(htmlplant, ".page-header"), trim = TRUE)
  ell[[i]] = html_text(html_nodes(htmlplant, "strong"), trim = TRUE)
}


# Create tibble
plants <-
  enframe(name, name = "ID", value = "Name") %>% unnest(Name)
# Create ellenberg values
ellenbergs <- enframe(ell, name = "ID", value = "Ellenbergs") %>%
  # Take only lists over 4 returns. Prevents incorrect habitats being returned
  filter(lengths(Ellenbergs) > 4) %>%
  # Take these out of the enframed list columns and name
  mutate(
    light = as.double(map(Ellenbergs, `[`, 1)),
    moisture = as.double(map(Ellenbergs, `[`, 2)),
    ph = as.double(map(Ellenbergs, `[`, 3)),
    nitrogen = as.double(map(Ellenbergs, `[`, 4)),
    salt = as.double(map(Ellenbergs, `[`, 5))
  ) %>%
  # Get rid of list column and unnest the useful ellenberg list columns
  select(-Ellenbergs) %>% unnest(cols = everything())


orig <- csv %>% filter(`Presence in Somerset` == "Y") %>% rowid_to_column("ID")
full <- left_join(plants, ellenbergs)

data <- left_join(full, orig) %>% select(-`UK Analog`, -`Presence in Somerset`, -URL)

coalescedellenberg <- data %>% mutate(light_ell = coalesce(light, ell_light_uk),
                        moisture_ell = coalesce(moisture, ell_moist_uk),
                        ph_ell = coalesce(ph, ell_pH_uk),
                        nitrogen_ell = coalesce(nitrogen, ell_N),
                        salt_ell = coalesce(salt, ell_S)) %>% 
  select(ID, Name, `Given Name`, contains("_ell"))

# Averages for given names
givennamedellenbergs <- coalescedellenberg %>% group_by(`Given Name`) %>% 
  summarise_at(.vars = vars(light_ell, moisture_ell, ph_ell, nitrogen_ell, salt_ell),
               .funs = ~ as.integer(mean(., na.rm = T))) %>% 
  rename(CleanNames = `Given Name`)

write_csv(givennamedellenbergs, "Data/Aggregated_BRC_TR8_ellenbergs.csv")


# # Main URL
# URL <- "https://www.brc.ac.uk/plantatlas/finder/plant/carex"
# # Start a browsing session
# s <- html_session(URL)
# # Read the page
# pg <- read_html(s)
# # Get links from the page
# links <- html_attr(html_nodes(pg, "a"), "href")
# # New pages of links
# newpages <- links %>% str_subset("page")
# # Go to links that are useful first
# plants <- links %>% str_subset("carex-")
#
# # Loop
# name <- list()
# ell <- list()
#
# for (i in seq_along(plants)) {
#   session = s
#   newsession <- jump_to(session, plants[i])
#   htmlplant <- read_html(newsession)
#   name[[i]] <-
#     html_text(html_nodes(htmlplant, ".page-header"), trim = TRUE)
#   ell[[i]] <- html_text(html_nodes(htmlplant, "strong"), trim = TRUE)
# }
