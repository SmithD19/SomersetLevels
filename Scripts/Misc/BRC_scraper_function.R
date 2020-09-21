## ---------------------------
##
## Script name: Quick Link Scraper for BRC
##
## Purpose of script: Takes all plant links for a brc webpage and copies them to clipboard
##
## Author: Daniel Smith
##
## Date Created: 2020-07-09
##
## Email: dansmi@ceh.ac.uk
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## load up the packages we will need:

library(tidyverse)
library(rvest)

## ---------------------------

brc_scrape <- function(brc_url) {
  require(tidyverse)
  require(rvest)
  require(clipr)
  # Main URL to get links from
  URL <- brc_url
  # Start a browsing session
  s <- html_session(URL)
  # Read the page
  pg <- read_html(s)
  # Get links from the page
  links <- html_attr(html_nodes(pg, "a"), "href")
  #  Links for plant records
  newpages <- links %>% str_subset("/plant/")
  # Print
  print("Writing links to clipboard")
  # Copy these to clipboard
  paste0("https://www.brc.ac.uk", newpages) %>% writeClipboard()
}

