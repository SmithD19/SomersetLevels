## ---------------------------
##
## Script name: Check Model Fit
##
## Purpose of script:
##
## Author: Daniel Smith
##
## Date Created: 2020-11-24
##
## Email: dansmi@ceh.ac.uk
##
## ---------------------------

library(coda)
library(Hmsc)

nice_load <- function(file, object, rename = NULL){
  
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("stringr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  # assertthat::assert_that(is.character(file), "file must be a string")
  # assertthat::assert_that(is.character(object), "object must be a string")
  # assertthat::assert_that((is.character(rename) | is.null(rename)), "rename must be a string or NULL")
  
  file_string <- stringr::str_replace(file, "^.*/", "")
  file_string <- stringr::str_replace(file, "\\.RData", "")
  
  # get data frame into local environment
  e = local({load(file); environment()})
  
  # make lazy-load database
  tools:::makeLazyLoadDB(e, file_string)
  lazyLoad(file_string)
  
  # load object
  get(object)
  
  if(!is.null(rename) ){
    # create object in local env that has name matching value for object, with new name same as rename
    assign(eval(rename), get(object), envir = .GlobalEnv)
    # assign(ls()[ls() == eval(object)], rename)
    rm(e)
    # return(get(eval(quote(rename))))
  }
  else{
    rm(e)
    assign(eval(object), get(object), envir = .GlobalEnv)
  }
}


# Load --------------------------------------------------------------------

abu <- nice_load("Models/Abundance_Thin300/ModelExtended.RData", "output")
pa <- nice_load("Models/PA_Thin300//ModelExtended.RData", "output")

# Check Model Fit ---------------------------------------------------------

## Explanatory Power

MF = list()

for (i in seq_along(models)) {
  # Explanatory Power - Not Cross Validated
  preds = computePredictedValues(models[[i]])
  MF[[i]] = evaluateModelFit(hM = models[[i]], predY = preds)  
}

names(MF) = names(models)






























