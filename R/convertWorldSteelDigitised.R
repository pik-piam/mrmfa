#' Convert data World Steel Association digitised 1978-2022 yearbooks.
#' @author Merlin Jo Hosak
#' @param x Magpie object
convertWorldSteelDigitised <- function(x, subtype="production") {
  # ---- list all available subtypes with functions doing all the work ----
  normalWSDigitisedConvert <- function(x) {
    x <- x * 1e3  # convert from kt to t
    
    # get new countries that will be added to the dataset 
    countries <- getItems(x, dim=1)
    new_countries <- read.csv2(system.file("extdata", "ISOhistorical.csv", package = "madrat"))
    new_countries <- new_countries[new_countries$fromISO %in% countries, "toISO"]
    missing_countries <- setdiff(new_countries, countries)
    
    # if missing ocountry list is not empty extend x 
    if (length(missing_countries) > 0) {
      missing_countries <- new.magpie(
        cells_and_regions = missing_countries,
        years = getItems(x, dim=2),
        names = "value",
        fill = 0,
        sets = names(dimnames(x))
      )
      
      x <- mbind(x, missing_countries)
    }
    
    # if HGK is in data, add it to China as for REMIND it does not make a difference
    if ('HGK' %in% countries) {
      x['CHN', ] <- x['CHN', ] + x['HGK', ]
      x <- x[!rownames(x) %in% 'HGK', ]
    }
    
    y <- toolISOhistorical(x, overwrite=TRUE) %>% suppressWarnings()
    # Fill missing countries with NA values, will be changed in calc file.
    # Verbosity is 2 so that no warning shows up about these added countries.
    z <- toolCountryFill(y, verbosity=2) 
    
    return(z)
  }
  
  switchboard <- list(
    'production' = function(x) {
      x <- normalWSDigitisedConvert(x)
      return(x)
    },
    
    'imports' = function(x) {
      x <- normalWSDigitisedConvert(x)
      return(x)
    },
    
    'exports' = function(x) {
      x <- normalWSDigitisedConvert(x)
      return(x)
    },
    
    'scrap_imports' = function(x) {
      x <- normalWSDigitisedConvert(x)
      return(x)
    },
    
    'scrap_exports' = function(x) {
      x <- normalWSDigitisedConvert(x)
      return(x)
    },
    
    'indirect_imports' = function(x) {
      x <- x * 1e3
      x <- toolISOhistorical(x, overwrite=TRUE) %>% suppressWarnings()
      # Fill missing countries with NA values, will be changed in calc file.
      # Verbosity is 2 so that no warning shows up about these added countries.
      z <- toolCountryFill(x, verbosity=2) 
      return(z)
    },
    
    'indirect_exports' = function(x) {
      x <- x * 1e3
      x <- toolISOhistorical(x, overwrite=TRUE) %>% suppressWarnings()
      # Fill missing countries with NA values, will be changed in calc file.
      # Verbosity is 2 so that no warning shows up about these added countries.
      z <- toolCountryFill(x, verbosity=2) 
      return(z)
    },
    
    NULL)
  
  # ---- check if the subtype called is available ----
  if (is_empty(intersect(subtype, names(switchboard)))) {
    stop(paste('Invalid subtype -- supported subtypes are:',
               names(switchboard)))
  } else {
    # ---- load data and do whatever ----
    return(switchboard[[subtype]](x))
  }
  
  return(x)
}