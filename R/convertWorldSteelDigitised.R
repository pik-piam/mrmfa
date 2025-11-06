#' Convert World Steel Digitised
#' @description Convert data World Steel Association digitised 1978-2022 yearbooks.
#' @author Merlin Jo Hosak
#' @param x Magpie object
convertWorldSteelDigitised <- function(x) {
  # Append missing regions
  countries <- getItems(x, dim=1)
  mapping <- read.csv2(system.file("extdata", "ISOhistorical.csv", package = "madrat"))
  newCountries <- mapping[mapping$fromISO %in% countries, "toISO"]
  missingCountries <- setdiff(newCountries, countries)
  
  x <- add_columns(x, addnm=missingCountries, dim=1, fill=NA)
  
  # Convert to historical ISO codes and fill countries
  y <- toolISOhistorical(x, overwrite=TRUE) %>% suppressWarnings()
  z <- toolCountryFill(y, verbosity=2) 
  
  return(z)
}
