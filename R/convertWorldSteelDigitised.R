#' Convert World Steel Digitised
#' @description Convert data World Steel Association digitised 1978-2022 yearbooks.
#' @author Merlin Jo Hosak
#' @importFrom utils read.csv2
#' @param x Magpie object
convertWorldSteelDigitised <- function(x, subtype) {

  # TODO: make sure all the subtypes have a working convert function (so far, only "production)

  # add regions not present in the magpie object yet needed for toolISOhistorical to work
  countries <- getItems(x, dim = 1)
  mapping <- read.csv2(system.file("extdata", "ISOhistorical.csv", package = "madrat")) %>%
    filter(.data$fromISO %in% countries)
  newCountries <- unique(mapping$toISO)
  missingCountries <- setdiff(newCountries, countries)

  x <- add_columns(x, addnm = missingCountries, dim = 1, fill = NA)

  # use additional mapping for BLX
  blx <- data.frame(
    fromISO = "BLX",
    toISO = c("BEL", "LUX"),
    lastYear = "y2003"
  )

  y <- toolISOhistorical(x, additional_mapping = blx, overwrite = TRUE)
  z <- toolCountryFill(y, verbosity = 2)

  return(z)
}
