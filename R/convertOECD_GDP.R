#' Convert OECD GDP
#' @description Convert OECD GDP per capita data from 1500-2016
#' @author Merlin Jo Hosak
convertOECD_GDP <- function(x) {

  countries <- getItems(x, dim = 1)

  # replace underscores with dots (special character in magclass)
  countries <- gsub("_", ".", countries)

  m <- toolGetMapping("MFA_rename_regions.csv", where = "mrmfa")
  additionalIsoMappings <- m$to
  names(additionalIsoMappings) <- m$from

  getItems(x, dim = 1) <- toolCountry2isocode(countries,
                                              mapping = additionalIsoMappings,
                                              ignoreCountries = c("Others"), warn = TRUE
  )

  x <- x[!is.na(getItems(x, dim = 1)), ] # remove rows without index (empty)

  # Delete Kosovo as data is per capita anyways and hence should not  differ significantly.
  # Delete Dutch Antilles as data is empty (NA) anyways.

  x <- x[c("KOS", "ANT"), , , invert = TRUE]

  # Fill missing countries with NA for Madrat format.
  x <- toolCountryFill(x, verbosity = 2)

  return(x)
}
