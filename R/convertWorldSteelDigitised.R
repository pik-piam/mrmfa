#' Convert World Steel Digitised
#' @description Convert data World Steel Association digitised 1978-2022 yearbooks.
#' @author Merlin Jo Hosak
#' @importFrom utils read.csv2
#' @param x Magpie object
convertWorldSteelDigitised <- function(x) {
  # manual corrections ----

  # BRG (West Germany before reunification) should be DEU after 1990
  # TODO: this could be a feature in toolISOhistorical, if we want to actively enhance the tool
  if ("BRG" %in% getItems(x, dim = 1) && any(getYears(x, as.integer = TRUE) > 1990)) {
    # if (!"DEU" %in% getItems(x,dim=1)){
    #   x <- add_columns(x, addnm = "DEU", dim = 1, fill = NA)
    # }
    #
    # x["DEU", getYears(x, as.integer = TRUE) > 1990, ] <- x["BRG", getYears(x, as.integer = TRUE) > 1990, ]
    # x <- x["BRG", , invert = TRUE]
  }

  # split BLX into LUX and BEL
  if ("BLX" %in% getItems(x, dim = 1) && !any(c("LUX", "BEL") %in% getItems(x, dim = 1))) {
    x <- add_columns(x, addnm = c("BEL", "LUX"), dim = 1, fill = NA)
    x["BEL", , ] <- x["BLX", , ] * 0.8
    x["LUX", , ] <- x["BLX", , ] * 0.2
    x <- x["BLX", , , invert = TRUE]
  }

  # prepare toolISOhistorical ----

  # add regions not present in the magpie object yet needed for toolISOhistorical to work
  countries <- getItems(x, dim = 1)
  mapping <- read.csv2(system.file("extdata", "ISOhistorical.csv", package = "madrat")) %>%
    filter(.data$fromISO %in% countries)
  newCountries <- unique(mapping$toISO)
  missingCountries <- setdiff(newCountries, countries)

  # TODO: more elegant way to do this ?

  if ("BRG" %in% countries && !"GDR" %in% countries) {
    missingCountries <- c(missingCountries, c("GDR"))
  }

  if ("GDR" %in% countries && !"BRG" %in% countries) {
    missingCountries <- c(missingCountries, c("BRG"))
  }

  x <- add_columns(x, addnm = missingCountries, dim = 1, fill = NA)

  # Convert to historical ISO codes and fill countries
  y <- toolISOhistorical(x, overwrite = TRUE)
  # %>%
  #   suppressWarnings()

  z <- toolCountryFill(y, verbosity = 2)

  return(z)
}
