#' Convert OECD Affordable Housing Database.
#' @author Bennet Weiss
#' @param x Magpie object
convertOECDAffordableHousingDatabase <- function(x) {
  mapping <- c(
    "The Netherlands" = "NLD",
    "UK (England)" = "GBR"
  )
  getItems(x, dim = 1) <- madrat::toolCountry2isocode(getItems(x, dim = 1), mapping = mapping)
  x <- madrat::toolCountryFill(x, verbosity = 2)
  return(x)
}
