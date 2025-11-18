#' @author Falk Benke
convertBIR <- function(x, subtype) {
  if (subtype == "scrapConsumption") {
    x <- x[c("EU 28", "World"), ,,invert = TRUE]
    getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1))
    x <- toolCountryFill(x, verbosity = 2)
  }
  return(x)
}
