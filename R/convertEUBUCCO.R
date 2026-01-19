#' Convert data from EUBUCCO
#'
#' @author Bennet Weiss
#' @param x Magpie object
convertEUBUCCO <- function(x) {
  getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1))

  # commercial is implicitly given in dataset
  x <- add_columns(x, "commercial", 3)
  x[,,"commercial"] <- x[,,"commercial and industry"] - x[,,"industry"]
  # remove unused variables
  x <- x[,,list(Variable = c("commercial and industry", "industry")), invert = TRUE]

  # rename variables
  getItems(x, dim = 3) <- c("Res", "Com")

  # only European countries in dataset
  x <- toolCountryFill(x, verbosity = 2)

  return(x)
}
