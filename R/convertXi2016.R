#' Convert data from Xi2016
#'
#' Normalises the split of China. No values are assigned to other countries; they are filled with NA.
#' Filling of countries without data happens in \code{\link{calcCeEndUseSplit}}.
#'
#' @author Bennet Weiss
#' @param x Magpie object
convertXi2016 <- function(x) {
  # normalize (data should be already 99% normalized.)
  x <- x / dimSums(x, dim = 3)
  return(toolCountryFill(x, fill = NA, verbosity = 2))
}
