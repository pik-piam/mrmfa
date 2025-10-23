#' Convert trade data from USGS DS140.
#'
#' @author Bennet Weiss
#' @param x Magpie object
convertUSGSDS140 <- function(x) {
  x <- madrat::toolCountryFill(x, fill = NA, verbosity = 2)
  return(x)
}
