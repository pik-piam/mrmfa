#' Convert buildings split data from GEM.
#'
#' @author Bennet Weiss
#' @param x Magpie object
convertGlobalExposureModel <- function(x) {
  x["SRB", ] <- x["SRB", ] + toolNAreplace(x["XKX", ])$x # add Kosovo to Serbia
  x_out <- madrat::toolCountryFill(x, verbosity = 2)
  return(x_out)
}
