#' Convert cement end-use shares from Cao et al. (2017)
#'
#' Normalises the digitised shares. Countries without data are filled with NA.
#' Which country series are trusted is decided in \code{\link{calcCeEndUseSplit}}.
#'
#' @author Bennet Weiss
#' @param x magpie object from \code{\link{readCao2017}}.
convertCao2017 <- function(x) {
  x <- toolCeNormalizeEndUseSplit(x)
  return(toolCountryFill(x, fill = NA, verbosity = 2))
}
