#' Convert cement end-use shares from VDZ (2022)
#'
#' Normalises the shares. Countries without data are filled with NA.
#'
#' @author Bennet Weiss
#' @param x magpie object from \code{\link{readVDZ2022}}.
convertVDZ2022 <- function(x) {
  x <- toolCeNormalizeEndUseSplit(x)
  return(toolCountryFill(x, fill = NA, verbosity = 2))
}
