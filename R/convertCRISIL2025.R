#' Convert cement end-use shares from CRISIL (2025)
#'
#' Normalises the shares (the midpoints of the source ranges do not sum exactly to 100 %).
#' Countries without data are filled with NA.
#'
#' @author Bennet Weiss
#' @param x magpie object from \code{\link{readCRISIL2025}}.
convertCRISIL2025 <- function(x) {
  x <- toolCeNormalizeEndUseSplit(x)
  return(toolCountryFill(x, fill = NA, verbosity = 2))
}
