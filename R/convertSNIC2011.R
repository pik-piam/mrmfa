#' Convert cement end-use shares from SNIC (2011)
#'
#' Normalises the shares after dropping the category "Others".
#' Countries without data are filled with NA.
#'
#' @author Bennet Weiss
#' @param x magpie object from \code{\link{readSNIC2011}}.
convertSNIC2011 <- function(x) {
  x <- toolCeNormalizeEndUseSplit(x)
  return(toolCountryFill(x, fill = NA, verbosity = 2))
}
