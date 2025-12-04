#' Convert Gapminder data to Magpie format
#' @author Merlin Jo Hosak
#' @param x MagPIE object
convertGapminder <- function(x) {
  x <- toolCountryFill(x, verbosity = 2)
  return(x)
}
