#' Convert Gapminder data to Magpie format
#' @author Merlin Jo Hosak
convertGapminder <- function(x) {
  x <- toolCountryFill(x, verbosity = 2)
  return(x)
}
