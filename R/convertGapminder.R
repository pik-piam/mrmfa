#' Convert Gapminder data to Magpie format
#' @author Merlin Jo Hosak, Bennet Weiss
#' @param x MagPIE object
#' @param subtype Regional scope of the data. Either "countries" or "global".
convertGapminder <- function(x, subtype) {
  if (subtype == "global") {
    stop(paste0("readSource('Gapminder', subtype = 'global') not intended to be disaggregated into countries. ",
                "Please use convert = FALSE in the readSource function if global data is desired."))
  }
  x <- toolCountryFill(x, verbosity = 2)
  return(x)
}
