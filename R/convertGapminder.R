#' Convert Gapminder data to Magpie format
#' @author Merlin Jo Hosak
#' @param x TODOMERLIN: document
#' @param subtype TODOMERLIN: document
#' @export
convertGapminder <- function(x, subtype = 'population') {
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    'population' = function(x) {
      x <- toolCountryFill(x, verbosity=2)
      
      return(x)
    },
    
    NULL)
  # ---- check if the subtype called is available ----
  if (is_empty(intersect(subtype, names(switchboard)))) {
    stop(paste('Invalid subtype -- supported subtypes are:',
               names(switchboard)))
  } else {
    # ---- load data and do whatever ----
    return(switchboard[[subtype]](x))
  }
}

