#' Convert OECD GDP per capita data from 1500-2016
#' @author Merlin Jo Hosak
#' @param x TODOMERLIN: document
#' @param subtype TODOMERLIN: document
#' @export
convertOECD_GDP <- function(x, subtype = 'gdppc') {
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    'gdppc' = function(x) {
      countries <- getItems(x, dim=1)
      getItems(x, dim=1) <- toolCountry2isocode(countries,ignoreCountries=c('Others'))
      x <- x[!is.na(getItems(x, dim=1)), ]  # remove rows without index (empty)
      
      # Delete countries not accepted by Madrat format
      ## Delete Kosovo as data is per capita anyways and hence should not 
      ## differ significantly.
      x <- x[getItems(x, dim=1) != 'KOS',]
      
      ## Delete Dutch Antilles as data is empty (NA) anyways
      x <- x[getItems(x, dim=1) != 'ANT',]
      
      # Fill missing countries with NA for Madrat format.
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
