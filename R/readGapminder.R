#' Read Gapminder population data.
#' @description Read Gapminder population data from 1800-2100 in yearly resolution.
#' @author Merlin Jo Hosak
#' @export
readGapminder <- function(subtype = 'population') {
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    'population' = function() {
      x <- readxl::read_excel(path = './v1.0/GM-Population - Dataset - v8.xlsx',
                              sheet = 'Unpivot-countries-year')
      
      # delete irrelevant columns
      x <- x[, -which(names(x) == 'Income levels')]
      x <- x[, -which(names(x) == 'name')]
      
      # make all iso3 letter codes in geo column upper case
      x$geo <- toupper(x$geo)
      
      x <- as.magpie(x, spatial='geo')
      
      return(x)
    },
    
    NULL)
  # ---- check if the subtype called is available ----
  if (is_empty(intersect(subtype, names(switchboard)))) {
    stop(paste('Invalid subtype -- supported subtypes are:',
               names(switchboard)))
  } else {
    # ---- load data and do whatever ----
    return(switchboard[[subtype]]())
  }
}

