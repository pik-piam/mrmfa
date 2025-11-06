#' Read Gapminder population data.
#' @description Read Gapminder population data from 1800-2100 in yearly resolution.
#' @author Merlin Jo Hosak
#' @param subtype TODOMERLIN: document
#' @export
readGapminder <- function(subtype = 'population') {
  version <- 'v1.0'
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    'population' = function() {
      path <- paste('./', version, '/GM-Population - Dataset - v8.xlsx', sep='')
      x <- readxl::read_excel(path = path,
                              sheet = 'Unpivot-countries-year')
      
      # delete irrelevant columns
      x <- x[, -which(names(x) == 'Income levels')]
      x <- x[, -which(names(x) == 'name')]
      
      # make all iso3 letter codes in geo column upper case and make this Magpie spatial dimension
      x$geo <- toupper(x$geo)
      x <- as.magpie(x, spatial='geo')
      
      getSets(x) <- c('Region', 'Year', 'value')  # Rename sets for unified format.
      getItems(x,dim=1)[getItems(x,dim=1)=='HOS']<-'VAT'  # Rename Vatican Iso3 code (HOS [Holy See] to VAT)
      
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

