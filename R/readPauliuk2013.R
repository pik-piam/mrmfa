#' Read Pauliuk et al. (2013)
#' @description
#' Read data from Pauliuk et al. (2013) papers 'Steel all over the world:
#' Estimating in-use stocks of iron for 200 countries' and 'The steel scrap
#' age'
#' @param subtype Subtype of Pauliuk 2013 data to load. Currently only
#' 'sectorSplits' is available, other potentially relevant data is
#' estimated stock saturation levels and times for various world regions
#' as well as stock sector splits.
#' @author Merlin Jo Hosak
#' @export
readPauliuk2013 <- function(subtype='sectorSplits') {
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    'sectorSplits' = function() {
      path <- './v1.0/Pauliuk2013SectorSplits.xlsx'
      df <- readxl::read_excel(path, sheet = 'Data')
      m <- as.magpie(df)
      getSets(m)<- c('Region', 'Year', 'Parameter')
      
      m <- list(x = m, 
                    weight = NULL,
                    unit=1,
                    description='Sector Splits')
      
      return(m)
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