#' TODOMERLIN: document
#' 
#' @author Merlin Jo Hosak
#' @param subtype TODOMERLIN: document
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