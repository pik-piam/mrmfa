#' TODOMERLIN: document
#' 
#' @author Merlin Jo Hosak
#' @param subtype TODOMERLIN: document
#' @export
readWorldSteelParameters <- function(subtype='recoveryRate') {
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    'recoveryRate' = function() {
      path <- './v1.0/WorldSteelRecoveryRate.xlsx'
      df <- readxl::read_excel(path, sheet = 'Data')
      m <- as.magpie(df)
      getSets(m)<- c('Region', 'Year', 'Parameter')
      
      return(m)
    },
    'scrapInBOFRate' = function() {
      path <- './v1.0/WorldSteelScrapInBOFRate.xlsx'
      df <- readxl::read_excel(path, sheet = 'Data')
      m <- as.magpie(df)
      getSets(m) <- c('Region', 'Year', 'Parameter')
      
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