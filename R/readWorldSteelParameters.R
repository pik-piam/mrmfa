#' Read World Steel Association Parameters
#' @description
#' Read World Steel Association (WSA) parameters from various sources like
#' yearbooks, pdfs and webpages. The data is stored in Excel files in the
#' WorldSteelParameters directory with metadata, the primary sources
#' are stored there as well if available.
#' @param subtype Subtype of World Steel parameter to read. Currently available
#' subtypes are: 
#' \itemize{
#' \item 'recoveryRate': Steel scrap recovery rates in the main end-use sectors
#' (construction, machinery, products, transport) 
#' \item 'scrapInBOFRate': Share of scrap used production in Basic Oxygen Furnaces
#' }
#' @author Merlin Jo Hosak
#' @export
readWorldSteelParameters <- function(subtype) {
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