#' @author Merlin Jo Hosak
#' @export
readBIR <- function() {
  path <- paste0('./v1.0/BIR_ScrapShareProduction.xlsx')
  df <- readxl::read_excel(path, sheet = 'Data')
  scrapShares <- as.magpie(df, spatial='Scrap share in production')
  
  return(scrapShares)
}