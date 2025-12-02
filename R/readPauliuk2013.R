#' Read Pauliuk et al. (2013)
#' @description
#' Read data from Pauliuk et al. (2013) papers 'Steel all over the world:
#' Estimating in-use stocks of iron for 200 countries' and 'The steel scrap
#' age'
#' Currently only 'sectorSplits' is available, other potentially relevant data is
#' estimated stock saturation levels and times for various world regions
#' as well as stock sector splits.
#' @author Merlin Jo Hosak
readPauliuk2013 <- function() {
  # TODO JD: shouldn't this be further split into countries and years?
  path <- file.path(".", "v1.0", "Pauliuk2013SectorSplits.xlsx")
  df <- readxl::read_excel(path, sheet = "Data")
  x <- as.magpie(df)
  getSets(x) <- c("Region", "Year", "Parameter")
  return(x)
}
