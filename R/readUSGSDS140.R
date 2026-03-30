#' Read cement statistic data sheet from USGS.
#' USGS DS140 combines historic data from USGS sources. Supply-Demand Statistics.
#'
#' U.S. Geological Survey, 2020, Cement statistics, in Kelly, T.D., and Matos, G.R., comps.,
#' Historical statistics for mineral and material commodities in the United States:
#' U.S. Geological Survey Data Series 140
#' accessed 12.02.2025, at
#' https://www.usgs.gov/centers/national-minerals-information-center/
#' historical-statistics-mineral-and-material-commodities.
#' @author Bennet Weiss.
#' @param subtype Character string specifying the column to be read. Supported are:
#'        - Imports
#'        - Exports
readUSGSDS140 <- function(subtype) {
  path <- file.path("v1", "ds140-cement-2021.xlsx")
  data <- readxl::read_xlsx(path, range = "A5:I127")
  data["region"] <- "USA"
  data["value"] <- data[[subtype]]
  x <- data[c("region", "Year", "value")]

  x <- magclass::as.magpie(x, spatial = 1, temporal = 2, datacol = 3)
  getNames(x) <- NULL
  return(x)
}
