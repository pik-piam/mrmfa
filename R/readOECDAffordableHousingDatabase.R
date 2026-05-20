#' Read dwelling vacancy share from OECD Affordable Housing Database
#' OECD (2024), OECD Affordable Housing Database - indicator HM1.1. Housing stock and construction, https://oe.cd/ahd
#'
#'
#' @author Bennet Weiss
#' @param subtype Specify column to be read. Can be "vacant", "holiday", "both"
readOECDAffordableHousingDatabase <- function(subtype) {
  path <- file.path("v1", "HM1-1-Housing-stock-and-construction.xlsx")
  col_names = c("Region", "vacant", "holiday", "both")
  col_types = c("text", "numeric", "numeric", "numeric")
  df <- readxl::read_xlsx(path, sheet = "HM1.1.2", range = "M8:P33", col_names = col_names, col_types = col_types)
  df <- na.omit(df[c("Region", subtype)])
  df <- dplyr::rename(df, "value" = subtype)
  x <- magclass::as.magpie(df, spatial = 1, tidy = TRUE)
  getNames(x) <- NULL
  return(x)
}
