#' Calc Pig Iron Preliminary Data
#'
#' @description
#'
#' Prepare pig iron data for final output functions. Loads World Steel Association
#' (WSA, s. \link{readWorldSteelDatabase}) and
#' Industrial Ecology Data Commons (IEDC, s. \link{readIEDC}) data.
#' The WSA data is used as a baseline (as for many other steel related data)
#' and forecasted using IEDC data as more recent WSA data is missing.
#'
#' @param subtype Options: 'production', 'imports', 'exports'
#'
#' @author Merlin Jo Hosak
calcStPigIronPreliminaryData <- function(subtype) {

  if (subtype == "production") {
    ws <- readSource("WorldSteelDatabase", subtype = "pigIronProduction")
    iedc <- readSource("IEDC", subtype = "pigIronProduction")
  } else if (subtype == "imports") {
    ws <- readSource("WorldSteelDatabase", subtype = "pigIronImports")
    iedc <- readSource("IEDC", subtype = "pigIronImports")
  } else if (subtype == "exports") {
    ws <- readSource("WorldSteelDatabase", subtype = "pigIronExports")
    iedc <- readSource("IEDC", subtype = "pigIronExports")
  } else {
    stop("Invalid subtype ", subtype)
  }

  # fill IEDC data with assumptions to make backcasting more reliable
  # global trend is derived only from countries without NAs in any years
  noNACountries <- getItems(iedc, dim = 1)[rowSums(is.na(iedc)) == 0]
  globalTrend <- colSums(iedc[noNACountries, ,])
  iedc <- toolBackcastByReference2D(iedc, globalTrend, doForecast = TRUE)
  iedc <- toolBackcastByReference2D(iedc, globalTrend)

  # backcast World Steel data with IEDC data
  final <- toolBackcastByReference2D(ws, iedc, doMakeZeroNA = TRUE)

  # assume 0 pig iron data in remaining cells
  final[is.na(final)] <- 0

  getNames(final) <- NULL

  result <- list(
    x = final,
    weight = NULL,
    unit = "Tonnes",
    description = paste0("Pig iron data of type '", subtype, "' merged from WorldSteel and IEDC data.")
  )

  return(result)
}
