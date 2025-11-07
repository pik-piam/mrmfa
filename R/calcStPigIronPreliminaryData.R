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
#' @export
calcStPigIronPreliminaryData <- function(subtype) {
  # Load data
  data <- readPigIronData(subtype = subtype)

  # Prepare data

  data <- splitHistPigIronData(data)
  data$iedc <- fillIEDCdata(data$iedc) # fill IEDC data with assumptions to make backcasting more reliable

  # Backcast World Steel data with IEDC data
  final <- toolBackcastByReference2D(data$ws, data$iedc, doMakeZeroNA = TRUE)

  # Finalite

  final[is.na(final)] <- 0 # assume 0 pig iron data in remaining cells

  result <- list(
    x = final,
    weight = NULL,
    unit = "Tonnes",
    description = paste("Pig iron data of type", subtype, "merged from WorldSteel and IEDC data.")
  )

  return(result)
}

fillIEDCdata <- function(iedc) {
  noNACountries <- getItems(iedc, dim = 1)[rowSums(is.na(iedc)) == 0]
  globalTrend <- colSums(iedc[noNACountries, ])
  iedc <- toolBackcastByReference2D(iedc, globalTrend, doForecast = TRUE)
  iedc <- toolBackcastByReference2D(iedc, globalTrend)

  return(iedc)
}

splitHistPigIronData <- function(data) {
  # Create merged dataset to use WorldSteel Data as weights to split historical IEDC data

  wsWeights <- data$ws[, getYears(data$ws) >= "y2009", ]
  wsWeights[is.na(wsWeights)] <- 0
  merged <- toolMerge2D(data$iedc, wsWeights)

  # Create historical mapping

  histMap <- toolGetMapping("ISOhistorical.csv", where = "madrat")

  countries <- getItems(data$iedc, dim = 1)
  superRegions <- intersect(unique(histMap$fromISO), countries)
  superRegions <- superRegions[superRegions != "DEU"]

  histMap <- histMap[histMap$fromISO %in% superRegions, ]
  histMap <- histMap
  histMap$lastYear <- "y2009"
  histMap[histMap$toISO == "SCG", ]$toISO <- "SRB"

  # Spit the regions and recreate the historical data

  split <- toolISOhistorical(merged, mapping = histMap) %>% suppressWarnings()
  split <- split[, getYears(split) <= "y2008", ]

  split <- toolCountryFill(split)
  data$iedc <- split

  return(data)
}

readPigIronData <- function(subtype) {
  pigIronSubtype <- paste("pigIron", tools::toTitleCase(subtype), sep = "")
  iedc <- readSource("IEDC", subtype = pigIronSubtype, convert = FALSE)
  ws <- readSource("WorldSteelDatabase", subtype = pigIronSubtype)

  return(list(
    iedc = iedc,
    ws = ws
  ))
}
