#' Calculate scrap consumption based on source WorldSteelDigitised
#'
#' @author Falk Benke
calcStScrapConsumptionWS <- function() {
  # get historical consumption ----
  prodHist <- readSource("WorldSteelDigitised", subtype = "production", convert = FALSE)
  prodHistGlobal <- readSource("WorldSteelDigitised", subtype = "worldProduction", convert = FALSE)
  prodHist <- toolBackcastByReference(prodHist, prodHistGlobal)

  historicalShare <- readSource("WorldSteelDigitised", subtype = "historicalScrapShare", convert = FALSE)

  # historical scrap production needed to multiply with historical shares as values are needed for former countries
  historical <- historicalShare * prodHist[getItems(historicalShare, dim = 1), getItems(historicalShare, dim = 2), ]

  # get current consumption ----
  current <- readSource("WorldSteelDigitised", subtype = "scrapConsumption", convert = FALSE)

  # merge all world steel digitised sources ----

  scrapConsumptionWS <- new.magpie(
    cells_and_regions = union(getItems(historical, dim = 1), getItems(current, dim = 1)),
    years = seq(1965, 2008, 1),
    names = NULL,
    fill = NA,
    sets = names(dimnames(historical))
  )

  scrapConsumptionWS[getItems(historical, dim = 1), getItems(historical, dim = 2), ] <- historical
  # note that this overwrites some data from historical for the overlapping years 1975 - 1979!
  scrapConsumptionWS[getItems(current, dim = 1), getItems(current, dim = 2), ] <- current

  # interpolate missing values ----
  scrapConsumptionWS <- toolInterpolate(scrapConsumptionWS)

  # split historical regions ----
  historicalMap <- utils::read.csv2(system.file("extdata", "ISOhistorical.csv", package = "madrat"))
  newCountries <- historicalMap[historicalMap$fromISO %in% getItems(scrapConsumptionWS, dim = 1), "toISO"]
  missingCountries <- setdiff(c(newCountries, "SRB", "MNE"), getItems(scrapConsumptionWS, dim = 1))
  scrapConsumptionWS <- add_columns(scrapConsumptionWS, addnm = missingCountries, dim = 1, fill = NA)

  scrapConsumptionWS <- toolISOhistorical(scrapConsumptionWS) %>%
    suppressSpecificWarnings("Weight in toolISOhistorical contained NAs. Set NAs to 0!")

  scrapConsumptionWS <- toolCountryFill(scrapConsumptionWS, verbosity = 2)

  result <- list(
    x = scrapConsumptionWS,
    weight = NULL,
    unit = "Tonnes",
    description = "scrap consumption based on source WorldSteelDigitised"
  )

  return(result)
}
