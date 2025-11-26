#' Calculate scrap consumption based on source WorldSteelDigitised
#'
#' @author Falk Benke
calcStScrapConsumptionWS <- function() {
  # get historic consumption ----
  prodHist <- readSource("WorldSteelDigitised", subtype = "production", convert = FALSE)
  prodHistGlobal <- readSource("WorldSteelDigitised", subtype = "worldProduction", convert = FALSE)
  prodHist <- toolBackcastByReference2D(prodHist, prodHistGlobal)

  historicShare <- readSource("WorldSteelDigitised", subtype = "historicScrapShare", convert = FALSE)

  # historic scrap production needed to multiply with historic shares as values are needed for former countries
  historic <- historicShare * prodHist[getItems(historicShare, dim = 1), getItems(historicShare, dim = 2), ]

  # get current consumption ----
  current <- readSource("WorldSteelDigitised", subtype = "scrapConsumption", convert = FALSE)

  # merge all world steel digitised sources ----

  scrapConsumptionWS <- new.magpie(
    cells_and_regions = union(getItems(historic, dim = 1), getItems(current, dim = 1)),
    years = seq(1965, 2008, 1),
    names = "value",
    fill = NA,
    sets = names(dimnames(historic))
  )

  scrapConsumptionWS[getItems(historic, dim = 1), getItems(historic, dim = 2), ] <- historic
  # note that this overwrites some data from historic for the overlapping years 1975 - 1979!
  scrapConsumptionWS[getItems(current, dim = 1), getItems(current, dim = 2), ] <- current

  # interpolate missing values ----
  scrapConsumptionWS <- toolInterpolate2D(scrapConsumptionWS)

  # split historic regions ----
  historicalMap <- read.csv2(system.file("extdata", "ISOhistorical.csv", package = "madrat"))
  newCountries <- historicalMap[historicalMap$fromISO %in% getItems(scrapConsumptionWS, dim = 1), "toISO"]
  missingCountries <- setdiff(c(newCountries, "SRB", "MNE"), getItems(scrapConsumptionWS, dim = 1))
  scrapConsumptionWS <- add_columns(scrapConsumptionWS, addnm = missingCountries, dim = 1, fill = NA)

  scrapConsumptionWS <- toolISOhistorical(scrapConsumptionWS)
  scrapConsumptionWS <- toolCountryFill(scrapConsumptionWS, verbosity = 2)

  result <- list(
    x = scrapConsumptionWS,
    weight = NULL,
    unit = "Tonnes",
    description = "scrap consumption based on source WorldSteelDigitised"
  )
}
