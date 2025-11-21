#' Calc steel scrap consumption
#' @description
#' Function to calculate steel scrap consumption based on World Steel
#' Association (see \link{readWorldSteelDigitised})
#' data and Bureau of International Recycling (BIR) (see
#' \link{readBIR}) data. Different subtypes allow to either use
#' assumptions to fill data gaps or not.
#'
#' With assumptions, basic backcasting by production data and forecasting
#' via BIR data is used. For no assumptions, only linear interpolation is used.
#' Data is manipulated so that when aggregating regions with missing data
#' in less relevant subunits, these missing data are set to 0 to avoid NAs in
#' the regional aggregation (as well as global aggregation).
#'
#' The countries were NA should be zeroed are defined in
#' scrap_consumption_countries_2_zero.csv .
#'
#' @param subtype Subtype of steel scrap consumption data to calculate.
#' Options: 'assumptions' or 'noAssumptions'. Must be set deliberately.
#' @param aggregate Aggregation level. TRUE uses regionmapping,
#' F for country level, 'GLO' for global level. See \link[madrat]{calcOutput}
#' documentation. ++REGGLO/regglo/reg+glo etc. deprecated.
#' @param regionmapping Regionmapping to use for regional aggregation. Should default
#' to H12 REMIND regions.
#'
#' @author Merlin Jo Hosak
calcStScrapConsumption <- function(subtype) {
  # internal helper functions ----
  # TODO: refactor this
  .calcSteelScrapConsumptionOnlyLinear <- function() {
    # get historic consumption ----

    prodHist <- readSource("WorldSteelDigitised", subtype = "production", convert = FALSE)
    prodHistGlobal <- readSource("WorldSteelDigitised", subtype = "worldProduction", convert = FALSE)
    prodHist <- toolBackcastByReference2D(prodHist, prodHistGlobal)

    historicShare <- readSource("WorldSteelDigitised", subtype = "specificScrapConsumption70s", convert = FALSE)

    # historic scrap production needed to multiply with historic shares as values are needed for former countries
    historic <- historicShare * prodHist[getItems(historicShare, dim = 1), getItems(historicShare, dim = 2), ]

    # get recent and current consumption ----
    recent <- readSource("WorldSteelDigitised", subtype = "scrapConsumptionYearbooks", convert = FALSE)
    current <- readSource("WorldSteelDigitised", subtype = "scrapConsumptionFigures", convert = FALSE)

    # split BLX in current according to ratio in recent ----
    # TODO: move to convert function
    lastRecentIdx <- length(getItems(recent, dim = 2))
    lastBELval <- recent["BEL", lastRecentIdx]
    lastLUXval <- recent["LUX", lastRecentIdx]
    lastBLXval <- lastBELval + lastLUXval

    current <- add_columns(current, addnm = c("BEL", "LUX"), dim = 1, fill = NA)
    current["BEL", ] <- current["BLX", ] * lastBELval / lastBLXval
    current["LUX", ] <- current["BLX", ] * lastLUXval / lastBLXval
    current <- current["BLX", , , invert = TRUE]

    # merge all world steel digitised sources ----
    allRegions <- union(getItems(historic, dim = 1), union(getItems(recent, dim = 1), getItems(current, dim = 1)))

    scrapConsumptionWS <- new.magpie(
      cells_and_regions = allRegions,
      years = paste0("y", 1965:2008),
      names = "value",
      fill = NA,
      sets = names(dimnames(historic))
    )

    scrapConsumptionWS[getItems(historic, dim = 1), getItems(historic, dim = 2), ] <- historic
    scrapConsumptionWS[getItems(recent, dim = 1), getItems(recent, dim = 2), ] <- recent
    scrapConsumptionWS[getItems(current, dim = 1), getItems(current, dim = 2), ] <- current

    # interpolate missing values ----
    scrapConsumptionWS <- toolInterpolate2D(scrapConsumptionWS)
    # split historic regions ----
    # TODO: this should be done much earlier in convert function (merge read subtypes?)
    map <- read.csv2(system.file("extdata", "ISOhistorical.csv", package = "madrat"))
    newCountries <- map[map$fromISO %in% allRegions, "toISO"]
    missingCountries <- setdiff(c(newCountries, "SRB", "MNE"), allRegions)

    scrapConsumptionWS <- add_columns(scrapConsumptionWS, addnm = missingCountries, dim = 1, fill = NA)

    scrapConsumptionWS <- toolISOhistorical(scrapConsumptionWS)

    scrapConsumptionWS <- toolCountryFill(scrapConsumptionWS, verbosity = 2)

    # combine WS and BIR scrap consumption ----

    scrapConsumption <- new.magpie(
      cells_and_regions = getItems(scrapConsumptionWS, dim = 1),
      years = paste0("y", 1965:2025),
      names = "value",
      fill = NA,
      sets = names(dimnames(scrapConsumptionWS))
    )

    scrapConsumptionBIR <- readSource("BIR", subtype = "scrapConsumption")

    # remove regions containing only NAs
    remove <- magpply(scrapConsumptionBIR, function(y) all(is.na(y)), MARGIN = 1)
    scrapConsumptionBIR <- scrapConsumptionBIR[!remove, , ]

    scrapConsumption[getItems(scrapConsumptionWS, dim = 1),
                     getItems(scrapConsumptionWS, dim = 2), ] <- scrapConsumptionWS
    scrapConsumption[getItems(scrapConsumptionBIR, dim = 1),
                     getItems(scrapConsumptionBIR, dim = 2), ] <- scrapConsumptionBIR

    scrapConsumption <- toolInterpolate2D(scrapConsumption)

    return(scrapConsumption)
  }

  .forecastEUData <- function(scAssumptions) {
    euCurrent <- readSource("BIR", subtype = "scrapConsumption", convert = FALSE)["EU 28", , ]

    h12 <- toolGetMapping("regionmappingH12.csv", where = "madrat")
    euCountries <- h12[h12$RegionCode == "EUR", ]$CountryCode

    scAssumptionsEU <- scAssumptions[euCountries, seq(1900, 2008, 1)]
    getItems(euCurrent, dim = 1) <- "GLO"
    scAssumptionsEU <- toolBackcastByReference2D(scAssumptionsEU, euCurrent,
      doForecast = TRUE, doMakeZeroNA = TRUE
    )
    scAssumptions[euCountries, ] <- scAssumptionsEU[euCountries, ]

    return(scAssumptions)
  }

  .forecastRestWithWorldData <- function(scAssumptions) {
    birGlobalSum <- readSource("BIR", subtype = "scrapConsumption", convert = FALSE)["World", , ]
    globalSum <- readSource("WorldSteelDigitised", subtype = "worldScrapConsumption", convert = FALSE)

    # Get sum of countries which have no NA before 2023
    noNAcountries <- getItems(scAssumptions[rowSums(is.na(scAssumptions[, 1:123])) == 0, ], dim = 1)
    scAssumptionsNoNA <- scAssumptions[noNAcountries, ]
    sumSCnoNA <- colSums(scAssumptionsNoNA, na.rm = TRUE)

    # Get assumption of world consumption
    worldCurrent <- toolBackcastByReference2D(globalSum, birGlobalSum, doForecast = TRUE, doMakeZeroNA = TRUE)
    worldSC <- toolBackcastByReference2D(worldCurrent, sumSCnoNA[, 1:109])

    # Calculate assumption for rest of world
    restWorldSC <- worldSC - sumSCnoNA

    # Fore- and backcast countries with missing data with assumptions for rest of the world
    restWorldCountries <- setdiff(getItems(scAssumptions, dim = 1), noNAcountries)

    scAssumptionsRest <- scAssumptions[restWorldCountries, ]

    # Forecast
    scAssumptionsRest <- toolBackcastByReference2D(scAssumptionsRest, restWorldSC,
      doForecast = TRUE,
      doMakeZeroNA = TRUE
    )

    # Backcast
    scAssumptionsRest <- toolBackcastByReference2D(scAssumptionsRest, restWorldSC[, 1:109])

    # Update scAssumptions
    scAssumptions[restWorldCountries, ] <- scAssumptionsRest
    return(scAssumptions)
  }

  # main logic ----

  scLinear <- .calcSteelScrapConsumptionOnlyLinear()


  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    "assumptions" = function() {
      # assume backcast with production and forecast with BIR data

      # backcast recent data with production (basically assumes constant production share) ----
      production <- calcOutput("StProduction", aggregate = FALSE)
      scAssumptions <- toolBackcastByReference2D(scLinear, production, doMakeZeroNA = TRUE)

      # forecast EU with EU data ----
      scAssumptions <- .forecastEUData(scAssumptions)

      # forcecast rest of World with World Data ----
      scAssumptions <- .forecastRestWithWorldData(scAssumptions)

      # finalize ----
      scAssumptions[is.na(scAssumptions)] <- 0 # Assume 0 scrap consumption for remaining cells
      scAssumptions <- scAssumptions[, seq(1900, 2022, 1), ] # Cut to the years 1900-2022

      result <- list(
        x = scAssumptions,
        weight = NULL,
        unit = "Tonnes",
        description = "Worldsteel data on steel scrap consumption with assumptions"
      )

      return(result)
    },
    "noAssumptions" = function() {

      # Load countries that are not as important within a region and where hence NAs
      # should be set to 0 to avoid NA as region aggregation
      f <- toolGetMapping("scrap_consumption_countries_2_zero.csv", where = "mrmfa", returnPathOnly = TRUE)
      countries2zero <- read.csv2(f, comment.char = "#")$Countries2Zero

      tmp <- scLinear[countries2zero, , ]
      tmp[is.na(tmp)] <- 0
      scLinear[countries2zero, , ] <- tmp

      # if EU H12 consumption is explicitly given, overwrite EU data with that
      # includes values from the original source for global region instead of calculating
      # it as the sum of all countries (as countries are incomplete)
      .customAggregate <- function(x, rel, to = NULL, eu28) {

        out <- toolAggregate(x, rel = rel, to = to)

        if ("EUR" %in% getItems(out, dim = 1)) {
          eu28 <- eu28[, !is.na(eu28), ] # select only years with data
          out["EUR", getYears(eu28), getNames(eu28)] <- eu28
        }

        return(out)
      }

      birEu28 <- readSource("BIR", subtype = "scrapConsumption", convert = FALSE)["EU 28", , ]

      result <- list(
        x = scLinear,
        weight = NULL,
        unit = "Tonnes",
        aggregationFunction = .customAggregate,
        aggregationArguments = list(eu28 = birEu28),
        description = "Worldsteel data on steel scrap consumption with limited assumptions"
      )

      return(result)
    }
  )

  # ---- check if the subtype called is available ----
  if (is_empty(intersect(subtype, names(switchboard)))) {
    stop(paste(
      "Invalid subtype -- supported subtypes are:",
      names(switchboard)
    ))
  } else {
    # ---- load data and do whatever ----
    return(switchboard[[subtype]]())
  }
}
