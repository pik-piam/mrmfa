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
#'
#' @author Merlin Jo Hosak
calcStScrapConsumption <- function(subtype) {
  # internal helper functions ----

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


  scrapConsumptionWS <- calcOutput("StScrapConsumptionWS", aggregate = FALSE, warnNA = FALSE)

  scrapConsumptionBIR <- readSource("BIR", subtype = "scrapConsumption")
  # remove regions containing only NAs
  remove <- magpply(scrapConsumptionBIR, function(y) all(is.na(y)), MARGIN = 1)
  scrapConsumptionBIR <- scrapConsumptionBIR[!remove, , ]

  # combine WS and BIR scrap consumption ----

  scrapConsumption <- new.magpie(
    cells_and_regions = getItems(scrapConsumptionWS, dim = 1),
    years = seq(1965, 2025, 1),
    names = "value",
    fill = NA,
    sets = names(dimnames(scrapConsumptionWS))
  )

  scrapConsumption[, getItems(scrapConsumptionWS, dim = 2), ] <- scrapConsumptionWS

  # note that this overwrites some data from WorldSteelDigitised for the overlapping years 1998 - 2008!
  scrapConsumption[
    getItems(scrapConsumptionBIR, dim = 1),
    getItems(scrapConsumptionBIR, dim = 2),
  ] <- scrapConsumptionBIR

  scLinear <- toolInterpolate2D(scrapConsumption)

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
