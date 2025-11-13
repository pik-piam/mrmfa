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
calcStScrapConsumption <- function(subtype, aggregate = NULL, regionmapping = NULL) {
  consumptionData <- loadSteelScrapConsumptionData()
  scLinear <- calcSteelScrapConsumptionOnlyLinear(consumptionData)
  context <- getAggregationContext(aggregate)

  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
  "assumptions" = function() {
      # Assume backcast with production and forecast with BIR data

      # Backcast recent data with production (basically assumes constant production share)
      scAssumptions <- toolBackcastByReference2D(scLinear, consumptionData$production, doMakeZeroNA = TRUE)

      scAssumptions <- forecastEUwithEUdata(scAssumptions, consumptionData$birEU)

      scAssumptions <- forecastRestWithWorldData(scAssumptions, consumptionData$global, consumptionData$birWorld)

      # Finalize

      scAssumptions[is.na(scAssumptions)] <- 0 # Assume 0 scrap consumption for remaining cells
      scAssumptions <- scAssumptions[, 1:123] # Cut to the years 1900-2022

      result <- list(
        x = scAssumptions,
        weights = NULL,
        unit = "Tonnes",
        description = paste(
          "Worldsteel data on steel scrap consumption with assumptions aggregated to",
          context, "level"
        )
      )

      return(result)
    },
    "noAssumptions" = function() {
      message("Detected aggregation context: ", context) # Log to sanity check if code is correct

      # Country specific
      if (context == "CTY") {
        result <- scLinear # Assume only linear interpolation between known values

        # Global
      } else if (context == "GLO") {
        result <- calcNoAssumptionsGlobal(scLinear, globalConsumption = consumptionData$global)

        # Regional
      } else { # Regional context
        # if H12 is in regionmapping string and EU data is available, use EU data to overwrite
        if (is.null(regionmapping)) {
          regionmapping <- getConfig("regionmapping")
        }
        if (grepl("H12", regionmapping)) {
          result <- calcNoAssumptionsRegional(scLinear, euConsumptionH12 = consumptionData$birEU)
        } else {
          result <- calcNoAssumptionsRegional(scLinear)
        }
      }

      result <- list(
        x = result,
        weights = NULL,
        unit = "Tonnes",
        description = paste(
          "Worldsteel data on steel scrap consumption with limited assumptions aggregated to",
          context, "level"
        )
      )

      return(result)
    },
    NULL
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

getAggregationContext <- function(aggregate = NULL) {
  if (isFALSE(aggregate)) {
    return("CTY")
  } else if (is.character(aggregate) && toupper(aggregate) == "GLO") {
    return("GLO")
  } else {
    return("REG")
  }
}

# ------------- NO ASSUMPTIONS FUNCTIONS -------------

calcNoAssumptionsGlobal <- function(scLinear, globalConsumption) {
  globalConsumption <- toolInterpolate2D(globalConsumption)

  wsGlobal <- new.magpie(
    cells_and_regions = getItems(scLinear, dim = 1),
    years = getItems(globalConsumption, dim = 2),
    names = "value",
    fill = 0,
    sets = names(dimnames(scLinear))
  )
  wsGlobal["ATA", ] <- globalConsumption #  assign to any country to make aggregation work

  return(wsGlobal)
}

calcNoAssumptionsRegional <- function(scLinear, euConsumptionH12 = NULL) {
  # Load countries that are not as important within a region and where hence NAs
  # should be set to 0 to avoid NA as region aggregation
  countries2zero <- read.csv2(
    system.file("extdata", "scrap_consumption_countries_2_zero.csv", package = "mrmfa"),
    comment.char = "#"
  )$Countries2Zero

  copy <- scLinear
  copy[!is.na(copy)] <- 0
  copy[is.na(copy)] <- 1

  h12 <- toolGetMapping("regionmappingH12.csv", where = "madrat")
  refCountries <- h12[h12$RegionCode == "REF", ]$CountryCode

  copy[refCountries, ] <- 1 # never set REF countries to zero

  # Get copy of scLinear where NAs are set to 0
  scLinearNoNA <- scLinear
  scLinearNoNA[is.na(scLinear)] <- 0

  # replace scLinear with copy for less important countries
  scLinear[countries2zero, ] <- scLinearNoNA[countries2zero, ]
  scLinear[is.na(scLinear[countries2zero, ])] <- 0

  # if EU H12 consumption is explicitly given, overwrite EU data with that
  if (!is.null(euConsumptionH12)) {
    euCountries <- getEUcountries()
    euConsumptionH12 <- euConsumptionH12[, !is.na(euConsumptionH12)] # select only years with data
    euConsumptionDataYears <- getItems(euConsumptionH12, dim = 2)
    scLinear[euCountries, euConsumptionDataYears] <- 0
    scLinear["ALA", euConsumptionDataYears] <- euConsumptionH12
  }

  return(scLinear)
}


# ------------- FORECASTING FUNCTIONS FOR ASSUMPTIONS -------------


forecastEUwithEUdata <- function(scAssumptions, euCurrent) {
  euCountries <- getEUcountries()
  scAssumptionsEU <- scAssumptions[euCountries, 1:109]

  getItems(euCurrent, dim = 1) <- "GLO"

  scAssumptionsEU <- toolBackcastByReference2D(scAssumptionsEU, euCurrent, doForecast = TRUE, doMakeZeroNA = TRUE)
  scAssumptions[euCountries, ] <- scAssumptionsEU[euCountries, ]

  return(scAssumptions)
}

getEUcountries <- function() {
  h12 <- toolGetMapping("regionmappingH12.csv", where = "madrat")
  euCountries <- h12[h12$RegionCode == "EUR", ]$CountryCode

  return(euCountries)
}

forecastRestWithWorldData <- function(scAssumptions, globalSum, birGlobalSum) {
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
  scAssumptionsRest <- toolBackcastByReference2D(scAssumptionsRest, restWorldSC, doForecast = TRUE, doMakeZeroNA = TRUE)
  # Backcast
  scAssumptionsRest <- toolBackcastByReference2D(scAssumptionsRest, restWorldSC[, 1:109])

  # Update scAssumptions
  scAssumptions[restWorldCountries, ] <- scAssumptionsRest
  return(scAssumptions)
}


# ------------- CALC BASELINE FUNCTIONS -------------


calcSteelScrapConsumptionOnlyLinear <- function(consumptionData) {
  consumptionData$historic <- getHistoricConsumption(
    consumptionData$historicShare,
    consumptionData$prodHist,
    consumptionData$prodHistGlobal
  )

  scrapConsumptionWS <- calcWorldSteelScrapConsumption(
    consumptionData$historic,
    consumptionData$recent,
    consumptionData$current
  )

  scrapConsumption <- combineWSandBIRscrapConsumption(
    scrapConsumptionWS,
    consumptionData$bir
  )
  return(scrapConsumption)
}


combineWSandBIRscrapConsumption <- function(scrapConsumptionWS,
                                            scrapConsumptionBIR) {
  scrapConsumption <- new.magpie(
    cells_and_regions = getItems(scrapConsumptionWS, dim = 1),
    years = paste0("y", 1965:2025),
    names = "value",
    fill = NA,
    sets = names(dimnames(scrapConsumptionWS))
  )

  scrapConsumption[getItems(scrapConsumptionWS, dim = 1), getItems(scrapConsumptionWS, dim = 2), ] <- scrapConsumptionWS
  scrapConsumption[getItems(scrapConsumptionBIR, dim = 1), getItems(scrapConsumptionBIR, dim = 2), ] <- scrapConsumptionBIR

  scrapConsumption <- toolInterpolate2D(scrapConsumption)

  return(scrapConsumption)
}


# ------------- PREPARE SUPPORTING DATA -------------

calcWorldSteelScrapConsumption <- function(historic, recent, current) {
  allRegions <- union(getItems(historic, dim = 1), union(getItems(recent, dim = 1), getItems(current, dim = 1)))

  scrapConsumptionWS <- new.magpie(
    cells_and_regions = allRegions,
    years = paste0("y", 1965:2008),
    names = "value",
    fill = NA,
    sets = names(dimnames(historic))
  )

  # Add Data

  scrapConsumptionWS[getItems(historic, dim = 1), getItems(historic, dim = 2), ] <- historic
  scrapConsumptionWS[getItems(recent, dim = 1), getItems(recent, dim = 2), ] <- recent
  scrapConsumptionWS[getItems(current, dim = 1), getItems(current, dim = 2), ] <- current

  # Interpolate missing values

  scrapConsumptionWS <- toolInterpolate2D(scrapConsumptionWS)

  # Split historic regions

  newCountries <- read.csv2(system.file("extdata", "ISOhistorical.csv", package = "madrat"))
  newCountries <- newCountries[newCountries$fromISO %in% allRegions, "toISO"]
  missingCountries <- setdiff(newCountries, allRegions)

  scrapConsumptionWS <- add_columns(scrapConsumptionWS, addnm = missingCountries, dim = 1, fill = NA)

  scrapConsumptionWS <- toolISOhistorical(scrapConsumptionWS) %>% suppressWarnings()
  scrapConsumptionWS <- toolCountryFill(scrapConsumptionWS, verbosity = 2) # Setting countries with no data to NA

  return(scrapConsumptionWS)
}


getHistoricConsumption <- function(historicShare, prodHist, prodHistGlobal) {
  # historic scrap production needed to multiply with historic shares as values are needed for former countries

  prodHist <- toolBackcastByReference2D(prodHist, prodHistGlobal)

  prodReduced <- prodHist[getItems(historicShare, dim = 1), getItems(historicShare, dim = 2), ]
  result <- historicShare * prodReduced

  return(result)
}


# ------------- DATA LOADING FUNCTIONS -------------


loadSteelScrapConsumptionData <- function() {
  # Load World Steel Data

  recent <- readSource("WorldSteelDigitised", subtype = "scrapConsumptionYearbooks", convert = FALSE)
  current <- readSource("WorldSteelDigitised", subtype = "scrapConsumptionFigures", convert = FALSE)
  historicShare <- readSource("WorldSteelDigitised", subtype = "specificScrapConsumption70s", convert = FALSE)
  global <- readSource("WorldSteelDigitised", subtype = "worldScrapConsumption", convert = FALSE)
  prodHistGlobal <- readSource("WorldSteelDigitised", subtype = "worldProduction", convert = FALSE)
  prodHist <- readSource("WorldSteelDigitised", subtype = "production", convert = FALSE)

  # Load BIR data

  bir <- readSource("BIR", subtype = "scrapConsumption", convert = FALSE)
  birEU <- readSource("BIR", subtype = "scrapConsumptionEU", convert = FALSE)
  birWorld <- readSource("BIR", subtype = "scrapConsumptionWorld", convert = FALSE)

  # Load other data

  production <- calcOutput("StProduction", aggregate = FALSE)

  # adapt current BLXdata
  current <- adaptBLXdata(current, recent)

  # Finalize

  consumptionData <- list(
    recent = recent,
    current = current,
    historicShare = historicShare,
    global = global,
    prodHistGlobal = prodHistGlobal,
    prodHist = prodHist,
    production = production,
    bir = bir,
    birEU = birEU,
    birWorld = birWorld
  )

  return(consumptionData)
}

adaptBLXdata <- function(current, recent) {
  lastRecentIdx <- length(getItems(recent, dim = 2))
  lastBELval <- recent["BEL", lastRecentIdx]
  lastLUXval <- recent["LUX", lastRecentIdx]
  lastBLXval <- lastBELval + lastLUXval

  current <- add_columns(current, addnm = c("BEL", "LUX"), dim = 1, fill = NA)
  current["BEL", ] <- current["BLX", ] * lastBELval / lastBLXval
  current["LUX", ] <- current["BLX", ] * lastLUXval / lastBLXval
  current <- current[setdiff(getItems(current, dim = 1), "BLX"), ]

  return(current)
}
