#' Get GDP from 1900-2150
#' @description
#' Calc GDP (PPP) from 1900-2150 yearly for the SIMSON format on a country
#' level. Can be aggregated to regions via calcOutput aggregate parameter.
#' Uses \link[=readOECD_GDP]{OECD_GDP} for historical GDP data as well as
#' \link[mrdrivers]{calcGDP} from mrdrivers for current and future GDP data
#' according to a specific scenario (see \code{vignette("scenarios")}
#' for more information). Population data from
#' \link[=calcCoPopulation1900To2150]{calcCoPopulation1900To2150} is used to
#' convert GDP per capita to total GDP.
#' GDP is given in 2005 USD (PPP). It's extrapolated to the past with historic
#' GDP datasets that use a different base year, which however does not matter
#' as only the relative values are used
#' (see \link{toolInterpolate2D}).
#' @author Merlin Jo Hosak
#' @param scenario Scenario to use for future GDP data (default: SSP2).
#' @param perCapita If TRUE, GDP is returned as per capita (default: FALSE).
#' @return List with Magpie object of GDP (given in 2005 USD) and metadata in calcOutput format.
calcCoGDP1900To2150 <- function(scenario = "SSP2", perCapita = FALSE) {
  # load data
  gdpData <- getGDP1900To2150Data(scenario = scenario)

  # interpolate
  gdpData <- interpolateGDP1900To2150(gdpData)

  # convert historic data from per capita to total
  # data before 1900 irrelevant (don't cut off before because it helps for interpolation)
  gdpData$histPC <- gdpData$histPC[, 1900:2016]
  gdpData$hist <- gdpData$pop[, 1:117] * gdpData$histPC

  # extrapolate
  gdp <- extrapolateGDP1900To2150(gdpData)

  # check if there are any NA left in gdp
  if (any(is.na(gdp))) {
    warning("There are still NA values in the GDP data.")
  }

  # finalize for calcOutput
  unit <- "2005 USD$PPP" # unit is that of calcGDP data as OECD data is just used for backcasting
  description <- "GDP from 1900-2150 yearly for the SIMSON format"
  weight <- NULL

  # convert to per capita if requested
  if (perCapita) {
    gdp <- gdp / gdpData$pop
    unit <- paste0(unit, " per capita")
    description <- "GDP per capita from 1900-2150 yearly for the SIMSON format"
    weight <- gdpData$pop
  }

  result <- list(
    x = gdp,
    weight = weight, # TODO adapt weight for per capita data
    unit = unit,
    description = description
  )


  return(result)
}

getGDP1900To2150Data <- function(scenario) {
  # load data
  pop <- calcOutput("Population1900To2150", aggregate = FALSE)
  gdpHistPC <- readSource("OECD_GDP", subtype = "gdpPC", convert = TRUE)
  gdpRecent <- calcOutput("GDP", scenario = scenario, aggregate = FALSE)

  # convert format
  gdpRecent <- gdpRecent * 1e6 # convert to million USD
  getItems(gdpRecent, dim = 3) <- "value"

  return(list(
    pop = pop,
    histPC = gdpHistPC,
    recent = gdpRecent
  ))
}

interpolateGDP1900To2150 <- function(gdpData) {
  gdpData$recent <- time_interpolate(gdpData$recent, 1965:2150)
  gdpData$histPC <- toolInterpolate2D(gdpData$histPC, method = "linear")

  return(gdpData)
}

extrapolateGDP1900To2150 <- function(gdpData) {
  # Extrapolate data with OECD data as reference where data is available
  gdp <- toolBackcastByReference2D(gdpData$recent, ref = gdpData$hist, doInterpolate = FALSE) # Interpolation already done


  # Extrapolate GDP data by global total for regions without OECD data

  ## get GDP of regions that have data up to 1900
  regions <- getRegions(gdp)
  regionsNotNA <- regions[!is.na(gdp[, 1])]
  gdpFrom1900 <- gdp[regionsNotNA, ]

  ## sum over these regions
  mapping <- data.frame(from = regionsNotNA, global = "GLO")
  sumGDPfrom1900 <- toolAggregate(gdpFrom1900, rel = mapping)


  # Extrapolate missing regions with the global average
  gdp <- toolBackcastByReference2D(gdp, ref = sumGDPfrom1900)

  return(gdp)
}
