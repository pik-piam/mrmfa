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
  pop <- calcOutput("CoPopulation1900To2150", aggregate = FALSE)

  gdpHistPC <- readSource("OECD_GDP")
  gdpHistPC <- toolInterpolate2D(gdpHistPC, method = "linear")

  gdpRecent <- calcOutput("GDP", scenario = scenario, aggregate = FALSE)
  gdpRecent <- gdpRecent * 1e6 # convert to million USD
  getItems(gdpRecent, dim = 3) <- "value"
  gdpRecent <- time_interpolate(gdpRecent, seq(1965, 2150, 1))

  # convert historic data from per capita to total
  # data before 1900 irrelevant (don't cut off before because it helps for interpolation)
  hist <- pop[, seq(1900, 2016, 1), ] * gdpHistPC[, seq(1900, 2016, 1), ]

  # extrapolate data with OECD data as reference where data is available
  gdp <- toolBackcastByReference2D(gdpRecent, ref = hist, doInterpolate = FALSE) # Interpolation already done

  # extrapolate GDP data by global total for regions without OECD data

  ## get GDP of regions that have data up to 1900
  regions <- getItems(gdp, dim = 1)
  regionsNotNA <- regions[!is.na(gdp[, 1900, ])]

  ## sum over these regions
  sumGDPfrom1900 <- dimSums(gdp[regionsNotNA, , ], dim = 1)
  getItems(sumGDPfrom1900, dim = 1) <- "GLO"

  # extrapolate missing regions with the global average
  gdp <- toolBackcastByReference2D(gdp, ref = sumGDPfrom1900)

  # finalize for calcOutput
  unit <- "2005 USD$PPP" # unit is that of calcGDP data as OECD data is just used for backcasting
  description <- "GDP from 1900-2150 yearly for the SIMSON format"
  weight <- NULL
  getNames(gdp) <- NULL

  # convert to per capita if requested
  if (perCapita) {
    gdp <- gdp / pop
    unit <- paste0(unit, " per capita")
    description <- "GDP per capita from 1900-2150 yearly for the SIMSON format"
    weight <- pop
  }

  result <- list(
    x = gdp,
    weight = weight, # TODO adapt weight for per capita data
    unit = unit,
    description = description
  )

  return(result)
}
