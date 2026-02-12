#' Get GDP from 1900-2150
#' @description
#' Calc GDP (PPP) from 1900-2150 yearly for the REMIND-MFA format on a country
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
#' (see \link{toolInterpolate}).
#' @author Merlin Jo Hosak, Bennet Weiss
#' @param scenario Scenario to use for future GDP data (default: SSP2).
#' @param perCapita If TRUE, GDP is returned as per capita (default: FALSE).
#' @param smooth If TRUE, data is smoothed using spline interpolation (default: TRUE).
#' @return List with Magpie object of GDP (given in 2005 USD) and metadata in calcOutput format.
calcCoGDP1900To2150 <- function(scenario = "SSP2", perCapita = FALSE, smooth = TRUE) {
  startyear <- 1900
  endyear <- 2150

  # load data
  pop <- calcOutput("CoPopulation1900To2150", aggregate = FALSE)

  # Historic GDP data that goes way back in time, with 1 year timestep
  gdpHistPC <- readSource("OECD_GDP")
  most_recent_hist_year <- tail(getYears(gdpHistPC, as.integer = TRUE), 1)
  gdpHistPC <- toolInterpolate(gdpHistPC, type = "monotone", maxgap = 20)

  # Historic and Future GDP data, with 5 year timestep, does not go back as far in time
  gdpRecent <- calcOutput("GDP", scenario = scenario, aggregate = FALSE)
  gdpRecent <- gdpRecent * 1e6 # convert from million USD to USD
  getItems(gdpRecent, dim = 3) <- "value"
  original_years <- getYears(gdpRecent)

  # convert historic data from per capita to total
  # data before startyear irrelevant (not cut off before because it helps for interpolation)
  hist <- pop[, startyear:most_recent_hist_year, ] * gdpHistPC[, startyear:most_recent_hist_year, ]

  # backcast data with OECD data as reference where data is available
  gdp <- toolBackcastByReference2D(gdpRecent, ref = hist, doInterpolate = FALSE) # Interpolation already done

  # backcast GDP data by global total for regions without OECD data

  ## get GDP of regions that have data back to startyear
  regions <- getItems(gdp, dim = 1)
  regionsNotNA <- regions[!is.na(gdp[, startyear, ])]

  ## sum over these regions
  sumAvaliableGDP <- dimSums(gdp[regionsNotNA, , ], dim = 1)
  getItems(sumAvaliableGDP, dim = 1) <- "GLO"

  ## backcast missing regions with the global average
  gdp <- toolBackcastByReference2D(gdp, ref = sumAvaliableGDP)

  if (smooth) {
    # smooth data and interpolate missing data. No NAs expected due to global backcasting.
    gdp <- toolTimeSpline(gdp, targetYears = seq(startyear, endyear, 1), peggedYears = original_years)
  } else {
    # just interpolate missing years without smoothing
    gdp <- toolInterpolate(gdp, years = seq(startyear, endyear, 1), type = "monotone")
  }

  # finalize for calcOutput
  unit <- "2005 USD$PPP" # unit is that of calcGDP data as OECD data is just used for backcasting
  description <- paste0("GDP from ", startyear, "-", endyear, " yearly")
  weight <- NULL
  getNames(gdp) <- NULL

  # convert to per capita if requested
  if (perCapita) {
    gdp <- gdp / pop
    unit <- paste0(unit, " per capita")
    description <- paste0("GDP per capita from ", startyear, "-", endyear, " yearly")
    weight <- pop
  }

  result <- list(
    x = gdp,
    weight = weight, # TODO adapt weight for per capita data
    unit = unit,
    description = description,
    note = "dimensions: (Time,Region,value)"
  )

  return(result)
}
