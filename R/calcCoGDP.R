#' Get GDP from 1800-2150
#' @description
#' Calc GDP (PPP) from 1800-2150 yearly for the REMIND-MFA format on a country
#' level. Can be aggregated to regions via calcOutput aggregate parameter.
#' Uses \link[=readOECD_GDP]{OECD_GDP} for historical GDP data as well as
#' \link[mrdrivers]{calcGDP} from mrdrivers for current and future GDP data
#' according to specific scenarios.
#' for more information). Population data from
#' \link[=calcCoPopulation]{calcCoPopulation} is used to
#' convert GDP per capita to total GDP.
#' GDP is given in 2005 USD (PPP). It's extrapolated to the past with historic
#' GDP datasets that use a different base year, which however does not matter
#' as only the relative values are used
#' (see \link{toolInterpolate}).
#' @author Merlin Jo Hosak, Bennet Weiss
#' @param perCapita Logical. If TRUE, GDP is returned as per capita.
#' @param scenarios Character vector or string specifying the scenarios to use for the future.
#' @param collapse Logical. If TRUE, redundant dimensions (e.g. scenario if only one requested) are removed.
#' @param smooth Logical. If TRUE, data is smoothed using spline interpolation.
#' @param dof Integer. Degrees of freedom for spline interpolation.
#' Higher values lead to a closer fit to the original data, while lower values result in smoother curves.
#' @return List with Magpie object of GDP (given in 2005 USD) and metadata in calcOutput format.
calcCoGDP <- function(perCapita = FALSE, scenarios = "SSP2", collapse = TRUE, smooth = FALSE, dof = 8) {
  startyear <- 1800
  endyear <- 2150

  # load population data, used for historical purposes only
  pop <- calcOutput("CoPopulation", aggregate = FALSE)

  # Historic GDP data that goes way back in time, with 1 year timestep
  gdpHistPC <- readSource("OECD_GDP")
  most_recent_hist_year <- tail(getYears(gdpHistPC, as.integer = TRUE), 1)
  gdpHistPC <- toolInterpolate(gdpHistPC, type = "monotone", maxgap = 20)

  # Historic and Future GDP data: 1960-2030 with 1 year timestep, thereafter with 5 year timestep
  # turn off average2020 to get yearly data where possible (and of course remove covid correction)
  gdpRecent <- calcOutput("GDP", scenario = scenarios, aggregate = FALSE, average2020 = FALSE)
  getSets(gdpRecent)[3] <- "scenario"
  gdpRecent <- gdpRecent * 1e6 # convert from million USD to USD
  original_years <- getYears(gdpRecent, as.integer = TRUE)
  gdpRecent <- toolInterpolate(gdpRecent, years = seq(original_years[1], endyear, 1), type = "monotone")

  # convert historic data from per capita to total
  # data before startyear irrelevant (not cut off before because it helps for interpolation)
  hist <- pop[, startyear:most_recent_hist_year, ] * gdpHistPC[, startyear:most_recent_hist_year, ]

  # backcast data with OECD data as reference where data is available
  gdp <- toolBackcastByReference(gdpRecent, ref = hist, doInterpolate = FALSE) # Interpolation already done

  # backcast GDP data by global total for regions without OECD data

  ## get GDP of regions that have complete data
  regions <- getItems(gdp, dim = 1)
  regionsNotNA <- regions[!is.na(dimSums(gdp, dim = c(2, 3)))]

  ## sum over these regions
  sumAvaliableGDP <- dimSums(gdp[regionsNotNA, , ], dim = 1)
  getItems(sumAvaliableGDP, dim = 1) <- "GLO"

  ## backcast missing regions with the global average
  gdp <- toolBackcastByReference(gdp, ref = sumAvaliableGDP)

  if (smooth) {
    # smooth data and interpolate missing data
    years <- startyear:2100
    gdp[, startyear:2100] <- toolTimeSpline(gdp[, startyear:2100], dof = dof, peggedYears = c(1900, 2023, 2100))
  }

  # finalize for calcOutput
  unit <- "2005 USD$PPP" # unit is that of calcGDP data as OECD data is just used for backcasting
  # build description incorporating scenario and optional smoothing
  smooth_suffix <- if (smooth) ", smoothed." else "."

  # convert to per capita if requested
  if (perCapita) {
    pop <- calcOutput(
      "CoPopulation",
      scenarios = scenarios,
      collapse = collapse,
      smooth = smooth,
      dof = dof,
      aggregate = FALSE
    )
    gdp <- gdp / pop
    unit <- paste0(unit, " per capita")
    # adjust description prefix
    description <- paste0("Yearly scenario-dependent GDP per capita (historical and future) ", smooth_suffix)
    weight <- pop
  } else {
    description <- paste0("Yearly scenario-dependent GDP (historical and future) ", smooth_suffix)
    weight <- NULL
  }

  if (collapse) {
    # remove redundant dimensions (e.g. scenario if only one requested)
    gdp <- collapseDim(gdp)
  }

  scenario_note <- if (length(scenarios) == 1 && collapse) "" else ",Driver Scenario"
  note <- paste0("dimensions: (Time,Region", scenario_note, ",value)")

  result <- list(
    x = gdp,
    weight = weight,
    unit = unit,
    description = description,
    note = note
  )

  return(result)
}
