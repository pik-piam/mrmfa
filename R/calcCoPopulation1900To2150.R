#' Get population from 1900-2100
#'
#' @description
#' Calc population from 1900-2100 yearly for the REMIND-MFA format on a country
#' level. Can be aggregated to regions via calcOutput aggregate parameter.
#' Uses \link{readGapminder} and \link[mrdrivers]{readUN_PopDiv}
#' datasets for historical population data as well as
#' \link[mrdrivers]{calcPopulation} from
#' mrdrivers for current and future population data according to a specific
#' scenario (see \code{vignette("scenarios")} for more information).
#'
#' @author Merlin Jo Hosak, Bennet Weiss
#' @param scenario Scenario to use for future population data (default: SSP2).
#' @param smooth If TRUE, data is smoothed using spline interpolation (default: TRUE).
#' @return List with Magpie object of population and metadata in calcOutput
#' format.
calcCoPopulation1900To2150 <- function(scenario = "SSP2", smooth = TRUE, dof = 8) {
  # The mrdrivers calcPopulation function provides population data from 1960 on
  # 1 year steps until 2030, 5 year steps thereafter.
  current <- calcOutput("Population", scenario = scenario, aggregate = FALSE)
  current <- current * 1e6 # convert from millions to inhabitants
  original_years <- getYears(current, as.integer = TRUE)
  current <- toolInterpolate(current, years = seq(original_years[1], 2150, 1), type = "monotone")

  # The Gapminder dataset reaches from 1800 to 2100, but lacks a few small
  # regions and lacks scenario information. Hence it is only used for
  # extrapolation of scenario data in the 20th century.
  # 1 year timestep
  hist <- readSource("Gapminder")[, seq(1900, 2000, 1), ]

  # extrapolate with Gapminder dataset as reference data for countries where such data exists
  pop <- toolBackcastByReference2D(x = current, ref = hist)

  # The UN_PopDiv dataset reaches from 1900 to 2150, in 10 year steps.
  # It is used to extrapolate 20th century data for the remaining regions.
  worldHist <- readSource("UNWorldPopulation")
  worldHist <- toolInterpolate(worldHist, years = seq(1900, 2150, 1), type = "monotone")[, 1900:2000, ]

  # extrapolate with world average as reference data for other countries
  pop <- toolBackcastByReference2D(x = pop, ref = worldHist)

  if (smooth) {
    # smooth data and interpolate mising years.
    pop[, 1900:2100] <- toolTimeSpline(pop[, 1900:2100], dof = dof, peggedYears = c(1900, 2100))
  }

  result <- list(
    x = pop,
    weight = NULL,
    unit = "inhabitants",
    description = "Population from 1900-2150 yearly",
    note = "dimensions: (Time,Region,value)"
  )

  return(result)
}
