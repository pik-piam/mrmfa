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
#' @author Merlin Jo Hosak
#' @param scenario Scenario to use for future population data (default: SSP2).
#' @return List with Magpie object of population and metadata in calcOutput
#' format.
calcCoPopulation1900To2150 <- function(scenario = "SSP2") {
  # The mrdrivers calcPopulation function provides population data from 1960 on
  current <- calcOutput("Population", scenario = scenario, aggregate = FALSE)
  current <- current * 1e6 # convert from millions to inhabitants

  # get yearly resolution for future
  future <- time_interpolate(current, seq(2030, 2150, 1))
  current <- mbind(current[, seq(1960, 2029, 1), ], future)

  # The Gapminder dataset reaches from 1800 to 2100, but lacks a few small
  # regions and lacks scenario information. Hence it is only used for
  # extrapolation of scenario data in the 20th century.
  hist <- readSource("Gapminder")[, seq(1900, 2000, 1), ]

  # extrapolate with Gapminder dataset as reference data for countries where such data exists
  pop <- toolBackcastByReference(x = current, ref = hist)

  # The UN_PopDiv dataset reaches from 1900 to 2150, in 5 year steps and is
  # therefore interpolated to 1 year resolution. It is used to extrapolate
  # 20th century data for the remaining regions.
  worldHist <- readSource("UNWorldPopulation")
  worldHist <- time_interpolate(worldHist, seq(1900, 2150, 1))
  worldHist <- worldHist * 1e3 # convert from thousands to inhabitants

  # extrapolate with world average as reference data for other countries
  pop <- toolBackcastByReference(x = pop, ref = worldHist)

  result <- list(
    x = pop,
    weight = NULL,
    unit = "inhabitants",
    description = "Population from 1900-2150 yearly",
    note = "dimensions: (Time,Region,value)"
  )

  return(result)
}
