#' Get population from 1900-2100
#'
#' @description
#' Calc population from 1900-2100 yearly for the REMIND-MFA format on a country
#' level. Can be aggregated to regions via calcOutput aggregate parameter.
#' Uses \link{readGapminder} and \link[mrdrivers]{readUN_PopDiv}
#' datasets for historical population data as well as
#' \link[mrdrivers]{calcPopulation} from
#' mrdrivers for current and future population data for different
#' scenarios (see \code{vignette("scenarios")} for more information).
#'
#' @author Merlin Jo Hosak, Bennet Weiss
#' @param scenarios Character vector or string specifying the scenarios to use for the future.
#' @param collapse Logical. If TRUE, redundant dimensions (e.g. scenario if only one requested) are removed.
#' @param smooth Logical. If TRUE, data is smoothed using spline interpolation.
#' @param dof Integer. Degrees of freedom for spline interpolation.
#' Higher values lead to a closer fit to the original data, while lower values result in smoother curves.
#' @return List with Magpie object of population and metadata in calcOutput
#' format.
calcCoPopulation1900To2150 <- function(scenarios = "SSP2", collapse = TRUE, smooth = FALSE, dof = 8) {

  # The mrdrivers calcPopulation function provides population data from 1960 on
  # 1 year steps until 2030, 5 year steps thereafter.
  current <- calcOutput("Population", scenario = scenarios, aggregate = FALSE)
  getSets(pop)[3] <- "scenario"
  current <- current * 1e6 # convert from millions to inhabitants
  original_years <- getYears(current, as.integer = TRUE)
  current <- toolInterpolate(current, years = seq(original_years[1], 2150, 1), type = "monotone")
  getSets(current)[3] <- "scenario"

  # The Gapminder dataset reaches from 1800 to 2100, but lacks a few small regions and scenario information.
  # Hence it is only used for backcasting data in the 20th century.
  # 1 year timestep
  hist <- readSource("Gapminder")[, seq(1900, 2000, 1), ]

  # extrapolate with Gapminder dataset as reference data for countries where such data exists
  pop <- toolBackcastByReference(x = current, ref = hist)

  # The UN_PopDiv dataset reaches from 1900 to 2150, in 10 year steps.
  # It is used to extrapolate 20th century data for the remaining regions.
  worldHist <- readSource("UNWorldPopulation")
  worldHist <- toolInterpolate(worldHist, years = seq(1900, 2150, 1), type = "monotone")[, 1900:2000, ]

  # extrapolate with world average as reference data for other countries
  pop <- toolBackcastByReference(x = pop, ref = worldHist)

  if (smooth) {
    # smooth data and interpolate missing data; ensure start, end of historic and end of SSP to remain similar
    pop[, 1900:2100] <- toolTimeSpline(pop[, 1900:2100], dof = dof, peggedYears = c(1900, 2023, 2100))
  }

  # build description including scenario and smoothing note
  smooth_suffix <- if (smooth) ", smoothed." else "."
  description <- paste0("Yearly scenario-dependent population 1900-2150", smooth_suffix)

  if (collapse) {
    # remove redundant dimensions (e.g. scenario if only one requested)
    pop <- collapseDim(pop)
  }

  scenario_note <- if (length(scenarios) == 1 && collapse) "" else ",Driver Scenario"
  note <- paste0("dimensions: (Time,Region", scenario_note, ",value)")

  result <- list(
    x = pop,
    weight = NULL,
    unit = "inhabitants",
    description = description,
    note = note
  )

  return(result)
}
