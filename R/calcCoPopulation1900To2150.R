#' Get population from 1900-2100
#'
#' @description
#' Calc population from 1900-2100 yearly for the REMIND-MFA format on a country
#' level. Can be aggregated to regions via calcOutput aggregate parameter.
#' Uses \link[=readGapminder]{Gapminder} and \link[mrdrivers]{readUN_PopDiv}
#' datasets for historical population data as well as \link[mrdrivers]{calcPopulation} from
#' mrdrivers for current and future population data according to a specific
#' scenario (see \code{vignette("scenarios")} for more information).
#'
#' @author Merlin Jo Hosak
#' @param scenario Scenario to use for future population data (default: SSP2).
#' @return List with Magpie object of population and metadata in calcOutput
#' format.
#' @export
calcCoPopulation1900To2150 <- function(scenario='SSP2') {
  pop_data <- getPopulation1900To2150Data(scenario=scenario)

  # get yearly resolution for future
  future <- time_interpolate(pop_data$current,2030:2150)
  pop_data$current <- mbind(pop_data$current[,1960:2029],future)

  # extrapolate with Gapminder dataset as reference data for countries where such data exists
  pop <- toolBackcastByReference2D(x=pop_data$current, ref=pop_data$hist)

  # extrapolate with world average as reference data for other countries
  pop <- toolBackcastByReference2D(x=pop, ref=pop_data$world_hist)


  # check if there are any NA left in pop
  if (any(is.na(pop))) {
    warning("There are still NA values in the population data after extrapolation.")
  }
  description='Population from 1900-2100 yearly for the REMIND-MFA format'
  description <- paste(description, "\ndimensions: (Time,Region,value)")

  result <- list(x = pop,
                 weight = NULL,
                 unit='inhabitants',
                 description=description)


  return(result)
}


getPopulation1900To2150Data <- function(scenario) {
  # Load datasets, convert to inhabitants, get one year resolution
  # via linear interpolation

  # The Gapminder dataset reaches from 1800 to 2100, but lacks a few small
  # regions and lacks scenario information. Hence it is only used for
  # extrapolation of scenario data in the 20th century.
  pop_hist <- readSource('Gapminder', subtype='population')
  pop_hist <- pop_hist[,1900:2000]

  # The UN_PopDiv dataset reaches from 1900 to 2150, in 5 year steps and is
  # therefore interpolated to 1 year resolution. It is used to extrapolate
  # 20th century data for the remaining regions.
  pop_world_hist <- readSource('UN_PopDiv', subtype='pop', subset='1900-2150', convert=F)
  pop_world_hist <- time_interpolate(pop_world_hist,1900:2150)

  # The mrdrivers calcPopulation function provides population data from 1960 on
  pop_current <- calcOutput('Population', scenario=scenario, aggregate=F)
  getItems(pop_current, dim=3) <- 'value'
  pop_current <- pop_current * 1e6 # convert from millions to inhabitants

  return(list(hist=pop_hist,
              world_hist=pop_world_hist,
              current=pop_current))
}








