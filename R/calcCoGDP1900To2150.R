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
#' (see \link[=toolInterpolate]{toolInterpolate}).
#' @author Merlin Jo Hosak
#' @param scenario Scenario to use for future GDP data (default: SSP2).
#' @param per_capita If TRUE, GDP is returned as per capita (default: TRUE).
#' @return List with Magpie object of GDP and metadata in calcOutput format.
#' @export
calcCoGDP1900To2150 <- function(scenario='SSP2', per_capita=TRUE) {
  # load data
  gdp_data <- getGDP1900To2150Data(scenario=scenario)

  # interpolate
  gdp_data <- interpolateGDP1900To2150(gdp_data)

  # convert historic data from per capita to total
  gdp_data$hist_pc <- gdp_data$hist_pc[,1900:2016]  # data before 1900 irrelevant (don't cut off before because it helps for interpolation)
  gdp_data$hist <- gdp_data$pop[,1:117] * gdp_data$hist_pc

  # extrapolate
  gdp <- extrapolateGDP1900To2150(gdp_data)

  # finalize for calcOutput

  unit <- '2005 USD$PPP'
  description='GDP from 1900-2150 yearly for the REMIND-MFA format'
  weight<-NULL

  # convert to per capita if requested
  if (per_capita) {
    gdp <- gdp / gdp_data$pop
    unit <- '2005 USD$PPP per capita'
    description='GDP per capita from 1900-2150 yearly for the REMIND-MFA format'
    weight <- gdp_data$pop
  }

  # check if there are any NA left in gdp
  if (any(is.na(gdp))) {
    warning("There are still NA values in the GDP data.")
  }

  description <- paste(description, "\ndimensions: (Time,Region,value)")

  result <- list(x = gdp,
                 weight = weight,  # TODO adapt weight for per capita data
                 unit=unit,
                 description=description)


  return(result)
}

getGDP1900To2150Data <- function(scenario){
  # load data
  pop <- calcOutput('CoPopulation1900To2150', aggregate=F)
  gdp_pc_hist <- readSource('OECD_GDP', subtype='gdppc',convert=T)
  gdp_recent <- calcOutput('GDP', scenario=scenario, aggregate=F)

  # convert format
  gdp_recent <- gdp_recent * 1e6  # convert to million USD
  getItems(gdp_recent,dim=3) <- 'value'

  return(list(pop=pop,
              hist_pc=gdp_pc_hist,
              recent=gdp_recent))
}

interpolateGDP1900To2150 <- function(gdp_data) {
  gdp_data$recent <- time_interpolate(gdp_data$recent, 1965:2150)
  gdp_data$hist_pc <- toolInterpolate2D(gdp_data$hist_pc, method = 'linear')

  return(gdp_data)
}

extrapolateGDP1900To2150 <- function(gdp_data) {
  # Extrapolate data with OECD data as reference where data is available
  gdp <- toolBackcastByReference2D(gdp_data$recent, ref=gdp_data$hist, do_interpolate=F)  # Interpolation already done


  # Extrapolate GDP data by global total for regions without OECD data

  ## get GDP of regions that have data up to 1900
  regions <- getRegions(gdp)
  regions_not_na <- regions[!is.na(gdp[,1])]
  gdp_from_1900 <-gdp[regions_not_na,]

  ## sum over these regions
  mapping <- data.frame(from = regions_not_na, global = 'GLO')
  sum_gdp_from_1900 <- toolAggregate(gdp_from_1900, rel = mapping)


  # Extrapolate missing regions with the global average
  gdp <- toolBackcastByReference2D(gdp, ref = sum_gdp_from_1900)

  return(gdp)
}






