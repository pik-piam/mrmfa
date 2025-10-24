#' Get Steel Production data
#' 
#' @description
#' Calc steel production from WorldSteel datasets. Can be aggregated to regions 
#' via calcOutput aggregate parameter. Uses 
#' \link{readWorldSteelDigitised} and 
#' \link{readWorldSteelDatabase} datasets, the former for
#' historic, the latter for current data.
#' @author Merlin Jo Hosak
#' @return Steel Production across all regions from 1900-2022 as magpie within 
#' list of metadata (in calcOutput format).
#' @export
calcSteelProduction <- function() {
  prod_data <- getSteelProductionData()
  
  # Interpolate
  prod_data$recent <- toolInterpolate2D(prod_data$recent, method='linear')
  prod_data$current <- toolInterpolate2D(prod_data$current, method='linear')
  
  # Extrapolate
  prod <- extrapolateSteelProduction(prod_data) 
  
  # Finalize for calcOutput
  prod[is.na(prod)] <- 0 ## fill remaining NA with zero (should be total rows of countries with insignificant production) TODO: check
  
  if (any(is.na(prod))) { ## check if there are any NA left in prod
    warning("There are still NA values in the production data after extrapolation.")
  }
  
  result <- list(x = prod, 
                 weight = NULL,
                 unit='Tonnes',
                 description='Steel production from 1900-2100 yearly for the REMIND-MFA format')
  
  return(result)
}

getSteelProductionData <- function() {
  # load data
  prod_hist <- readSource('WorldSteelDigitised', subtype='world_production',convert=F)
  prod_recent <- readSource('WorldSteelDigitised', subtype='production')
  prod_current <- readSource('WorldSteelDatabase', subtype='production')
  
  return(list(hist=prod_hist, 
              recent=prod_recent, 
              current=prod_current))
}

extrapolateSteelProduction <- function(prod_data) {
  # Extrapolate current by recnet for regions where data overlaps
  prod <- toolBackcastByReference2D(prod_data$current, ref=prod_data$recent, 
                                    do_interpolate=FALSE)  # already interpolated
  
  # calculate World total of regions where data is available until 1900
  world_ref <- getWorldSteelProductionTrend(prod, prod_data$hist)
  
  # Extrapolate remaining regions by world reference
  prod <- toolBackcastByReference2D(prod, ref=world_ref,
                                    do_interpolate=FALSE)  # already interpolated
  
  # use constant (last observation carried forward) interpolation for 
  # remaining NaN values in the future
  prod <- toolInterpolate2D(prod, method='constant')
  
  return(prod)
}

getWorldSteelProductionTrend <- function(prod, prod_hist) {
  non_na_indices <- which(!is.na(prod[,1]))
  prod_non_na_regions <- prod[non_na_indices,]
  non_na_regions <- getItems(prod_non_na_regions, dim=1)
  
  mapping <- data.frame(from = non_na_regions, global = 'GLO')
  sum_non_na_regions <- toolAggregate(prod_non_na_regions, rel = mapping)
  
  # TODO: potentially the other way around? (Extrapolate hist to future with global recent sum)
  
  world_ref <- toolExtrapolate(x=sum_non_na_regions,
                               ref=prod_hist,
                               extrapolate_method='ref')
  
  return(world_ref)
}
  
  
  
  
  
  
  
  
