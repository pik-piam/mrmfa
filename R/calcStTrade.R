#' Get steel trade data
#' @description
#' Calc steel trade from WorldSteel datasets. Can be aggregated to regions 
#' via calcOutput aggregate parameter. Uses 
#' \link[readWorldSteelDigitised]{WorldSteelDigitised} and 
#' \link[readWorldSteelDatabase]{WorldSteelDatabase} datasets, the former for
#' historic, the latter for current data. Further, uses 
#' \link[calcSteelProduction]{SteelProduction} to backcast historic trade data.
#' @author Merlin Jo Hosak
#' @return Steel trade across all regions from 1900-2022 as magpie within 
#' list of metadata (in calcOutput format).
#' @export
calcStTrade <- function(subtype = 'imports') {
  indirect <- subtype %in% c('indirectImports', 'indirectExports')
  tradeData <- getSteelTradeData(subtype, indirect=indirect)
  
  # Interpolate and Extrapolate
  tradeData$database <- toolInterpolate2D(tradeData$database)
  
  if (indirect) {
    trade <- tradeData$database
  } else {  # indirect trade isn't given in digitised yearbooks, only digitised 2013 shares
    tradeData$digitised[tradeData$digitised<1] = NA # if values are too small, they are not fit for extrapolation by reference (potentially creating infinite/unrealistic values)
    trade <- toolBackcastByReference2D(tradeData$database,
                                       ref=tradeData$digitised)
  } 
  
  trade <- toolBackcastByReference2D(trade, ref=tradeData$production)
  
  # use constant (last observation carried forward) interpolation for remaining NaN values in the future
  trade <- toolInterpolate2D(trade, method='constant')
  
  # Split indirect trade
  if (indirect) {
    # Split indirect trade into direct trade
    shares <- tradeData$digitised
    trade <- splitIndirectTrade(trade, shares)
  }
  
  # Finalize
  trade[is.na(trade)] <- 0  # fill remaining NA with zero
  
  trade <- list(x = trade, 
                 weight = NULL,
                 unit='Tonnes',
                 description=paste0('Steel trade:', subtype, 
                                    'from 1900-2021 yearly for the SIMSON format.'))
  
  return(trade)
}


splitIndirectTrade <- function(trade, shares) {
  # Multiply indirect trade with shares for intersecting countries
  intersectingCountries <- intersect(getItems(trade, 1), getItems(shares, 1))
  tradeIntersecting <- trade[intersectingCountries, ] * shares[intersectingCountries, ]
  
  # For non-intersecting countries, use global average shares
  averageShare <- colSums(shares) / nregions(shares)  # assuming all countries with data have same weight. Calculation works because rows sum to 1
  nonIntersectingCountries <- setdiff(getItems(trade, 1), intersectingCountries)
  tradeNonIntersecting <- trade[nonIntersectingCountries, ] * averageShare
  
  # Combine both
  trade <- mbind(tradeIntersecting, tradeNonIntersecting)
  trade <- toolCountryFill(trade, verbosity=2, fill=0) # Fill missing countries with zeroes
  
  return(trade)
  
}

getSteelTradeData <- function(subtype='imports', indirect=FALSE) {
  # load data
  production <- calcOutput('StProduction', aggregate=FALSE)
  database <- readSource('WorldSteelDatabase', subtype=subtype)
  
  if (indirect) {
    subtype = paste0(subtype, 'ByCategory2013')
  }
  
  digitised <- readSource('WorldSteelDigitised', subtype=subtype, convert=!indirect)
  
  return(list(production=production,
              database=database,
              digitised=digitised))
}






