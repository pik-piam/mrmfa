#' Get Steel Production data
#' 
#' @description
#' Calc steel production from WorldSteel datasets. Can be aggregated to regions 
#' via calcOutput aggregate parameter. Uses 
#' \link[readWorldSteelDigitised]{WorldSteelDigitised} and 
#' \link[readWorldSteelDatabase]{WorldSteelDatabase} datasets, the former for
#' historic, the latter for current data.
#' @author Merlin Jo Hosak
#' @return Steel Production across all regions from 1900-2022 as magpie within 
#' list of metadata (in calcOutput format).
#' @export
calcSteelTrade <- function(subtype = 'imports') {
  indirect <- subtype %in% c('indirect_imports', 'indirect_exports')
  trade_data <- getSteelTradeData(subtype, indirect=indirect)
  
  # Interpolate and Extrapolate
  trade_data$database <- toolInterpolate2D(trade_data$database)
  
  if (indirect) {
    trade <- trade_data$database
  } else {
    trade <- extendTradeWithDigitisedWSData(trade_data)
  } 
  
  trade <- toolExtrapolate(trade, ref=trade_data$production, 
                            extrapolate_method = 'ref')
  
  # use constant (last observation carried forward) interpolation for 
  # remaining NaN values in the future
  trade <- toolInterpolate2D(trade, method='constant')
  
  # Finalize for calcOutput
  trade[is.na(trade)] <- 0 ## fill remaining NA with zero
  
  if (indirect) {
    # Split indirect trade into direct trade
    shares <- trade_data$digitised
    trade <- splitIndirectTrade(trade, shares)
  }
  
  trade <- list(x = trade, 
                 weight = NULL,
                 unit='Tonnes',
                 description=paste0('Steel trade:', subtype, 
                                    'from 1900-2021 yearly for the REMIND-MFA format.'))
  
  return(trade)
}


extendTradeWithDigitisedWSData <- function(trade_data) {
  trade_data$digitised <- toolInterpolate2D(trade_data$digitised)
  trade_data$digitised[trade_data$digitised<1] = NA # if values are too small, they are not fit for extrapolation by reference (potentially creating infinite/unrealistic values)
  trade <- toolExtrapolate(trade_data$database, 
                           ref=trade_data$digitised, 
                           extrapolate_method = 'ref')
  return(trade)
}


splitIndirectTrade <- function(trade, shares) {
  # Multiply by shares
  
  intersecting_countries <- intersect(getItems(trade, 1), getItems(shares, 1))
  trade <- trade[intersecting_countries, ]
  shares <- shares[intersecting_countries, ]
  
  trade <- trade * shares
  
  trade <- toolCountryFill(trade, verbosity=2, fill=0) # Fill missing countries with zeroes
  
  return(trade)
  
}

getSteelTradeData <- function(subtype='imports', indirect=FALSE) {
  # load data
  production <- calcOutput('SteelProduction', aggregate=FALSE)
  database <- readSource('WorldSteelDatabase', subtype=subtype)
  
  if (indirect) {
    subtype = paste0(subtype, '_by_category_2013')
  }
  
  digitised <- readSource('WorldSteelDigitised', subtype=subtype, convert=!indirect)
  
  return(list(production=production,
              database=database,
              digitised=digitised))
}






