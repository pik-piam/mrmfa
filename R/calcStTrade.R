#' Get steel trade data
#' @description
#' Calc steel trade from WorldSteel datasets. Can be aggregated to regions
#' via calcOutput aggregate parameter. Uses
#' \link{readWorldSteelDigitised} and
#' \link{readWorldSteelDatabase} datasets, the former for
#' historic, the latter for current data. Further, uses
#' \link{calcStProduction} to backcast historic trade data.
#' @author Merlin Jo Hosak
#' @param subtype Type of trade data to retrieve. Options: "imports", "exports",
#' "scrapImports", "scrapExports", "indirectImports", "indirectExports"
#'
#' @return Steel trade across all regions from 1900-2022 as magpie within
#' list of metadata.
calcStTrade <- function(subtype = "imports") {

  # helper functions ----

  .splitIndirectTrade <- function(trade, shares) {

    # remove regions containing only NAs
    remove <- magpply(shares, function(y) all(is.na(y)), MARGIN = 1)
    shares <- shares[!remove, , ]

    # Multiply indirect trade with shares for intersecting countries
    intersectingCountries <- intersect(getItems(trade, 1), getItems(shares, 1))
    tradeIntersecting <- trade[intersectingCountries, , ] * shares[intersectingCountries, , ]

    # For non-intersecting countries, use global average shares
    # assuming all countries with data have same weight. Calculation works because rows sum to 1
    averageShare <- colSums(shares) / nregions(shares)
    nonIntersectingCountries <- setdiff(getItems(trade, 1), intersectingCountries)
    tradeNonIntersecting <- trade[nonIntersectingCountries, , ] * averageShare

    # Combine both
    trade <- mbind(tradeIntersecting, tradeNonIntersecting) %>%
      collapseDim()

    return(trade)
  }

  # main routine ----

  indirect <- subtype %in% c("indirectImports", "indirectExports")

  database <- readSource("WorldSteelDatabase", subtype = subtype)

  # Interpolate and Extrapolate
  database <- toolInterpolate2D(database)

  if (indirect) {
    trade <- database
  } else { # indirect trade isn't given in digitised yearbooks, only digitised 2013 shares
    # if values are too small, they are not fit for extrapolation by reference
    # (potentially creating infinite/unrealistic values)
    digitised <- readSource("WorldSteelDigitised", subtype)
    digitised[digitised < 1] <- NA
    trade <- toolBackcastByReference2D(database, ref = digitised)
  }

  production <- calcOutput("StProduction", aggregate = FALSE)
  trade <- toolBackcastByReference2D(trade, ref = production)

  # use constant (last observation carried forward) interpolation for remaining NaN values in the future
  trade <- toolInterpolate2D(trade, method = "constant")

  # Split indirect trade
  if (indirect) {
    if (subtype == "indirectImports") {
      shares <- calcOutput("StIndirectTradeShares", aggregate = FALSE, warnNA = FALSE)[, , "imports"] %>%
        collapseDim()
    } else if (subtype == "indirectExports") {
      shares <- calcOutput("StIndirectTradeShares", aggregate = FALSE, warnNA = FALSE)[, , "exports"] %>%
        collapseDim()
    }
    # Split indirect trade into direct trade
    trade <- .splitIndirectTrade(trade, shares = shares)
  }

  # Finalize
  trade[is.na(trade)] <- 0 # fill remaining NA with zero

  getNames(trade) <- NULL

  trade <- list(
    x = trade,
    weight = NULL,
    unit = "Tonnes",
    description = paste0("Steel trade: ", subtype, " from 1900-2021 yearly for the SIMSON format.")
  )

  return(trade)
}
