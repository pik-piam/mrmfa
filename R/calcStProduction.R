#' Calc Steel Production
#' @description
#' Calc steel production from WorldSteel datasets. Can be aggregated to regions
#' via calcOutput aggregate parameter. Uses \link{readWorldSteelDigitised} and
#' \link{readWorldSteelDatabase} datasets, the former for historic, the latter for current data.
#' @author Merlin Jo Hosak
#' @return Steel Production across all regions from 1900-2022 as magpie within
#' list of metadata (in calcOutput format).
calcStProduction <- function() {
  # steel production from 1969 - 2009
  prodRecent <- readSource("WorldSteelDigitised", subtype = "production")
  prodRecent <- toolInterpolate2D(prodRecent, method = "linear")

  # steel production from 2003 - 2022
  prodCurrent <- readSource("WorldSteelDatabase", subtype = "production")
  prodCurrent <- toolInterpolate2D(prodCurrent, method = "linear")

  # extrapolate current by recent for regions where data overlaps
  prod <- toolBackcastByReference(
    prodCurrent,
    ref = prodRecent,
    doInterpolate = FALSE
  )

  # calculate estimate of World Production
  nonNaIndices <- which(!is.na(rowSums(prod)))
  prodNonNaRegions <- prod[nonNaIndices, , ]
  # sum of regions which have no NAs in any years used as reference
  sumNonNaRegions <- colSums(prodNonNaRegions)

  prodWorld <- readSource("WorldSteelDigitised", subtype = "worldProduction", convert = FALSE)

  worldRef <- toolBackcastByReference(
    prodWorld,
    ref = sumNonNaRegions,
    doForecast = TRUE,
    doInterpolate = FALSE
  )

  # extrapolate remaining regions by world reference
  prod <- toolBackcastByReference(
    prod,
    ref = worldRef,
    doInterpolate = FALSE
  )

  # use constant (last observation carried forward) interpolation for
  # remaining NaN values in the future
  prod <- toolInterpolate2D(prod, method = "constant")

  result <- list(
    x = prod,
    weight = NULL,
    unit = "Tonnes",
    description = "Steel production from 1900-2022 yearly",
    note = "dimensions: (Historic Time,Region,value)"
  )

  return(result)
}
