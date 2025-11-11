#' Calc Steel Production
#' @description
#' Calc steel production from WorldSteel datasets. Can be aggregated to regions
#' via calcOutput aggregate parameter. Uses
#' \link{readWorldSteelDigitised} and
#' \link{readWorldSteelDatabase} datasets, the former for
#' historic, the latter for current data.
#' @author Merlin Jo Hosak
#' @return Steel Production across all regions from 1900-2022 as magpie within
#' list of metadata (in calcOutput format).
calcStProduction <- function() {

  prodHist <- readSource("WorldSteelDigitised", subtype = "worldProduction", convert = FALSE)
  prodRecent <- readSource("WorldSteelDigitised", subtype = "production")
  prodCurrent <- readSource("WorldSteelDatabase", subtype = "production")

  # Interpolate ----
  prodRecent <- toolInterpolate2D(prodRecent, method = "linear")
  prodCurrent <- toolInterpolate2D(prodCurrent, method = "linear")

  # Extrapolate ----

  # Extrapolate current by recent for regions where data overlaps
  prod <- toolBackcastByReference2D(prodCurrent,
                                    ref = prodRecent,
                                    doInterpolate = FALSE
  ) # already interpolated


  # calculate estimate of World Production

  nonNaIndices <- which(!is.na(rowSums(prod)))
  prodNonNaRegions <- prod[nonNaIndices, ]
  sumNonNaRegions <- colSums(prodNonNaRegions)

  worldRef <- toolBackcastByReference2D(prodHist,
                                        ref = sumNonNaRegions,
                                        doForecast = TRUE,
                                        doInterpolate = FALSE
  )

  # Extrapolate remaining regions by world reference
  prod <- toolBackcastByReference2D(prod,
                                    ref = worldRef,
                                    doInterpolate = FALSE
  ) # already interpolated

  # use constant (last observation carried forward) interpolation for
  # remaining NaN values in the future
  prod <- toolInterpolate2D(prod, method = "constant")

  # Check if there are any NA left in prod
  if (any(is.na(prod))) { # check if there are any NA left in prod
    warning("There are still NA values in the production data after extrapolation.")
  }

  result <- list(
    x = prod,
    weight = NULL,
    unit = "Tonnes",
    description = "Steel production from 1900-2022 yearly for the SIMSON format"
  )

  return(result)
}
