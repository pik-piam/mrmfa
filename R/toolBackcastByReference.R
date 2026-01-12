#' Backcast (or forecast) magpie data by reference data
#'
#' @description
#'
#' This function backcasts (or forecasts) missing values in a magpie object x
#' based on reference data ref. The backcasting/forecasting is done by calculating
#' ratios of x to ref for shared years, computing weights for these ratios based
#' on their recency (linearly weighted), and applying these weights to scale ref
#' for missing years in x.
#'
#' @param x A magpie object to be backcasted/forecasted.
#' @param ref A magpie object to be used as reference for
#' the backcasting/forecasting.
#' @param doInterpolate Logical, whether to interpolate missing values in x and ref
#' before calculating weights. Recommended to ensure more stable weight calculation.
#' @param maxN Maximum number of years to consider for weight calculation (defaults to 5).
#' @param doForecast Logical, whether to do forecasting instead of backcasting.
#' @param doMakeZeroNA Logical, whether to convert 0 values in final output to NA.
#' As sometimes 0 is wrongly implicitly assumed.
#'
#' @author Merlin Jo Hosak, Falk Benke
#'
toolBackcastByReference <- function(x, ref, doInterpolate = TRUE, maxN = 5,
                                    doForecast = FALSE, doMakeZeroNA = FALSE) {
  # internal functions ----

  # check x and ref for right format
  .validateInputParameters <- function(x, ref) {
    if (!is.magpie(x)) {
      stop("Input must be a magpie object.")
    }

    if (!is.magpie(ref)) {
      stop("Reference data must be a magpie object.")
    }

    # check if x and ref share some years
    if (length(intersect(getItems(x, dim = 2), getItems(ref, dim = 2))) == 0) {
      stop("x and ref must share at least one year for backcasting.")
    }

    if (is.null(getItems(ref, dim = 1))) {
      stop("no regions names found in reference")
    }
  }

  # matches reference regions to regions of x
  .adaptRefRegions <- function(x, ref) {
    newRef <- magclass::matchDim(ref, x, dim = 1)
    refRegions <- getItems(ref, dim = 1)
    if ("GLO" %in% refRegions) {
      # if GLO is in ref, copy it to all regions
      newRef[, , ] <- ref["GLO", , ]
      xRegions <- getItems(x, dim = 1)
      # fill rest of new ref with existing ref data
      newRef[refRegions %in% xRegions, , ] <- ref[refRegions %in% xRegions, , ]
    }
    return(newRef)
  }

  # TODO: add documentation
  .calcBackcastWeights <- function(ratios, maxN = 5, doForecast = FALSE) {
    nSharedYears <- nyears(ratios)

    baseWeights <- nSharedYears:1
    if (doForecast) {
      baseWeights <- 1:nSharedYears
    }

    names(baseWeights) <- getYears(ratios)

    totalWeights <- ratios
    totalWeights[, , ] <- NA

    for (y in getYears(totalWeights)) {
      totalWeights[, y, ] <- baseWeights[y]
    }

    # TODO: not sure if this works as intended in all cases, should be
    # investigated further, as the original author is no longer around

    # Ensure linear weights start end at one
    totalWeights[is.na(ratios)] <- nSharedYears + 1
    totalWeights[ratios == Inf] <- nSharedYears + 1
    rowMin <- magpply(totalWeights, min, MARGIN = c(1, 3))
    totalWeights <- totalWeights - rowMin + 1

    # Ensure not more than maxN weights are used
    totalWeights[is.na(ratios)] <- -1
    totalWeights[ratios == Inf] <- -1
    rowMax <- magpply(totalWeights, max, MARGIN = c(1, 3))
    offset <- maxN - rowMax
    offset[offset > 0] <- 0 # ensure no positive offset
    totalWeights <- totalWeights + offset
    totalWeights[totalWeights < 1] <- NA # ensure positive weights

    # Remove weights for years where there is no data in x or ref (and hence ratios)
    totalWeights[is.na(ratios)] <- NA

    # Normalize weights
    sums <- dimSums(totalWeights, dim = 2, na.rm = TRUE)
    normalizedWeights <- totalWeights / sums

    return(normalizedWeights)
  }

  # prepare input objects ----

  x <- magpiesort(x)
  ref <- magpiesort(ref)

  # make sure structure of input objects is supported
  .validateInputParameters(x, ref)

  # adjust regions in ref to match x
  ref <- .adaptRefRegions(x, ref)

  # apply interpolation
  if (doInterpolate) {

    # Interpolate missing values in x and ref
    # Recommended so that the weight calculation is regular and more stable

    if (ndata(x) != 1) {
      for (n in getNames(x)) {
        x[, , n] <- toolInterpolate2D(x[, , n])
      }
    } else {
      x <- toolInterpolate2D(x)
    }

    if (ndata(ref) != 1) {
      for (n in getNames(ref)) {
        ref[, , n] <- toolInterpolate2D(ref[, , n])
      }
    } else {
      ref <- toolInterpolate2D(ref)
    }
  }

  # cut unnecessary years from ref
  xYears <- getItems(x, dim = 2)
  refYears <- getItems(ref, dim = 2)

  if (doForecast) {
    # cut ref if it extends to the past over x (not necessary for forecasting)
    ref <- ref[, refYears >= min(xYears), ]
  } else {
    # cut ref if it extends to the future over x (not necessary for backcasting)
    ref <- ref[, refYears <= max(xYears), ]
  }

  # scale the reference values ----
  sharedYears <- intersect(xYears, refYears)

  # calculate ratio between x and ref for shared years
  ratios <- x[, sharedYears, ] / ref[, sharedYears, ]

  # ratios of overlapping years are weighted (with later years weighted higher
  # than earlier years for backcasting), only maxN overlapping years are considered
  weights <- .calcBackcastWeights(ratios, maxN = maxN, doForecast = doForecast)

  # calculate weighted average of ratios
  finalRatio <- dimSums(ratios * weights, dim = 2, na.rm = TRUE)

  # scale reference values according to weighted average ratio
  scaledRef <- ref * finalRatio

  # fill missing values with scaled reference values ----

  final <- new.magpie(
    cells_and_regions = getItems(x, dim = 1),
    years = sort(union(refYears, xYears)),
    names = getNames(x),
    fill = NA,
    sets = names(dimnames(x))
  )

  # fill with x
  final[, xYears, ] <- x[, xYears, ]

  refExtended <- final # simple copy of structure
  refExtended[, refYears, ] <- scaledRef[, refYears, ]

  # fill final with refExtended where final is NA
  final[is.na(final)] <- refExtended[is.na(final)]

  if (doMakeZeroNA) {
    final[final == 0] <- NA
  }

  return(final)
}
