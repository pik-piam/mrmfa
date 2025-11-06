#' Backcast (or forecast) 2D magpie data by reference data
#' 
#' @description
#' 
#' This function backcasts (or forecasts) missing values in a 2D magpie object x
#' based on reference data ref. The backcasting/forecasting is done by calculating
#' ratios of x to ref for shared years, computing weights for these ratios based
#' on their recency (linearly weighted), 
#' and applying these weights to scale ref for missing years in x.
#' 
#' @param x A 2D magpie object (regions x years) to be backcasted/forecasted.
#' @param ref A 2D magpie object (regions x years) to be used as reference for
#' the backcasting/forecasting.
#' @param doInterpolate Logical, whether to interpolate missing values in x and ref
#' before calculating weights. Recommended to ensure more stable weight calculation.
#' @param maxN Maximum number of years to consider for weight calculation (default 5).
#' @param doForecast Logical, whether to do forecasting instead of backcasting.
#' @param doMakeZeroNA Logical, whether to convert 0 values in final output to NA. As sometimes 0 is wrongly implicitly assumed.
#' @author Merlin Jo Hosak
#' @export
toolBackcastByReference2D <- function(x, 
                                      ref, 
                                      doInterpolate=TRUE, 
                                      maxN=5,
                                      doForecast=FALSE,
                                      doMakeZeroNA=FALSE) {
  checkBackcastByReference2D(x, ref)
  ref <- adaptRefRegions(x, ref)
  
  if (doInterpolate) {
    # Interpolate missing values in x and ref
    # Recommended so that the weight calculation is regular and more stable
    x <- toolInterpolate2D(x)
    ref <- toolInterpolate2D(ref)
  }
  
  xYears <- getItems(x, dim=2)
  refYears <- getItems(ref, dim=2)
  
  if (doForecast) {
    # cut ref if it extends to the past over x (not necessary for backcasting)
    ref <- ref[, refYears>=min(xYears)]
  } else {
    # cut ref if it extends to the future over x (not necessary for backcasting)
    ref <- ref[, refYears<=max(xYears)]
  }
  refYears <- getItems(ref, dim=2)
  
  sharedYears <- intersect(xYears, refYears)
  nSharedYears <- length(sharedYears)
  
  # calculate ratios and weights
  ratios <- x[,sharedYears] / ref[, sharedYears]
  weights <- calcBackcastWeights(ratios, nSharedYears, maxN=maxN, doForecast=doForecast)
  
  finalRatio <- rowSums(ratios * weights, na.rm = TRUE)
  
  scaledRef <- ref * finalRatio
  
  # create final magpie and fill with x and scaledRef
  final <- new.magpie(
    cells_and_regions = getItems(x, dim=1),
    years = sort(union(refYears, xYears)),
    names = "value",
    fill = NA,
    sets = names(dimnames(x))
  )
  
  final[,xYears] <- x[,xYears]
  
  refExtended <- final  # simple copy of structure
  refExtended[,refYears] <- scaledRef[,refYears]
  
  # fill final with refExtended where final is NA
  
  final[is.na(final)] <- refExtended[is.na(final)]  # update gaps and not existing data in x/final
  
  if (doMakeZeroNA) {
    final[final==0] <- NA
  }
  
  return(final)
}

calcBackcastWeights <- function(ratios, 
                                nSharedYears=nyears(ratios), 
                                maxN=5,
                                doForecast=FALSE) {  #TODO Check if you can do that in R parameters in function statements
  baseWeights <- nSharedYears:1
  if (doForecast) {
    baseWeights <- 1:nSharedYears
  }
  
  # make new magpie with same regions as ratios and for every region the values 1,2,3...
  sampleWeights <- new.magpie(
    years = getItems(ratios, dim=2),
    names = "value",
    fill = baseWeights,
    sets = names(dimnames(ratios))
  )
  
  totalWeights <- new.magpie(
    cells_and_regions = getItems(ratios, dim=1),
    years = getItems(ratios, dim=2),
    names = "value",
    fill = NA,
    sets = names(dimnames(ratios))
  )
  
  totalWeights[,] <- sampleWeights[,]  # TODO better workaround than creating sampleWeights?
  
  # Ensure linear weights start end at one
  totalWeights[is.na(ratios)]<-nSharedYears+1
  totalWeights[ratios==Inf]<-nSharedYears+1
  rowMin <- apply2D(totalWeights, 1, min)
  totalWeights <- totalWeights - rowMin + 1
  
  # Ensure not more than maxN weights are used
  totalWeights[is.na(ratios)]<- -1
  totalWeights[ratios==Inf]<- -1
  rowMax <- apply2D(totalWeights, 1, max)
  offset <- maxN - rowMax
  offset[offset>0] <- 0  # ensure no positive offset
  totalWeights <- totalWeights + offset
  totalWeights[totalWeights<1] <- NA  # ensure positive weights
  
  # remove weights for years where there is no data in x or ref (and hence ratios)
  totalWeights[is.na(ratios)]<-NA
  
  # Normalize weights
  sums <- rowSums(totalWeights, na.rm=TRUE)
  normalizedWeights <- totalWeights / sums
  
  return(normalizedWeights)
}

apply2D <- function(x, margin, fun,...,simplify=TRUE) {
  if (ndata(x) != 1) {
    stop("x must be only 2D magpie object (regions x years).")
  }

  df <- mtab(x)[,2:(1+nyears(x))]
  result <- apply(df, margin, fun,...,simplify=simplify)
  return(result)
}

adaptRefRegions <- function(x, ref) {
  xRegions <- getItems(x, dim=1)
  refRegions <- getItems(ref, dim=1)
  
  newRef <- new.magpie(
    cells_and_regions = xRegions,
    years = getItems(ref, dim=2),
    names = "value",
    fill = NA,
    sets = names(dimnames(ref))
  )
  
  if ('GLO' %in% refRegions) {
    newRef[,] <- ref['GLO', ]  # if GLO is in ref, copy it to all regions
  }
  
  # Fill rest of new ref with existing ref data
  newRef[refRegions %in% xRegions, ] <- ref[refRegions %in% xRegions, ]
  
  return(newRef)
}

checkBackcastByReference2D <- function(x, ref) {
  if (!is.magpie(x)) {
    stop("Input must be a magpie object.")
  }
  
  if (!is.magpie(ref)) {
    stop("Reference data must be a magpie object.")
  }
  
  if (ndata(x) != 1 || ndata(ref) != 1) {
    stop("Both x and ref must be only 2D magpie objects (regions x years).")
  }
  
  # Check if x and ref share some years
  if (length(intersect(getItems(x, dim=2), getItems(ref, dim=2))) == 0) {
    stop("x and ref must share at least one year for backcasting.")
  }
}

