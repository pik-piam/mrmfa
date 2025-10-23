#' Interpolate (and/or extrapolate) 2D magclass objects.
#' @param x magpie object with years as columns that has to be interpolated 
#' and/or extrapolated
#' @param interpolate If TRUE, the function will interpolate missing values
#' (default: TRUE).
#' @param extrapolate If TRUE, the function will extrapolate missing values
#' (default: FALSE).
#' @param from Start year for interpolation/extrapolation (default: NA,
#' which will be set to the first year in x).
#' @param to End year for interpolation/extrapolation (default: NA,
#' which will be set to the last year in sx).
#' @param method Interpolation method to use (default: 'linear'). Options:
#' 'linear', 'constant'. # TODO add others
#' @param extrapolate_method Extrapolation method to use (default: 'constant').
#' Options: 'constant', 'ref'. # TODO add more methods. If 'ref', a reference 
#' magpie object must be provided. In 'ref' extrapolation, the values are 
#' extrapolated based on the shape of the reference data, see 
#' \link[=toolExtrapolateRegion]{toolExtrapolateRegion}.
#' @param ref Reference magpie object for extrapolation (default: NA).
#' @author Merlin Jo Hosak
#' @export
toolInterpolate <- function(x, interpolate=TRUE, extrapolate=FALSE,
                            from = NA, to = NA,
                            method='linear', extrapolate_method='constant', 
                            ref=NA) {
  if (!is.magpie(x)) {
    stop("Input must be a magpie object.")
  }
  
  if(ndata(x)!=1) {
    stop("Input must be a 2D magpie object with only regions and years.")
  }
  
  regions <- getItems(x, dim=1)
  
  duplicates <- duplicated(regions)
  if (any(duplicates)) {
    stop("Input regions must be unique. Duplicates found: ", 
         paste(regions[duplicates], collapse = ", "))
  }
  
  if (interpolate) {
    x <- interpolate2D(x, method, regions, from, to)
  }
  
  if (extrapolate) {
    x <- extrapolate2D(x, regions, extrapolate_method, ref)
  }
  
  return(x)
}

interpolate2D <- function(x, method, regions, from=NA, to=NA) {
  range <- getInterpolationRange(x, from, to)
  # loop through regions and interpolate
  for (region in regions) {
    x[region, range] <- toolInterpolateRegion(x[region, range],
                                              method = method)
  }
  
  return(x)
}


getInterpolationRange <- function(x, from, to) {
  columns <- getItems(x, dim=2)
  
  # if from is NA set it to the first year in x
  if (is.na(from)) {
    from <- min(columns)
    from_index <- which(columns==from)
  } 
  
  # if to is NA set it to the last year in x
  if (is.na(to)) {
    to <- max(columns)
    to_index <- which(columns==to)
  }
  
  range <- from_index:to_index
  
  return(range)
}


extrapolate2D <- function(x, regions, extrapolate_method='constant', ref=NA) {
  ref_row <- NA
  all_regions_in_ref <- FALSE
  if (extrapolate_method=='ref') {
    if (!is.magpie(ref)) {
      stop("Reference data must be provided for extrapolation with 'ref' method.")
    }
    x <- extendXWithRef(x, ref)  # extend x dimensions according to ref
    
    # check if all regions in x are present in ref
    ref_regions <- getItems(ref, dim=1)
    if (all(regions %in% ref_regions)) {
      all_regions_in_ref <- TRUE
    } else if ('GLO' %in% ref_regions) {
      ref_row <- ref['GLO',]
    } else {
      stop("For extrapolation by reference, all regions in x must be present 
      in the reference data or a global region 'GLO' must be present.")
    }
  } 
  
  # loop through regions and extrapolate
  
  for (region in regions) {
    row <- x[region,]
    if (all_regions_in_ref) {
      ref_row <- ref[region,]
    }
    x[region,] <- toolExtrapolateRegion(row,
                                        method = extrapolate_method,
                                        ref=ref_row)
    
  }
  return(x)
} 

extendXWithRef <- function(x, ref) {
  # add cols that are in ref but not in x to x 
  
  first_x_year <- min(getItems(x, dim=2))
  first_ref_year <- min(getItems(ref, dim=2))
  
  if (first_ref_year < first_x_year) {
    # extrapolate before
    
    missing_before <- new.magpie(
      cells_and_regions = getItems(x, dim=1),
      years = getItems(ref,dim=2)[which(getItems(ref, dim=2) < first_x_year)],
      names = "value",
      fill = NA,
      sets = names(dimnames(ref))
    )
    
    x <- mbind(missing_before, x)
    
  }
  
  last_x_year <- max(getItems(x, dim=2))
  last_ref_year <- max(getItems(ref, dim=2))
  
  if (last_ref_year > last_x_year) {
    # extrapolate after
    
    missing_after <- new.magpie(
      cells_and_regions = getItems(x, dim=1),
      years = getItems(ref,dim=2)[which(getItems(ref, dim=2) > last_x_year)],
      names = "value",
      fill = NA,
      sets = names(dimnames(ref))
    )
    
    x <- mbind(x, missing_after)
    
  }
  return(x)
}

toolExtrapolateRegion <- function(row,
                                  method = 'constant',
                                  ref_row = NA,
                                  max_n=5) {
  if (!is.magpie(row)) {
    stop("Input must be a magpie object.")
  }
  
  getItems(ref_row, dim=1) <- getItems(row, dim=1)  # ensure that ref_row has the same regions as row if row
  
  # Get the years and values
  years <- colnames(row)
  values <- as.numeric(row[1, ])
  ref_values <- as.numeric(ref_row[1, ])
  
  if (all(is.na(values))) {
    # if all values are NA, return the row as is
    return(row)
  }
  
  if (method=='ref' && all(is.na(ref_values))) {
    # if method is 'ref' and all ref values are NA, return the row as is
    return(row)
  }
  
  first_valid_idx <- which(!is.na(values))[1]
  last_valid_idx <- tail(which(!is.na(values)), 1)
  
  first_valid_ref_idx <- which(!is.na(ref_values))[1]
  last_valid_ref_idx <- tail(which(!is.na(ref_values)), 1)
  
  last_idx <- length(values)
  last_ref_idx <- length(ref_values)
  
  possible_n <- last_valid_idx-first_valid_idx + 1
  n <- min(possible_n, max_n)
  alpha <- 0.5
  weights <- alpha**(1:n)
  weights <- weights / sum(weights)  # TODO: store weights in dictionary?
  
  # Extrapolate before
  
  if (!is.na(first_valid_idx)&&first_valid_idx > 1&&(!method=='ref'||(first_valid_idx<last_valid_ref_idx))) {  # Extrapolate before the first valid value
    row <- toolExtrapolateRegionPast(row, 
                                     method = method, 
                                     ref_row = ref_row,
                                     values = values,
                                     weights = weights,
                                     first_valid_idx = first_valid_idx,
                                     last_idx = last_idx,
                                     n = n)
  }
  
  # Extrapolate after
  
  if (!is.na(last_valid_idx)&&last_valid_idx<last_idx&&(!method=='ref'||last_valid_idx<last_valid_ref_idx)) {  # Extrapolate after the last valid value
    row <- toolExtrapolateRegionFuture(row, 
                                       method = method, 
                                       ref_row = ref_row,
                                       values = values,
                                       weights = weights,
                                       last_valid_idx = last_valid_idx, 
                                       last_ref_idx = last_valid_ref_idx, 
                                       last_idx = last_idx, 
                                       n = n)
  }
  
  return(row)
}

toolExtrapolateRegionPast <- function(row,
                                      method = 'constant', 
                                      ref_row = NA,
                                      values,
                                      weights=NA,
                                      first_valid_idx,
                                      last_idx,
                                      n) {
  if (method == 'constant') {
    # Use the first valid value to extrapolate backwards
    row_begin <- row
    row_begin[1:(first_valid_idx - 1)] <- values[first_valid_idx]
  } else if (method == 'ref') {
    average_x_start <- sum(row[first_valid_idx:(first_valid_idx + n - 1)] * weights)
    average_ref_start <- sum(ref_row[first_valid_idx:(first_valid_idx + n - 1)] * weights)
    
    ratio <- average_x_start / average_ref_start
    
    row_begin <- ref_row * ratio
    
  } else {
    stop("Invalid method for past extrapolation")
  }
  
  
  row <- mbind(row_begin[,1:first_valid_idx-1], row[,first_valid_idx:last_idx])
  return(row)
}

toolExtrapolateRegionFuture <- function(row, 
                                      method = 'constant', 
                                      ref_row = NA,
                                      values,
                                      weights=NA,
                                      last_valid_idx, 
                                      last_ref_idx, 
                                      last_idx, 
                                      n){
  if (method == 'constant') {
    # Use the last valid value to extrapolate forwards
    row_end <- row
    row_end[(last_valid_idx + 1):last_idx] <- values[last_valid_idx]
    row <- mbind(row[,1:last_valid_idx], row_end[,(last_valid_idx+1):last_idx])
  } else if (method == 'ref') {
    # reverse weights order
    weights <- rev(weights)
    
    average_x_end <- sum(row[(last_valid_idx - n + 1):last_valid_idx] * weights)
    average_ref_end <- sum(ref_row[(last_valid_idx - n + 1):last_valid_idx] * weights)
    
    ratio <- average_x_end / average_ref_end
    
    row_end <- ref_row * ratio
    row <- mbind(row[,1:last_valid_idx], row_end[,(last_valid_idx+1):last_ref_idx])
    
    if(last_ref_idx<last_idx) {  # TODO: Check: can this happen?
      row<-mbind(row[,1:last_ref_idx], row_end[,(last_ref_idx+1):last_idx])
    }
    
  } else {
    stop("Invalid method for future extrapolation")
  }
  return(row)
}

#' Interpolate data for a single region
toolInterpolateRegion <- function(row, method = 'linear'){
  if (!is.magpie(row)) {
    stop("Input must be a magpie object.")
  }
  
  # Get the years and values
  years <- colnames(row)
  values <- as.numeric(row[1, ])
  
  # calculate interpolation markers (valid indices where values are not NA),
  # these are used to define areas that need to be interpolated together.
  
  valid_idxs <-which(!is.na(values))
  diff_valid_idxs <- diff(valid_idxs)
  
  if(length(which(diff_valid_idxs > 1))==0) {
    # no interpolation needed, return the row as is
    # TODO: potentially extrapolation needed - do extrapolation first ?
    return(row)
  }
  
  markers <- valid_idxs[which(diff_valid_idxs > 1)]
  lengths <- diff_valid_idxs[which(diff_valid_idxs > 1)]
  
  # interpolate values between markers
  if (length(markers) < 1) {
    stop("Not enough valid data points for interpolation.")
  }
  
  # loop through markers/interpolation areas and interpolate
  for (i in 1:(length(markers))) {
    start <- markers[i]
    end <- start + lengths[i]
    # sequence <- row[markers[1]:markers[1+1]]
    if (method == 'linear') {
      # linear interpolation
      interpolated_seq <- toolLinearInterpolateRegion(row[start:end])
    } else if (method == 'constant') {
      # constant interpolation
      interpolated_seq <- row[markers[i]]
    } else {
      stop("Invalid method. Use 'linear' or 'constant'.")
    }
    
    row[start:end] <- interpolated_seq
  }
  
  return(row)
}

toolLinearInterpolateRegion <- function(sequence) {
  interpolated_seq <- seq(sequence[1], sequence[length(sequence)], length.out = length(sequence))
  
  return(interpolated_seq)
}

#' Extrapolate (or Interpolate) 2D magclass objects.
#' @description This function is a wrapper for \link[=toolInterpolate]{toolInterpolate}.
#' #@param x magpie object with years as columns that has to be interpolated 
#' and/or extrapolated
#' @param interpolate If TRUE, the function will interpolate missing values
#' (default: TRUE).
#' @param extrapolate If TRUE, the function will extrapolate missing values
#' (default: FALSE).
#' @param from Start year for interpolation/extrapolation (default: NA,
#' which will be set to the first year in x).
#' @param to End year for interpolation/extrapolation (default: NA,
#' which will be set to the last year in x).
#' @param method Interpolation method to use (default: 'linear'). Options:
#' 'linear', 'constant'. # TODO add others
#' @param extrapolate_method Extrapolation method to use (default: 'constant').
#' Options: 'constant', 'ref'. # TODO add more methods. If 'ref', a reference 
#' magpie object must be provided. In 'ref' extrapolation, the values are 
#' extrapolated based on the shape of the reference data, see 
#' \link[=toolExtrapolateRegion]{toolExtrapolateRegion}.
#' @param ref Reference magpie object for extrapolation (default: NA).
#' @author Merlin Jo Hosak
#' @export
toolExtrapolate <- function(x, interpolate=FALSE, extrapolate=TRUE,
                            from = NA, to = NA,
                            method='linear', extrapolate_method='constant', 
                            ref=NA) {
  result <- toolInterpolate(x, interpolate=interpolate, extrapolate=extrapolate,
                            from=from, to=to,
                            method=method, extrapolate_method=extrapolate_method, 
                            ref=ref)
  return(result)
}

