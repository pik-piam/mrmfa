toolBackcastByReference2D <- function(x, 
                                      ref, 
                                      do_interpolate=TRUE, 
                                      do_fill_ref_zeroes=FALSE,
                                      max_n=5) {
  checkBackcastByReference2D(x, ref)
  ref <- adaptRefRegions(x, ref)
  
  if (do_interpolate) {
    # Interpolate missing values in x and ref
    # Recommended so that the weight calculation is regular and more stable
    x <- toolInterpolate2D(x)
    ref <- toolInterpolate2D(ref)
  }  # TODO: Ammend description of magclass during toolInterpolate2D so that it is clear if it happend, if so don't do it, otherwise warning if do_interpolate=F ?
  
  x_years <- getItems(x, dim=2)
  ref_years <- getItems(ref, dim=2)
  
  # cut ref if it extends to the future over x (not necessary for backcasting)
  ref <- ref[, ref_years<=max(x_years)]
  ref_years <- getItems(ref, dim=2)
  
  shared_years <- intersect(x_years, ref_years)
  n_shared_years <- length(shared_years)
  
  # fill 0/nan in ref regions that don't have any values in shared years
  if (do_fill_ref_zeroes) {
    # TODO ref <- fillRefZeroes(ref, shared_years)
  }
  
  # calculate ratios and weights
  ratios <- x[,shared_years] / ref[, shared_years]
  weights <- calcBackcastWeights(ratios, n_shared_years, max_n=5)
  
  final_ratio <- rowSums(ratios * weights, na.rm = TRUE)
  
  scaled_ref <- ref * final_ratio
  
  # create final magpie and fill with x and scaled_ref
  final <- new.magpie(
    cells_and_regions = getItems(x, dim=1),
    years = union(ref_years, x_years),
    names = "value",
    fill = NA,
    sets = names(dimnames(x))
  )
  
  final[,x_years] <- x[,x_years]
  
  ref_extended <- final  # simple copy of structure
  ref_extended[,ref_years] <- scaled_ref[,ref_years]
  
  final[is.na(final)] <- ref_extended[is.na(final)]  # update gaps and not existing data in x/final
  
  return(final)
}

calcBackcastWeights <- function(ratios, 
                                n_shared_years=nyears(ratios), 
                                max_n=5) {  #TODO Check if you can do that in R parameters in function statements
  # make new magpie with same regions as ratios and for every region the values 1,2,3...
  sample_weights <- new.magpie(
    years = getItems(ratios, dim=2),
    names = "value",
    fill = n_shared_years:1,
    sets = names(dimnames(ratios))
  )
  
  total_weights <- new.magpie(
    cells_and_regions = getItems(ratios, dim=1),
    years = getItems(ratios, dim=2),
    names = "value",
    fill = NA,
    sets = names(dimnames(ratios))
  )
  
  total_weights[,] <- sample_weights[,]  # TODO better workaround than creating sample_weights?
  
  # Ensure linear weights start end at one
  total_weights[is.na(ratios)]<-n_shared_years+1
  total_weights[ratios==Inf]<-n_shared_years+1
  row_min <- apply2D(total_weights, 1, min)
  total_weights <- total_weights - row_min + 1
  
  # Ensure not more than max_n weights are used
  total_weights[is.na(ratios)]<- -1
  total_weights[ratios==Inf]<- -1
  row_max <- apply2D(total_weights, 1, max)
  offset <- max_n - row_max
  offset[offset>0] <- 0  # ensure no positive offset
  total_weights <- total_weights + offset
  total_weights[total_weights<1] <- NA  # ensure positive weights
  
  # remove weights for years where there is no data in x or ref (and hence ratios)
  total_weights[is.na(ratios)]<-NA
  
  # Normalize weights
  sums <- rowSums(total_weights, na.rm=TRUE)
  normalized_weights <- total_weights / sums
  
  return(normalized_weights)
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
  x_regions <- getItems(x, dim=1)
  ref_regions <- getItems(ref, dim=1)
  
  new_ref <- new.magpie(
    cells_and_regions = x_regions,
    years = getItems(ref, dim=2),
    names = "value",
    fill = NA,
    sets = names(dimnames(ref))
  )
  
  if ('GLO' %in% ref_regions) {
    new_ref[,] <- ref['GLO', ]  # if GLO is in ref, copy it to all regions
  }
  
  # Fill rest of new ref with existing ref data
  new_ref[ref_regions %in% x_regions, ] <- ref[ref_regions %in% x_regions, ]
  
  return(new_ref)
}

fillRefZeroes <- function(ref, shared_years) {
  # get regions that have no data in shared years
  shared <- ref[,shared_years]
  shared[is.na(shared)]<-0
  shared <- rowSums(shared)
  test <- shared>0
  shared <- shared!=0
  test <- rowSums(shared)
  # get rows where all values are 0
  
  
  
  shared[shared<0] <- abs(shared[shared<0])  # ensure no negative values
  
  ref_regions <- getItems(ref, dim=1)
  ref_years <- getItems(ref, dim=2)
  missing_regions <- ref_regions[!ref_regions %in% getItems(ref[, shared_years], dim=1)]
  
  return(ref)
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

