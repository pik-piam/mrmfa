#' Merge two 2D magpie objects
#' @description
#' Perform an outer merge on two 2D magpie objects (regions x years).
#'
#' @author Merlin Jo Hosak
#' @param a First magpie object to merge
#' @param b Second magpie object to merge
#' @param fill Value to use for cells not present in either input (default: NA)
#' @export
toolMerge2D <- function(a, b, fill = NA) {
  if (!(is.magpie(a) && is.magpie(b))) {
    stop("Both a and b must be magpie objects")
  }
  aRegions <- getRegions(a)
  bRegions <- getRegions(b)
  aYears <- getYears(a)
  bYears <- getYears(b)

  final <- new.magpie(
    cells_and_regions = union(aRegions, bRegions),
    years = union(aYears, bYears),
    fill = fill
  )
  final[aRegions, aYears] <- a
  final[bRegions, bYears] <- b

  return(final)
}
