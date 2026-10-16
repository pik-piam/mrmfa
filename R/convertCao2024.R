#' Convert data from Cao
#'
#' Distribution parameters of the source regions are copied to all countries of a region.
#'
#' @author Bennet Weiss
#' @param x Magpie object
convertCao2024 <- function(x) {
  region_mapping <- toolGetMapping("regionmappingR10_extended.csv", where = "mrmfa", type = "regional")
  x_out <- toolAggregate(x, region_mapping, from = "RegionName", to = "CountryCode")
  return(x_out)
}
