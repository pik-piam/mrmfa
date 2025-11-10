#' Convert data from Cao
#'
#' @author Bennet Weiss
#' @param x Magpie object
convertCao2024 <- function(x){
  region_mapping <- toolGetMapping("regionmapping_9Regions.csv", where = "mappingfolder", type = "regional")

  x_out <- toolAggregate(x, region_mapping, from="RegionName", to="CountryCode")

  return(x_out)
}
