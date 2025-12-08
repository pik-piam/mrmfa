#' Convert data from Cao
#'
#' @author Bennet Weiss
#' @param x Magpie object
convertCao2024 <- function(x){

  if (length(x) == 1) {
    x_out <- toolCountryFill(x, fill = x[1], verbosity = 2)

  } else {
    region_mapping <- toolGetMapping("regionmappingR10_extended.csv", where = "mappingfolder", type = "regional")
    x_out <- toolAggregate(x, region_mapping, from="RegionName", to="CountryCode")
  }

  return(x_out)
}
