#' Convert data from RASMI
#'
#' @author Bennet Weiss
#' @param x Magpie object
convertRASMI <- function(x) {
  region_mapping <- toolGetMapping("regionmapping_SSP32.csv", where = "mrmfa", type = "regional")
  # cut SSPR5 name
  getItems(x, dim = 1) <- gsub("^.*_", "", getItems(x, dim = 1))
  x_out <- toolAggregate(x, region_mapping, from = "RegionCode", to = "CountryCode")
  return(x_out)
}
