#' Convert data from RASMI
#'
#' @author Bennet Weiss
#' @param x Magpie object
convertRASMI <- function(x) {
  region_mapping <- toolGetMapping("regionmapping_SSP32.csv", where = "mappingfolder", type = "regional")

  # create empty magpie object with all countries to be filled with values
  no_remove_warning <- getItems(x, dim = 1)
  x_out <- madrat::toolCountryFill(x, fill = NA, verbosity = 2, no_remove_warning)

  # get regions from x
  ssp32Regions <- getItems(x, dim = 1)

  # fill countries with values from regions
  for (region in ssp32Regions) {
    # cut SSPR5 name
    short_region <- strsplit(region, "_")[[1]][2]
    countries_in_region <- toolGetRegionCountries(x_out, short_region, region_mapping)
    x_out[countries_in_region,,] <- x[region,,]
  }

  return(x_out)
}
