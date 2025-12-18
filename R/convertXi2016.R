#' Convert data from Xi2016
#'
#' @author Bennet Weiss
#' @param x Magpie object
convertXi2016 <- function(x) {
  # create empty country list
  x_out <- toolCountryFill(x, verbosity = 2)

  # set regions EUR, NEU, CAZ to USA values
  regionmapping <- toolGetMapping("regionmapping_21_EU11.csv", where = "mappingfolder", type = "regional")

  # EUR
  # fill all countries with European average
  eur_regions <- unique(regionmapping$RegionCode[regionmapping$missingH12 == "EUR"])
  eur_countries <- toolGetRegionCountries(x_out, eur_regions, regionmapping)
  x_out[eur_countries, , ] <- x["USA", , ]

  # NEU
  # EUR values
  neu_regions <- unique(regionmapping$RegionCode[regionmapping$missingH12 == "NEU"])
  neu_countries <- toolGetRegionCountries(x_out, neu_regions, regionmapping)
  x_out[neu_countries, , ] <- x["USA", , ]

  # CAZ
  caz_countries <- toolGetRegionCountries(x_out, "CAZ", regionmapping)
  x_out[caz_countries, , ] <- x["USA", , ]

  # set all other countries to China values
  for (stock_type in getItems(x, dim = 3)) {
    x_out[, , stock_type][is.na(x_out[, , stock_type])] <- x["CHN", , stock_type]
  }

  return(x_out)
}
