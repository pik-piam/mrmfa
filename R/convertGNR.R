#' Convert data from GNR
#'
#' @author Bennet Weiss
#' @param x Magpie object
convertGNR <- function(x) {
  isocodes <- suppressWarnings(madrat::toolCountry2isocode(getItems(x, dim = 1)))
  magclass::getItems(x, dim = 1)[!is.na(isocodes)] <- isocodes[!is.na(isocodes)]

  no_remove_warning <- c(
    "Morocco + Algeria + Tunisia", "EU27", "South/Latin America", "World", # are disaggregated manually
    "Cembureau members", "FICEM members", "Northeast Asia" # omitted
  )
  x_out <- madrat::toolCountryFill(x, fill = NA, verbosity = 2, no_remove_warning = no_remove_warning)

  # Morocco, Algeria and Tunisia
  x_out["MAR", ] <- x["Morocco + Algeria + Tunisia", ]
  x_out["DZA", ] <- x["Morocco + Algeria + Tunisia", ]
  x_out["TUN", ] <- x["Morocco + Algeria + Tunisia", ]

  # EU 27
  eu27_iso3 <- c(
    "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST",
    "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA",
    "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK",
    "SVN", "ESP", "SWE"
  )
  eu27_filtered <- eu27_iso3[!eu27_iso3 %in% isocodes]
  x_out[eu27_filtered, , ] <- x["EU27", ]

  # South/Latin America
  # "Rest of South America" Residential values
  regionmapping <- toolGetMapping("regionmapping_21_EU11.csv", where = "mappingfolder", type = "regional")
  lam_countries <- toolGetRegionCountries(x_out, "LAM", regionmapping)
  lam_countries_filtered <- lam_countries[!lam_countries %in% isocodes]
  x_out[lam_countries_filtered, , ] <- x["South/Latin America", ]

  # World
  # set rest to world values
  set_countries <- Filter(Negate(is.na), c(isocodes, eu27_filtered, lam_countries_filtered, c("MAR", "DZA", "TUN")))
  left_countries <- getItems(x_out, dim = 1)[!getItems(x_out, dim = 1) %in% set_countries]
  x_out[left_countries, , ] <- x["World", ]

  return(x_out)
}
