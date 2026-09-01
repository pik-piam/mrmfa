#' Convert data from PostedBuiltLifespan. Lifetimes are assigned to region based on regionmapping.
#' @author Bennet Weiss
#' @param x Magpie object
convertPostedBuiltLifespan <- function(x) {
  # 1. Fill lifetimes on H12 level, where available
  h12_regionmapping <- toolGetMapping("H12.csv", where = "mappingfolder", type = "regional")
  x_h12 <- magclass::new.magpie(
    cells_and_regions = unique(h12_regionmapping$RegionCode),
    years = magclass::getYears(x),
    names = magclass::getNames(x),
    sets = magclass::getSets(x)
  )
  x_h12["REF", , ] <- x["CIS", , ]
  x_h12["EUR", , ] <- x["Europe", , ]
  x_h12["NEU", , ] <- x["Europe", , ]
  x_h12["CHA", , ] <- x["China", , ]
  x_h12 <- toolAggregate(x_h12, h12_regionmapping, from = "RegionCode", to = "CountryCode")

  # 2. Fill lifetimes on custom region level
  custom_regionmapping <- toolGetMapping(
    name = "regionmapping_postedBuiltLifespan.csv",
    where = "mrmfa",
    type = "regional"
  )
  custom_regions <- unique(custom_regionmapping$Region)
  x_out <- x[custom_regions, , ]
  x_out <- toolAggregate(x_out, custom_regionmapping, from = "Region", to = "CountryCode")

  # 3. Fill lifetimes for individual countries
  filled_regions <- c(custom_regions, "CIS", "Europe", "China")
  isocodes <- madrat::toolCountry2isocode(getItems(x, dim = 1), ignoreCountries = filled_regions)
  magclass::getItems(x, dim = 1)[!is.na(isocodes)] <- isocodes[!is.na(isocodes)]
  x_country <- madrat::toolCountryFill(x, fill = NA, verbosity = 2, no_remove_warning = filled_regions)

  # 4. Merge the three datasets
  x_out <- ifelse(!is.na(x_h12), x_h12, x_out)
  x_out <- ifelse(!is.na(x_country), x_country, x_out)

  return(x_out)
}
