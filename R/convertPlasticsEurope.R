#' Convert Plastics Europe production data to ISO country level
#'
#' @param x MagPIE object containing Plastics Europe production data at regional resolution.
#' @return MagPIE object of the Plastics Europe production data disaggregated to country level.
#' @author Leonie Schweiger
#' @examples
#' \dontrun{
#' a <- convertPlasticsEurope(x)
#' }
convertPlasticsEurope <- function(x) {

  # disaggregate to iso3 country level by chemical energy consumption weighting
  # Countries mapped to "Rest" are outside PlasticsEurope's 8 reporting regions
  # (e.g. Western Balkans, Georgia). They are intentionally dropped here and
  # filled with 0 production below, since PlasticsEurope does not cover them.
  region_map <- toolGetMapping("regionmappingPlasticsEurope.csv", type = "regional", where = "mrmfa") %>%
    filter(.data$PlasticsEuropeReg != "Rest")
  chem <- calcOutput("ChemicalTotal", aggregate = FALSE)
  chem <- toolInterpolate(chem, union(getYears(chem), getYears(x)), extrapolate = TRUE)
  x <- toolAggregate(x,
                     rel = region_map, dim = 1,
                     from = "PlasticsEuropeReg", to = "CountryCode",
                     weight = chem[unique(region_map$CountryCode), getYears(x), ])
  x <- toolCountryFill(x, fill = 0, verbosity = 2)

  return(x)
}
