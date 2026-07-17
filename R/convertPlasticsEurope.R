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

  # disaggregate to iso3 country level by GDP weighting
  region_map <- toolGetMapping("regionmappingPlasticsEurope.csv", type = "regional", where = "mrmfa") %>%
    filter(.data$PlasticsEuropeReg != "Rest")
  GDP <- calcOutput("CoGDP", perCapita = FALSE, aggregate = FALSE)[, getYears(x), ]
  x <- toolAggregate(x,
                     rel = region_map, dim = 1,
                     from = "PlasticsEuropeReg", to = "CountryCode",
                     weight = GDP[unique(region_map$CountryCode), , ])
  x <- toolCountryFill(x, fill = 0)

  return(x)
}
