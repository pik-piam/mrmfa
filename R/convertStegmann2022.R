#' Convert Stegmann 2022 plastics data to ISO country level
#'
#' @description
#' Disaggregate the 26 Stegmann (IMAGE) regions to ISO3 country level. The
#' regional production (PJ/yr) and population (million) are distributed across the
#' member countries of each region proportionally to population from
#' \code{calcOutput("CoPopulation", scenarios = "SSP2")}, whose yearly 1800-2150
#' coverage spans the Stegmann years (2005-2100). 
#'
#' @param x MagPIE object with Stegmann2022 data at the 26-region resolution.
#' @return MagPIE object with the Stegmann2022 data disaggregated to ISO3 country
#' level (production in PJ/yr, population in million).
#' @author Leonie Schweiger
#' @seealso \code{\link{readStegmann2022}}
#' @importFrom magclass getYears
convertStegmann2022 <- function(x) {
  # region -> country mapping (every ISO3 country assigned to exactly one region)
  map <- toolGetMapping("regionmappingStegmann2022.csv", type = "regional", where = "mrmfa")

  # country-level population as disaggregation weight, restricted to the mapped
  # countries and the Stegmann years
  pop <- calcOutput("CoPopulation", scenarios = "SSP2", aggregate = FALSE)
  pop <- pop[unique(map$CountryCode), getYears(x), ]

  # distribute each region's values across its countries proportionally to population
  x <- toolAggregate(x,
    rel = map, dim = 1,
    from = "StegmannReg", to = "CountryCode",
    weight = pop
  )

  # complete the country set (countries outside the source regions get 0)
  x <- toolCountryFill(x, fill = 0)

  return(x)
}
