#' Convert Pottinger 2024 plastics data to ISO country level
#'
#' @description
#' Disaggregate the four Pottinger modelling regions (china, eu30, nafta, row)
#' to ISO3 country level. The regional Mt flows are distributed across the
#' member countries of each region proportionally to population from the UN
#' World Population Prospects (\code{readSource("UN_PopDiv", "pop", ...)}). The
#' "estimates" subset (1950-2021) covers the historical part of the series and
#' the "medium" variant projection (2022-2100) covers the future part, together
#' spanning the yearly 2011-2050 Pottinger years. Countries not covered by the
#' source regions are filled with 0.
#'
#' @param x MagPIE object with Pottinger2024 data at the four-region resolution.
#' @return MagPIE object with the Pottinger2024 data disaggregated to ISO3
#' country level, in Mt.
#' @author Leonie Schweiger
#' @seealso \code{\link{readPottinger2024}}
#' @importFrom magclass mbind getYears getNames<-
convertPottinger2024 <- function(x) {
  # region -> country mapping (every ISO3 country assigned to exactly one region)
  map <- toolGetMapping("regionmappingPottinger2024.csv", type = "regional", where = "mrmfa")

  # UN World Population Prospects population as disaggregation weight: historical
  # estimates (through 2021) plus the medium-variant projection (from 2022 on).
  # The variant name in the data dimension is dropped so the two can be combined.
  popEstimates <- readSource("UN_PopDiv", subtype = "pop", subset = "estimates")
  popMedium <- readSource("UN_PopDiv", subtype = "pop", subset = "medium")
  getNames(popEstimates) <- NULL
  getNames(popMedium) <- NULL
  pop <- mbind(popEstimates, popMedium)

  # restrict weight to the mapped countries and the yearly Pottinger years
  pop <- pop[unique(map$CountryCode), getYears(x), ]

  # distribute each region's flows across its countries proportionally to population
  x <- toolAggregate(x,
    rel = map, dim = 1,
    from = "PottingerReg", to = "CountryCode",
    weight = pop
  )

  # complete the country set (countries outside the source regions get 0)
  x <- toolCountryFill(x, fill = 0)

  return(x)
}
