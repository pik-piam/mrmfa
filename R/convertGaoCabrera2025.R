#' Convert Gao & Cabrera-Serrenho 2025 consumption to ISO country level
#'
#' @description
#' Disaggregate the eight Gao & Cabrera-Serrenho (2025) world regions to ISO3
#' country level. The regional kt consumption is distributed across the member
#' countries of each region proportionally to population from the UN World
#' Population Prospects (\code{readSource("UN_PopDiv", "pop", subset = "estimates")}),
#' whose "estimates" subset (1950-2021) spans the full Gao year range (1978-2021).
#' Countries not covered by the source regions are filled with 0.
#'
#' @param x MagPIE object with GaoCabrera2025 data at the eight-region resolution.
#' @return MagPIE object with the GaoCabrera2025 apparent consumption
#' disaggregated to ISO3 country level, in kt.
#' @author Leonie Schweiger
#' @seealso \code{\link{readGaoCabrera2025}}
#' @importFrom magclass getYears getNames<-
convertGaoCabrera2025 <- function(x) {
  # region -> country mapping. Drop rows without an ISO3 code (historical /
  # composite trade entities) and any duplicated ISO3 codes (e.g. PSE appears
  # twice) so each country is assigned to exactly one region.
  map <- toolGetMapping("regionmappingGaoCabrera2025.csv", type = "regional", where = "mrmfa")
  map <- map[map$iso3_country_code != "" & !duplicated(map$iso3_country_code), ]

  # UN World Population Prospects population as disaggregation weight. The
  # "estimates" subset covers 1950-2021, spanning all Gao years; the variant
  # name in the data dimension is dropped.
  pop <- readSource("UN_PopDiv", subtype = "pop", subset = "estimates")
  getNames(pop) <- NULL

  # restrict weight to the mapped countries and the Gao years
  pop <- pop[unique(map$iso3_country_code), getYears(x), ]

  # distribute each region's consumption across its countries proportionally to population
  x <- toolAggregate(x,
    rel = map, dim = 1,
    from = "region", to = "iso3_country_code",
    weight = pop
  )

  # complete the country set (countries outside the source regions get 0)
  x <- toolCountryFill(x, fill = 0)

  return(x)
}
