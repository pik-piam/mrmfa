#' Convert Plastics Europe production data to ISO country level
#'
#' Disaggregates the regional Plastics Europe production to ISO country level,
#' weighted by each country's primary polymer exports from BACI
#' (\code{calcOutput("PlBACI", subtype = "plastics_UNEP", category = "Primary")},
#' summed to gross exports per country with \code{\link{toolAggregateBilateralTrade}}
#' and restricted to the plastics \code{type}). This is a better proxy for polymer
#' production location than total chemical energy use, which over-weights
#' ammonia/methanol feedstock producers such as Trinidad & Tobago. Exports are an
#' imperfect proxy: producers serving mainly domestic demand are under-weighted
#' and re-export hubs slightly over-weighted, but only the within-region country
#' split is affected - regional totals are unchanged.
#'
#' @param x MagPIE object containing Plastics Europe production data at regional resolution.
#' @return MagPIE object of the Plastics Europe production data disaggregated to country level.
#' @author Leonie Schweiger
#' @examples
#' \dontrun{
#' a <- convertPlasticsEurope(x)
#' }
#' @importFrom magclass getYears mselect dimSums
convertPlasticsEurope <- function(x) {

  # Countries mapped to "Rest" are outside PlasticsEurope's 8 reporting regions
  # (e.g. Western Balkans, Georgia). They are intentionally dropped here and
  # filled with 0 production below, since PlasticsEurope does not cover them.
  region_map <- toolGetMapping("regionmappingPlasticsEurope.csv", type = "regional", where = "mrmfa") %>%
    filter(.data$PlasticsEuropeReg != "Rest")

  # per-country primary polymer exports as disaggregation weight (see @description).
  # The primary trade holds both polymer resins (type "Plastics") and synthetic
  # rubber (type "Rubber"); keep the plastics type only.
  baci <- calcOutput("PlBACI", subtype = "plastics_UNEP", category = "Primary",
                     HS = "92", aggregate = FALSE)
  iso <- as.character(getISOlist())
  idMap <- data.frame(country = iso, region = iso)
  exports <- toolAggregateBilateralTrade(baci, rel = idMap, flow_label = "Exports")
  exports <- dimSums(mselect(exports, type = "Plastics"), dim = 3)
  exports <- toolCountryFill(exports, fill = 0, verbosity = 2)
  weight <- toolInterpolate(exports, union(getYears(exports), getYears(x)), extrapolate = TRUE)
  # epsilon guard: a region whose countries all have zero exports is split evenly
  # instead of producing NaN when toolAggregate normalises the weight.
  weight <- (weight + 1e-9)[, getYears(x), ]

  x <- toolAggregate(x,
                     rel = region_map, dim = 1,
                     from = "PlasticsEuropeReg", to = "CountryCode",
                     weight = weight[unique(region_map$CountryCode), , ])
  x <- toolCountryFill(x, fill = 0, verbosity = 2)

  return(x)
}
