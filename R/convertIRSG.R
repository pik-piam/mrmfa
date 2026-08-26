#' Convert IRSG synthetic rubber data to a magpie object at country level
#'
#' Disaggregates the IRSG regional synthetic rubber production to ISO country
#' level. The regions (Asia, Europe, Americas, Africa) are split into their
#' member countries (see \code{regionmappingIRSG.csv}) weighted by each country's
#' synthetic rubber exports from BACI
#' (\code{calcOutput("PlBACI", subtype = "plastics_UNEP", category = "Primary")},
#' summed to gross exports per country with \code{\link{toolAggregateBilateralTrade}}
#' and restricted to the rubber \code{type}). This is a better proxy for rubber
#' production location than total chemical energy use, which over-weights
#' ammonia/methanol feedstock producers. Exports are an imperfect proxy: producers
#' serving mainly domestic demand are under-weighted and re-export hubs slightly
#' over-weighted, but only the within-region country split is affected - regional
#' totals are unchanged.
#'
#' Only the "regional" subtype is country-resolved. The "global_total" subtype
#' is global data and must be read with \code{convert = FALSE}.
#'
#' @param x magpie object of IRSG regional data.
#' @param subtype Character string, must be "regional".
#' @return magpie object of the IRSG data disaggregated to country level.
#' @author Leonie Schweiger
#'
#' @importFrom magclass getYears setYears dimSums mselect
convertIRSG <- function(x, subtype) {
  if (subtype != "regional") {
    stop(
      "convertIRSG only supports subtype 'regional'. Global data ('global_total') ",
      "is not country-resolved; read it with convert = FALSE."
    )
  }

  # region -> country mapping (IRSG grouping from the source's country lists)
  map <- toolGetMapping("regionmappingIRSG.csv", type = "regional", where = "mrmfa")
  countries <- unique(map$CountryCode)

  # disaggregation weight: per-country synthetic rubber exports (see @description).
  # The primary trade holds both polymer resins (type "Plastics") and synthetic
  # rubber (type "Rubber"); keep the rubber type only.
  baci <- calcOutput("PlBACI", subtype = "plastics_UNEP", category = "Primary",
                     HS = "92", aggregate = FALSE)
  iso <- as.character(getISOlist())
  idMap <- data.frame(country = iso, region = iso)
  exports <- toolAggregateBilateralTrade(baci, rel = idMap, flow_label = "Exports")
  exports <- dimSums(mselect(exports, type = "Rubber"), dim = 3)
  exports <- toolCountryFill(exports, fill = 0, verbosity = 2)
  weight <- toolInterpolate(exports, union(getYears(exports), getYears(x)), extrapolate = TRUE)
  # epsilon guard: a region whose countries all have zero exports is split evenly
  # instead of producing NaN when toolAggregate normalises the weight.
  weight <- (weight + 1e-9)[countries, getYears(x), ]

  # disaggregate regions to countries, weighted by synthetic rubber exports
  x <- toolAggregate(x,
    rel = map, dim = 1,
    from = "IRSGReg", to = "CountryCode", weight = weight
  )

  # countries outside the source's regions get zero production
  x <- toolCountryFill(x, fill = 0, verbosity = 2)

  return(x)
}
