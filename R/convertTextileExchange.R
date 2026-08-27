#' Convert Textile Exchange regional shares to country-level shares
#'
#' Disaggregates the year-varying (2007-2022) chemical-fibre production shares to
#' ISO country level. The source split has eight named countries (DEU, USA, JPN,
#' PAK, IND, KOR, TWN, CHN) plus two residual groups, Western Europe (excl. DE)
#' and Other. The countries belonging to each group are taken from
#' \code{regionmappingTextileExchange.csv}, and each group's share is distributed
#' across its countries proportionally to their primary polymer exports from BACI
#' (\code{calcOutput("PlBACI", subtype = "plastics_UNEP", category = "Primary")},
#' summed to gross exports per country with \code{\link{toolAggregateBilateralTrade}}
#' and restricted to the plastics \code{type}); named single-country groups pass
#' their share through unchanged.
#'
#' The primary trade contains no synthetic fibre (primary fibre exports are always
#' 0), so the same primary polymer export weight as \code{\link{convertPlasticsEurope}}
#' is used: synthetic-fibre production co-locates with polymer/petrochemical
#' production, and the weight only splits the two residual groups.
#'
#' Only the \code{"region_share"} subtype is convertible; the
#' \code{"timeseries_by_type"} subtype is global and must be read with
#' \code{convert = FALSE}.
#'
#' @param x MagPIE object of Textile Exchange regional shares (\code{region_share}).
#' @param subtype Character string specifying the dataset:
#'        - "timeseries_by_type": global synthetic fibre production in Mt by fibre
#'          type and year (fibre, year, production_Mt) - this subtype should not be
#'          regionally disaggregated, so the convert function throws an error for this
#'          subtype
#'        - "region_share": chemical fibre production shares by region and year,
#'          2007-2022, over the split WEU (Western Europe excl. DE), DEU, USA, JPN,
#'          PAK, IND, KOR, TWN, CHN and Other (region, year, share) - to be converted
#'          to iso-country level
#' @return MagPIE object of the shares disaggregated to ISO country level.
#' @author Leonie Schweiger
#' @examples
#' \dontrun{
#' a <- readSource("TextileExchange", subtype = "region_share")
#' }
#' @importFrom magclass getComment getYears setYears mselect dimSums
convertTextileExchange <- function(x, subtype) {
  if (subtype != "region_share") {
    stop("convertTextileExchange only supports the 'region_share' subtype; ",
         "read 'timeseries_by_type' with convert = FALSE.")
  }

  # region -> country mapping (named countries only, from the source comment column)
  map <- toolGetMapping("regionmappingTextileExchange.csv", type = "regional", where = "mrmfa")

  # disaggregation weight: per-country primary polymer exports (see @description).
  baci <- calcOutput("PlBACI", subtype = "plastics_UNEP", category = "Primary",
                     HS = "92", aggregate = FALSE)
  iso <- as.character(getISOlist())
  idMap <- data.frame(country = iso, region = iso)
  exports <- toolAggregateBilateralTrade(baci, rel = idMap, flow_label = "Exports")
  exports <- dimSums(mselect(exports, type = "Plastics"), dim = 3)
  exports <- toolCountryFill(exports, fill = 0, verbosity = 2)
  exports[is.na(exports)] <- 0
  weight <- toolInterpolate(exports, union(getYears(exports), getYears(x)), extrapolate = TRUE)
  # epsilon guard against zero-export residual groups (see convertPlasticsEurope)
  weight <- (weight + 1e-9)[, getYears(x), ]

  x <- toolAggregate(x,
    rel = map, dim = 1,
    from = "TextileExchangeReg", to = "CountryCode",
    weight = weight
  )

  x <- toolCountryFill(x, fill = 0)

  return(x)
}
