#' Convert Textile Exchange regional shares to country-level shares
#'
#' Disaggregates the year-varying (2007-2022) chemical-fibre production shares to
#' ISO country level. The source split has eight named countries (DEU, USA, JPN,
#' PAK, IND, KOR, TWN, CHN) plus two residual groups, Western Europe (excl. DE)
#' and Other. The countries belonging to each group are taken from
#' \code{regionmappingTextileExchange.csv}, and each group's share is distributed
#' across its countries proportionally to their chemical energy consumption
#' (\code{calcOutput("ChemicalTotal")}, from mrindustry); named single-country
#' groups pass their share through unchanged.
#'
#' Only the \code{"region_share"} subtype is convertible; the
#' \code{"timeseries_by_type"} subtype is global and must be read with
#' \code{convert = FALSE}.
#'
#' @param x MagPIE object of Textile Exchange regional shares (\code{region_share}).
#' @return MagPIE object of the shares disaggregated to ISO country level.
#' @author Leonie Schweiger
#' @examples
#' \dontrun{
#' a <- readSource("TextileExchange", subtype = "region_share")
#' }
#' @importFrom magclass getComment getYears setYears
convertTextileExchange <- function(x) {
  if (!any(grepl("region_share", getComment(x)))) {
    stop("convertTextileExchange only supports the 'region_share' subtype; ",
         "read 'timeseries_by_type' with convert = FALSE.")
  }

  # region -> country mapping (named countries only, from the source comment column)
  map <- toolGetMapping("regionmappingTextileExchange.csv", type = "regional", where = "mrmfa")

  # disaggregation weights: country-level chemical energy consumption.
  # ChemicalTotal does not reach the share year (2024), so use its latest available
  # year and align it to the share year.
  chem <- calcOutput("ChemicalTotal", aggregate = FALSE)
  chem <- toolInterpolate(chem, union(getYears(chem), getYears(x)), extrapolate = TRUE)
  chem <- chem[, getYears(x), ]
  getNames(chem) <- NULL

  x <- toolAggregate(x,
    rel = map, dim = 1,
    from = "TextileExchangeReg", to = "CountryCode",
    weight = chem
  )

  x <- toolCountryFill(x, fill = 0)

  return(x)
}
