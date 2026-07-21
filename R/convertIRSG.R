#' Convert IRSG synthetic rubber data to a magpie object at country level
#'
#' Disaggregates the IRSG regional synthetic rubber production to ISO country
#' level. The regions (Asia, Europe, Americas, Africa) are split into their
#' member countries (see \code{regionmappingIRSG.csv}) weighted by country-level
#' chemical energy consumption from \code{\link[mrindustry]{calcChemicalTotal}}.
#'
#' Only the "regional" subtype is country-resolved. The "global_total" subtype
#' is global data and must be read with \code{convert = FALSE}.
#'
#' @param x magpie object of IRSG regional data.
#' @param subtype Character string, must be "regional".
#' @return magpie object of the IRSG data disaggregated to country level.
#' @author Leonie Schweiger
#'
#' @importFrom magclass getYears setYears dimSums
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

  # country-level chemical energy consumption as disaggregation weight
  weight <- calcOutput("ChemicalTotal", aggregate = FALSE)[countries,getYears(x),]
  getNames(weight) <- NULL

  # disaggregate regions to countries, weighted by chemical energy consumption
  x <- toolAggregate(x,
    rel = map, dim = 1,
    from = "IRSGReg", to = "CountryCode", weight = weight
  )

  # countries outside the source's regions get zero production
  x <- toolCountryFill(x, fill = 0, verbosity = 2)

  return(x)
}
