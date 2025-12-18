#' Sets the regionally differentiated cement stock saturation level.
#' Based on expert guess informed by historic saturation levels.
#' @author Bennet Weiss
calcCeStockSaturationLevel <- function() {
  regionmapping <- toolGetMapping("h12.csv", type = "regional")

  region_saturation <- c(
    CAZ = 18,
    CHA = 30,
    EUR = 24,
    IND = 20,
    JPN = 18,
    LAM = 20,
    MEA = 20,
    NEU = 24,
    OAS = 20,
    REF = 20,
    SSA = 20,
    USA = 18
  )

  region_saturation <- as.magpie(region_saturation, spatial = 1)

  country_saturation <- toolAggregate(
    region_saturation,
    regionmapping,
    from = "RegionCode",
    to = "CountryCode"
  )

  weight <- toolCeCumulativeCementProduction()
  unit <- "tonnes cement per capita"
  description <- paste(
    "Regionally differentiated cement stock saturation level. ",
    "Based on expert guess informed by historic saturation levels."
  )
  note <- "dimensions: (Region,value)"

  output <- list(
    x = country_saturation,
    weight = weight,
    unit = unit,
    description = description,
    note = note
  )
  return(output)
}
