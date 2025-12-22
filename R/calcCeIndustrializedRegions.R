#' Define Regions that have industrialized already.
#' Based on expert judgement for H12 regions.
#' When calling this function via calcOutput() for other than H12 region set,
#' use round=0 as an argument if a binary output is desired.
#'
#' @author Bennet Weiss
calcCeIndustrializedRegions <- function() {
  regionmapping <- toolGetMapping("h12.csv", type = "regional")

  industrializedRegions <- c(
    "EUR" = TRUE,
    "NEU" = TRUE,
    "CAZ" = TRUE,
    "CHA" = TRUE,
    "JPN" = TRUE,
    "USA" = TRUE
  )

  industrialized_mp <- as.magpie(industrializedRegions, spatial = 1)

  industrialized_mp <- toolCountryFill(
    industrialized_mp,
    fill = FALSE,
    countrylist = unique(regionmapping$RegionCode),
    verbosity = 2
  )

  # aggregate transforms TRUE to 1 and FALSE to 0
  industrialized_mp <- toolAggregate(
    industrialized_mp,
    regionmapping,
    from = "RegionCode",
    to = "CountryCode"
  )

  weight <- toolCeCumulativeCementProduction()
  unit <- "tonnes cement per capita"
  description <- paste(
    "Industrialized regions marked with 1, non-industrialized with 0."
  )
  note <- "dimensions: (Region,value)"
  output <- list(
    x = industrialized_mp,
    weight = weight,
    unit = unit,
    description = description,
    note = note,
    min = 0,
    max = 1
  )
  return(output)
}
