#' Calculate carbonation rate of different strength concrete.
#'
#' @param subtype Type of carbonation rate. May be "base", "additives", "co2", "coating", "base_buried"
#' @author Bennet Weiss
calcCeCarbonationRate <- function(subtype = "base"){

  full_name <- list(
    base        = "carbonation_rate",
    additives   = "carbonation_rate_factor_additives",
    co2         = "carbonation_rate_factor_co2",
    coating     = "carbonation_rate_factor_coating",
    base_buried = "carbonation_rate_buried"
  )

  unit <- "factor"
  note <- "dimensions: (value)"
  weight <- NULL
  isocountries <- FALSE
  convert <- FALSE

  if (subtype %in% c("base", "base_buried")) {
    unit <- "m/sqrt(a)"
    note <- "dimensions: (Region,Product Application,value)"
    # use aggregated cement production as weight
    isocountries <- TRUE
    convert <- TRUE
  }

  x <- readSource("Cao2024", subtype = full_name[[subtype]], convert = convert)
  if (convert) {
    weight <- toolCeCumulativeCementProduction(castto = x)
    x <- x * 1e-3 # convert from mm/sqrt(yr) to m/sqrt(yr)
  }

  description <- paste(
    "Carbonation rate ", subtype, " of concrete of different strength classes.",
    "Data from Cao2024."
  )
  output <- list(
    x = x,
    weight = weight,
    unit = unit,
    description = description,
    note = note,
    isocountries = isocountries
  )
}
