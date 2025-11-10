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

  x <- readSource("Cao2024", subtype = full_name[[subtype]])
  unit <- "factor"

  if (subtype %in% c("base", "base_buried")) {
    x <- x * 1e-3 # convert from mm/sqrt(yr) to m/sqrt(yr)
    unit <- "m/sqrt(a)"
  }

  # create new magpie object and fill with ones
  weight <- new.magpie(cells_and_regions = NULL)
  weight <- toolCountryFill(weight, fill = 1, verbosity = 2)

  description <- paste(
    "Carbonation rate ", subtype, " of concrete of different strength classes.",
    "Data from Cao2024."
  )
  output <- list(x = x, weight = weight, unit = unit, description = description)
}
