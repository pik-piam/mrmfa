#' Calculate carbonation rate of different strength concrete.
#'
#' @param subtype Type of carbonation rate. May be "base", "additives", "co2", "coating", "base_buried"
#' @author Bennet Weiss
calcCeCarbonationRate <- function(subtype = "base") {
  mortar_apps <- c("finishing", "masonry", "maintenance")

  unit <- "factor"
  note <- "dimensions: (value)"
  weight <- NULL
  isocountries <- FALSE

  if (subtype == "base") {
    concrete <- readSource("Cao2024", subtype = "carbonation_rate_concrete")
    concrete <- toolCeDistributionMean(concrete, "Uniform")
    mortar <- readSource("Cao2024", subtype = "carbonation_rate_mortar")
    mortar <- toolCeDistributionMean(mortar, "Triangular")
    # Assumption: Cao2024 gives one carbonation rate for mortar, used for all mortar applications.
    mortar <- toolCeCopyItem(mortar, mortar_apps, dim = "Product Application")
    x <- mbind(concrete, mortar)
  } else if (subtype == "base_buried") {
    concrete <- readSource("Cao2024", subtype = "carbonation_rate_buried_concrete")
    concrete <- toolCeDistributionMean(concrete, "Point")
    # Assumption: Cao2024 gives no buried carbonation rate for mortar, the one of C15 concrete is used.
    # Not important, as mortar is fully carbonated after the in-use phase.
    mortar <- toolCeCopyItem(concrete[, , "C15"], mortar_apps, dim = "Product Application")
    x <- mbind(concrete, mortar)
  } else if (subtype %in% c("additives", "co2", "coating")) {
    x <- readSource("Cao2024", subtype = paste0("carbonation_factor_", subtype), convert = FALSE)
    x <- dimReduce(toolCeDistributionMean(x, "Weibull"))
  } else {
    stop(paste("Subtype ", subtype, " not implemented."))
  }

  if (subtype %in% c("base", "base_buried")) {
    unit <- "m/sqrt(a)"
    note <- "dimensions: (Region,Product Application,value)"
    # use aggregated cement production as weight
    isocountries <- TRUE
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
  return(output)
}
