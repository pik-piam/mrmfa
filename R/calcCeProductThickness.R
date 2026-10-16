#' Calculate thickness of product applications.
#'
#' The harmonic mean is used, as the thickness enters the carbonation calculation as a divisor.
#'
#' @author Bennet Weiss
calcCeProductThickness <- function() {
  concrete_apps <- c("C15", "C20", "C30", "C35")

  concrete <- readSource("Cao2024", subtype = "wall_thickness", convert = FALSE)
  concrete <- toolCeDistributionMean(concrete, "Uniform", harmonic = TRUE)
  # Assumption: Cao2024 gives one wall thickness, used for all concrete strength classes.
  concrete <- toolCeCopyItem(concrete, concrete_apps, dim = "Product Application")
  mortar <- readSource("Cao2024", subtype = "mortar_thickness", convert = FALSE)
  mortar <- toolCeDistributionMean(mortar, "Weibull", harmonic = TRUE)
  x <- dimReduce(mbind(concrete, mortar))
  x <- x * 1e-3 # convert from mm to m

  unit <- "m"
  description <- paste(
    "Thickness of product application.",
    "Data from Cao2024."
  )
  note <- "dimensions: (Product Application,value)"

  output <- list(
    x = x,
    weight = NULL,
    unit = unit,
    description = description,
    note = note,
    isocountries = FALSE
  )
  return(output)
}
