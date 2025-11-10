#' Calculate strength class distribution of concrete.
#'
#' @author Bennet Weiss
calcCeProductThickness <- function(){
  x <- readSource("Cao2024", subtype = "product_thickness")
  x <- x * 1e-3 # convert from mm to m

  # create new magpie object and fill with ones
  weight <- new.magpie(cells_and_regions = NULL)
  weight <- toolCountryFill(weight, fill = 1, verbosity = 2)

  unit <- "m"
  description <- paste(
    "Thickness of product application.",
    "Data from Cao2024."
  )
  output <- list(x = x, weight = weight, unit = unit, description = description)
}
