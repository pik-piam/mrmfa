#' Calculate strength class distribution of concrete.
#'
#' @author Bennet Weiss
calcMCeProductCementContent <- function(){
  x <- readSource("Cao2024", subtype = "product_cement_content")
  x <- x * 1e-3 # convert from kg to tonnes

  # create new magpie object and fill with ones
  weight <- new.magpie(cells_and_regions = NULL)
  weight <- toolCountryFill(weight, fill = 1, verbosity = 2)

  unit <- "tonnes per cubic meter (t/m3)"
  description <- paste(
    "Product cement content.",
    "Data from Cao2024."
  )
  output <- list(x = x, weight = weight, unit = unit, description = description)
}
