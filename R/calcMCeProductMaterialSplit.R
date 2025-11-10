#' Calculate end use product share of cement.
#'
#' @author Bennet Weiss
calcMCeProductMaterialSplit <- function(){
  x <- readSource("Cao2024", subtype = "product_material_split")

  # create new magpie object and fill with ones
  weight <- new.magpie(cells_and_regions = NULL)
  weight <- toolCountryFill(weight, fill = 1, verbosity = 2)

  unit <- "ratio"
  description <- paste(
    "Split cement into product materials.",
    "Data from Cao2024."
  )
  output <- list(x = x, weight = weight, unit = unit, description = description)
}
