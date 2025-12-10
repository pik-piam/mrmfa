#' Calculate strength class distribution of concrete.
#'
#' @author Bennet Weiss
calcCeProductThickness <- function(){
  x <- readSource("Cao2024", subtype = "product_thickness", convert = FALSE)
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
