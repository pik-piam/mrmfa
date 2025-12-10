#' Calculate strength class distribution of concrete.
#'
#' @author Bennet Weiss
calcCeProductCementContent <- function(){
  x <- readSource("Cao2024", subtype = "product_cement_content", convert = FALSE)
  x <- x * 1e-3 # convert from kg to tonnes

  unit <- "tonnes per cubic meter (t/m3)"
  description <- paste(
    "Product cement content.",
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
