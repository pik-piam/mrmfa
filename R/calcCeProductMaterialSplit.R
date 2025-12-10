#' Calculate end use product share of cement.
#'
#' @author Bennet Weiss
calcCeProductMaterialSplit <- function(){
  x <- readSource("Cao2024", subtype = "product_material_split")

  weight <- toolCeCumulativeCementProduction(x)

  unit <- "ratio"
  description <- paste(
    "Split cement into product materials.",
    "Data from Cao2024."
  )
  note = "dimensions: (Region,Product Material,value)"

  output <- list(
    x = x,
    weight = weight,
    unit = unit,
    description = description,
    note = note
  )
  return(output)
}
