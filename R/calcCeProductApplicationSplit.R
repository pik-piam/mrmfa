#' Calculate strength class distribution of concrete.
#'
#' @author Bennet Weiss
calcCeProductApplicationSplit <- function() {
  x <- readSource("Cao2024", subtype = "product_application_split")

  weight <- toolCeCumulativeCementProduction(castto = x)
  unit <- "ratio"
  description <- paste(
    "Split of product materials by application.",
    "Data from Cao2024."
  )
  note <- "dimensions: (Region,Product Application,value)"

  output <- list(
    x = x,
    weight = weight,
    unit = unit,
    description = description,
    note = note
  )
  return(output)
}
