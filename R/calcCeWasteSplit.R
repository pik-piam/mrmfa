#' Calculate where concrete/mortar waste goes.
#'
#' @author Bennet Weiss
calcCeWasteSplit <- function(){

  x <- readSource("Cao2024", subtype = "waste_split")

  weight <- toolCumulativeCementProduction(castto = x)
  unit <- "ratio"
  description <- paste(
    "Concrete/mortar waste split.",
    "Data from Cao2024."
  )
  note <- "dimensions: (Region,Waste Type,value)"

  output <- list(
    x = x,
    weight = weight,
    unit = unit,
    description = description,
    note = note
  )
}
