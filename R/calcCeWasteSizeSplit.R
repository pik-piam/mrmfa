#' Calculate what size distribution concrete/mortar waste has.
#' Size categories defined by particle radius.
#'
#' @author Bennet Weiss
calcCeWasteSizeSplit <- function(){

  x <- readSource("Cao2024", subtype = "waste_size_split")

  weight <- toolCumulativeCementProduction(castto = x)
  unit <- "ratio"
  description <- paste(
    "Concrete/mortar waste particle size split.",
    "Data from Cao2024."
  )
  note <- "dimensions: (Region,Waste Type,Waste Size,value)"

  output <- list(
    x = x,
    weight = weight,
    unit = unit,
    description = description,
    note = note
  )
}
