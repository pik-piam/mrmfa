#' Calculates how cement consumption is split into end uses:
#' Residential, Commercial, Industrial and Civil Engineering.
#' @author Bennet Weiss
calcCeEndUseSplit <- function() {
  x <- readSource("Xi2016")
  x <- toolCeRenameBuildingItems(x, dim = 3, type = "End Use")

  weight <- toolCeCumulativeCementProduction(castto = x)
  unit <- "ratio"
  description <- paste(
    "Split to sort cement consumption into end uses Residential, Commercial, Industrial, and Civil Engineering.",
    "Data based on Xi 2016."
  )
  note <- "dimensions: (Region,End Use,value)"

  output <- list(
    x = x,
    weight = weight,
    unit = unit,
    description = description,
    note = note
  )
  return(output)
}
