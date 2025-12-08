#' Calculates how cement consumption is split into stock types.
#' @author Bennet Weiss
calcCeStockTypeSplit <- function() {
  x <- readSource("Xi2016")

  weight <- toolCumulativeCementProduction(castto = x)
  unit <- "ratio"
  description <- paste(
    "Split to sort cement consumption into stock types Res, Com, Ind, and Civ.",
    "Data based on Xi 2016."
  )
  note <- "dimensions: (Region,Stock Type,value)"

  output <- list(
    x = x,
    weight = weight,
    unit = unit,
    description = description,
    note = note
  )
  return(output)
}
