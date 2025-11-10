#' Calculates how cement consumption is split into stock types.
#' @author Bennet Weiss
calcCeStockTypeSplit <- function() {
  data <- readSource("Xi2016")
  unit <- "ratio"
  description <- paste(
    "Split to sort cement consumption into stock types Res, Com, Ind, and Civ.",
    "Data based on Xi 2016."
  )
  weight <- data
  weight[, , ] <- 1
  output <- list(
    x = data,
    weight = weight,
    unit = unit,
    description = description
  )
  return(output)
}
