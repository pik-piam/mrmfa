#' Calculates how cement consumption is split into end uses:
#' Res (residential), Com (commercial), Ind (industrial) and Civ (civil engineering).
#' @author Bennet Weiss
calcCeEndUseSplit <- function() {
  x <- readSource("Xi2016")

  weight <- toolCeCumulativeCementProduction(castto = x)
  unit <- "ratio"
  description <- paste(
    "Split to sort cement consumption into end uses Res, Com, Ind, and Civ.",
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
