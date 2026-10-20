#' Calculates the lifetimes of residential, commercial and industrial buildings, as well as of civil engineering.
#' @author Bennet Weiss
calcCeBuiltLifespan <- function() {
  # Prepare data to have same dimensions as main driver cement production
  weight <- calcOutput("CeCementProduction", aggregate = FALSE)
  data <- readSource("PostedBuiltLifespan")
  data <- toolCeRenameBuildingItems(data, dim = 3, type = "End Use")

  # Extend constantly into the past and future to cover the full time range of cement production
  data <- toolInterpolate(data, years = getYears(weight), type = "linear", extrapolate = TRUE)

  weight <- magpie_expand(weight, data)
  weight[weight == 0] <- 1e-9

  unit <- "years (a)"
  description <- paste(
    "Lifetimes of residential, commercial and industrial buildings, as well as of civil engineering.",
    "Aggregated data from literature research.",
    "Dataset can be found in https://github.com/bennet21/posted/tree/lifetimes.",
    "Documentation can be found in Posted (https://github.com/PhilippVerpoort/posted)."
  )
  note <- "dimensions: (Historical Time,Region,End Use,value)"

  output <- list(
    x = data,
    weight = weight,
    unit = unit,
    description = description,
    note = note
  )
  return(output)
}
