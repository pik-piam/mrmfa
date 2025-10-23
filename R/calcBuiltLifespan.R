#' Calculates the lifetimes of residential and non-residential buildings, as well as of civil engeneering.
#' @author Bennet Weiss
calcBuiltLifespan <- function() {
  data <- readSource("PostedBuiltLifespan")
  unit <- "years (a)"
  description <- paste(
    "Lifetimes of residential and non-residential buildings, as well as of civil engeneering.",
    "Aggregated data from literature research.",
    "Documentation can be found in Posted (https://github.com/PhilippVerpoort/posted)"
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
