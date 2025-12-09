#' Calculates the lifetimes of residential and non-residential buildings, as well as of civil engeneering.
#' @author Bennet Weiss
calcCeBuiltLifespan <- function() {
  data <- readSource("PostedBuiltLifespan")

  # Split NonRes into Com and Ind and remove NonRes category
  # TODO: reflect this in POSTED instead of here
  data_new <- add_columns(data, addnm = c("Com", "Ind"), dim = 3.1)
  data_new[, , "Com"] <- data[, , "NonRes"]
  data_new[, , "Ind"] <- data[, , "NonRes"]
  data_final <- data_new[, , "NonRes", invert = TRUE]

  unit <- "years (a)"
  description <- paste(
    "Lifetimes of residential and non-residential buildings, as well as of civil engeneering.",
    "Aggregated data from literature research.",
    "Documentation can be found in Posted (https://github.com/PhilippVerpoort/posted)"
  )
  note <- "dimensions: (Historic Time,Region,Stock Type,value)"
  weight <- toolCumulativeCementProduction(data_final)

  output <- list(
    x = data_final,
    weight = weight,
    unit = unit,
    description = description,
    note = note
  )
  return(output)
}
