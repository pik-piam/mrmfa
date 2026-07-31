#' Calculates the lifetimes of residential and non-residential buildings, as well as of civil engineering.
#' @author Bennet Weiss
calcCeBuiltLifespan <- function() {
  data <- readSource("PostedBuiltLifespan")

  # Split NonRes into Com and Ind and remove NonRes category
  data_new <- add_columns(data, addnm = c("Com", "Ind"), dim = 3.1)
  data_new[, , "Com"] <- data[, , "NonRes"]
  data_new[, , "Ind"] <- data[, , "NonRes"]
  # Split Res into single- (RS) and multi-family (RM) homes and remove Res category
  data_new <- add_columns(data_new, addnm = c("RS", "RM"), dim = 3.1)
  data_new[, , "RS"] <- data[, , "Res"]
  data_new[, , "RM"] <- data[, , "Res"]
  data_final <- data_new[, , c("NonRes", "Res"), invert = TRUE]

  unit <- "years (a)"
  description <- paste(
    "Lifetimes of residential and non-residential buildings, as well as of civil engeneering.",
    "Aggregated data from literature research.",
    "Documentation can be found in Posted (https://github.com/PhilippVerpoort/posted)"
  )
  note <- "dimensions: (Historic Time,Region,Good,value)"
  weight <- toolCeCumulativeCementProduction(data_final)

  output <- list(
    x = data_final,
    weight = weight,
    unit = unit,
    description = description,
    note = note
  )
  return(output)
}
