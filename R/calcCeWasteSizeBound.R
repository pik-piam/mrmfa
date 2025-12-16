#' Calculate the size bounds of waste particles for different waste types.
#' Based on categorization from Cao2024
#'
#' @author Bennet Weiss
#' @param subtype Type of Bound.
#' Can be upper (subtype = "max") oder lower (subtype ="min") bound.
#'
calcCeWasteSizeBound <- function(subtype) {

  vals_min <- c(
    0, 5, 10, 20, # new concrete
    0, 1, 10, 30, # aggregates
    0, 5, 30, 50, # landfill
    0, 5, 10, 20 # asphalt
  )

  vals_max <- c(
    5, 10, 20, 32, # new concrete
    1, 10, 30, 60, # aggregates
    10, 30, 50, 100, # landfill
    5, 10, 20, 32 # asphalt
  )

  dimnames <- list(
    c("new concrete", "aggregates", "landfill", "asphalt"),
    c("A", "B", "C", "D")
  )

  arr_min <- array(
    vals_min,
    dim = c(4, 4),
    dimnames = dimnames
  )

  arr_max <- array(
    vals_max,
    dim = c(4, 4),
    dimnames = dimnames
  )

  x_min <- as.magpie(arr_min)
  x_max <- as.magpie(arr_max)

  if (subtype == "min") {
    x <- x_min
  } else if (subtype == "max") {
    x <- x_max
  } else {
    stop(paste("Subtype ", subtype, " not implemented."))
  }

  x <- x * 1e-3 # convert from mm to m
  unit <- "m"
  description <- paste(subtype, " size of waste particles of a size category for different waste types.")
  note <- "dimensions: (Waste Type,Size Category,value)"

  output <- list(
    x = x,
    weight = NULL,
    unit = unit,
    description = description,
    note = note,
    isocountries = FALSE
  )
  return(output)
}
