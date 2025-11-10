#' Calculate where concrete/mortar waste goes.
#'
#' @author Bennet Weiss
calcMCeWasteSplit <- function(){

  x <- readSource("Cao2024", subtype = "waste_split")

  # create new magpie object and fill with ones
  weight <- new.magpie(cells_and_regions = NULL)
  weight <- toolCountryFill(weight, fill = 1, verbosity = 2)

  unit <- "ratio"
  description <- paste(
    "Concrete/mortar waste split.",
    "Data from Cao2024."
  )
  output <- list(x = x, weight = weight, unit = unit, description = description)
}
