#' Calculate Losses in cement cycle.
#'
#' @author Bennet Weiss
#' @param subtype Loss type: can be "cement_loss_construction" or "clinker_loss_production"
calcMCeCementLosses <- function(subtype){
  x <- readSource("Cao2024", subtype = subtype)

  # create new magpie object and fill with ones
  weight <- new.magpie(cells_and_regions = NULL)
  weight <- toolCountryFill(weight, fill = 1, verbosity = 2)

  unit <- "ratio"
  description <- paste(
    "Losses in the cement cycle for subtype ", subtype, ".",
    "Data from Cao2024."
  )
  output <- list(x = x, weight = weight, unit = unit, description = description)
}
