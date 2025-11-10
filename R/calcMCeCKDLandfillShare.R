#' Calculate share of CKD that goes to landfill.
#'
#' @author Bennet Weiss
calcMCeCKDLandfillShare <- function(){
  x <- readSource("Cao2024", subtype = "CKD_landfill_share")

  # create new magpie object and fill with ones
  weight <- new.magpie(cells_and_regions = NULL)
  weight <- toolCountryFill(weight, fill = 1, verbosity = 2)

  unit <- "ratio"
  description <- paste(
    "Share of CKD that goes to landfill.",
    "Data from Cao2024."
  )
  output <- list(x = x, weight = weight, unit = unit, description = description)
}
