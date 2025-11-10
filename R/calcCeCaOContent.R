#' Calculate share of CaO in cement or clinker.
#'
#' @param subtype Material of which CaO content is of interest. May be "clinker" or "CKD"
#' @author Bennet Weiss
calcCeCaOContent <- function(subtype){

  if (subtype == "CKD"){
    x <- readSource("Cao2024", subtype = "CKD_cao_content")
  } else if (subtype == "clinker") {
    x <- readSource("Cao2024", subtype = "clinker_cao_content")
  } else {
    stop(paste("Subtype ", subtype, " not implemented."))
  }

  # create new magpie object and fill with ones
  weight <- new.magpie(cells_and_regions = NULL)
  weight <- toolCountryFill(weight, fill = 1, verbosity = 2)

  unit <- "ratio"
  description <- paste(
    "Share of CaO in ", subtype, ".",
    "Data from Cao2024."
  )
  output <- list(x = x, weight = weight, unit = unit, description = description)
}
