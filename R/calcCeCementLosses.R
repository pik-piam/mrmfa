#' Calculate Losses in cement cycle.
#'
#' @author Bennet Weiss
#' @param subtype Loss type: can be "cement_loss_construction" or "clinker_loss_production"
calcCeCementLosses <- function(subtype) {
  if (!subtype %in% c("cement_loss_construction", "clinker_loss_production")) {
    stop(paste("Subtype ", subtype, " not implemented."))
  }
  x <- readSource("Cao2024", subtype = subtype)
  x <- toolCeDistributionMean(x, "Triangular")

  weight <- toolCeCumulativeCementProduction(castto = x)

  unit <- "ratio"
  description <- paste(
    "Losses in the cement cycle for subtype ", subtype, ".",
    "Data from Cao2024."
  )
  note <- "dimensions: (Region,value)"

  output <- list(
    x = x,
    weight = weight,
    unit = unit,
    description = description,
    note = note
  )
  return(output)
}
