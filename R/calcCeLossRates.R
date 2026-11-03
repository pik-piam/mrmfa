#' Calculate loss rates in the cement cycle.
#'
#' @author Bennet Weiss
#' @param subtype Loss type: can be "cement_loss_construction" or "clinker_loss_production"
calcCeLossRates <- function(subtype) {
  if (!subtype %in% c("cement_loss_construction", "clinker_loss_production")) {
    stop(paste("Subtype ", subtype, " not implemented."))
  }
  x <- readSource("Cao2024", subtype = subtype)
  x <- collapseDim(toolCeDistributionMean(x, "Triangular"), dim = 3)

  unit <- "ratio"
  description <- paste(
    "Loss rates in the cement cycle for subtype ", subtype, ".",
    "Data from Cao2024."
  )
  note <- "dimensions: (Region,value)"
  weight <- toolCeCumulativeCementProduction(castto = x)

  output <- list(
    x = x,
    weight = weight,
    unit = unit,
    description = description,
    note = note
  )
  return(output)
}
