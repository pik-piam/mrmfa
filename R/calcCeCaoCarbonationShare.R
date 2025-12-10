#' Calculate share of cao available for carbonation.
#'
#' @author Bennet Weiss
calcCeCaOCarbonationShare <- function() {
  x <- readSource("Cao2024", subtype = "cao_carbonation_share", convert = FALSE)

  unit <- "ratio"
  description <- paste(
    "Share of CaO in End-Use Product available for carbonation.",
    "Data from Cao2024."
  )
  note <- "dimensions: (value)"

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
