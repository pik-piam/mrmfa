#' Calculate share of CKD that goes to landfill.
#'
#' @author Bennet Weiss
calcCeCKDLandfillShare <- function() {
  x <- readSource("Cao2024", subtype = "CKD_landfill_share", convert = FALSE)

  unit <- "ratio"
  description <- paste(
    "Share of CKD that goes to landfill.",
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
