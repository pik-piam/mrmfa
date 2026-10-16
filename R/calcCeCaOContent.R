#' Calculate share of CaO in cement or clinker.
#'
#' @param subtype Material of which CaO content is of interest. May be "clinker" or "CKD"
#' @author Bennet Weiss
calcCeCaOContent <- function(subtype) {
  if (subtype == "CKD") {
    x <- readSource("Cao2024", subtype = "CKD_cao_content", convert = FALSE)
    x <- toolCeDistributionMean(x, "Normal")
  } else if (subtype == "clinker") {
    x <- readSource("Cao2024", subtype = "clinker_cao_content", convert = FALSE)
    x <- toolCeDistributionMean(x, "Triangular")
  } else {
    stop(paste("Subtype ", subtype, " not implemented."))
  }
  x <- dimReduce(x)

  unit <- "ratio"
  description <- paste(
    "Share of CaO in ", subtype, ".",
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
