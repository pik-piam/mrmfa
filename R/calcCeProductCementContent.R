#' Calculate cement content of product applications.
#'
#' @author Bennet Weiss
calcCeProductCementContent <- function() {
  mortar_apps <- c("finishing", "masonry", "maintenance")

  concrete <- readSource("Cao2024", subtype = "concrete_cement_content", convert = FALSE)
  concrete <- toolCeDistributionMean(concrete, "Uniform")
  # Assumption: Cao2024 gives no cement content for mortar, the one of C15 concrete is used.
  mortar <- toolCeCopyItem(concrete[, , "C15"], mortar_apps, dim = "Product Application")
  x <- dimReduce(mbind(concrete, mortar))
  x <- x * 1e-3 # convert from kg to tonnes

  unit <- "tonnes per cubic meter (t/m3)"
  description <- paste(
    "Product cement content.",
    "Data from Cao2024."
  )
  note <- "dimensions: (Product Application,value)"

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
