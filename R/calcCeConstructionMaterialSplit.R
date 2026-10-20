#' Calculate the split of cement into construction materials.
#'
#' @author Bennet Weiss
calcCeConstructionMaterialSplit <- function() {
  x <- readSource("Cao2024", subtype = "material_split")
  x <- toolCeDistributionMean(x, "Weibull")
  x <- x / dimSums(x, dim = "Construction Material")

  weight <- toolCeCumulativeCementProduction(x)

  unit <- "ratio"
  description <- paste(
    "Split cement into construction materials.",
    "Data from Cao2024."
  )
  note <- "dimensions: (Region,Construction Material,value)"

  output <- list(
    x = x,
    weight = weight,
    unit = unit,
    description = description,
    note = note
  )
  return(output)
}
