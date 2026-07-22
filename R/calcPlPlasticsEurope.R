#' This function returns plastics production data from Plastics Europe
#'
#' @author Leonie Schweiger
#'
calcPlPlasticsEurope <- function() {
  # ---------------------------------------------------------------------------
  # Read source data
  # ---------------------------------------------------------------------------
  x <- readSource("PlasticsEurope", subtype = "Production_region")

  # ---------------------------------------------------------------------------
  # Return packaged output
  # ---------------------------------------------------------------------------
  return(list(
    x = x,
    weight = NULL,
    unit = "Mt Plastic",
    description = "Production data from Plastics Europe"
  ))
}
