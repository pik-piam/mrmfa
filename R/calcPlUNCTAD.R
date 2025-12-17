#' Calculate UNCTAD Plastic Trade Flows
#'
#' Read and aggregate UNCTAD plastic trade data (imports/exports) for multiple plastic categories
#' at country level.
#'
#' @param subtype Character; scenario to read. Options:
#'   \itemize{
#'     \item Final        - Imports/exports of final plastics by country
#'     \item Waste        - Plastic waste imports/exports by country
#'     \item Primary      - Imports/exports of primary plastics by country
#'     \item Intermediate - Imports/exports of intermediate forms of plastic by country
#'     \item Manufactured - Imports/exports of intermediate manufactured plastic goods by country
#'   }
#'
#' @author Qianzhi Zhang
calcPlUNCTAD <- function(subtype) {
  # ---------------------------------------------------------------------------
  # Setup: files, mappings, and weights
  # ---------------------------------------------------------------------------
  data <- readSource("UNCTAD", convert = TRUE)

  # ---------------------------------------------------------------------------
  # Helper: build flows for given product
  # ---------------------------------------------------------------------------
  build_country_flow <- function(prod_label, data2_tag) {
    x <- data[, , grepl(prod_label, getItems(data, dim = 3))]
    getItems(x, dim = 3.2) <- data2_tag
    return(x / 1000) # thousand tons to Mt
  }

  # ---------------------------------------------------------------------------
  # Dispatch country-level subtypes
  # ---------------------------------------------------------------------------
  if (subtype == "Final") {
    return(list(
      x           = build_country_flow("Final manufactured plastics goods", "Final Plastic"),
      weight      = NULL,
      unit        = "Mt",
      description = "Country-level imports/exports of final plastics"
    ))
  }
  if (subtype == "Primary") {
    return(list(
      x           = build_country_flow("Plastics in primary forms", "Primary Plastic"),
      weight      = NULL,
      unit        = "Mt",
      description = "Country-level imports/exports of primary plastics"
    ))
  }
  if (subtype == "Intermediate") {
    return(list(
      x           = build_country_flow("Intermediate forms of plastic", "Intermediate Plastic"),
      weight      = NULL,
      unit        = "Mt",
      description = "Country-level imports/exports of intermediate plastic forms"
    ))
  }
  if (subtype == "Manufactured") {
    return(list(
      x           = build_country_flow("Intermediate manufactured plastic goods", "Manufactured Plastic"),
      weight      = NULL,
      unit        = "Mt",
      description = "Country-level imports/exports of manufactured plastic goods"
    ))
  }
  if (subtype == "Waste") {
    x <- build_country_flow("Plastic waste", "Plastic waste")
    return(list(
      x           = x,
      weight      = NULL,
      unit        = "Mt",
      description = "Country-level plastic waste flows"
    ))
  }

  # ---------------------------------------------------------------------------
  # Error handling for unknown subtype
  # ---------------------------------------------------------------------------
  stop("Unknown subtype: ", subtype)
}
