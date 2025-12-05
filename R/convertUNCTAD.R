#' Convert UNCTAD
#'
#' Convert UNCTAD data to ISO country level.
#'
#' @param x MagPIE object containing UNCTAD data
#' @return MagPIE object of the UNCTAD data mapped to ISO countries
#' @author Leonie Schweiger
#' @examples
#' \dontrun{
#' a <- convertUNCTAD(x)
#' }
#'
#' @importFrom magclass where
#' @importFrom magpiesets findset
#'
convertUNCTAD <- function(x) {
  # Convert country names to ISO codes with a specific mapping.
  getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1),
    mapping = c(
      "Lao People's Dem_ Rep_" = "LAO",
      "Congo, Dem_ Rep_ of the" = "COD",
      "Netherlands (Kingdom of the)" = "NLD",
      "Venezuela (Bolivarian Rep_ of)" = "VEN",
      "Switzerland, Liechtenstein" = "CHE",
      "State of Palestine" = "PSE"
    )
  )
  # Exclude NA and fill missing country entries with 0.
  x <- x[!is.na(getItems(x, 1)), , ]
  x <- toolCountryFill(x, fill = 0)

  # ---------------------------------------------------------------------------
  # Return the Processed MagPIE Object
  # ---------------------------------------------------------------------------
  return(x)
}
