#' Convert UNCTAD
#'
#' Convert UNCTAD data to ISO country level.
#'
#' @param x MagPIE object containing UNCTAD data
#' @return MagPIE object of the UNCTAD data mapped to ISO countries
#' @author Leonie Schweiger
#' @examples
#' \dontrun{
#'   a <- convertUNCTAD(x)
#' }
#'
#' @importFrom magclass where
#' @importFrom magpiesets findset
#'
convertUNCTAD <- function(x) {

  # Convert country names to ISO codes with a specific mapping.
  getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1),
                                              mapping = c("Lao People's Dem_ Rep_"        = "LAO",
                                                          "Congo, Dem_ Rep_ of the"       = "COD",
                                                          "Netherlands (Kingdom of the)" = "NLD",
                                                          "Venezuela (Bolivarian Rep_ of)"= "VEN",
                                                          "Switzerland, Liechtenstein"    = "CHE",
                                                          "State of Palestine"            = "PSE"))
  # exclude NA regions
  x <- x[!is.na(getItems(x,1)), , ]

  # convert historical countries by using toolISOhistorical
  # get new countries that will be added to the dataset
  countries <- getItems(x, dim=1)
  new_countries <- read.csv2(system.file("extdata", "ISOhistorical.csv", package = "madrat"))
  new_countries <- new_countries[new_countries$fromISO %in% countries, "toISO"]
  missing_countries <- setdiff(new_countries, countries)
  # if missing country list is not empty extend x
  if (length(missing_countries) > 0) {
    missing_countries <- new.magpie(
      cells_and_regions = missing_countries,
      years = getItems(x, dim=2),
      names = getItems(x, dim = 3),
      fill = 0,
      sets = names(dimnames(x))
    )

    x <- mbind(x, missing_countries)
  }
  x <- toolISOhistorical(x, overwrite=TRUE)

  # Fill missing country entries with 0.
  x <- toolCountryFill(x, fill = 0)

  # ---------------------------------------------------------------------------
  # Return the Processed MagPIE Object
  # ---------------------------------------------------------------------------
  return(x)
}

