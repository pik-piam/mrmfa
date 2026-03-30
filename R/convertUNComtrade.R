#' Convert trade data from UN Comtrade.
#'
#' @author Bennet Weiss
#' @param x Magpie object
convertUNComtrade <- function(x) {
  # Manually add missing regions required for ANT disaggregation in toolISOhistorical
  missing_iso <- c("SXM", "BES")
  extra <- new.magpie(cells_and_regions = missing_iso,
                      years = getYears(x),
                      names = getNames(x),
                      fill = 0)
  x <- mbind(x, extra)

  # ZA1 is Southern African Customs Union which includes Botswana, Eswatini, Lesotho, Namibia, South Africa
  # Existed until 1999 (according to dataset).
  add_map <- list(
    c("ZA1", "BWA", "y1999"), # Botswana
    c("ZA1", "SWZ", "y1999"), # Eswatini
    c("ZA1", "LSO", "y1999"), # Lesotho
    c("ZA1", "NAM", "y1999"), # Namibia
    c("ZA1", "ZAF", "y1999")  # South Africa
  )
  x <- suppressWarnings(madrat::toolISOhistorical(x, additional_mapping = add_map))
  no_remove_warning <- c("S19") # other Asia, cannot be attributed properly
  x <- madrat::toolCountryFill(x, fill = NA, verbosity = 2, no_remove_warning = no_remove_warning)
  x <- x / 1000 # unit change: kg to t

  # Filter out wrong data
  x["MYS", 2008,] <- NA

  return(x)
}
