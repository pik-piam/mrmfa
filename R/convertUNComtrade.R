#' Convert trade data from UN Comtrade.
#'
#' @author Bennet Weiss
#' @param x Magpie object
convertUNComtrade <- function(x) {
  # ZA1 is Southern African Customs Union which includes Botswana, Eswatini, Lesotho, Namibia, South Africa
  # Existed until 1999 (according to dataset). For simplicity, map them to South Africa.
  # Automatic mapping of SCG fails probably because transition is not included in time horizon of x.
  add_map <- list(
    c("ZA1", "ZAF", "y1999"),
    c("SCG", "SRB", "y1999"),
    c("SCG", "MNE", "y1999")
  )
  x <- suppressWarnings(madrat::toolISOhistorical(x, additional_mapping = add_map))
  no_remove_warning <- c("S19") # other Asia, cannot be attributed properly
  x <- madrat::toolCountryFill(x, fill = NA, verbosity = 2, no_remove_warning = no_remove_warning)
  x <- x / 1000 # unit change: kg to t
  return(x)
}
