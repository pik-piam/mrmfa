#' Convert trade data from Chatham House
#'
#' @author Bennet Weiss
#' @param x Magpie object
convertChathamHouse <- function(x) {
  x <- suppressWarnings(madrat::toolISOhistorical(x, overwrite = FALSE))
  x <- madrat::toolCountryFill(x, fill = NA, verbosity = 2)
  return(x)
}
