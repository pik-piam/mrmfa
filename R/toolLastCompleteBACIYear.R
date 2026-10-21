#' Get the last complete year of BACI trade data.
#'
#' The most recent BACI year is usually incomplete (e.g. clinker exports for Vietnam, UAE
#' and imports for Bangladesh are underestimated), so this function can be used to filter it out.
#'
#' @param years Integer vector of the years contained in the BACI data.
#' @return Integer, the last complete year.
#' @author Bennet Weiss
toolLastCompleteBACIYear <- function(years) {
  return(max(as.integer(years)) - 1L)
}
