#' Convert a transcribed table of cement end-use shares into a magpie object
#'
#' Maps the categories of a source to the cement end uses and sums them per
#' region and year. Categories mapped to \code{NA} are dropped (e.g. "unknown"
#' or "others"); the remaining shares are normalised in the convert functions
#' (see \code{\link{toolCeNormalizeEndUseSplit}}).
#'
#' @author Bennet Weiss
#' @param data data.frame with the columns \code{region}, \code{period}, \code{category} and \code{value}.
#' @param categoryMapping Named character vector mapping each source category to an end use
#'   ("Residential", "Non-Residential", "Civil Engineering", "Commercial" or "Industrial"),
#'   or to \code{NA} to drop it. Every category in \code{data} must be listed.
#' @return magpie object with the sets region, year and End Use.
toolCeEndUseTable <- function(data, categoryMapping) {
  unmapped <- setdiff(unique(data$category), names(categoryMapping))
  if (length(unmapped) > 0) {
    stop("Unmapped end-use categories: ", paste(unmapped, collapse = ", "))
  }
  data$enduse <- unname(categoryMapping[data$category])
  data <- data[!is.na(data$enduse), ]
  data <- stats::aggregate(value ~ region + period + enduse, data = data, FUN = sum)
  x <- magclass::as.magpie(data[, c("region", "period", "enduse", "value")],
                           spatial = 1, temporal = 2, datacol = 4)
  getSets(x) <- c("region", "year", "End Use")
  return(x)
}

#' Normalise cement end-use shares to sum to one
#'
#' Divides all items by the sum of the three main end uses Residential, Non-Residential
#' and Civil Engineering. Sub-items of Non-Residential (Commercial, Industrial), if present,
#' are scaled with the same factor, so that they keep summing to Non-Residential.
#'
#' @author Bennet Weiss
#' @param x magpie object with an End Use dimension containing at least the three main end uses.
#' @return magpie object of the same dimensions, normalised.
toolCeNormalizeEndUseSplit <- function(x) {
  mainEndUses <- c("Residential", "Non-Residential", "Civil Engineering")
  missing <- setdiff(mainEndUses, getNames(x))
  if (length(missing) > 0) stop("Missing main end uses: ", paste(missing, collapse = ", "))
  total <- dimSums(x[, , mainEndUses], dim = 3, na.rm = FALSE)
  return(x / total)
}
