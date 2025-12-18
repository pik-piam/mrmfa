#' Removes NA values from a Magpie object. Only works if all regions show the same temporal pattern of data scarcity.
#' @author Bennet Weiss
#' @param x Magpie object
toolRemoveNA <- function(x) {
  df <- as.data.frame(x, rev = 3)
  df_clean <- df[stats::complete.cases(df), ]
  df_clean <- stats::na.omit(df)
  x_clean <- as.magpie(df_clean)
  return(x_clean)
}

#' Create mask for countries that have less than n_valid values which are not NA.
#' @author Bennet Weiss
#' @param x Magpie object
#' @param n_valid Threshold minimum number of valid (not NA) values.
toolMaskNACountries <- function(x, n_valid = 5) {
  arr <- as.array(x)
  counts <- apply(!is.na(arr), 1, sum)
  mask <- names(counts[counts < n_valid])
  return(mask)
}
