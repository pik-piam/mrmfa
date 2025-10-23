#' Removes NA values from a Magpie object. Only works if all regions show the same temporal pattern of data scarcity.
#' @author Bennet Weiss
#' @param x Magpie object
toolRemoveNA <- function(x) {
  df <- as.data.frame(x, rev = 3)
  df_clean <- df[stats::complete.cases(df), ]
  df_clean <- na.omit(df)
  x_clean <- as.magpie(df_clean)
  return(x_clean)
}

#' Create mask for countries that have >= n_noNA NA values.
#' @author Bennet Weiss
#' @param x Magpie object
#' @param n_noNA Minimum values not NA values of a country to be included in the mask
toolMaskNACountries <- function(x, n_noNA = 5) {
  arr <- as.array(x)
  counts <- apply(!is.na(arr), 1, sum)
  mask <- names(counts[counts < n_noNA])
  return(mask)
}
