#' Interpolate missing values in a 2D magpie object
#' @description
#' This function interpolates missing values (NAs) in a 2D magpie
#' object along the time dimension (years).
#'
#' @param x A 2D magpie object (regions x years) with missing values to interpolate
#' @param method 3 interpolation methods are supported: 'linear' (default), 'spline', and 'constant' (last observation carried forward).
#' @param ... Additional arguments passed to interpolation functions
#' @return A 2D magpie object with interpolated values
#' @author Merlin Jo Hosak
toolInterpolate2D <- function(x, method = "linear", ...) {

  regions <- getItems(x, dim = 1)

  # turn into data frame with same index
  df <- magclass::as.data.frame(x)
  df <- df[, c("Region", "Year", "Value")]
  df <- tidyr::pivot_wider(df, names_from = .data$Year, values_from = .data$Value)
  df <- tibble::column_to_rownames(df, "Region")

  # transpose and use zoo methods to interpolate missing values
  df_transposed <- t(df)

  interpolation_methods <- list(
    "linear" = zoo::na.approx,
    "spline" = zoo::na.spline,
    "constant" = zoo::na.locf
  )

  if (!method %in% names(interpolation_methods)) {
    stop("Unknown method for interpolation. Use 'linear', 'spline' or 'constant'.")
  }

  df_transposed_interpolated <-
    interpolation_methods[[method]](df_transposed, na.rm = FALSE, rule = 1) # , ...)

  df_interpolated <- t(df_transposed_interpolated)

  df[, ] <- df_interpolated[, ] # get the same dimensions as the original data frame

  # turn back to magpie object
  df <- tibble::rownames_to_column(df, "Region")
  y <- as.magpie(df, spatial = "Region")
  getNames(y) <- NULL

  return(y)
}

