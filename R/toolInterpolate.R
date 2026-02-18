#' Interpolate missing values in a magpie object.
#'
#' Interpolates missing values (NAs) in a magpie object along the time dimension (years), or to given years.
#'
#' @param x A magpie object (2D or 3D) with missing values to interpolate.
#' @param years Optional vector of years to interpolate to. If NULL, uses years in x.
#' @param type Interpolation method: one of 'linear', 'spline', 'monotone', or 'constant'. Default is 'linear'.
#' @param extrapolate Logical. Whether to allow extrapolation outside the range of available data. Default is FALSE.
#' @param ... Additional arguments passed to interpolation functions.
#' @return A magpie object with interpolated values for the specified years and all data slices.
#' @author Bennet Weiss
toolInterpolate <- function(x, years = NULL, type = "linear", extrapolate = FALSE, ...) {
  # Extract all names from the 3rd dimension
  data_names <- getNames(x)

  # Bypass loop if there is no 3rd dimension
  if (is.null(data_names)) {
    return(.toolInterpolateSlice(x, years = years, type = type, extrapolate = extrapolate, ...))
  }

  x_interpolated <- NULL
  # Loop through each element in the data dimension
  for (dname in data_names) {
    # Extract the 2D slice for this specific data name.
    x_slice <- x[, , dname]

    # Run your existing 2D interpolation on the slice
    interpolated_slice <- .toolInterpolateSlice(x_slice, years = years, type = type, extrapolate = extrapolate, ...)

    # save the interpolated data
    if (is.null(x_interpolated)) {
      x_interpolated <- interpolated_slice
    } else {
      x_interpolated <- magclass::mbind(x_interpolated, interpolated_slice)
    }
  }

  return(x_interpolated)
}

#' Interpolate a 2D magpie object to new years (internal)
#'
#' Optionally expands a 2D magpie object to new years, then interpolates missing values using the specified method.
#'
#' @param x A 2D magpie object (regions x years) with possible missing values.
#' @param years Optional vector of years to interpolate to. If NULL, uses years in x.
#' @param type Interpolation method: one of 'linear', 'spline', 'monotone', or 'constant'.
#' @param extrapolate Logical. Whether to allow extrapolation outside the range of available data.
#' @param ... Additional arguments passed to interpolation functions.
#' @return A 2D magpie object with interpolated values for the specified years.
#' @author Bennet Weiss
.toolInterpolateSlice <- function(x, years = NULL, type, extrapolate, ...) {
  # If years are provided, create a new magpie object with those years
  if (!is.null(years)) {
    # Create empty magclass and fill in existing data
    x_new <- new.magpie(
      cells_and_regions = getItems(x, dim = 1),
      years = years,
      names = getNames(x),
      fill = NA
    )
    x_new[, getYears(x), ] <- x
  } else {
    x_new <- x
  }

  # Interpolate
  x_interpolated <- .toolInterpolateSliceNa(x_new, type = type, extrapolate = extrapolate, ...)

  return(x_interpolated)
}

#' Interpolate missing values in a 2D magpie object (internal)
#'
#' Interpolates missing values (NAs) in a 2D magpie object (regions x years)
#' along the time dimension (years) using the specified method.
#'
#' @param x A 2D magpie object (regions x years) with missing values to interpolate.
#' @param type Interpolation method:
#' one of 'linear', 'spline', 'monotone' (monotone cubic spline), or 'constant' (last observation carried forward).
#' @param extrapolate Logical. Whether to allow extrapolation outside the range of available data.
#' If FALSE, values outside the data range remain NA.
#' @param ... Additional arguments passed to interpolation functions.
#' @return A 2D magpie object with interpolated values.
#' @author Merlin Jo Hosak, Bennet Weiss
.toolInterpolateSliceNa <- function(x, type, extrapolate, ...) {
  # turn into data frame with same index
  df <- magclass::as.data.frame(x)
  df <- df[, c("Region", "Year", "Value")]
  df <- tidyr::pivot_wider(df, names_from = .data$Year, values_from = .data$Value)
  df <- tibble::column_to_rownames(df, "Region")

  # transpose and use zoo methods to interpolate missing values
  df_transposed <- t(df)

  interpolation_methods <- list(
    "linear" = function(...) zoo::na.approx(..., rule = 2),
    "constant" = function(...) zoo::na.locf(..., rule = 2),
    "spline" = zoo::na.spline,
    "monotone" = function(...) zoo::na.spline(..., method = "monoH.FC")
  )

  if (!type %in% names(interpolation_methods)) {
    stop("Unknown method for interpolation. Use 'linear', 'spline', 'monotone', or 'constant'.")
  }

  # interpolate
  df_transposed_interpolated <-
    interpolation_methods[[type]](df_transposed, na.rm = FALSE, ...)

  # Remove interpolation outside of data range if extrapolation is FALSE
  remove_extrapolation <- function(orig_mat, interp_mat) {
    # Generate logical matrix identifying leading/trailing NAs in the original data
    mask <- apply(orig_mat, 2, function(v) {
      cumsum(!is.na(v)) == 0 | rev(cumsum(!is.na(rev(v)))) == 0
    })

    # Overwrite extrapolated cells with NA
    interp_mat[mask] <- NA
    return(interp_mat)
  }
  if (!extrapolate) {
    df_transposed_interpolated <- remove_extrapolation(
      orig_mat = df_transposed,
      interp_mat = df_transposed_interpolated
    )
  }

  # some data wrangling
  df_interpolated <- t(df_transposed_interpolated)
  df[, ] <- df_interpolated[, ] # get the same dimensions as the original data frame

  # turn back to magpie object
  df <- tibble::rownames_to_column(df, "Region")
  x_interpolated <- as.magpie(df, spatial = "Region")
  getNames(x_interpolated) <- getNames(x)
  getSets(x_interpolated) <- getSets(x)

  return(x_interpolated)
}
