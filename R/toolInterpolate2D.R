#' @importFrom tidyr pivot_wider
#' Interpolate missing values in a 2D magpie object
#' @description 
#' This function interpolates missing values (NAs) in a 2D magpie 
#' object along the time dimension (years).
#' 
#' @param method 3 interpolation methods are supported: 'linear', 'spline', and 'constant' (last observation carried forward).
#' @author Merlin Jo Hosak
#' @export
toolInterpolate2D <- function(x, method='linear',...) {
  regions <- getItems(x, dim=1)
  # turn into data frame with same index
  df <- mtab(x)
  df <- tibble::column_to_rownames(df, 'Region')

  # transpose and use zoo methods to interpolate missing values
  df_transposed <- t(df)

  interpolation_methods <- list(
    'linear' = zoo::na.approx,
    'spline' = zoo::na.spline,
    'constant' = zoo::na.locf
  )

  if (!method %in% names(interpolation_methods)) {
    stop("Unknown method for interpolation. Use 'linear', 'spline' or 'constant'.")
  }

  df_transposed_interpolated <-
    interpolation_methods[[method]](df_transposed, na.rm=FALSE, rule=1)#, ...)

  df_interpolated <- t(df_transposed_interpolated)

  df[,] <- df_interpolated[,]  # get the same dimensions as the original data frame

  # turn back to magpie object
  df <- tibble::rownames_to_column(df, 'Region')
  y <- as.magpie(df,spatial='Region')

  return(y)
}

mtab <- function(x){
  # function to convert a 2D-magpie object in a table data frame format
  # x: magpie object
  # returns: data.frame with the same content as the magpie object
  # but in a more readable format
  df <- as.data.frame(x)
  df <- df[,c('Region', 'Year', 'Value')]
  df <- pivot_wider(df,names_from=.data$Year,values_from=.data$Value)
  return(df)
}
