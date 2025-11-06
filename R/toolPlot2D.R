#' Plot a 2D Magpie Object
#'
#' @description
#' This function visualizes a 2-dimensional Magpie object (regions × time)
#' as a line plot with time on the x-axis, values on the y-axis, and one line
#' per region.
#' 
#' TODO Finalize
#'
#' @param x A 2D Magpie object (region × time).
#' @param title Optional plot title.
#' @param ylab Optional y-axis label.
#' @param xlab Optional x-axis label.
#' @return A ggplot object showing the time series for all regions.
#'
#' @author Merlin Jo Hosak
#' @export
toolPlot2D <- function(x, title = '2D Plot') {
  
  # --- Checks ---
  if (!magclass::is.magpie(x)) {
    stop("Input x must be a Magpie object.")
  }
  if (magclass::ndata(x) != 1) {
    stop("Input x must be a 2D Magpie object (regions × time).")
  }
  
  # --- Convert Magpie to Data Frame ---
  df <- as.data.frame(x)
  # Magclass data frames have columns: region, year, value
  colnames(df) <- c("Region", "Year", "Value")
  
  # --- Plot ---
  p <- ggplot2::ggplot(df, ggplot2::aes(x = Year, y = Value, color = Region)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(title = title, x = 'Year', y = 'Value', color = "Region") +
    ggplot2::theme(
      legend.position = "right",
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
    )
  
  return(p)
}
