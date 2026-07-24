#' Set Country-Level Mechanical Recycling Yield
#'
#' @author Qianzhi Zhang
#'
calcPlMechReYield <- function() {

  x <- new.magpie(fill = 0.79)

  return(list(
    x           = x,
    weight      = NULL,
    unit        = "% Mechanical recycling efficiency",
    isocountries= FALSE,
    description = "Mechanical recycling yield trajectories aggregated to country level for 1950-2100.",
    note        = "dimensions: (value)"
  ))
}
