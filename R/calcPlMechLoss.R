#' Set Country-Level Mechanical Recycling Loss 
#'
#' @author Qianzhi Zhang
#'
calcPlMechLoss <- function() {

  x <- new.magpie(fill = 0.05)

  description <- paste0(
    "Uncontrolled loss rate of mechanical recycling based on Brown et al. 2023, ",
    "(https://doi.org/10.1016/j.hazadv.2023.100309)"
  )
  return(list(
    x           = x,
    weight      = NULL,
    unit        = "% Mechanical Recycling Loss",
    isocountries= FALSE,
    description = description,
    note        = "dimensions: (value)"
  ))
}
