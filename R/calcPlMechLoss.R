#' Calculate Country-Level Mechanical Recycling Loss Trajectories
#'
#' Generate time series of mechanical recycling loss trajectories by sector and region,
#' then aggregate to countries for 1950-2100.
#'
#' @author Qianzhi Zhang
#'
calcPlMechLoss <- function() {

  sector_map <- toolGetMapping(
    "structuremappingPlasticManu.csv",
    type = "sectoral", where = "mrmfa"
  )

  targets <- setdiff(unique(sector_map$Target), "Total")

  x <- new.magpie(
    cells_and_regions = madrat::getISOlist(),
    years = 1950:2100,
    names = targets,
    fill = 0.05
  )

  weight <- x
  weight[, ] <- 1

  return(list(
    x           = x,
    weight      = weight,
    unit        = "% Mechanical Recycling Loss",
    description = "Uncontrolled loss rate of mechanical recycling based on Brown et al. 2023 (https://doi.org/10.1016/j.hazadv.2023.100309)",
    note        = "dimensions: (Time,Region,Material,value)"
  ))
}
