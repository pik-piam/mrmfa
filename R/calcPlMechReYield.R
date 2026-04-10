#' Calculate Country-Level Mechanical Recycling Yield Trajectories
#'
#' Generate time series of mechanical recycling yield (efficiency) trajectories by sector and region,
#' then aggregate to countries for 1950-2100.
#'
#' @author Qianzhi Zhang
#'
calcPlMechReYield <- function() {

  sector_map <- toolGetMapping(
    "structuremappingPlasticManu.csv",
    type = "sectoral", where = "mrmfa"
  )
  targets <- setdiff(unique(sector_map$Target), "Total")

  x <- new.magpie(
    cells_and_regions = madrat::getISOlist(),
    years = 1950:2100,
    names = targets,
    fill = 0.79
  )

  weight <- x
  weight[, ] <- 1

  return(list(
    x           = x,
    weight      = weight,
    unit        = "% Mechanical recycling efficiency",
    description = "Mechanical recycling yield trajectories aggregated to country level for 1950-2100.",
    note        = "dimensions: (Time,Region,Material,value)"
  ))
}
