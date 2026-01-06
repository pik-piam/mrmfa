#' Calculates relative floor area of SF, MF and NR buildings by structure type.
#'
#' @author Bennet Weiss
calcCeBuildingSplit <- function() {
  floorArea <- calcOutput("CeBuildingFloorArea", aggregate = FALSE)
  floorArea_byStockType <- calcOutput("CeBuildingFloorArea", subtype = "Stock_Type", aggregate = FALSE)
  relFloorArea <- floorArea / floorArea_byStockType

  # output
  relFloorArea[is.na(relFloorArea)] <- 0
  weight <- floorArea_byStockType # use normalizing floor area as weight
  unit <- "ratio"
  description <- paste(
    "Relative floor area of buildings by stock type, function and structure.",
    "Calculated as (floor area of RS/RM/Com)/(total floor area of RES/Com in the same country).",
    "Data from Global Exposure Model (GEM), categories harmonized with RASMI.",
    "Yepes-Estrada, C., Calderon, A., Costa, C., Crowley, H., Dabbeek, J., Hoyos, M., Martins, L., Paul, N., Rao, A., Silva, V. (2023).",
    "Global Building Exposure Model for Earthquake Risk Assessment. Earthquake Spectra. doi:10.1177/87552930231194048."
  )
  note <- "dimensions: (Region,Stock Type,Structure,Function,value)"
  output <- list(
    x = relFloorArea,
    weight = weight,
    unit = unit,
    description = description,
    note = note
  )
}
