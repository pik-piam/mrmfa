#' Calculates relative floor area of buildings by structure within each end use.
#' Based on splits in Global Exposure Model (GEM).
#'
#' @author Bennet Weiss
calcCeStructureSplit <- function() {
  floorArea_categorized <- calcOutput("CeFloorspaceGEM", subtype = c("Function", "Structure"), aggregate = FALSE)
  floorArea_byFunction <- calcOutput("CeFloorspaceGEM", subtype = "Function", aggregate = FALSE)

  relFloorArea <- floorArea_categorized / floorArea_byFunction

  # fill countries lacking floor area for an end use with their H12 region's split
  h12 <- toolGetMapping("h12.csv", type = "regional", where = "mappingfolder")
  relFloorArea <- replace_non_finite(relFloorArea, replace = NA)
  relFloorArea <- toolFillWithRegionAvg(
    x = relFloorArea,
    valueToReplace = NA,
    weight = floorArea_byFunction,
    regionmapping = h12
  )

  # output
  getSets(relFloorArea)["d3.1"] <- "End Use"
  weight <- floorArea_byFunction # use normalizing floor area as weight
  unit <- "ratio"
  description <- paste0(
    "Relative floor area of buildings by End Use and Structure. ",
    "Calculated as (floor area of Structure)/(total floor area of End Use in the same country). ",
    "Data from Global Exposure Model (GEM), categories harmonized with RASMI. ",
    "Yepes-Estrada, C., Calderon, A., Costa, C., Crowley, H., Dabbeek, ",
    "J., Hoyos, M., Martins, L., Paul, N., Rao, A., Silva, V. (2023). ",
    "Global Building Exposure Model for Earthquake Risk Assessment. Earthquake Spectra. doi:10.1177/87552930231194048."
  )
  note <- "dimensions: (Region,Bottom-up End Use,Structure,value)"
  output <- list(
    x = complete_magpie(relFloorArea, fill = 0),
    weight = complete_magpie(weight, fill = 0),
    unit = unit,
    description = description,
    note = note
  )
  return(output)
}
