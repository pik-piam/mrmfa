#' Calculates relative floor area of buildings by structure within each end use.
#' Based on splits in Global Exposure Model (GEM).
#'
#' @author Bennet Weiss
calcCeStructureSplit <- function() {
  floorArea_categorized <- calcOutput("CeFloorspaceGEM", subtype = c("Function", "Structure"), aggregate = FALSE)
  floorArea_byFunction <- calcOutput("CeFloorspaceGEM", subtype = "Function", aggregate = FALSE)

  relFloorArea <- floorArea_categorized / floorArea_byFunction

  # fill countries lacking floor area for an end use with their H12 region's split.
  # toolFillWithRegionAvg only accepts a single data dimension element, so loop over end uses and structure
  h12 <- toolGetMapping("h12.csv", type = "regional", where = "mappingfolder")
  relFloorArea <- replace_non_finite(relFloorArea, replace = NA)
  filled <- lapply(getItems(relFloorArea, dim = 3.1), function(endUse) {
    dataSub <- relFloorArea[, , endUse]
    weightSub <- collapseDim(floorArea_byFunction[, , endUse])
    # if one end use is NA, all structures belonging to it are NA, too. Replacing all of them respects norm.
    slices <- lapply(getItems(dataSub, dim = 3.2), function(structure) {
      toolFillWithRegionAvg(
        x = dataSub[, , structure],
        valueToReplace = NA,
        weight = weightSub,
        regionmapping = h12,
        verbose = FALSE
      )
    })
    mbind(slices)
  })
  relFloorArea <- mbind(filled)

  # output
  getSets(relFloorArea)["d3.1"] <- "End Use"
  weight <- floorArea_byFunction # use normalizing floor area as weight
  weight[weight == 0] <- 1e-9
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
    x = relFloorArea,
    weight = weight,
    unit = unit,
    description = description,
    note = note
  )
  return(output)
}
