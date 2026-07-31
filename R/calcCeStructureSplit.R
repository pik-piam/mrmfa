#' Calculates relative floor area of buildings by structure within each good.
#' Based on splits in Global Exposure Model (GEM).
#'
#' @author Bennet Weiss
calcCeStructureSplit <- function() {
  floorArea_categorized <- calcOutput("CeFloorspaceGEM", subtype = c("Function", "Structure"), aggregate = FALSE)
  floorArea_byFunction <- calcOutput("CeFloorspaceGEM", subtype = "Function", aggregate = FALSE)

  relFloorArea <- floorArea_categorized / floorArea_byFunction

  # fill countries lacking floor area for a good with their H12 region's split
  h12 <- toolGetMapping("h12.csv", type = "regional", where = "mappingfolder")
  regional_relFloorArea <- toolAggregate(
    x = replace_non_finite(relFloorArea, replace = 0),
    rel = h12,
    weight = floorArea_byFunction,
    from = "CountryCode",
    to = "RegionCode"
  )
  region_fill <- toolAggregate(regional_relFloorArea, h12, from = "RegionCode", to = "CountryCode")
  relFloorArea[!is.finite(relFloorArea)] <- region_fill[!is.finite(relFloorArea)]

  # output
  relFloorArea <- replace_non_finite(relFloorArea, replace = 0)
  # Ind and Civ lie entirely in the unspecified structure U
  relFloorArea <- add_columns(relFloorArea, addnm = c("Ind.U", "Civ.U"), dim = 3, fill = 1)
  getSets(relFloorArea)["d3.1"] <- "Good"
  weight <- floorArea_byFunction # use normalizing floor area as weight
  weight <- add_columns(weight, addnm = c("Ind", "Civ"), dim = 3, fill = 1)
  unit <- "ratio"
  description <- paste0(
    "Relative floor area of buildings by Good and Structure. ",
    "Calculated as (floor area of Structure)/(total floor area of Good in the same country). ",
    "Data from Global Exposure Model (GEM), categories harmonized with RASMI. ",
    "Yepes-Estrada, C., Calderon, A., Costa, C., Crowley, H., Dabbeek, ",
    "J., Hoyos, M., Martins, L., Paul, N., Rao, A., Silva, V. (2023). ",
    "Global Building Exposure Model for Earthquake Risk Assessment. Earthquake Spectra. doi:10.1177/87552930231194048."
  )
  note <- "dimensions: (Region,Good,Structure,value)"
  output <- list(
    x = complete_magpie(relFloorArea, fill = 0),
    weight = complete_magpie(weight, fill = 0),
    unit = unit,
    description = description,
    note = note
  )
  return(output)
}
