#' Calculates relative floor area of buildings by subtype.
#' Based on splits in Global Exposure Model (GEM)
#'
#' @author Bennet Weiss
#' @param subtype Split category. Can be "Function" or "Structure"
calcCeBuildingsSplit <- function(subtype) {
  if (subtype == "Function") {
    compare_type <- "Stock_Type"
    # GEM only covers buildings (Res, Com); other stock types get their full share
    # in the placeholder function "N/A"
    placeholder_names <- c("Ind.N/A", "Civ.N/A")
    placeholder_weight_names <- c("Ind", "Civ")
  } else if (subtype == "Structure") {
    compare_type <- "Function"
    # the placeholder function "N/A" maps fully to the placeholder structure "N/A"
    placeholder_names <- "N/A.N/A"
    placeholder_weight_names <- "N/A"
  } else {
    stop("Invalid subtype: must be 'Function' or 'Structure'")
  }

  floorArea_categorized <- calcOutput("CeFloorspaceGEM", subtype = c(compare_type, subtype), aggregate = FALSE)
  floorArea_byCompareType <- calcOutput("CeFloorspaceGEM", subtype = compare_type, aggregate = FALSE)

  relFloorArea <- floorArea_categorized / floorArea_byCompareType

  # output
  compare_type <- gsub("_", " ", compare_type)
  relFloorArea <- replace_non_finite(relFloorArea, replace = 0)
  relFloorArea <- toolAddPlaceholder(relFloorArea, placeholder_names, fill = 1)
  weight <- floorArea_byCompareType # use normalizing floor area as weight
  weight <- toolAddPlaceholder(weight, placeholder_weight_names, fill = 1)
  unit <- "ratio"
  description <- paste0(
    "Relative floor area of buildings by ", compare_type, " and ", subtype, ".",
    "Calculated as (floor area of ", subtype, ")/(total floor area of ", compare_type, " in the same country). ",
    "Data from Global Exposure Model (GEM), categories harmonized with RASMI. ",
    "Yepes-Estrada, C., Calderon, A., Costa, C., Crowley, H., Dabbeek, ",
    "J., Hoyos, M., Martins, L., Paul, N., Rao, A., Silva, V. (2023). ",
    "Global Building Exposure Model for Earthquake Risk Assessment. Earthquake Spectra. doi:10.1177/87552930231194048."
  )
  note <- paste0("dimensions: (Region,", compare_type, ",", subtype, ",value)")
  output <- list(
    x = relFloorArea,
    weight = weight,
    unit = unit,
    description = description,
    note = note
  )
  return(output)
}
