#' Calculate Floor Area from CeGHS-OBAT Data.
#' The data only provides total footprint of buildings and their average height.
#' Floor height data is approximated from EUBUCCO to convert to floor area.
#'
#' @param floor_height Scalar providing the manual floor height for calculation.
#' Defaults to NULL, in which case the average floor height is inferred from EUBUCCO data.
#' @author Bennet Weiss
calcCeFloorspaceGHSOBAT <- function(floor_height = NULL) {
  # 0. get building volume from GHS-OBAT
  buildings_footprint <- readSource("GHSOBAT", subtype = "surface") * 1e4 # from hectare to m2
  buildings_height <- readSource("GHSOBAT", subtype = "height") # in m
  buildings_volume <- buildings_footprint * buildings_height

  # 1. Remove industrial buildings from nonres to get commercial buildings
  # Country-specific industry share comes from GEM
  GEM_floor_area <- calcOutput("CeFloorspaceGEM", subtype = "Stock_Type",
                               remove_ind = FALSE, aggregate = FALSE)
  com_share <- GEM_floor_area[,,"Com"] / (GEM_floor_area[,, "Com"] + GEM_floor_area[,, "Ind"])
  com_share <- mean(com_share, na.rm = TRUE) # global average if NA TODO: weighted averages
  buildings_volume[,, "non_residential"] <- buildings_volume[,, "non_residential"] * com_share

  # 2. Rename variables
  getItems(buildings_volume, dim = 3) <- c("Res", "Com")

  # 3. Calculate average floor height from EUBUCCO data if not provided
  if (is.null(floor_height)){
    eubucco_floorspace <- readSource("EUBUCCO")
    eubucco_floorspace_eu <- dimSums(eubucco_floorspace, dim=c(1,3), na.rm = TRUE)
    eubucco_mask <- !is.na(eubucco_floorspace)
    buildings_volume_eu <- buildings_volume * eubucco_mask
    buildings_volume_eu <- dimSums(buildings_volume_eu, dim=c(1,3))

    floor_height <- buildings_volume_eu / eubucco_floorspace_eu
  }

  # 4. Calculate floor area
  floor_area <- buildings_volume / floor_height

  # 5. Output
  description <- paste(
    "Floor area calculated from GHS-OBAT."
  )
  note <- "dimensions: (Region,Stock Type,value)"
  output <- list(x = floor_area, weight = NULL, unit = "m2", description = description, note = note)
  return(output)
}
