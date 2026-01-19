#' Calculate Floor Area from CeGHS-OBAT Data
#'
#' @author Bennet Weiss
calcCeFloorspaceGHSOBAT <- function() {
  # 0. get building volume from GHS-OBAT
  buildings_surface <- readSource("GHSOBAT", subtype = "surface") * 1e4 # from hectare to m2
  buildings_height <- readSource("GHSOBAT", subtype = "height") # in m
  buildings_volume <- buildings_surface * buildings_height # in m3

  # 1. Remove industrial buildings from nonres to get commercial buildings
  # Country-specific industry share comes from GEM
  GEM_floor_area <- calcOutput("CeFloorspaceGEM", subtype = "Stock_Type",
                               remove_ind = FALSE, aggregate = FALSE)
  com_share <- GEM_floor_area[,,"Com"] / (GEM_floor_area[,, "Com"] + GEM_floor_area[,, "Ind"])
  com_share <- mean(com_share, na.rm = TRUE) # global average if NA TODO: weighted averages
  buildings_volume[,, "non_residential"] <- buildings_volume[,, "non_residential"] * com_share

  # 2. TODO Get floor height from EUBUCCO - one value for all EU data
  # TODO see if it makes sense to differentiate com/res.
  # eubucco_floor_area <- readSource("EUBUCCO")
  # TODO sum EU eubucco/GHS-OBAT
  # floor_height <- eubucco_floor_area_eu_total / buildings_surface_eu_total # in m
  floor_height <- 3

  # 3. Calculate floor area
  floor_area <- buildings_volume / floor_height

  # 4. Rename
  getItems(floor_area, dim = 3) <- c("Res", "Com")

  # 5. Output
  description <- paste(
    "Floor area calculated from GHS-OBAT."
  )
  note <- "dimensions: (Region,Stock Type,value)"
  output <- list(x = floor_area, weight = NULL, unit = "m2", description = description, note = note)
  return(output)
}
