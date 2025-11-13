#' Calculates a factor to translate energy-related floor area to material-related floor area.
#' Related to from net to gross floor area.
#'
#' @author Bennet Weiss
calcCeBuildingFloorAreaCalibration <- function() {

  # data for 2020
  # TODO check if 2020 data is scenario agnostic (as it should be)
  edgeb_floor_area <- calcOutput(
    type = "Floorspace",
    regionmapping = "regionmapping_ISO_2_ISO.csv",
    scenario = "SSP2"
  )[,2020]

  # data for 2020
  eubucco_floor_area <- readSource("EUBUCCO")

  # open how this can be used
  # data for 2021
  gem_floor_area <- calcOutput(
    type = "CeBuildingFloorArea",
    regionmapping = "regionmapping_ISO_2_ISO.csv",
    subtype = "Stock_Type"
  )
}
