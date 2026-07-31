#' Share of single-family homes in residential floor area.
#'
#' Floor area of single-family homes (RS) relative to the total residential floor
#' area (RS + RM), based on the Global Exposure Model (GEM).
#' @author Bennet Weiss
calcCeSingleFamilyHomeShare <- function() {
  floorArea <- calcOutput("CeFloorspaceGEM", subtype = "Function", aggregate = FALSE)
  res_floorArea <- dimSums(mselect(floorArea, Function = c("RS", "RM")), dim = 3)
  share <- collapseDim(mselect(floorArea, Function = "RS")) / res_floorArea

  # fill countries without residential floor area with their H12 region's share
  h12 <- toolGetMapping("h12.csv", type = "regional", where = "mappingfolder")
  regional_share <- toolAggregate(
    x = replace_non_finite(share, replace = 0),
    rel = h12,
    weight = res_floorArea,
    from = "CountryCode",
    to = "RegionCode"
  )
  region_fill <- toolAggregate(regional_share, h12, from = "RegionCode", to = "CountryCode")
  share[!is.finite(share)] <- region_fill[!is.finite(share)]

  weight <- res_floorArea
  unit <- "ratio"
  description <- paste(
    "Share of single-family homes (RS) in total residential (RS + RM) floor area.",
    "Data from Global Exposure Model (GEM), categories harmonized with RASMI."
  )
  note <- "dimensions: (Region,value)"

  output <- list(
    x = share,
    weight = weight,
    unit = unit,
    description = description,
    note = note,
    min = 0,
    max = 1
  )
  return(output)
}
