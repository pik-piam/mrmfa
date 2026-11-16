#' Split of residential floor area into single- and multi-family homes.
#'
#' Two-item split (sums to 1): floor area of single-family homes relative to the
#' total residential floor area, based on the Global Exposure Model (GEM).
#' Storeys and dwellings per building decide the split (see toolSingleFamilyShare).
#' Both types get at least 2% of the floor area, because the GEM data show no multi family homes
#' in some countries (e.g. Costa Rica).
#' @author Bennet Weiss
calcCeResidentialTypeSplit <- function() {
  floorArea <- calcOutput("CeFloorspaceGEM", subtype = "Function", aggregate = FALSE)
  res_floorArea <- dimSums(mselect(floorArea, Function = c("Residential Single-Family", "Residential Multi-Family")), dim = 3)
  share <- collapseDim(mselect(floorArea, Function = "Residential Single-Family")) / res_floorArea

  # fill countries without residential floor area with their H12 region's share
  h12 <- toolGetMapping("h12.csv", type = "regional", where = "mappingfolder")
  share <- replace_non_finite(share, replace = NA)
  share <- toolFillWithRegionAvg(
    x = share,
    valueToReplace = NA,
    weight = res_floorArea,
    regionmapping = h12,
    verbose = FALSE
  )

  # minimum share of both types: zero shares are likely GEM artefacts
  minShare <- 0.02
  share[share < minShare] <- minShare
  share[share > 1 - minShare] <- 1 - minShare

  x_rs <- addDim(share, dim = 3.1, dimName = "Residential Type", item = "Residential Single-Family")
  x_rm <- addDim(1 - share, dim = 3.1, dimName = "Residential Type", item = "Residential Multi-Family")
  x <- mbind(x_rs, x_rm)
  weight <- magpie_expand(x = res_floorArea, ref = x)
  weight[weight == 0] <- 1e-9

  unit <- "ratio"
  description <- paste(
    "Split of residential floor area into single- and multi-family homes.",
    "Data from Global Exposure Model (GEM), categories harmonized with RASMI.",
    "Split by storeys and dwellings per building."
  )
  note <- "dimensions: (Region,Residential Type,value)"

  output <- list(
    x = x,
    weight = weight,
    unit = unit,
    description = description,
    note = note,
    min = 0,
    max = 1
  )
  return(output)
}
