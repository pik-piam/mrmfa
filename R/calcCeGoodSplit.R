#' Calculates how cement consumption is split into goods:
#' RS (single-family residential), RM (multi-family residential), Com, Ind and Civ
#'
#' The sectoral split (Res/Com/Ind/Civ) is based on Xi 2016. The residential share is
#' further split into RS and RM using the single-family home share of floor area
#' weighted by the RASMI concrete material intensity, so that the sub-split represents
#' a share of concrete mass rather than of floor area.
#' @author Bennet Weiss
calcCeGoodSplit <- function() {
  x <- readSource("Xi2016")

  share_rs <- toolCeMiWeightedRSShare()

  # split Res into RS and RM, keep Com, Ind, Civ
  x <- add_columns(x, addnm = c("RS", "RM"), dim = 3.1, fill = 0)
  res <- collapseDim(x[, , "Res"])
  x[, , "RS"] <- res * share_rs
  x[, , "RM"] <- res * (1 - share_rs)
  x <- x[, , "Res", invert = TRUE]

  weight <- toolCeCumulativeCementProduction(castto = x)
  unit <- "ratio"
  description <- paste(
    "Split to sort cement consumption into goods RS, RM, Com, Ind, and Civ.",
    "Sectoral split based on Xi 2016. Residential RS/RM sub-split derived from",
    "the GEM single-family home share weighted with the RASMI concrete material intensity."
  )
  note <- "dimensions: (Region,Good,value)"

  output <- list(
    x = x,
    weight = weight,
    unit = unit,
    description = description,
    note = note
  )
  return(output)
}

#' RS share of residential concrete mass.
#' @author Bennet Weiss
toolCeMiWeightedRSShare <- function() {
  res_goods <- c("RS", "RM")
  sfh_share <- calcOutput("CeSingleFamilyHomeShare", aggregate = FALSE)
  structure_split <- calcOutput("CeStructureSplit", aggregate = FALSE)
  mi <- calcOutput("CeBuildingsMI", subtype = "concrete", aggregate = FALSE)

  # before calculation, select only relevant dimensions
  res_area_split <- mbind(setNames(sfh_share, "RS"), setNames(1 - sfh_share, "RM"))
  getSets(res_area_split)["d3.1"] <- "Good"
  res_structure_split <- mselect(structure_split, Good = res_goods)
  res_mi <- mselect(mi, Good = res_goods)

  res_mass <- res_structure_split * res_area_split * res_mi
  res_mass <- collapseDim(dimSums(res_mass, dim = "Structure"))
  return(collapseDim(res_mass[, , "RS"] / dimSums(res_mass, dim = 3)))
}
