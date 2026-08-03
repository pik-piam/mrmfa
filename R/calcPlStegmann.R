#' Stegmann 2022 plastics production and demand in IAMC variables
#'
#' @description
#' Return total plastics production and demand (Stegmann assumes no trade, so
#' demand = production) from Stegmann et al. (2022, PLAIA/IMAGE) at ISO3 country
#' level, for all four scenarios. The eight Plastics|Production|Sector|*
#' flows (PJ/yr) are summed to a total and converted to mass using the lower
#' heating value of plastics of 35 GJ/t (Mt/yr = PJ/yr / 35).
#'
#' @param subtype Character. \code{"total"} (default) returns absolute production
#' and demand in Mt/yr; \code{"perCapita"} returns demand per capita in kg/cap,
#' using Stegmann's own Population variable.
#'
#' @author Leonie Schweiger
#' @return List with a MagPIE object of plastics production/demand (Mt/yr, or
#' kg/cap if \code{subtype = "perCapita"}) in IAMC variables and metadata in
#' calcOutput format.
#' @seealso \code{\link[mrindustry]{readStegmann2022}}
#' @importFrom magclass getItems getItems<- getNames dimSums add_dimension
#'   collapseNames mbind mselect
calcPlStegmann <- function(subtype = "total") {
  if (!subtype %in% c("total", "perCapita")) {
    stop("Unknown subtype '", subtype, "'. Use 'total' or 'perCapita'.")
  }

  # get plastics production and population variables
  x <- readSource("Stegmann2022", subtype = "PopWeighted")
  # the read object carries a singleton Model and a Unit sub-dimension that is not needed downstream
  x <- collapseNames(x, collapsedim = c("Model", "Unit"))

  # rename the scenario codes to readable article-equivalent labels (Overview sheet)
  scenMap <- c(
    "SSP2"                     = "Baseline (SSP2)",
    "SSP2_SPA0_26I_D"          = "2C",
    "SSP2_SPA0_26I_D_Circular" = "2C - Circular Economy",
    "SSP2_SPA0_26I_D_CBE"      = "2C - Circular Bioeconomy"
  )
  getItems(x, dim = "Scenario") <- unname(scenMap[getItems(x, dim = "Scenario")])

  # total plastics production: sum the eight production-by-sector flows (PJ/yr)
  # and convert energy -> mass via the 35 GJ/t lower heating value (-> Mt/yr).
  sectorNames <- grep("Plastics\\|Production\\|Sector", getNames(x), value = TRUE)
  totalProd <- dimSums(x[, , sectorNames], dim = 3.2) / 35

  if (subtype == "total") {
    # demand equals production (Stegmann does not consider trade)
    production <- add_dimension(totalProd, dim = 3.2, add = "Variable", nm = "Production|Chemicals|Plastics")
    demand <- add_dimension(totalProd, dim = 3.2, add = "Variable", nm = "Material Demand|Chemicals|Plastics")
    out <- mbind(production, demand)

    return(list(
      x = out,
      weight = NULL,
      unit = "Mt/yr",
      description = paste(
        "Total plastics production and demand 2005-2100 from Stegmann et al.",
        "(2022, PLAIA/IMAGE), all scenarios; demand = production (no trade).",
        "Summed over 8 production sectors, PJ/yr converted to Mt/yr via 35 GJ/t;",
        "disaggregated from 26 IMAGE regions to ISO3 via population weighting."
      )
    ))
  }

  # per-capita demand: divide by Stegmann's own regional population (million)
  popNames <- grep("Population", getNames(x), value = TRUE)
  stegPop <- collapseNames(x[, , popNames], collapsedim = "Variable")
  perCap <- 1000 * totalProd / stegPop # Mt / million people -> kg/cap
  perCap[is.na(perCap) | is.infinite(perCap)] <- 0

  perCapVar <- "Material Demand|Chemicals|Plastics|per capita"
  perCap <- add_dimension(perCap, dim = 3.2, add = "Variable", nm = perCapVar)
  weight <- add_dimension(stegPop, dim = 3.2, add = "Variable", nm = perCapVar)

  return(list(
    x = perCap,
    weight = weight,
    unit = "kg/cap",
    description = paste(
      "Per-capita plastics demand 2005-2100 from Stegmann et al. (2022, PLAIA/IMAGE),",
      "all scenarios; population from Stegmann's own reported population."
    )
  ))
}
