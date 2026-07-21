#' Fritzeen et al. 2023 global organic chemicals production in IAMC variables
#'
#' @description
#' Return the global organic chemicals production time series (1990-2100) by scenario from
#' Fritzeen et al. (2023), summed over all production technologies (Fig. 6 and
#' Fig. S17 combined). The scenario total is reported at the global level under the
#' IAMC variables \code{Production|Chemicals|Plastics} and
#' \code{Material Demand|Chemicals|Plastics} (same value under both names), since Fritzeen 
#' states they "assume that all organic chemicals end up as plastics". The data
#' are global only and are not disaggregated to countries or regions.
#'
#' @param subtype Character. \code{"total"} (default) returns absolute production
#' and demand in Mt/yr; \code{"perCapita"} returns demand per capita in kg/cap,
#' using SSP2 population trajectories from \code{\link{calcCoPopulation}}.
#' @author Leonie Schweiger
#' @return List with a global-only MagPIE object of plastics production (Mt/yr, or
#' kg/cap if \code{perCapita}) in IAMC variables and metadata in calcOutput format.
#' @seealso \code{\link{readFritzeen2023}}, \code{\link{calcPlIEA}},
#' \code{\link{calcPlGeyer}}
#' @importFrom magclass dimSums add_dimension mbind getYears
calcPlFritzeen2023 <- function(subtype = "total") {
  if (!subtype %in% c("total", "perCapita")) {
    stop("Unknown subtype '", subtype, "'. Use 'total' or 'perCapita'.")
  }

  x <- readSource("Fritzeen2023", convert = FALSE)     # GLO, year, scenario.technology (Mt/yr)
  total <- dimSums(x, dim = "technology", na.rm = TRUE) # per-scenario total, GLO.year.scenario

  if (subtype == "total") {
    out <- mbind(
      add_dimension(total, dim = 3.2, add = "variable", nm = "Production|Chemicals|Plastics"),
      add_dimension(total, dim = 3.2, add = "variable", nm = "Material Demand|Chemicals|Plastics")
    )

    return(list(
      x = out,
      weight = NULL,
      unit = "Mt/yr",
      description = paste(
        "Global plastics production 1990-2100 by scenario from Fritzeen et al. (2023),",
        "summed over all production technologies (Fig. 6 + Fig. S17), reported under the IAMC",
        "variables Production|Chemicals|Plastics and Material Demand|Chemicals|Plastics."
      ),
      isocountries = FALSE,
      note = "Global only; no regional disaggregation. Same total under both variable names."
    ))
  }

  # per-capita demand: divide global total by SSP2 global population (inhabitants)
  pop <- calcOutput("CoPopulation", scenarios = "SSP2")
  pop <- dimSums(pop, dim = 1)
  getItems(pop, dim = 1) <- "GLO"
  pop <- pop[, getYears(total), ]

  perCap <- total / pop * 1e9 # convert Mt/cap to kg/cap (1 Mt = 1e9 kg)
  perCap[!is.finite(perCap)] <- NA
  perCap <- add_dimension(perCap, dim = 3.2, add = "variable",
                          nm = "Material Demand|Chemicals|Plastics|per capita")

  return(list(
    x = perCap,
    weight = NULL,
    unit = "kg/cap",
    description = paste(
      "Per-capita global plastics demand 1990-2100 by scenario from Fritzeen et al. (2023),",
      "summed over all technologies and divided by SSP2 population (calcOutput('CoPopulation'))."
    ),
    isocountries = FALSE,
    note = "Global only; SSP2 population; one value per scenario."
  ))
}
