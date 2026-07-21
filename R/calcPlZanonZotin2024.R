#' Zanon-Zotin et al. 2024 global HVC production in IAMC variables
#'
#' @description
#' Return the global high-value chemical (HVC) production time series (2010-2100)
#' by scenario from Zanon-Zotin et al. (2024),
#' doi:10.1038/s41467-024-52434-y (sheet fig3b, COFFEE 1.5 model). The three
#' source routes (Multi-product, On-purpose, Refinery-sourced) are summed to the
#' aggregate IAMC variable \code{Production|Industry|Chemicals|HVC}, in kt/yr,
#' keeping each scenario separate. The data are global only and are not
#' disaggregated to countries or regions.
#'
#' @param subtype Character. \code{"total"} (default) returns absolute production
#' and demand in Mt/yr; \code{"perCapita"} returns demand per capita in kg/cap,
#' using SSP2 population trajectories.
#' @author Leonie Schweiger
#' @return List with a global-only MagPIE object of HVC production (Mt/yr) in the
#' IAMC variable \code{Production|Industry|Chemicals|HVC} and metadata in
#' calcOutput format.
#' @seealso \code{\link{readZanonZotin2024}}, \code{\link{calcPlIEA}},
#' \code{\link{calcPlGeyer}}
#' @importFrom magclass dimSums add_dimension
calcPlZanonZotin2024 <- function(subtype = "total") {
  if (!subtype %in% c("total", "perCapita")) {
    stop("Unknown subtype '", subtype, "'. Use 'total' or 'perCapita'.")
  }

  x <- readSource("ZanonZotin2024", subtype = "fig3b", convert = FALSE)

  # sum the three HVC sub-components (Multi-product, On-purpose, Refinery-sourced)
  # to the HVC total, keeping each scenario separate
  hvc <- dimSums(x, dim = "variable")/1000  # convert kt/yr -> Mt/yr

  if (subtype == "total") {
    hvc <- add_dimension(hvc, dim = 3.2, add = "variable",
                         nm = "Production|Industry|Chemicals|HVC")

    return(list(
      x = hvc,
      weight = NULL,
      unit = "Mt/yr",
      description = paste(
        "Global HVC production 2010-2100 by scenario from Zanon-Zotin et al. (2024),",
        "doi:10.1038/s41467-024-52434-y, sheet fig3b (COFFEE 1.5 model), reported under",
        "the IAMC variable Production|Industry|Chemicals|HVC (sum of Multi-product,",
        "On-purpose and Refinery-sourced HVC)."
      ),
      isocountries = FALSE,
      note = "Global only; no regional disaggregation. One value per scenario."
    ))
  }

  # per-capita demand: divide by SSP2 population
  pop <- calcOutput("CoPopulation", scenarios = "SSP2")
  pop <- dimSums(pop, dim = 1)
  pop <- pop[, getYears(hvc), ]

  perCap <- hvc / pop * 1e9 # Mt/cap -> kg/cap
  perCap[!is.finite(perCap)] <- NA

  perCapVar <- "Production|Industry|Chemicals|HVC|per capita"
  perCap <- add_dimension(perCap, dim = 3.2, add = "variable", nm = perCapVar)

  return(list(
    x = perCap,
    weight = NULL,
    unit = "kg/cap",
    description = paste(
      "Per-capita HVC production 2010-2100 from Zanon-Zotin et al. (2024),",
      "doi:10.1038/s41467-024-52434-y, sheet fig3b (COFFEE 1.5 model), reported under",
      "the IAMC variable Production|Industry|Chemicals|HVC|per capita."
    ),
    isocountries = FALSE,
    note = "Global only; no regional disaggregation. One value per scenario."
  ))

}
