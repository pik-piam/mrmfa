#' Pottinger 2024 plastics material flows in IAMC variables
#'
#' @description
#' Return the plastics material-flow data from Pottinger et al. (2024) for a
#' specified scenario (Business-as-Usual or policy variants), at ISO3 country
#' level, with the source flow columns renamed to a subset of IAMC variables.
#' All flows are in Mt/yr.
#'
#' @param subtype Character, scenario to extract (e.g., "businessAsUsual" for BAU).
#' Must match a scenario name encoded in the data dimension from readPottinger2024.
#' @param perCapita Logical. If \code{FALSE} (default), return absolute flows in
#' Mt/yr. If \code{TRUE}, return per-capita flows in kg/cap, using the same UN
#' World Population Prospects population as \code{\link{convertPottinger2024}}.
#'
#' @author Leonie Schweiger
#' @return List with a MagPIE object of plastics flows (Mt/yr, or kg/cap if
#' \code{perCapita = TRUE}) in IAMC variables and metadata in calcOutput format.
#' @seealso \code{\link{readPottinger2024}}
#' @importFrom madrat readSource
#' @importFrom magclass getItems getItems<- mbind getYears getNames<-
calcPlPottinger <- function(subtype = "businessAsUsual", perCapita = FALSE) {
  x <- readSource("Pottinger2024")
  scenarios <- getNames(x, dim=1)
  variables <- getNames(x, dim=2)

  if (subtype == "all") {
    x <- x
  } else if (!(subtype %in% scenarios)) {
    stop("Scenario '", subtype, "' not found in Pottinger2024 data. ",
      "Available scenarios: ", paste(scenarios, collapse = ", "))
  } else {
    x <- x[, , subtype]
  }

  # map raw source flow columns to IAMC variables
  iamc <- c(
    totalConsumptionMt     = "Material Demand|Chemicals|Plastics",
    primaryProductionMt    = "Production|Chemicals|Plastics|Primary",
    secondaryProductionMt  = "Production|Chemicals|Plastics|Secondary"
  )

  # identify which of the mapped variables are present in this scenario
  keep_idx <- which(variables %in% names(iamc))
  variables_kept <- variables[keep_idx]

  if (length(keep_idx) == 0) {
    stop("No mapped variables found for scenario '", subtype, "' in Pottinger2024. ",
      "Expected one or more of: ", paste(names(iamc), collapse = ", "))
  }

  # subset to keep only mapped variables
  x <- x[, , variables_kept]
  new_names <- iamc[variables_kept]
  getItems(x, dim = 3.2) <- as.vector(new_names)

  if (!perCapita) {
    return(list(
      x = x,
      weight = NULL,
      unit = "Mt/yr",
      description = paste(
        "Plastics material-flow pathway 2011-2050 from Pottinger et al. (2024),",
        "scenario:", subtype, "in IAMC variables"
      ),
      note = "Disaggregated from 4 Pottinger regions to ISO3 via population weighting"
    ))
  }

  # per-capita: same UN World Population Prospects population as convertPottinger2024
  # (estimates through 2021 + medium variant from 2022), in millions of people
  popEstimates <- readSource("UN_PopDiv", subtype = "pop", subset = "estimates")
  popMedium <- readSource("UN_PopDiv", subtype = "pop", subset = "medium")
  getNames(popEstimates) <- NULL
  getNames(popMedium) <- NULL
  pop <- mbind(popEstimates, popMedium)
  pop <- pop[getItems(x, dim = 1), getYears(x), ] # ISO3 countries & Pottinger years

  x <- 1000 * x / pop # Mt / million people -> kg/cap
  x[!is.finite(x)] <- NA
  getItems(x, dim = 3.2) <- paste0(as.vector(new_names), "|per capita")

  return(list(
    x = x,
    weight = pop,
    unit = "kg/cap",
    description = paste(
      "Per-capita plastics material-flow pathway 2011-2050 from Pottinger et al. (2024),",
      "scenario:", subtype, "in IAMC variables; population from UN WPP (estimates + medium)."
    ),
    note = "Population-weighted; intensive variable aggregated as weighted mean."
  ))
}
