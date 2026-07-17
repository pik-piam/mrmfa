#' IEA 2018 global plastics production in IAMC variables
#'
#' @description
#' Return the global annual production of key thermoplastics (1980-2050) from the
#' IEA report "The Future of Petrochemicals" (2018), Figure 4.2 (Reference
#' Technology Scenario), read via \code{readSource("IEA_Petrochem",
#' subtype = "plastics_All")}. The reported total (the source "SUM" series, in Mt)
#' is returned at the global level under the IAMC variables
#' \code{Production|Chemicals|Plastics} and \code{Material Demand|Chemicals|Plastics}.
#' The data are global only and are not disaggregated to countries or regions.
#'
#' @param subtype Character. \code{"total"} (default) returns absolute production
#' and demand in Mt/yr; \code{"perCapita"} returns demand per capita in kg/cap,
#' using Stegmann's own Population variable.
#'
#' @author Leonie Schweiger
#' @return List with a global-only MagPIE object of plastics production (Mt/yr) in
#' IAMC variables.
#' @importFrom madrat readSource
#' @importFrom magclass setNames mbind getItems getItems<-
calcPlIEA <- function(subtype) {
  if (!subtype %in% c("total", "perCapita")) {
    stop("Unknown subtype '", subtype, "'. Use 'total' or 'perCapita'.")
  }

  x <- readSource("IEA_Petrochem", subtype = "plastics_All", convert = FALSE)

  # readIEA_Petrochem runs make.names() on the header row, which prefixes the
  # numeric year columns with "X" (e.g. 1980 -> X1980). For the plastics subtype
  # these become the temporal dimension, so normalise to proper year format.
  getYears(x) <- as.integer(sub("^[Xy]", "", getYears(x)))

  # get the total production over all polymers
  x <- dimSums(x, dim = 3, na.rm = TRUE)

  # same global figure reported under both IAMC variables (setNames overwrites the
  # single source data-column name with the IAMC variable name)
  out <- mbind(
    setNames(x, "Production|Chemicals|Plastics"),
    setNames(x, "Material Demand|Chemicals|Plastics")
  )

  if (subtype == "total") {
    return(list(
      x = out,
      weight = NULL,
      unit = "Mt/yr",
      description = paste(
        "Global production of key thermoplastics 1980-2050 from IEA 'The Future of",
        "Petrochemicals' (2018), Figure 4.2 (Reference Technology Scenario), reported",
        "at the global level under the IAMC variables Production|Chemicals|Plastics",
        "and Material Demand|Chemicals|Plastics."
      ),
      isocountries = FALSE,
      note = paste(
        "Global only; IEA 'key thermoplastics' is a subset of all plastics",
        "(excludes thermosets and other polymers)."
      )
    ))
  }

  popEstimates <- readSource("UN_PopDiv", subtype = "pop", subset = "estimates")
  popMedium <- readSource("UN_PopDiv", subtype = "pop", subset = "medium")
  getNames(popEstimates) <- NULL
  getNames(popMedium) <- NULL
  pop <- dimSums(mbind(popEstimates, popMedium), dim = 1)
  pop <- pop[, getYears(out), ]

  out <- 1000 * out / pop # Mt / million people -> kg/cap
  out[is.na(out) | is.infinite(out)] <- 0
  getItems(out, dim = 3) <- paste0(getItems(out, dim = 3), "|per capita")

  return(list(
    x = out,
    weight = pop,
    unit = "kg/cap",
    description = paste(
      "Per-capita Global production of key thermoplastics 1980-2050 from IEA 'The Future of",
      "Petrochemicals' (2018), Figure 4.2 (Reference Technology Scenario), reported",
      "at the global level under the IAMC variables Production|Chemicals|Plastics",
      "and Material Demand|Chemicals|Plastics.; population from UN WPP (estimates + medium)."
    ),
    note = "Population-weighted; intensive variable aggregated as weighted mean."
  ))

}
