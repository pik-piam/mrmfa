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
#' @author Leonie Schweiger
#' @return List with a global-only MagPIE object of plastics production (Mt/yr) in
#' IAMC variables.
#' @importFrom madrat readSource
#' @importFrom magclass setNames mbind getItems getItems<-
calcPlIEA <- function() {
  x <- readSource("IEA_Petrochem", subtype = "plastics_All", convert = FALSE)

  # get the total production over all polymers
  x <- dimSums(x, dim = 3, na.rm = TRUE)

  # same global figure reported under both IAMC variables (setNames overwrites the
  # single source data-column name with the IAMC variable name)
  out <- mbind(
    setNames(x, "Production|Chemicals|Plastics"),
    setNames(x, "Material Demand|Chemicals|Plastics")
  )

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
