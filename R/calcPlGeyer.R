#' Geyer et al. 2017 global plastics production in IAMC variables
#'
#' @description
#' Return the global annual plastics production time series (1950-2015) from
#' Geyer et al. (2017), doi:10.1126/sciadv.1700782 (Table S1), reported at the
#' global level under the IAMC variables \code{Production|Chemicals|Plastics}
#' and \code{Material Demand|Chemicals|Plastics}, both in Mt/yr. The data are
#' global only and are not disaggregated to countries or regions.
#'
#' @author Leonie Schweiger
#' @return List with a global-only MagPIE object of plastics production (Mt/yr)
#' in IAMC variables and metadata in calcOutput format.
#' @seealso \code{\link{readGeyer}}, \code{\link{calcPlPottinger}},
#' \code{\link{calcPlGaoCabrera}}
#' @importFrom magclass setNames mbind
calcPlGeyer <- function() {
  x <- readSource("Geyer", subtype = "Prod_1950-2015", convert = FALSE)

  # same global figure reported under both IAMC variables (setNames overwrites
  # the single source data-column name with the IAMC variable name)
  out <- mbind(
    setNames(x, "Production|Chemicals|Plastics"),
    setNames(x, "Material Demand|Chemicals|Plastics")
  )

  return(list(
    x = out,
    weight = NULL,
    unit = "Mt/yr",
    description = paste(
      "Global annual plastics production 1950-2015 from Geyer et al. (2017),",
      "Table S1, reported at the global level under the IAMC variables",
      "Production|Chemicals|Plastics and Material Demand|Chemicals|Plastics.",
      "Global only; no regional disaggregation."
    ),
    isocountries = FALSE
  ))
}
