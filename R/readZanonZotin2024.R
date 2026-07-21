#' Read Zanon-Zotin et al. 2024 HVC production scenarios
#'
#' @description
#' Read global high-value chemical (HVC) production projections from Zanon-Zotin
#' et al. (2024), Nature Communications, doi:10.1038/s41467-024-52434-y,
#' supplementary data file \code{41467_2024_52434_MOESM3_ESM.xlsx}. Sheet
#' \code{fig3b} holds, at the global level ("World"), HVC production 2010-2100
#' by scenario (COFFEE 1.5 model) split into three source routes
#' (Multi-product, On-purpose, Refinery-sourced), in kt/yr. The data are global
#' only and are not disaggregated to countries or regions.
#'
#' @param subtype Character. Sheet to read; currently only \code{"fig3b"}.
#' @author Leonie Schweiger
#' @return MagPIE object with dimensions (region = GLO, year, scenario.variable)
#' holding HVC production in kt/yr.
#' @seealso \code{\link[madrat]{readSource}}, \code{\link{calcPlZanonZotin2024}}
#' @examples
#' \dontrun{
#' readSource("ZanonZotin2024", subtype = "fig3b", convert = FALSE)
#' }
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate select
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom magclass as.magpie
readZanonZotin2024 <- function(subtype = "fig3b") {
  sheets <- list("fig3b")
  if (!subtype %in% sheets) {
    stop("Invalid subtype '", subtype, "'. Must be one of: ", paste(sheets, collapse = ", "))
  }

  # The single model ("COFFEE 1.5") and the unit ("kt/yr") are dropped from the
  # data dimension: the model is re-attached at write time via writeArgs (its "."
  # would collide with magpie's subdim separator), the unit via the calc return list.
  x <- read_excel("41467_2024_52434_MOESM3_ESM.xlsx", sheet = subtype) %>%
    mutate(region = "GLO") %>%
    select(-"model", -"unit") %>%
    pivot_longer(cols = -c("region", "scenario", "variable"),
                 names_to = "year", values_to = "value") %>%
    select("region", "scenario", "variable", "year", "value")

  as.magpie(x, spatial = "region", temporal = "year", datacol = "value")
}
