#' Read Fritzeen et al. 2023 organic chemicals production scenarios
#'
#' @description
#' Read global organic chemicals production projections by technology from 
#' Fritzeen et al. (2023). Two supplementary Excel files are combined, 
#' as they provide results for disjoint sets of scenarios:
#' \itemize{
#'   \item \code{Fig_6.xlsx} (sheet \code{Fig6}): scenarios Reference, Net-Zero,
#'   Net-Zero Constant WTO.
#'   \item \code{Fig_S17.xlsx} (sheet \code{F17}): scenarios Net-Zero Ambitious,
#'   Net-Zero BioCE, Net-Zero w/ DAC, Net-Zero Advanced.
#' }
#' Both sheets hold, at the global level, organic chemicals output (Mt/yr) 1990-2100 by
#' production technology and scenario. The technology label is lower-cased so that
#' "NGL steamcrack" (Fig6) and "ngl steamcrack" (F17) refer to the same technology.
#' The data are global only and are not disaggregated to countries or regions.
#'
#' @author Leonie Schweiger
#' @return MagPIE object with dimensions (region = GLO, year, scenario.technology)
#' holding organic chemicals output in Mt/yr.
#' @seealso \code{\link[madrat]{readSource}}, \code{\link{calcPlFritzeen2023}}
#' @examples
#' \dontrun{
#' readSource("Fritzeen2023", convert = FALSE)
#' }
#' @importFrom readxl read_excel
#' @importFrom dplyr bind_rows mutate select
#' @importFrom rlang .data
#' @importFrom magclass as.magpie
readFritzeen2023 <- function() {
  read <- function(file, sheet) {
    read_excel(file, sheet = sheet) %>%
      setNames(c("technology", "year", "value", "scenario"))
  }
  a <- read("Fig_6.xlsx", "Fig6")
  b <- read("Fig_S17.xlsx", "F17")

  # lowercase technology harmonises "NGL steamcrack" (Fig6) with "ngl steamcrack" (F17)
  x <- bind_rows(a, b) %>%
    mutate(technology = tolower(.data$technology), region = "GLO") %>%
    select("region", "scenario", "technology", "year", "value")

  as.magpie(x, spatial = "region", temporal = "year", datacol = "value")
}
