#' Read Stegmann 2022 plastics production data
#'
#' @description
#' Read the plastics production-by-sector and population data from Stegmann et al.
#' (2022), "Plastic futures and their CO2 emissions"
#' (doi:10.1038/s41586-022-05422-5), modelled with PLAIA/IMAGE. The "Data" sheet
#' of the supplementary Excel file is read; only the eight
#' Plastics|Production|Sector|* variables (in PJ/yr) and Population
#' (in million) are kept, for all four scenarios SSP2 baseline and the
#' three 2 degC variants). The World region is dropped (it cannot be
#' disaggregated to countries); the 26 IMAGE regions are kept.
#' Plastic flows are expressed in energy terms (PJ/yr); to translate them into
#' mass, use the assumed average lower heating value of plastics of 35 GJ/t.
#'
#' @author Leonie Schweiger
#' @return MagPIE object with the raw Stegmann production and population variables,
#' dimensions (region, year, scenario.variable), production in PJ/yr and
#' population in million.
#' @seealso \code{\link[madrat]{readSource}}
#' @examples
#' \dontrun{
#' readSource("Stegmann2022", convert = FALSE)
#' }
#' @importFrom readxl read_excel
#' @importFrom dplyr filter select
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom magclass as.magpie
readStegmann2022 <- function() {
  raw <- readxl::read_excel("41586_2022_5422_MOESM1_ESM.xlsx", sheet = "Data")

  # keep only plastics production-by-sector (PJ/yr) and population (million),
  # drop the non-disaggregatable global "World" region
  df <- raw %>%
    filter(
      grepl("^Plastics\\|Production\\|Sector\\|", .data$Variable) |
        .data$Variable == "Population",
      .data$Region != "World"
    ) %>%
    select(-"Model", -"Unit")

  # reshape the year columns into the temporal dimension
  dfLong <- df %>%
    pivot_longer(
      cols = -c("Scenario", "Region", "Variable"),
      names_to = "year", values_to = "value"
    ) %>%
    select("Region", "year", "Scenario", "Variable", "value")

  x <- as.magpie(dfLong, spatial = "Region", temporal = "year")

  return(x)
}
