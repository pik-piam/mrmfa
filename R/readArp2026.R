#' Read Arp et al. 2026 IMAGE material consumption projections
#'
#' @description
#' Read projected material consumption from the supplement of Arp et al. (2026),
#' "Projected futures for the consumption of metals and non-metallic minerals"
#' (\code{Supplement_model_results.xlsx}). Each material is stored in its own
#' sheet, holding consumption in Mt/yr for the 26 IMAGE regions, four SSP2
#' scenarios and six end-use sectors, in 5-year steps from 2020 to 2100.
#' Scenario and sector labels are only given in the first row of each block and
#' are filled down.
#'
#' @param subtype Character, material sheet to read: \code{"steel"} or
#' \code{"cement"}.
#' @author Bennet Weiss
#' @return MagPIE object with dimensions (IMAGE region, year, scenario.sector)
#' holding material consumption in Mt/yr.
#' @seealso \code{\link[madrat]{readSource}}, \code{\link{convertArp2026}}
#' @examples
#' \dontrun{
#' readSource("Arp2026", subtype = "steel", convert = FALSE)
#' }
#' @importFrom dplyr select
#' @importFrom magclass as.magpie
readArp2026 <- function(subtype) {
  if (!subtype %in% c("steel", "cement")) {
    stop("Unknown subtype '", subtype, "'. Use 'steel' or 'cement'.")
  }

  df <- readxl::read_excel("Supplement_model_results.xlsx", sheet = subtype)

  if (any(df$Unit != "Mt")) {
    stop("Unexpected unit in Arp2026 sheet '", subtype, "', expected Mt.")
  }

  # scenario and sector are only given in the first row of each block
  df <- df %>%
    tidyr::fill("Scenario", "Sector", .direction = "down") %>%
    select(-"Unit") %>%
    tidyr::pivot_longer(cols = -c("Scenario", "Sector", "Region"), names_to = "year", values_to = "value") %>%
    select("Region", "year", "Scenario", "Sector", "value")

  x <- as.magpie(df, spatial = "Region", temporal = "year", datacol = "value")

  return(x)
}
