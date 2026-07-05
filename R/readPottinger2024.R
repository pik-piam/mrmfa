#' Read Pottinger 2024 plastics material-flow data
#'
#' @description
#' Read the global plastics material-flow dataset from Pottinger et al. (2024),
#' "science.adr3837_data_s1.csv". Only the Business-as-Usual (no policies)
#' pathway is read. In the source, the BAU time series is spread across
#' scenarios: the scenarios \code{businessAsUsual2011}..\code{businessAsUsual2049}
#' each hold a single year, and \code{businessAsUsual} holds the year 2050.
#' Selecting all scenarios matching \code{^businessAsUsual} therefore
#' reconstructs the full 2011-2050 series. The global aggregate region is
#' dropped (it cannot be disaggregated to countries); the four modelling regions
#' china, eu30, nafta and row are kept. All flows are in Mt (megatonnes) per year.
#'
#' @author Leonie Schweiger
#' @return MagPIE object with the raw Pottinger flow variables, dimensions
#' (regionKey, year, variable), in Mt.
#' @seealso \code{\link[madrat]{readSource}}
#' @examples
#' \dontrun{
#' readSource("Pottinger2024", convert = FALSE)
#' }
#' @importFrom readr read_csv
#' @importFrom dplyr filter select
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom magclass as.magpie
readPottinger2024 <- function() {
  raw <- readr::read_csv("science.adr3837_data_s1.csv", show_col_types = FALSE)

  # keep only the Business-as-Usual pathway and the four modelling regions
  # (drop the "global" aggregate, which cannot be disaggregated to countries)
  df <- raw %>%
    filter(
      grepl("^businessAsUsual", .data$scenarioKey),
      .data$regionKey %in% c("china", "eu30", "nafta", "row")
    ) %>%
    # drop identifier columns; the BAU year lives in the scenario, not needed once
    # (regionKey, year) is the spatial-temporal key
    select(-"scenarioKey", -"scenarioDescription", -"region", -"isGlobal")

  # each businessAsUsual* scenario contributes exactly one year, so after dropping
  # the scenario column every (regionKey, year) pair must be unique
  if (anyDuplicated(df[, c("regionKey", "year")]) > 0) {
    stop("Unexpected duplicate (regionKey, year) rows in Pottinger2024 BAU data.")
  }

  # reshape all flow columns into the data dimension
  dfLong <- df %>%
    pivot_longer(cols = -c("regionKey", "year"), names_to = "variable", values_to = "value")

  # build magpie object: spatial = regionKey, temporal = year, data = variable
  x <- as.magpie(dfLong, spatial = "regionKey", temporal = "year")
  x[is.na(x)] <- 0

  return(x)
}
