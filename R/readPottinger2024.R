#' Read Pottinger 2024 plastics material-flow data
#'
#' @description
#' Read the global plastics material-flow dataset from Pottinger et al. (2024),
#' "science.adr3837_data_s1.csv". All scenarios (Business-as-Usual and policy
#' variants) are read. Each scenario's time series is spread across multiple rows:
#' for each scenario, rows exist for years 2011–2050 (one year per row after
#' extracting the year from the scenarioKey suffix). The global aggregate region
#' is dropped (it cannot be disaggregated to countries); the four modelling regions
#' china, eu30, nafta and row are kept.
#' All flows are in Mt (megatonnes) per year.
#'
#' @author Leonie Schweiger
#' @return MagPIE object with the raw Pottinger flow variables, dimensions
#' (regionKey, year, scenario.variable), in Mt. 
#' @seealso \code{\link[madrat]{readSource}}
#' @examples
#' \dontrun{
#' readSource("Pottinger2024", convert = FALSE)
#' }
#' @importFrom dplyr filter mutate select
#' @importFrom magclass as.magpie
readPottinger2024 <- function() {
  raw <- readr::read_csv("science.adr3837_data_s1.csv", show_col_types = FALSE)

  # keep only the four modelling regions (drop the "global" aggregate)
  df <- raw %>%
    filter(.data$regionKey %in% c("china", "eu30", "nafta", "row")) %>%
    # extract scenario name: strip year suffix from scenarioKey
    # e.g., "businessAsUsual2011" -> "businessAsUsual", "businessAsUsual" (year 2050) -> "businessAsUsual"
    mutate(scenario = gsub("\\d{4}$", "", .data$scenarioKey)) %>%
    # drop identifier columns; keep regionKey, year, scenario, and 20 flow cols
    select(-"scenarioKey", -"scenarioDescription", -"region", -"isGlobal")

  # verify each (regionKey, year, scenario) triplet is unique
  if (anyDuplicated(df[, c("regionKey", "year", "scenario")]) > 0) {
    stop("Unexpected duplicate (regionKey, year, scenario) rows in Pottinger2024 data.")
  }

  # reshape all flow columns into the data dimension
  dfLong <- df %>%
    tidyr::pivot_longer(cols = -c("regionKey", "year", "scenario"), names_to = "variable", values_to = "value") %>%
    select("regionKey", "year", "scenario", "variable", "value")

  x <- as.magpie(dfLong, spatial = 1, temporal = 2)

  return(x)
}
