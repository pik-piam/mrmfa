#' Literature projections of cement production in IAMC variables
#'
#' @description
#' Return cement production projections transcribed from the following publications.
#' The values are hard-coded because the sources are only available as PDF tables.
#' \itemize{
#'   \item \code{"IEAWEO2025"}: IEA (2025), World Energy Outlook 2025, Annex A, row "Cement" of
#'   "Industrial production (Mt)" in Table A.5b (Stated Policies, p. 425) and Table A.5c
#'   (Net Zero Emissions by 2050, p. 431). The Current Policies Scenario (Table A.5a, p. 419) is
#'   identical to Stated Policies and therefore omitted. Global only.
#'   \url{https://iea.blob.core.windows.net/assets/9753df19-0a71-422a-b725-012c555763b3/WorldEnergyOutlook2025.pdf}
#'   \item \code{"WCA2024"}: Riley, I. (2024), WCA Long Term Forecast for Cement and Clinker Demand,
#'   World Cement Association.
#'   \url{https://worldcementassociation.org/images/download-selector/Articles/WCA\%20Whitepaper.pdf}
#'   With \code{regional = FALSE}: global "Adjusted cement demand" (including clinker-free
#'   cements) of the three scenarios (without BAU), Table 3, p. 9.
#'   With \code{regional = TRUE}: "Business as usual" (BAU) cement demand of 13 WCA regions,
#'   Table 2, p. 6, disaggregated to ISO3 countries by SSP2 population with
#'   \code{regionmappingWCA.csv}. WCA does not publish country lists for its regions; the mapping
#'   is our own assignment. Its sum equals the global BAU of Table 3, so BAU is only returned at
#'   regional level to avoid writing it twice to the validation data.
#' }
#' The same value is reported as production and as demand.
#'
#' @param subtype Character. \code{"IEAWEO2025"} or \code{"WCA2024"}.
#' @param regional Logical. If \code{TRUE}, return the regional BAU data of \code{"WCA2024"} at
#' ISO3 country level instead of the global scenarios.
#' @author Bennet Weiss
#' @return List with a MagPIE object of cement production (Mt/yr) in IAMC variables
#' and metadata in calcOutput format.
calcCeProductionProjections <- function(subtype, regional = FALSE) {
  # cement production in Mt/yr, one row per scenario or region
  if (subtype == "IEAWEO2025") {
    if (regional) {
      stop("IEA WEO 2025 provides only global values.")
    }
    data <- tibble::tribble(
      ~scenario,                    ~`2010`, ~`2023`, ~`2024`, ~`2035`, ~`2040`, ~`2050`,
      "Stated Policies",            3280,    4110,    3950,    4200,    4314,    4364,    # Table A.5b, p. 425
      "Net Zero Emissions by 2050", 3280,    4110,    3950,    3881,    3755,    3535     # Table A.5c, p. 431
    )
    data$region <- "GLO"
    description <- paste(
      "Global cement production from IEA (2025), World Energy Outlook 2025, Annex A,",
      "Table A.5b (Stated Policies, p. 425) and Table A.5c (Net Zero Emissions by 2050, p. 431)."
    )
  } else if (subtype == "WCA2024" && !regional) {
    # Table 3, p. 9; 2024 is an estimate, scenarios 1-3 are the adjusted cement demand.
    # BAU is returned with regional = TRUE.
    data <- tibble::tribble(
      ~scenario,                   ~`2020`, ~`2024`, ~`2035`, ~`2050`,
      "Scenario 1 (Slow change)",  4194,    3834,    3498,    3200,
      "Scenario 2 (Central)",      4194,    3834,    3407,    2982,
      "Scenario 3 (Rapid change)", 4194,    3834,    3297,    2437
    )
    data$region <- "GLO"
    description <- paste(
      "Global cement demand from Riley (2024), WCA Long Term Forecast for Cement and Clinker Demand,",
      "World Cement Association, Table 3, p. 9. Scenarios 1-3 are the adjusted cement demand",
      "including clinker-free cements."
    )
  } else if (subtype == "WCA2024" && regional) {
    # Table 2, p. 6; 2024 is an estimate. In the whitepaper the region labels of the
    # "Broadly stable" and "Slow growth" blocks are shifted against the values. The labels
    # below follow the corrected table in Riley (2025), Global Cement Magazine article 1368,
    # which is consistent with the shares in Table 1 and the regional totals in the text.
    # South Asia 2035 keeps the whitepaper value 813 (the article has 771), as only 813
    # matches the global BAU of 3643 in Tables 2 and 3 (the rows sum to 3644 due to rounding).
    data <- tibble::tribble(
      ~region,                     ~`2020`, ~`2024`, ~`2035`, ~`2050`,
      "China",                     2411,    1825,    1183,    928,
      "North America",             115,     120,     131,     138,
      "Western Europe",            131,     126,     129,     125,
      "Eastern Europe and Turkey", 120,     127,     129,     98,
      "Oceania",                   12,      12,      13,      14,
      "North East Asia",           97,      97,      89,      85,
      "South East Asia",           236,     240,     275,     297,
      "North Africa",              93,      97,      111,     131,
      "Latin America",             164,     176,     202,     215,
      "CIS",                       97,      116,     127,     136,
      "Middle East",               196,     208,     235,     254,
      "South Asia",                392,     537,     813,     908,
      "Sub-Saharan Africa",        130,     153,     207,     308
    )
    data$scenario <- "BAU"
    description <- paste(
      "Regional BAU cement demand from Riley (2024), WCA Long Term Forecast for Cement and Clinker",
      "Demand, World Cement Association, Table 2, p. 6, with region labels corrected after",
      "Riley (2025), Global Cement Magazine, and disaggregated to countries by population."
    )
  } else {
    stop("Unknown subtype '", subtype, "'. Use 'IEAWEO2025' or 'WCA2024'.")
  }

  data <- tidyr::pivot_longer(data, cols = -c("region", "scenario"), names_to = "year", values_to = "value")
  x <- as.magpie(data, spatial = "region", temporal = "year", datacol = "value")
  x <- add_dimension(
    x,
    dim = 3.2, add = "variable",
    nm = c("Production|Non-Metallic Minerals|Cement", "Material Demand|Non-Metallic Minerals|Cement")
  )

  if (regional) {
    map <- toolGetMapping("regionmappingWCA.csv", type = "regional", where = "mrmfa")

    # SSP2 population as disaggregation weight
    pop <- calcOutput("CoPopulation", scenarios = "SSP2", aggregate = FALSE)
    pop <- pop[, getYears(x), ]

    x <- toolAggregate(x, rel = map, dim = 1, from = "RegionCode", to = "CountryCode", weight = pop)
  }

  return(list(
    x = x,
    weight = NULL,
    unit = "Mt/yr",
    description = description,
    isocountries = regional
  ))
}
