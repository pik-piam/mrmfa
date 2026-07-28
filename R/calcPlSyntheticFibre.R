#' Calculate synthetic fibre production by country
#'
#' Combines the global synthetic fibre production time series (Polyester,
#' Polyamide (nylon), Acrylic, Polypropylene and Elastane) from Textile Exchange
#' with the estimated country-level production shares based on Credence Research
#' market data on monoethylene glycol (MEG) to yield a total synthetic
#' fibre production by country and year.
#'
#' @return A list in \code{\link[madrat]{calcOutput}} format with total synthetic
#'   fibre production by country and year.
#' @author Leonie Schweiger
#' @seealso [readTextileExchange()]
#' @examples
#' \dontrun{
#' a <- calcOutput("PlSyntheticFibre")
#' }
#' @importFrom magclass dimSums collapseDim setYears getItems getYears new.magpie magpiesort
#' @export
calcPlSyntheticFibre <- function() {
  # read source and interpolate missing years
  data <- readSource("TextileExchange", subtype = "timeseries_by_type", convert = FALSE) %>% magpiesort()

  data_years <- getYears(data, as.integer = TRUE)
  interpolated <- toolInterpolate(
    data,
    years = seq(data_years[1], data_years[length(data_years)], 1),
    type = "linear"
  )
  # backcast missing years by oecd (first historic years differ between fibre types)
  oecdTotal <- dimSums(readSource("OECD_Plastic", subtype = "Use_1990-2019_region"), dim = 1)
  getItems(oecdTotal, dim = 3) <- NULL
  getItems(oecdTotal, dim = 1) <- "GLO"
  extrapolated <- toolBackcastByReference(interpolated, oecdTotal)

  # global synthetic fibre production per year (sum over the three fibre types,
  # differentiating whether fibers are included in PlasticsEurope figures or not)
  total <- dimSums(extrapolated, dim = 3.1)

  # country-level production shares (fraction); single fibre (Polyester proxy) and
  # single year (2024), applied as a static distribution key across all years
  share <- setYears(collapseDim(
    readSource("TextileExchange", subtype = "region_share", convert = TRUE)
  ), NULL) / 100

  # production by country and year = global total(year) * country share
  x <- total * share

  return(list(
    x = x,
    weight = NULL,
    unit = "Mt",
    description = paste(
      "Synthetic fibre production (Polyester + Polyamide (nylon) + Acrylic) by",
      "country, 2020-2024, from Textile Exchange Materials Market Report 2025.",
      "Global totals disaggregated to countries using the 2024 regional split",
      "(polyester/MEG proxy) distributed by chemical energy consumption."
    ),
    note = "dimensions: (Time,Region,value)",
    min = 0
  ))
}
