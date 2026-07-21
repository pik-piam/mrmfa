#' Calculate synthetic fibre production by country
#'
#' Combines the global synthetic fibre production time series (Polyester,
#' Polyamide (nylon) and Acrylic) from Textile Exchange with the estimated
#' country-level production shares based on Credence Research market data
#' on monoethylene glycol (MEG) to yield a total synthetic
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
#' @importFrom magclass dimSums collapseDim setYears getItems getYears new.magpie
#' @export
calcPlSyntheticFibre <- function() {
  # global synthetic fibre production per year (sum over the three fibre types)
  total <- dimSums(
    readSource("TextileExchange", subtype = "timeseries_by_type", convert = FALSE),
    dim = 3
  )

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
