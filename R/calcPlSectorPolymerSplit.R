#' Calculate polymer and sector split shares within each plastics type
#'
#' @description
#' Split total Fibre, Rubber and Plastics consumption into individual polymers
#' and end-use sectors. Consumption of each polymer in each of the 8 end-use
#' sectors is normalized within each type (\code{Fibre}, \code{Rubber},
#' \code{Plastics}) so that the shares over \code{(polymer, sector)} sum to 1.
#'
#' Multiplying the result with a \code{(time, region, type)} total - e.g. the
#' output of \code{\link{calcPlProduction}} - yields absolute values per
#' \code{(time, region, type, polymer, sector)}. That multiplication happens in
#' remind-mfa, not here. The three fibre polymers form type \code{Fibre},
#' \code{Rubbers} forms type \code{Rubber}, and the remaining polymers form type
#' \code{Plastics}, matching the \code{type} dimension of
#' \code{\link{calcPlProduction}}.
#'
#' @return A list in \code{\link[madrat]{calcOutput}} format with the polymer and
#'   sector split shares by country, year, type, polymer and sector (1978-2021).
#'   The weight is the per-type total consumption. Years 2020-2021 rely on
#'   incomplete Gao trade data.
#' @param target_years integer vector of target years for the output data.
#' Enables forecasting of Gao data to include same years as production data.
#' @author Leonie Schweiger
#' @seealso \code{\link{readGaoCabrera2025}}, \code{\link{calcPlProduction}}
#' @examples
#' \dontrun{
#' a <- calcOutput("PlSectorPolymerSplit")
#' }
#' @importFrom magclass dimSums
#' @export
calcPlSectorPolymerSplit <- function(target_years = NULL) {

  data <- calcOutput("PlGaoCabrera2025", aggregate = FALSE)

  # ---------------------------------------------------------------------------
  # Forecast to match years of production data
  # ---------------------------------------------------------------------------
  production <- calcOutput("PlProduction", years = target_years)
  data_forecast <- time_interpolate(data,
                                    interpolated_year = getYears(production),
                                    integrate_interpolated_years = TRUE,
                                    extrapolation_type = "constant")
  data_forecast <- data_forecast[, getYears(production), ]

  # ---------------------------------------------------------------------------
  # Normalize within type. Each polymer's sector shares sum to 1, so the per-type
  # total equals that type's summed polymer consumption.
  # ---------------------------------------------------------------------------
  typeTotal <- dimSums(data_forecast, dim = c("polymer", "sector")) # (region, year, type)
  x <- data_forecast / typeTotal # broadcasts typeTotal over polymer.sector via the type subdim
  x[is.na(x)] <- 0 # 0/0 for types with no consumption -> 0

  return(list(
    x = x,
    weight = typeTotal,
    unit = "share",
    description = paste(
      "Share of each polymer and end-use sector within total Fibre/Rubber/Plastics",
      "apparent consumption per region, from Gao & Cabrera-Serrenho (2025) polymer",
      "consumption times the polymer-specific sector distribution. Sums to 1 over",
      "(polymer, sector) within each (region, year, type). Multiply by a",
      "(time, region, type) total to obtain absolute values (done in remind-mfa)."
    ),
    note = "dimensions: (Time, Region, Type, Material, Good, value)",
    min = 0,
    max = 1
  ))
}
