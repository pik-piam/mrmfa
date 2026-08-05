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
#' remind-mfa, not here. Countries with no consumption of a type (so a locally
#' undefined split) fall back to the global \code{(polymer, sector)} split of
#' that type and year, so the shares always sum to 1 and the multiplication
#' conserves mass. The three fibre polymers form type \code{Fibre},
#' \code{Rubbers} forms type \code{Rubber}, and the remaining polymers form type
#' \code{Plastics}, matching the \code{type} dimension of
#' \code{\link{calcPlProduction}}.
#'
#' @return A list in \code{\link[madrat]{calcOutput}} format with the polymer and
#'   sector split shares by country, year, type, polymer and sector (1978-2021).
#'   The weight is the per-type Gao consumption total. Years 2020-2021 rely on
#'   incomplete Gao trade data.
#' @param target_years integer vector of target years for the output data.
#' Enables forecasting of Gao data to include same years as production data.
#' @author Leonie Schweiger
#' @seealso \code{\link{readGaoCabrera2025}}, \code{\link{calcPlProduction}}
#' @examples
#' \dontrun{
#' a <- calcOutput("PlSectorPolymerSplit")
#' }
#' @importFrom magclass dimSums setItems
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

  # Where a country has no consumption of a type (typeTotal == 0) the local split
  # is undefined (0/0) and would collapse to all-zeros. Multiplying that by a
  # non-zero production total in remind-mfa would lose mass. Fall back to the
  # global (polymer, sector) split of that type and year, which sums to 1 and
  # hence conserves mass; countries that do have data keep their own split.
  globalByType <- dimSums(data_forecast, dim = "region") # (GLO, year, type.polymer.sector)
  globalShare <- globalByType / dimSums(globalByType, dim = c("polymer", "sector"))
  globalShare[is.na(globalShare)] <- 0 # type absent even globally -> genuinely 0
  globalShare <- setItems(globalShare, dim = 1, "GLO") # mark global so it broadcasts over region

  missing <- typeTotal == 0 # (region, year, type)
  x[is.na(x)] <- 0 # start the undefined blocks from 0 ...
  x <- x + missing * globalShare # ... then fill only those blocks with the global split

  # Weight the shares by Gao consumption, but floor the zero weights of fallback
  # (region, year, type) blocks to a negligible positive value. Otherwise a model
  # region whose countries all lack Gao data for a type sums to zero weight and
  # its aggregated share becomes NaN (mass loss). The floor is tiny relative to
  # real consumption, so it does not shift data-rich regions; where it does apply
  # every country carries the same global split, so the region resolves to it.
  weight <- typeTotal
  weight[weight == 0] <- 1e-9 * max(weight)

  return(list(
    x = x,
    weight = weight,
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
