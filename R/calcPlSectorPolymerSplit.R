#' Calculate polymer and sector split shares within each plastics type
#'
#' @description
#' Split total Fibre, Rubber and Plastics consumption into individual polymers
#' and end-use sectors. Combines the apparent polymer consumption of Gao &
#' Cabrera-Serrenho (2025) (\code{readSource("GaoCabrera2025", "consumption")})
#' with their polymer-specific end-use sector distribution
#' (\code{readSource("GaoCabrera2025", "sector_shares")}) to obtain, for every
#' country and year, the consumption of each polymer in each of the 8 end-use
#' sectors. These are normalized within each type (\code{Fibre}, \code{Rubber},
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
#' @author Leonie Schweiger
#' @seealso \code{\link{readGaoCabrera2025}}, \code{\link{calcPlProduction}}
#' @examples
#' \dontrun{
#' a <- calcOutput("PlSectorPolymerSplit")
#' }
#' @importFrom magclass getItems collapseDim add_dimension mselect mbind dimSums
#' @importFrom magclass getSets getSets<-
#' @export
calcPlSectorPolymerSplit <- function() {
  
  data <- calcOutput("PlGaoCabrera2025", aggregate=FALSE)

  # ---------------------------------------------------------------------------
  # Add the type subdim (Fibre / Rubber / Plastics) by splitting on the polymer
  # groups. `type` becomes the first subdim so a (time, region, type) object
  # multiplies cleanly across polymer/sector.
  # ---------------------------------------------------------------------------
  fibrePolymers <- c("Polyester fibre", "Polyamide fibre", "Other fibre (acrylic)")
  rubberPolymers <- "Rubbers"
  plasticsPolymers <- setdiff(getItems(data, dim = "polymer"), c(fibrePolymers, rubberPolymers))

  tagType <- function(polys, typeName) {
    add_dimension(mselect(data, polymer = polys), dim = 3.1, add = "type", nm = typeName)
  }
  absTyped <- mbind(
    tagType(plasticsPolymers, "Plastics"),
    tagType(fibrePolymers, "Fibre"),
    tagType(rubberPolymers, "Rubber")
  )

  # ---------------------------------------------------------------------------
  # Normalize within type. Each polymer's sector shares sum to 1, so the per-type
  # total equals that type's summed polymer consumption.
  # ---------------------------------------------------------------------------
  typeTotal <- dimSums(absTyped, dim = c("polymer", "sector")) # (region, year, type)
  x <- absTyped / typeTotal # broadcasts typeTotal over polymer.sector via the type subdim
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
    note = paste(
      "dimensions: (Time, Region, Type, Polymer, Sector); shares sum to 1 within",
      "each (Time, Region, Type). Years 2020-2021 rely on incomplete Gao trade data."
    ),
    min = 0,
    max = 1
  ))
}
