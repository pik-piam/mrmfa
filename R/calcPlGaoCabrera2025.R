#' Calculate absolute consumption per polymer and sector from Gao & Cabrera-Serrenho (2025)
#'
#' @description
#' Combines the apparent polymer consumption of Gao & Cabrera-Serrenho (2025)
#' (\code{readSource("GaoCabrera2025", "consumption")})
#' with their polymer-specific end-use sector distribution
#' (\code{readSource("GaoCabrera2025", "sector_shares")}) to obtain, for every
#' country and year, the consumption of each polymer in each of the 8 end-use
#' sectors.
#'
#' @return A list in \code{\link[madrat]{calcOutput}} format with the absolute
#'   consumption by country, year, type, polymer and sector (1978-2021).
#'   Years 2020-2021 rely on incomplete Gao trade data.
#' @author Leonie Schweiger
#' @seealso \code{\link{readGaoCabrera2025}}, \code{\link{calcPlProduction}}
#' @examples
#' \dontrun{
#' a <- calcOutput("PlGaoCabrera2025")
#' }
#' @importFrom magclass collapseDim
calcPlGaoCabrera2025 <- function() {
  # ---------------------------------------------------------------------------
  # Inputs: apparent polymer consumption (ISO3 x year x polymer, kt) and the
  # polymer-specific end-use sector distribution (sector x polymer, unitless).
  # ---------------------------------------------------------------------------
  cons <- readSource("GaoCabrera2025", subtype = "consumption")
  # sector_shares carries both sector and polymer as name subdims (sector.polymer)
  # with only dummy spatial/temporal dims; collapse those so it broadcasts cleanly.
  shares <- collapseDim(readSource("GaoCabrera2025", subtype = "sector_shares", convert = FALSE))

  # ---------------------------------------------------------------------------
  # Outer product cons x shares -> absolute consumption per polymer and sector.
  # `cons` has name dim polymer; `shares` has name dims sector.polymer. The
  # product matches on the shared polymer subdim and broadcasts cons over sector
  # (and the single shares value over region/year).
  # ---------------------------------------------------------------------------
  absolute <- cons * shares / 1000 # (region, year, polymer.sector), kt -> Mt

  # Merge LLDPE into LDPE (report both polyethylenes together as LDPE)
  polyMap <- data.frame(from = getItems(absolute, dim = "polymer"))
  polyMap$to <- ifelse(polyMap$from == "LLDPE", "LDPE", polyMap$from)
  absolute <- toolAggregate(absolute, rel = polyMap, dim = 3.1, from = "from", to = "to")
  # TODO Other fibre category only includes acrylic fibres; however, for remind-mfa,
  # this includes also PP fibre and Elastane (PU fibre) which need to be subtracted from PP and PUR

  # ---------------------------------------------------------------------------
  # Backcast data to 1950 using Geyer et al. (2023) global polymer consumption data
  # ---------------------------------------------------------------------------
  geyer <- readSource("Geyer", subtype = "Prod_1950-2015", convert = FALSE)
  absoluteBackcasted <- toolBackcastByReference(absolute, geyer)

  # ---------------------------------------------------------------------------
  # Add the type subdim (Fibre / Rubber / Plastics) by splitting on the polymer
  # groups.
  # ---------------------------------------------------------------------------
  fibrePolymers <- c("PET fibre", "Polyamide fibre", "Other fibre")
  rubberPolymers <- "Rubbers"
  plasticsPolymers <- setdiff(getItems(absoluteBackcasted, dim = "polymer"), c(fibrePolymers, rubberPolymers))

  tagType <- function(polys, typeName) {
    add_dimension(mselect(absoluteBackcasted, polymer = polys), dim = 3.1, add = "type", nm = typeName)
  }
  absTyped <- mbind(
    tagType(plasticsPolymers, "Plastics"),
    tagType(fibrePolymers, "Fibre"),
    tagType(rubberPolymers, "Rubber")
  )

  return(list(
    x = absTyped,
    weight = NULL,
    unit = "Mt",
    description = paste(
      "Apparent consumption per polymer and end-use sector, from Gao & Cabrera-Serrenho (2025)."
    )
  ))
}
