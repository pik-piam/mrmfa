#' Calculate the density of cement products in kg/m3.
#'
#' @author Bennet Weiss
#'
calcCeProductDensity <- function() {
  # Sources
  # nolint start
  # concrete:
  # https://www.oekobaudat.de/OEKOBAU.DAT/resource/sources/bb51b371-e826-43e4-a795-1ba33e20f3a0/Beton_der_Druckfestigkeitsklasse_C_2025_10521.pdf?version=00.02.000
  # mortar:
  # https://oekobaudat.de/OEKOBAU.DAT/datasetdetail/productFlow.xhtml?uuid=0df9d60c-1c62-4a62-b290-567a4626fce6&version=24.01.000
  # nolint end

  rho_concrete <- 2.3 # nolint C20/25
  rho_mortar <- 2.0 # general mortar

  x <- new.magpie(names = c("concrete", "mortar"), fill = c(rho_concrete, rho_mortar))

  unit <- "t/m^3"
  description <- "Density of cement products concrete and mortar."
  note <- "dimensions: (Product Material,value)"

  output <- list(
    x = x,
    weight = NULL,
    unit = unit,
    description = description,
    note = note,
    isocountries = FALSE
  )
  return(output)
}
