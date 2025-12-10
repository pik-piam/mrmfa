#' Calculate the density of cement products in kg/m3.
#'
#' @author Bennet Weiss
#'
calcCeProductDensity <- function() {
  # TODO find a source to read in this data from.
  # https://www.oekobaudat.de/OEKOBAU.DAT/resource/sources/bb51b371-e826-43e4-a795-1ba33e20f3a0/Beton_der_Druckfestigkeitsklasse_C_2025_10521.pdf?version=00.02.000
  rho_concrete <- 2.3 # C20/25
  # https://oekobaudat.de/OEKOBAU.DAT/datasetdetail/productFlow.xhtml?uuid=0df9d60c-1c62-4a62-b290-567a4626fce6&version=24.01.000
  rho_mortar <- 2.0 # general mortar

  # There is much more data:
  # C12/15: https://www.oekobaudat.de/OEKOBAU.DAT/resource/sources/4900c505-9ffa-430c-b1da-556a063aad06/Beton_der_Druckfestigkeitsklasse_C1215_18917.pdf?version=00.01.000
  # C16/20: https://www.beton.org/fileadmin/beton-org/media/Dokumente/PDF/Wissen/Beton-Bautechnik/Nachhaltigkeit/2023-10-20-EPD-C16-20.pdf
  # C25/30: https://www.oekobaudat.de/OEKOBAU.DAT/resource/sources/62cc9175-a326-4215-bbbb-5bc6e3467a84/Beton_der_Druckfestigkeitsklasse_C_2530_18765.pdf?version=00.04.000
  # C30/37: https://www.oekobaudat.de/OEKOBAU.DAT/resource/sources/dc600cd9-9a55-414e-baf5-f36c54269803/Beton_der_Druckfestigkeitsklasse_C_3037_10616.pdf?version=00.03.000
  # C35/45: https://oekobaudat.de/OEKOBAU.DAT/resource/sources/5bf98353-346e-4f9c-bd06-1ac27ea1cdf0/EPD_IZB_2013411_C35_45_D.pdf?version=00.01.000
  # C50/60: https://www.oekobaudat.de/OEKOBAU.DAT/resource/sources/892ca341-33e1-4136-81b9-ccd8418e9d73/Beton_der_Druckfestigkeitsklasse_C_5060_10526.pdf?version=00.02.000

  x <- new.magpie(names = c("concrete", "mortar"), fill = c(rho_concrete, rho_mortar))

  unit <- "t/m³"
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
