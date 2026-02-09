#' Read HS Comtrade product classification codes for plastics products and their plastics percentage from
#' Tool T1.4 of the UNEP National Guidance for Plastic Pollution Hotspotting and Shaping Action
#'
#' @return magpie object of the UNEP NGP HS Codes
#'
#' @param subtype whether to return 4 digit or 6 digit codes or all
#'        - k4
#'        - k6
#'        - all
#'
#' @author Leonie Schweiger
#'
#' @seealso [readSource()]
#'
#' @examples
#' \dontrun{
#' a <- readSource(type = "UNEP_NGP")
#' }
#' @importFrom readxl read_excel
#'
readUNEP_NGP <- function(subtype) {

  # Read selected COMTRADE codes from UNEP_NGP
  SelectedCOMCodes <- read_excel("TOOL_T1.4a_v1.2_Trade data modelling.xlsx",
                                 sheet = "SelectedCOMCodes", skip = 12) %>%
    dplyr::rename(code = "Code", polymer = "Polymer Type", application = "Application Type", stage = "Type", plastic_percentage = "Plastic percentage", sector = "Sector", label = "Extensive description on comtrade") %>%
    dplyr::select("code", "polymer", "application", "stage", "sector", "label", "plastic_percentage")
  # remove duplicates in raw data
  SelectedCOMCodes <- unique(SelectedCOMCodes)

  # ---------------------------------------------------------------------------
  # Convert to magpie object
  # ---------------------------------------------------------------------------
  if(subtype=="k4"){
    magpie_data <- as.magpie(SelectedCOMCodes%>% filter(.data$code<10000), spatial=0, temporal=0)
  }
  if(subtype=="k6"){
    magpie_data <- as.magpie(SelectedCOMCodes%>% filter(.data$code>10000), spatial=0, temporal=0)
  }
  if(subtype=="all"){
    magpie_data <- as.magpie(SelectedCOMCodes, spatial=0, temporal=0)
  }

  return(magpie_data)
}
