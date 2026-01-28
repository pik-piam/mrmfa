#' Read HS Comtrade product classification codes for plastics products from
#' UNCTAD
#'
#' @return magpie object of the UNEP NGP HS Codes
#'
#' @param subtype Character string specifying specifying the year of the HS (Harmonized System) revision of the data, available are
#'        - 02
#'        - 07
#'        - 12
#'        - 17
#'        - 22
#'
#' @author Leonie Schweiger
#'
#' @seealso [readSource()]
#'
#' @examples
#' \dontrun{
#' a <- readSource(type = "UNCTAD_PlasticsHSCodes")
#' }
#' @importFrom readxl read_excel
#'
readUNCTAD_PlasticsHSCodes <- function(subtype) {

  # HS Codes are revised every 5 years; if an older revision than 2002 is used, use the oldest available (2002)
  if(!(subtype %in% c("02","07","12","17","22"))){
    subtype = "02"
  }
  UNCTAD_path <- paste0("DimHS20",subtype,"Products_Plastics_Hierarchy.xls")
  UNCTAD_product_codes <- read_excel(UNCTAD_path, skip = 2)
  # Identify header rows
  is_header <- grepl("^P_", UNCTAD_product_codes[[1]])
  # Create a new variable from column 2 of header rows
  UNCTAD_product_codes$Group <- UNCTAD_product_codes[[2]][is_header][cumsum(is_header)]
  # Remove the header rows
  product_groups <- UNCTAD_product_codes[!is_header, ]
  product_groups$Code <- as.integer(product_groups$Code)

  # ---------------------------------------------------------------------------
  # Convert to magpie object
  # ---------------------------------------------------------------------------
  magpie_data <- as.magpie(product_groups, spatial=0, temporal=0)

  return(magpie_data)
}
