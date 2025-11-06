#' Read UNCTAD
#'
#' Read-in US_PlasticsTradebyPartner file from the
#' United Nations Conference on Trade and Development (UNCTAD).
#' Data is filtered for Trading Partner == World, i.e. all trading partners aggregated.
#'
#' @return magpie object of the UNCTAD data
#'
#' @author Qianzhi Zhang, Leonie Schweiger
#'
#' @seealso [readSource()]
#'
#' @examples
#' \dontrun{
#' a <- readSource(type = "UNCTAD")
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom readr read_csv
#'
readUNCTAD <- function() {
  # ---------------------------------------------------------------------------
  # Read Data from Excel
  # ---------------------------------------------------------------------------
  data <- read_csv("US_PlasticsTradebyPartner.csv") %>%
    select(1,3,5,7,9,13) %>%
    dplyr::rename(Region = "Economy Label", Flow = "Flow Label", Product = "Product Label") %>%
    filter(.data$`Partner Label`=="World") %>%
    select(-"Partner Label")

  data <- as.magpie(data, temporal = 1, spatial = 2)

  # ---------------------------------------------------------------------------
  # Replace any NA values in the magpie object with 0.
  # ---------------------------------------------------------------------------
  data[is.na(data)] <- 0

  # ---------------------------------------------------------------------------
  # Return the Processed MagPIE Object
  # ---------------------------------------------------------------------------
  return(data)
}
