#' Read China_PlasticEoL Data into a magpie Object
#'
#' This function reads data on plastics EoL treatment in China from different sources
#'
#' @return magpie object of the China Plastics EoL Data
#'
#' @author Leonie Schweiger
#'
#' @seealso [readSource()]
#'
#' @examples
#' \dontrun{
#' a <- readSource(type = "China_PlasticEoL")
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr select
#' @importFrom tidyr pivot_longer
#'
readChina_PlasticEoL <- function() {
  # ---------------------------------------------------------------------------
  # Read raw data from excel
  raw_df <- read_excel(
    path  = "PlasticEol.xlsx",
    sheet = "PlasticEol"
  )

  # ---------------------------------------------------------------------------
  # Clean and pivot data
  df <- raw_df %>%
    select(-"Source") %>%
    pivot_longer(
      cols = -"Year",
      names_to = "Treatment",
      values_to = "Value"
    )

  # ---------------------------------------------------------------------------
  # Convert to magpie object and clean missing values
  # ---------------------------------------------------------------------------
  magpie_data <- as.magpie(df, temporal = 1)

  magpie_data[is.na(magpie_data)] <- 0
  getItems(magpie_data, dim=1) <- "CHA"

  return(magpie_data)
}

