#' Read Plastics Carbon Content from stochiometric calculations for defined polymers and
#' rough estimates for broader classes
#'
#' @return magpie object of the Carbon Content for different plastic types/polymers
#'
#' @author Leonie Schweiger
#'
#' @seealso [readSource()]
#'
#' @examples
#' \dontrun{
#' a <- readSource(type = "Plastics_CarbonContent")
#' }
#'
readPlastics_CarbonContent <- function() {
  # ---------------------------------------------------------------------------
  # Read raw data from Excel
  df <- read_excel(
    path  = "Plastics_CarbonContent.xlsx",
    sheet = "final",
    range = "A1:B13"
  )

  # ---------------------------------------------------------------------------
  # Convert to magpie object
  magpie_data <- as.magpie(df)

  return(magpie_data)
}
