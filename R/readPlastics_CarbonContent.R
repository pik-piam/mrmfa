#' Read Carbon Content for HVCs or plastics (from stochiometric calculations for defined polymers and
#' rough estimates for broader classes)
#'
#' @return magpie object of the Carbon Content for different plastic types/polymers or HVC
#'
#' @param subtype plastics or HVC
#'
#' @author Leonie Schweiger
#'
#' @seealso [readSource()]
#'
#' @examples
#' \dontrun{
#' a <- readSource(type = "Plastics_CarbonContent", subtype="plastics")
#' }
#'
readPlastics_CarbonContent <- function(subtype) {
  # ---------------------------------------------------------------------------
  # Read raw data from Excel
  switchboard <- list(
    "plastics" = function() {
      df <- read_excel(
        path  = file.path("v2.0", "Plastics_CarbonContent.xlsx"),
        sheet = "final",
        range = "A1:B13"
      )
      x <- as.magpie(df)
      return(x)
    },
    "HVC" = function() {
      df <- read_excel(
        path  = file.path("v2.0", "Plastics_CarbonContent.xlsx"),
        sheet = "HVC",
        range = "A1:B8"
      )
      x <- as.magpie(df)
      return(x)
    }
  )

  # ---- check if the subtype called is available ----
  if (is_empty(intersect(subtype, names(switchboard)))) {
    stop(
      "Invalid subtype -- supported subtypes are:",
      paste0(names(switchboard), collapse = ", ")
    )
  } else {
    # ---- load data and do whatever ----
    return(switchboard[[subtype]]())
  }
}
