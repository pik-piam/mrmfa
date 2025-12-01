#' calculate Steel Recovery Rate
#'
#' @author Merlin Jo Hosak
calcStRecoveryRate <- function(subtype) {
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    "WorldSteel" = function() {
      ws <- readSource("WorldSteelParameters", subtype = "recoveryRate")

      final <- list(
        x = ws,
        weight = NULL,
        unit = 1,
        isocountries = FALSE,
        description = "World Steel Association steel scrap recovery rate",
        note = "dimensions: (Good,value)"
      )

      return(final)
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
