#' Calc World Steel Static Parameters
#' @description
#' Load static (singular) parameters based on World Steel Association data.
#' See \link{readWorldSteelParameters} and the
#' WorldSteelParameters folder for more information.
#' @param subtype Parameter to load. Currently only 'scrapInBOFRate' available.
#' @author Merlin Jo Hosak
#' @export
calcStWorldSteelStaticParameters <- function(subtype) {
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
  "scrapInBOFrate" = function() {
      scrapInBOFRate <- readSource("WorldSteelParameters", subtype = "scrapInBOFRate")

      final <- list(
        x = scrapInBOFRate,
        weights = NULL,
        description = "Scrap in BOF rate (according to the World Steel Association)",
        unit = 1
      )
      return(final)
    },
    NULL
  )
  # ---- check if the subtype called is available ----
  if (is_empty(intersect(subtype, names(switchboard)))) {
    stop(paste(
      "Invalid subtype -- supported subtypes are:",
      names(switchboard)
    ))
  } else {
    # ---- load data and do whatever ----
  return(switchboard[[subtype]]())
  }
}
