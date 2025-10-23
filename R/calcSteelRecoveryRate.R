#' @author Merlin Jo Hosak
#' @export
calcSteelRecoveryRate <- function(subtype) {
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    'WorldSteel' = function() {
      ws <- readSource('WorldSteelParameters', subtype='recoveryRate')
      
      final <- list(x = ws, 
                    weight = NULL,
                    unit=1,
                    description='World Steel Association steel scrap recovery rate')
      
      return(final)
    },
    NULL)
  # ---- check if the subtype called is available ----
  if (is_empty(intersect(subtype, names(switchboard)))) {
    stop(paste('Invalid subtype -- supported subtypes are:',
               names(switchboard)))
  } else {
    # ---- load data and do whatever ----
    return(switchboard[[subtype]]())
  }
}