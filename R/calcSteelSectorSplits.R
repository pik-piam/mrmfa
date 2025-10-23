#' @author Merlin Jo Hosak
#' @export
calcSteelSectorSplits <- function(subtype) {
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    'Pauliuk2013' = function() {
      pauliuk <- readSource('Pauliuk2013', subtype='sectorSplits', convert=F)
      
      final <- list(x = pauliuk, 
                    weight = NULL,
                    unit=1,
                    description='Cooper 2014 Steel Lifetimes Mean & SD')
      
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