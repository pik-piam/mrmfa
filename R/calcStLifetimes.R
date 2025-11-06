#' Calc Steel Lifetimes
#' @description Function to load steel lifetimes data in years. 
#' @param subtype Subtype of steel lifetimes data to load. Currently only 
#' 'Cooper2014' is available. Other options might include Pauliuk or Wittig (
#' see respective folders).
#' @author Merlin Jo Hosak
#' @param subtype TODOMERLIN: document
#' @importFrom purrr is_empty
#' @export
calcStLifetimes <- function(subtype) {
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    'Cooper2014' = function() {
      cooperLifetimes <- readSource('Cooper2014', subtype='lifetimes')
      
      final <- list(x = cooperLifetimes, 
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
