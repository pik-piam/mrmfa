#' @author Merlin Jo Hosak
#' @importFrom purrr is_empty
#' @export
calcSteelLifetimes <- function(subtype) {
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    'Cooper2014' = function() {
      cooper <- readSource('Cooper2014', subtype='lifetimes')

      final <- list(x = cooper,
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
