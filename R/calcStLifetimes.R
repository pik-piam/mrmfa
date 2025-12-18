#' Calc Steel Lifetimes
#' @description Function to load steel lifetimes data in years.
#' @param subtype Subtype of steel lifetimes data to load. Currently, only
#' 'Cooper2014' is available. Other options might include Pauliuk or Wittig
#' (see respective folders).
#' @param unit 'mean' to return mean lifetimes, 'std' to return standard deviation
#' @author Merlin Jo Hosak
#' @importFrom purrr is_empty
calcStLifetimes <- function(subtype, unit) {
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    "Cooper2014" = function() {
      cooperLifetimes <- readSource("Cooper2014")
      if (unit == "mean") {
        x <- collapseDim(mselect(cooperLifetimes, unit = "Mean"), dim = 3.2)
      } else if (unit == "std") {
        x <- collapseDim(mselect(cooperLifetimes, unit = "Standard Deviation"), dim = 3.2)
      } else {
        stop(
          "Invalid unit -- supported units are: 'mean', 'std'"
        )
      }
      final <- list(
        x = x,
        weight = NULL,
        unit = 1,
        isocountries = FALSE,
        description = "Cooper 2014 Steel Lifetimes Mean & SD",
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
