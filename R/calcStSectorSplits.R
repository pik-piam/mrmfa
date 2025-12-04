#' Calc steel sector splits
#' @description
#' Function to load steel sector splits data in the four main end-use sectors
#' (construction, machinery, products, transport). Currently summarized data
#' from Pauliuk et al. (2013) is used that is only available for the US in
#' 2004, India in 1995-1999 and the UK in 1960-65, 1970, 1974-79. These are
#' based on the American Iron and Steel Institute, Spark Steel & Economy
#' Research, the Iron and Steel Statistics Bureau and Dahlström et al. (2004)
#' (see Metadata in File). See \link{readPauliuk2013} for
#' preprocessing.
#' @param subtype Sector split source, currently using Pauliuk  et al. (2013).
#' Other options is to use Pauliuk et al.'s stock sector splits or other
#' MFAs.
#' @author Merlin Jo Hosak
calcStSectorSplits <- function(subtype = "Pauliuk2013") {
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    "Pauliuk2013" = function() {
      x <- readSource("Pauliuk2013")

      final <- list(
        x = x,
        weight = NULL,
        unit = 1,
        isocountries = FALSE,
        description = "Cooper 2014 Steel Lifetimes Mean & SD"
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
