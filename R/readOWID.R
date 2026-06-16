#' Read data from Our World in Data.
#'
#' @author Bennet Weiss
#' @param subtype Character string specifying the data and subfolder name to be read in. Supported:
#'       - "shipping_costs": Get sea freight costs relative to 1930 in percent.
readOWID <- function(subtype) {
  path <- file.path(subtype, "real-transport-and-communication-costs.csv")
  data <- suppressMessages(read_csv(path))

  if (subtype == "shipping_costs") {
    x <- data[c("Year", "Sea freight cost (relative to 1930)")]
    x <- magclass::as.magpie(x, temporal = 1, datacol = 2)
    getNames(x) <- NULL
    return(x)
  } else {
    stop(paste("Unsupported subtype:", subtype))
  }
}
