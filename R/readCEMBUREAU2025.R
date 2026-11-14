#' Read cement end-use shares for the EU27 from CEMBUREAU (2025)
#'
#' CEMBUREAU - The European Cement Association: "Key facts & figures", June 2025,
#' \url{https://cementeurope.eu/media/4t2ms0xx/key-fact-figures-publication-june-2025.pdf}.
#' Chart "End uses EU27 - Cement consumption 2023" (PDF p. 6). The values are transcribed in
#' \code{values.csv}; see \code{SOURCE_INFO.txt}.
#'
#' Building renovation is split between residential and non-residential buildings in proportion to
#' their new-building shares. Civil engineering renovation is added to civil engineering.
#' The category "Unknown" is dropped.
#'
#' @author Bennet Weiss
#' @return magpie object of end-use shares in percent for the region "EU27", not normalised.
readCEMBUREAU2025 <- function() {
  data <- utils::read.csv(file.path("v1", "values.csv"))
  categoryMapping <- c(
    "Residential building new" = "Residential",
    "Non-residential building new" = "Non-Residential",
    "Building renovation" = "Building renovation",
    "Civil engineering new" = "Civil Engineering",
    "Civil engineering renovation" = "Civil Engineering",
    "Unknown" = NA
  )
  x <- toolCeEndUseTable(data, categoryMapping)

  # distribute building renovation proportionally to the new-building shares
  buildings <- c("Residential", "Non-Residential")
  buildingShares <- x[, , buildings] / dimSums(x[, , buildings], dim = 3)
  x[, , buildings] <- x[, , buildings] + x[, , "Building renovation"] * buildingShares
  x <- x[, , "Building renovation", invert = TRUE]
  return(x)
}
