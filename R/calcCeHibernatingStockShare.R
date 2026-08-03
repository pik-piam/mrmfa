#' Calculate share of hibernating (unused but not demolished) building stock.
#'
#'
#' @author Bennet Weiss
calcCeHibernatingStockShare <- function() {
  weight <- dimSums(calcOutput("CeFloorspaceEDGEB", aggregate = FALSE, smooth = TRUE)[, 2023], dim = 3)

  share_vacant <- readSource("OECDAffordableHousingDatabase", subtype = "vacant") / 100

  # use approximate based total share wherever no direct vacant share is available
  share_total <- readSource("OECDAffordableHousingDatabase", subtype = "both") / 100
  ratio <- share_vacant / share_total
  avg_vacant_on_total_share <- (
    dimSums(ratio * weight, na.rm = TRUE, dim = 1)
    / sum(weight[!is.na(ratio)])
  )
  share_vacant[is.na(share_vacant)] <- share_total[is.na(share_vacant)] * as.vector(avg_vacant_on_total_share)

  # set rest of world to average vacant share (excluding China)
  share_vacant[is.na(share_vacant)] <- (
    dimSums(share_vacant * weight, na.rm = TRUE, dim = 1)
    / sum(weight[!is.na(share_vacant)], na.rm = TRUE)
  )

  # set China manually based on:
  # Zheng, H., Zhang, R., Yin, X. et al.
  # Unused housing in urban China and its carbon emission impact.
  # Nat Commun 16, 1985 (2025). https://doi.org/10.1038/s41467-025-57217-7
  share_vacant["CHN", ] <- 0.174

  unit <- "ratio"
  description <- paste0(
    c(
      "Share of total building stock that is hibernating (built but unused and not demolished).",
      "For China, based on:",
      "Zheng, H., Zhang, R., Yin, X. et al.",
      "Unused housing in urban China and its carbon emission impact.",
      "Nat Commun 16, 1985 (2025). https://doi.org/10.1038/s41467-025-57217-7",
      "ROW based on:",
      "OECD (2024), OECD Affordable Housing Database - indicator HM1.1. Housing stock and construction,",
      "https://oe.cd/ahd"
    ),
    collapse = "\n"
  )
  note <- "dimensions: (Region,value)"

  output <- list(
    x = share_vacant,
    weight = weight,
    unit = unit,
    description = description,
    note = note,
    min = 0,
    max = 1
  )
  return(output)
}
