#' Calculate share of hibernating (unused but not demolished) building stock.
#'
#'
#' @author Bennet Weiss
calcCeHibernatingStockShare <- function() {
  weight <- dimSums(calcOutput("CeFloorspace", aggregate = FALSE)[, 2023], dim = 3)

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
  # Zhang, C., Yang, L., Wiedenhofer, D. et al.
  # Building material stock drives embodied carbon emissions and risks future climate goals in China.
  # Nat. Clim. Chang. 16, 164-171 (2026). https://doi.org/10.1038/s41558-025-02527-3
  share_vacant["CHN", ] <- 0.174

  unit <- "ratio"
  description <- paste0(
    c(
      "Share of total building stock that is hibernating (built but unused and not demolished).",
      "For China, based on:",
      "# Zhang, C., Yang, L., Wiedenhofer, D. et al.",
      "Building material stock drives embodied carbon emissions and risks future climate goals in China.",
      "Nat. Clim. Chang. 16, 164-171 (2026). https://doi.org/10.1038/s41558-025-02527-3.",
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
