#' OECD Global Plastics Outlook 2022 projections in IAMC variables
#'
#' @description
#' Return the plastics-use projection 1980-2060) from the OECD Global Plastics Outlook 2022 
#' at ISO3 country level, mapped to IAMC variables.
#' \code{subtype = "total"} returns total plastics use as
#' \code{Material Demand|Chemicals|Plastics} (Mt/yr); \code{subtype = "perCapita"}
#' returns \code{Material Demand|Chemicals|Plastics|per capita} (kg/cap), using the
#' OECD population projection where available (2019-2060) and \code{\link{calcCoPopulation}}
#' (SSP2) for the earlier years (1980-2018).
#'
#' @param subtype Character, either "total" (plastics use, Mt/yr, summed on aggregation) or
#' "perCapita" (plastics use per capita, kg/cap, population-weighted mean on aggregation).
#'
#' @author Leonie Schweiger
#' @return List with a MagPIE object of the OECD plastics projection in IAMC variables and
#' metadata.
#' @seealso \code{\link{readOECD_Plastic}}, \code{\link{calcPlGaoCabrera}},
#' \code{\link{calcCoPopulation}}
#' @importFrom magclass setNames getYears getNames<-
calcPlOECDProjection <- function(subtype = "total") {
  # plastics use projection (Mt), ISO3, 1980-2060
  use <- setNames(readSource("OECD_Plastic", "Use_1980-2060_projection"), NULL)

  if (subtype == "total") {
    getNames(use) <- "Material Demand|Chemicals|Plastics"
    return(list(
      x = use,
      weight = NULL,
      unit = "Mt/yr",
      description = paste(
        "Plastics use projection 1980-2060 from the OECD Global Plastics Outlook 2022.",
        "Disaggregated from 15 OECD-Outlook regions to ISO3 via population weighting."
      )
    ))
  } else if (subtype == "perCapita") {
    # OECD population sheet (billion people), ISO3, 2019-2060
    popOECD <- setNames(readSource("OECD_Plastic", "Pop_2019-2060_projection"), NULL)
    # reference population (calcCoPopulation is in inhabitants -> convert to billion people)
    popRef <- setNames(calcOutput("CoPopulation", scenarios = "SSP2", aggregate = FALSE), NULL) / 1e9

    years <- getYears(use)                          # 1980-2060
    pop <- popRef[, years, ]                         # CoPopulation for all years...
    overlap <- intersect(years, getYears(popOECD))   # 2019-2060
    pop[, overlap, ] <- popOECD[, overlap, ]         # ...prefer OECD source pop where available

    perCap <- use / pop                              # Mt / billion people = kg/cap
    perCap[is.na(perCap) | is.infinite(perCap)] <- 0
    getNames(perCap) <- "Material Demand|Chemicals|Plastics|per capita"
    return(list(
      x = perCap,
      weight = pop,
      unit = "kg/cap",
      description = paste(
        "Per-capita plastics use projection 1980-2060 from the OECD Global Plastics",
        "Outlook 2022. OECD population is used for 2019-2060 and ",
        "calcCoPopulation (SSP2) for 1980-2018."
      )
    ))
  }

  stop("Invalid subtype: ", subtype, ". Use 'total' or 'perCapita'.")
}
