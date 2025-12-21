#' Calculate kg Co2 emissions generated per one kg of CaO
#' @description Calculates the stoichiometric CO2 emission factor for Calcium Oxide (CaO) production.
#' @details
#' The emission factor is derived from the reaction CaCO3 -> CaO + CO2.
#' Atomic weights are based on IUPAC Conventional Atomic Weights.
#' - Carbon (C): 12.011 g/mol
#' - Oxygen (O): 15.999 g/mol
#' - Calcium (Ca): 40.078 g/mol
#'
#' @author Bennet Weiss
calcCeCaOEmissionFactor <- function() {
  # Atomic weights (g/mol) based on IUPAC standard
  mw_C <- 12.011
  mw_O <- 15.999
  mw_Ca <- 40.078

  # Molecular weights
  mw_CO2 <- mw_C + (2 * mw_O)
  mw_CaO <- mw_Ca + mw_O

  # Calculate stoichiometric ratio (kg CO2 / kg CaO)
  ef_CaO <- mw_CO2 / mw_CaO

  out <- new.magpie(fill = ef_CaO)

  unit <- "ratio"
  description <- "Kg CO2 emitted per kg of CaO produced based on stoichiometric calculations."
  note <- "dimensions: (value)"

  output <- list(
    x = out,
    weight = NULL,
    unit = unit,
    description = description,
    note = note,
    isocountries = FALSE
  )
  return(output)
}
