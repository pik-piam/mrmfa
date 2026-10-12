#' Ren et al. 2025 China plastics production and consumption in IAMC variables
#'
#' @description
#' Return the China plastics production and apparent consumption from Ren et al.
#' (2025, \doi{10.1038/s41597-025-06363-0}), reduced to totals and reported under
#' a subset of IAMC variables in Mt/yr. Building on \code{\link{calcPlRen2025}}
#' (which splits fibre tonnages off from plastics), three variables are produced:
#' \code{Production|Chemicals|Plastics|Plastics} and
#' \code{Production|Chemicals|Plastics|Fibre} (from the production output, summed
#' over polymers) and \code{Material Demand|Chemicals|Plastics|Plastics} (from the
#' consumption output, summed over polymers and sectors). Ren et al. (2025) only
#' cover China, so only CHN carries data; all other countries are 0.
#'
#' @author Leonie Schweiger
#' @return List with a MagPIE object of China plastics production and consumption
#' (Mt/yr) in IAMC variables and metadata in calcOutput format.
#' @seealso \code{\link{calcPlRen2025}}, \code{\link{calcPlPottinger}},
#' \code{\link{calcPlGaoCabrera}}
#' @importFrom magclass dimSums getItems getItems<- getNames<- collapseDim mbind
calcPlRen2025Validation <- function() {
  # production (t), type = Plastics / Fibre over polymers -> total per type, in Mt
  prod <- calcOutput("PlRen2025", subtype = "production", aggregate = FALSE)
  prod <- dimSums(prod, dim = "polymer") / 1e6 # (region, year, type), t -> Mt
  typeMap <- c(
    Plastics = "Production|Chemicals|Plastics|Plastics",
    Fibre    = "Production|Chemicals|Plastics|Fibre"
  )
  getItems(prod, dim = "type") <- unname(typeMap[getItems(prod, dim = "type")])
  prod <- collapseDim(prod)

  # apparent consumption (t), type = Plastics over polymers & sectors -> total, in Mt
  cons <- calcOutput("PlRen2025", subtype = "consumption", aggregate = FALSE)
  cons <- dimSums(cons, dim = c("polymer", "sector")) / 1e6 # (region, year, type), t -> Mt
  cons <- collapseDim(cons)
  getNames(cons) <- "Material Demand|Chemicals|Plastics|Plastics"

  out <- mbind(prod, cons)

  return(list(
    x = out,
    weight = NULL,
    unit = "Mt/yr",
    description = paste(
      "China plastics production (split into Plastics and Fibre) and apparent",
      "consumption from Ren et al. (2025), reported under IAMC variables in Mt/yr.",
      "Only China (CHN) carries data; other countries are 0."
    ),
    isocountries = TRUE,
    min = 0
  ))
}
