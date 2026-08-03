#' Gao & Cabrera-Serrenho 2025 polymer consumption in IAMC variables
#'
#' @description
#' Return total apparent polymer consumption from Gao & Cabrera-Serrenho (2025),
#' "Global Plastic MFA", at ISO3 country level. The 14 polymer groups (plastics,
#' fibres and rubber) are summed into a single total and mapped to the IAMC
#' variable \code{Material Demand|Chemicals|Plastics} in Mt/yr. Note that the
#' years 2020-2021 rely on incomplete trade data.
#'
#' @author Leonie Schweiger
#' @return Total polymer consumption (Mt/yr)
#' @seealso \code{\link{readGaoCabrera2025}}, \code{\link{calcPlPottinger}}
#' @importFrom magclass dimSums getNames<-
calcPlGaoCabrera <- function() {
  x <- readSource("GaoCabrera2025", subtype = "consumption")

  # sum all 14 polymers into a single total and convert kt -> Mt
  x <- dimSums(x, dim = 3) / 1000

  getNames(x) <- "Material Demand|Chemicals|Plastics"

  return(list(
    x = x,
    weight = NULL,
    unit = "Mt/yr",
    description = paste(
      "Apparent polymer consumption 1978-2021 from Gao & Cabrera-Serrenho (2025),",
      "total over all 14 polymer groups (incl. fibres and rubber), in the IAMC",
      "variable Material Demand|Chemicals|Plastics. Years 2020-2021 rely on",
      "incomplete trade data.",
      "Disaggregated from 8 Gao regions to ISO3 via population weighting."
    )
  ))
}
