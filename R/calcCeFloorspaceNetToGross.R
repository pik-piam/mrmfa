#' Calculate the conversion factor from net to gross floorspace
#'
#' Constant factor of 1.25 for all regions and common end uses, based on Röck et al. (2020).
#'
#' @author Bennet Weiss
calcCeFloorspaceNetToGross <- function() {
  # Röck, M., Saade, M. R. M., Balouktsi, M. et al.
  # Embodied GHG emissions of buildings - The hidden challenge for effective climate change mitigation.
  # Applied Energy 258, 114107 (2020). https://doi.org/10.1016/j.apenergy.2019.114107
  end_uses <- c("Residential", "Commercial")
  x <- new.magpie(cells_and_regions = getISOlist(), years = NULL, names = end_uses, fill = 1.25)
  # uniform weight, so that regional aggregation keeps the constant value
  weight <- new.magpie(cells_and_regions = getISOlist(), years = NULL, names = end_uses, fill = 1)

  unit <- "ratio"
  description <- paste0(
    c(
      "Conversion factor from net to gross floorspace, equal for all regions and common end uses.",
      "Based on:",
      "Röck, M., Saade, M. R. M., Balouktsi, M. et al.",
      "Embodied GHG emissions of buildings - The hidden challenge for effective climate change mitigation.",
      "Applied Energy 258, 114107 (2020). https://doi.org/10.1016/j.apenergy.2019.114107"
    ),
    collapse = "\n"
  )
  note <- "dimensions: (Region,Common End Use,value)"

  output <- list(
    x = x,
    weight = weight,
    unit = unit,
    description = description,
    note = note,
    min = 1
  )
  return(output)
}
