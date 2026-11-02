#' Rename the building category codes used internally (from GEM, RASMI, Xi2016 and
#' Posted) to the item names used in the cement MFA.
#'
#' @author Jakob Duerrwaechter
#' @param x magpie object.
#' @param dim Dimension of x in which items are renamed, e.g. 3 or 3.1.
#' Items without a mapping entry are kept.
#' @param type Type of building category. Either "End Use" or "Structure".
toolCeRenameBuildingItems <- function(x, dim, type) {
  mappings <- list(
    "End Use" = c(
      "RS" = "Residential Single-Family",
      "RM" = "Residential Multi-Family",
      "Res" = "Residential",
      "Com" = "Commercial",
      "Ind" = "Industrial",
      "Civ" = "Civil Engineering"
    ),
    "Structure" = c(
      "C" = "Reinforced Concrete Structure",
      "M" = "Masonry Structure",
      "T" = "Timber Structure",
      "S" = "Steel Frame Structure",
      "U" = "Unspecified Structure"
    )
  )
  if (!type %in% names(mappings)) {
    stop(paste("Invalid type: must be one of", paste(names(mappings), collapse = ", ")))
  }
  mapping <- mappings[[type]]

  items <- getItems(x, dim = dim)
  toRename <- items %in% names(mapping)
  items[toRename] <- unname(mapping[items[toRename]])
  getItems(x, dim = dim) <- items
  return(x)
}
