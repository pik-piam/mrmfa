#' Calculate the split of product material mass across product applications by region.
#'
#' Each material (concrete/mortar) is mapped to it's respective product applications.
#'
#' @author Bennet Weiss
#'
calcCeMaterialApplicationSplit <- function() {
  # Application split normalized within material groups (r, Product Application)
  x <- calcOutput("CeProductApplicationSplit", aggregate = FALSE)

  concrete_apps <- c("C15", "C20", "C30", "C35")
  mortar_apps <- c("finishing", "masonry", "maintenance")

  # concrete: keep concrete app values, zero mortar apps
  x_concrete <- x
  x_concrete[, , mortar_apps] <- 0
  x_concrete <- add_dimension(x_concrete, dim = 3.1, add = "Product Material", nm = "concrete")

  # mortar: keep mortar app values, zero concrete apps
  x_mortar <- x
  x_mortar[, , concrete_apps] <- 0
  x_mortar <- add_dimension(x_mortar, dim = 3.1, add = "Product Material", nm = "mortar")

  x <- mbind(x_concrete, x_mortar)

  weight <- toolCeCumulativeCementProduction(castto = x)

  output <- list(
    x = x,
    weight = weight,
    unit = "fraction",
    description = paste(
      "Split of product material mass across product applications by region.",
      "Within-material application splits from Cao2024.",
      "Each material row sums to 1 per region; applications of the other material are 0."
    ),
    note = "dimensions: (Region,Product Material,Product Application,value)",
    isocountries = TRUE
  )
  return(output)
}
