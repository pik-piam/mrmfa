#' Calculate the ratio of cement mass to total product mass for concrete and mortar.
#'
#' For each material, the cement ratio is computed as the application-split-weighted
#' average of (cement content / product density) across all applications of that material.
#' Cement content data (kg cement / m3 product) and application splits are from Cao2024.
#' Density values from calcCeProductDensity.
#'
#' @author Bennet Weiss
#'
calcCeCementRatio <- function() {
  cement_content <- calcOutput("CeProductCementContent", aggregate = FALSE)
  application_split <- calcOutput("CeProductApplicationSplit", aggregate = FALSE)
  density <- calcOutput("CeProductDensity", aggregate = FALSE)

  concrete_apps <- c("C15", "C20", "C30", "C35")
  mortar_apps <- c("finishing", "masonry", "maintenance")

  # cement_ratio(m) = sum_{a in m} (cement_content(a) / density(m)) * application_split(a)
  # application_split is already normalized to sum to 1 within each material group
  concrete_ratio <- dimSums(
    cement_content[, , concrete_apps] / density[, , "concrete"] * application_split[, , concrete_apps],
    dim = "Product Application"
  )
  mortar_ratio <- dimSums(
    cement_content[, , mortar_apps] / density[, , "mortar"] * application_split[, , mortar_apps],
    dim = "Product Application"
  )

  getNames(concrete_ratio) <- "concrete"
  getNames(mortar_ratio) <- "mortar"
  x <- mbind(concrete_ratio, mortar_ratio)
  weight <- toolCeCumulativeCementProduction(castto = x)

  output <- list(
    x = x,
    weight = weight,
    unit = "ratio",
    description = paste(
      "Ratio of cement mass to total product mass for concrete and mortar.",
      "Computed as the application-split-weighted average of cement content",
      "divided by product density across applications within each material.",
      "Data from Cao2024."
    ),
    note = "dimensions: (Region,Product Material,value)",
    isocountries = FALSE
  )
  return(output)
}
