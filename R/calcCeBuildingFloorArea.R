#' Calculates relative floor area of SF, MF and NR buildings by structure type.
#'
#' @param subtype Floor Area grouped by: "Stock_Type", "Structure", "Function", NULL (i.e. all).
#' @author Bennet Weiss
calcCeBuildingFloorArea <- function(subtype = NULL) {
  possible_subtypes <- c("Stock_Type", "Structure", "Function")

  data <- readSource("GEM")

  # transform to df for easier handling
  df <- as.data.frame(data, rev = 3)
  # remove Ind buildings
  df <- df[df$Stock_Type != "Ind", ]
  floorArea <- as.magpie(df, spatial = 1)

  if (is.null(subtype)){
    x_out <- floorArea
  } else if (all(subtype %in% possible_subtypes)) {
    df_byType <- group_by(df, ISO3, across(all_of(subtype))) %>%
      summarise(TOTALAREA_SQM = sum(.value, na.rm = TRUE), .groups = "drop")
    floorArea_byType <- as.magpie(df_byType, spatial = 1)

    x_out <- floorArea_byType
  } else {
    stop(paste("Invalid subtype: must be one of ", paste(possible_subtypes, collapse = ", ")))
  }

  # output
  x_out[is.na(x_out)] <- 0
  weight <- NULL
  unit <- "square meter"
  description <- paste(
    "Total buildings floor area by ",
    paste(setdiff(possible_subtypes, subtype), collapse = " and "), ".",
    "Data from Global Exposure Model (GEM), categories harmonized with RASMI.",
    "Yepes-Estrada, C., Calderon, A., Costa, C., Crowley, H., Dabbeek, J., Hoyos, M., Martins, L., Paul, N., Rao, A., Silva, V. (2023).",
    "Global Building Exposure Model for Earthquake Risk Assessment. Earthquake Spectra. doi:10.1177/87552930231194048."
  )
  output <- list(
    x = x_out,
    weight = weight,
    unit = unit,
    description = description
  )
}
