#' Calculate total floorspace of buildings from GEM, optionally grouped by subtype.
#'
#' @param subtype str or list(str) or None, Categories by which floorspace is resolved. Other dimensions are aggregated.
#'                Possible entries: "Stock_Type", "Structure", "Function", NULL (i.e. all). Defaults to NULL.
#' @param remove_ind Logical, if TRUE (default), industrial buildings are removed from the dataset.
#' @author Bennet Weiss
calcCeFloorspaceGEM <- function(subtype = NULL, remove_ind = TRUE) {
  possible_subtypes <- c("Stock_Type", "Function", "Structure")

  floorArea <- readSource("GlobalExposureModel")

  if (remove_ind) {
    floorArea <- mselect(floorArea, "Stock_Type" = c("Res", "Com"))
    floorArea <- mselect(floorArea, "Function" = c("Com", "RM", "RS"))
  }

  if (is.null(subtype)){
    x_out <- floorArea
  } else if (all(subtype %in% possible_subtypes)) {
    dims_to_sum <- setdiff(possible_subtypes, subtype)
    x_out <- dimSums(floorArea, dim = dims_to_sum, na.rm = T)
  } else {
    stop(paste("Invalid subtype: must be one of ", paste(possible_subtypes, collapse = ", ")))
  }

  # sort dimensions in expected order given by possible_subtypes
  current_dim_order <- getSets(x_out, fulldim = TRUE)[-(1:2)] # exclude region and time dim
  target_order <- match(subtype, current_dim_order)
  x_out <- dimOrder(x_out, perm = target_order, dim = 3)

  # output
  subtype <- gsub("_", " ", subtype)
  x_out[is.na(x_out)] <- 0
  weight <- NULL
  unit <- "square meter"
  description <- paste(
    "Total buildings floor area by ",
    paste(subtype, collapse = " and "), ".",
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
