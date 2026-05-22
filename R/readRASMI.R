#' Read material intensity (MI) from RASMI.
#' MI is a function of use/function type (RS, RM, NR) and structure type (S, T, C, M)
#'
#' Fishman, T., Mastrucci, A., Peled, Y. et al.
#' RASMI: Global ranges of building material intensities differentiated by region, structure, and function.
#' Sci Data 11, 418 (2024). https://doi.org/10.1038/s41597-024-03190-7
#' Data available on Zenodo: https://zenodo.org/records/10124952
#' Data available on Github: https://github.com/TomerFishman/MaterialIntensityEstimator
#'
#' @param subtype Material subtype. Possible values are "concrete", "steel", "plastics".
#' @author Bennet Weiss.
readRASMI <- function(subtype = "concrete") {
  path <- file.path("v20230905", "MI_ranges_20230905.xlsx")
  df <- readxl::read_xlsx(path, sheet = subtype)
  data <- select(df, "R5_32", "function", "structure", "p_50")
  data <- setNames(data, c("region", "Function", "Structure", "value"))
  data$Function[data$Function == "NR"] <- "Com"
  x <- magclass::as.magpie(data, spatial = 1)
  return(x)
}
