#' Read data from EUBUCCO
#' This is preliminary and unpublished data.
#' Data received on 30.09.2025, PIK-internal personal communication.
#'
#' @author Bennet Weiss
#' @param subtype Variable to be read in. Currently, only "clinker_ratio" is supported.
readEUBUCCO <- function(subtype = "concrete") {
  path <- file.path("sneak_peak_v1", "EUBUCCO_sneak_peak.xlsx")
  df <- readxl::read_xlsx(path, sheet = "data")

  # remove unnecessary columns
  drop_columns <- c("Model", "Scenario", "Unit", "2060 Model estimation")
  df <- df[, !names(df) %in% drop_columns]

  # select desired rows
  rename_map <- c(
    "Product Stock|Buildings|Commercial|Other|All" = "industry",
    "Product Stock|Buildings|Residential|All" = "residential",
    "Product Stock|Buildings|Commercial|All" = "commercial and industry"
  )
  df <- subset(df, Variable %in% names(rename_map))
  df$Variable <- rename_map[df$Variable]

  # remove redundant aggregated data
  df <- df[df$Region != "EU27+3", ]

  x <- magclass::as.magpie(df, spatial = 1)
}
