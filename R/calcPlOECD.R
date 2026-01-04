#' This function returns OECD plastic use or waste data from OECD Plastic outlook
#' based on a specified subtype in a MagPIE object aggregated to ISO country level.
#'
#' @param subtype Character string specifying the dataset and scope.
#'  Valid formats include:
#'  - "Use_2019_region" (plastic use in Mt, dimensions: region, application, polymer)
#'  - "Use_1990-2019_region" (plastic use in Mt, dimensions: region, year)
#'  - "Use_1990-2019_world" (plastic use in Mt, dimensions: year, application, polymer, type (primary/secondary))
#'  - "WasteEOL_1990-2019_region" (plastic waste in Mt, dimensions: region, year, EOL fate, collected for recycling)
#'  - "WasteType_2019_region" (plastic waste in Mt, dimensions: region, application, polymer)
#'
#' @author Qianzhi Zhang
#'
calcPlOECD <- function(subtype) {
  # ---------------------------------------------------------------------------
  # Read source data
  # ---------------------------------------------------------------------------
  x <- readSource("OECD_Plastic", subtype = subtype)

  # ---------------------------------------------------------------------------
  # Return packaged output
  # ---------------------------------------------------------------------------
  return(list(
    x = x,
    weight = NULL,
    unit = "Mt Plastic",
    description = "Data on Plastic MFA from OECD Plastic Outlook"
  ))
}
