#' Read Plastics Europe Data into a magpie Object
#'
#' This function reads Plastics Europe data on plastics production by region,
#' demand shares by use category and EoL treatment from Excel files,
#' based on a specified subtype, and returns a cleaned magpie object.
#'
#' @param subtype Character string specifying the dataset and scope.
#'        Valid formats include:
#'        - "PlasticProduction_region" (plastic production in Mt, dimensions: region, year)
#'        - "PlasticShare_EU" (plastic demand shares (%) by use category in the EU, dimensions: category, year)
#'        - "PlasticEoL_EU" (plastic waste treatment in the EU in Mt, dimensions: year, treatment)
#'
#' @return magpie object of the Plastics Europe data
#'
#' @author Leonie Schweiger
#'
#' @seealso [readSource()]
#'
#' @examples
#' \dontrun{
#' a <- readSource(type = "PlasticsEurope", subtype = "PlasticProduction_region")
#' }
#' @importFrom readxl read_excel
#' @importFrom dplyr select filter
#' @importFrom magclass as.magpie getComment<-
#' @importFrom tidyr pivot_longer
#'
readPlasticsEurope <- function(subtype) {
  # ---------------------------------------------------------------------------
  # Map subtype to Excel file parameters
  params <- switch(subtype,
                   "PlasticProduction_region" = list(
                     sheet  = "PlasticProduction_region",
                     range  = "A1:J20"
                   ),
                   "PlasticShare_EU" = list(
                     sheet  = "PlasticShare_EU",
                     range  = "A1:I20"
                   ),
                   "PlasticEoL_EU" = list(
                     sheet  = "PlasticEoL_EU",
                     range  = "A1:D16"
                   ),
                   stop("Invalid subtype: ", subtype)
  )

  # ---------------------------------------------------------------------------
  # Read raw data from Excel
  raw_df <- read_excel(
    path  = "PlasticsEurope.xlsx",
    sheet = params$sheet,
    range = params$range,
    skip  = 1
  )

  # ---------------------------------------------------------------------------
  # Select and filter columns based on subtype
  df <- switch(
    subtype,

    "PlasticProduction_region" = raw_df %>%
      pivot_longer(
        cols = -"Year",
        names_to = "Region",
        values_to = "Production"
      ) %>%
      filter(.data$Region != "Total Production (Mt)"),

    "PlasticShare_EU" = raw_df %>%
      pivot_longer(
        cols = -"Year",
        names_to = "Type",
        values_to = "Share"
      ) %>%
      filter(.data$Type != "Total Demand (Mt)"),

    "PlasticEoL_EU" = raw_df %>%
      pivot_longer(
        cols = -"Year",
        names_to = "EoL",
        values_to = "Share"
      ),
    stop("Unsupported subtype: ", subtype)
  )

  # ---------------------------------------------------------------------------
  # Convert to magpie object and clean missing values
  # ---------------------------------------------------------------------------
  magpie_data <- switch(
    subtype,
    "PlasticProduction_region" = as.magpie(df, temporal = 1, spatial = 2),
    "PlasticShare_EU" = as.magpie(df, temporal = 1),
    "PlasticEoL_EU" = as.magpie(df, temporal = 1),
    stop("Unsupported subtype: ", subtype)
  )

  magpie_data[is.na(magpie_data)] <- 0
  if(subtype != "PlasticProduction_region"){
    getItems(magpie_data, dim=1) <- "EUR"
  }
  getComment(magpie_data) <- subtype

  return(magpie_data)
}

