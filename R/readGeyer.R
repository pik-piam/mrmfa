#' Read Geyer et al. SI Global plastic production 1950-2015
#'
#' @param subtype Character string specifying the dataset and scope
#'        - "Prod_1950-2015"
#'        - "Lifetime_mean"
#'        - "Lifetime_std"
#'
#' @return magpie object of the Geyer et al. Plastic data
#'
#' @author Leonie Schweiger
#'
#' @seealso [readSource()]
#'
#' @examples
#' \dontrun{
#' a <- readSource(type = "Geyer", subtype = "Prod_1950-2015")
#' }
#' @importFrom readxl read_excel
#' @importFrom dplyr select filter
#' @importFrom magclass as.magpie getComment<-
#'
readGeyer <- function(subtype) {
  # ---------------------------------------------------------------------------
  # Map key to Excel file parameters
  params <- switch(subtype,
                   "Prod_1950-2015" = list(
                     file   = "Geyer.xlsx",
                     sheet  = "Table S1",
                     range  = "A1:B67"
                   ),
                   "Lifetime_mean" = list(
                     file   = "Geyer.xlsx",
                     sheet  = "Table S4",
                     range  = "A1:C9"
                   ),
                   "Lifetime_std" = list(
                     file   = "Geyer.xlsx",
                     sheet  = "Table S4",
                     range  = "A1:C9"
                   ),
                   stop("Invalid subtype: ", subtype)
  )

  # ---------------------------------------------------------------------------
  # Read raw data from Excel
  df <- read_excel(
    path  = params$file,
    sheet = params$sheet,
    range = params$range,
    skip  = 1
  )

  # ---------------------------------------------------------------------------
  # Convert to magpie object
  # ---------------------------------------------------------------------------
  if(subtype=="Prod_1950-2015"){
    magpie_data <- as.magpie(df, temporal = 1)
  }
  if(subtype=="Lifetime_mean"){
    magpie_data <- as.magpie(df %>% select("Market Sector", "Mean (in years)"))
  }
  if(subtype=="Lifetime_std"){
    magpie_data <- as.magpie(df %>% select("Market Sector", "Standard deviation"))
  }
  getComment(magpie_data) <- subtype

  return(magpie_data)
}

