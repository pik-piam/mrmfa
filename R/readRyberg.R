#' Read Ryberg et al. 2019 Global environmental losses of plastics across their value chains
#'
#' @param subtype Character string specifying the dataset and scope
#'        - "Sector_split"
#'
#' @return magpie object of the Ryberg et al. Plastic data
#'
#' @author Leonie Schweiger
#'
#' @seealso [readSource()]
#'
#' @examples
#' \dontrun{
#' a <- readSource(type = "Ryberg", subtype = "Sector_split")
#' }
#' @importFrom readxl read_excel
#' @importFrom dplyr select
#' @importFrom magclass as.magpie getComment<-
#'
readRyberg <- function(subtype) {
  # ---------------------------------------------------------------------------
  # Map key to Excel file parameters
  params <- switch(subtype,
    "Sector_split" = list(
      file   = "Ryberg.xlsx",
      sheet  = "Table S3",
      range  = "A1:C14"
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
  if (subtype == "Sector_split") {
    magpie_data <- as.magpie(df %>% select("Application", "Share [%]"))
  }
  getComment(magpie_data) <- subtype

  return(magpie_data)
}
