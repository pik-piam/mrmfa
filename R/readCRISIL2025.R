#' Read cement end-use shares for India from CRISIL (2025)
#'
#' Crisil Intelligence (2025): "Market review of Indian cement sector. For JSW Cement",
#' \url{https://www.jswcement.in/pdf/offer-documents/CRISIL_Industry_Report_Final-Signed.pdf}.
#' The shares of Figure 10 "Sectoral mix of cement demand" (PDF p. 26, title on p. 25) are
#' transcribed in \code{values.csv}; see \code{SOURCE_INFO.txt} for the exact locations.
#' Only FY21 and FY25 are used (FY30 is a projection). The source gives ranges; their midpoints
#' are used. Fiscal years (April-March) are assigned to the calendar year of their start:
#' FY21 to 2020 and FY25 to 2024.
#'
#' @author Bennet Weiss
#' @return magpie object of end-use shares in percent, not normalised.
readCRISIL2025 <- function() {
  data <- utils::read.csv(file.path("v1", "values.csv"))
  fiscalYears <- c(FY21 = 2020, FY25 = 2024)
  data <- data[data$period %in% names(fiscalYears), ]
  data$period <- unname(fiscalYears[data$period])
  data$value <- (data$lower + data$upper) / 2

  categoryMapping <- c(
    "Rural housing" = "Residential",
    "Urban housing" = "Residential",
    "Industrial & Commercial" = "Non-Residential",
    "Infrastructure" = "Civil Engineering"
  )
  return(toolCeEndUseTable(data, categoryMapping))
}
