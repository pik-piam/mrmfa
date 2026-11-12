#' Read cement end-use shares for Germany from VDZ (2022)
#'
#' VDZ - Verein Deutscher Zementwerke: "Zementindustrie im Ueberblick 2022/2023",
#' PDF p. 11, section "Verwendungsarten und Baubereiche" and chart
#' "Zementverbrauch 2021 nach Baubereichen". The values are transcribed in \code{values.csv};
#' see \code{SOURCE_INFO.txt}.
#'
#' @author Bennet Weiss
#' @return magpie object of end-use shares in percent, not normalised.
readVDZ2022 <- function() {
  data <- utils::read.csv(file.path("v1", "values.csv"))
  categoryMapping <- c(
    "Wohnungsbau" = "Residential",
    "Nichtwohnbau" = "Non-Residential",
    "Tiefbau" = "Civil Engineering"
  )
  return(toolCeEndUseTable(data, categoryMapping))
}
