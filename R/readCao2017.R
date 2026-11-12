#' Read cement end-use shares from Cao et al. (2017)
#'
#' Cao, Z., Shen, L., Lovik, A. N., Mueller, D. B. & Liu, G. (2017): "Elaborating the History of Our
#' Cementing Societies: An in-Use Stock Perspective", Environmental Science & Technology 51(19),
#' 11468-11475, \doi{10.1021/acs.est.7b03077}.
#' Split ratios of residential buildings (RES), non-residential buildings (NRES) and civil engineering
#' (CE) from Fig. S1 of the supporting information (pp. S6-S10), based on statistics of cement
#' industry associations (e.g. PCA, Cembureau).
#'
#' The values are not tabulated in the source; they were digitised by eye from the charts
#' (accuracy about +-3 percentage points, up to +-5 in steep sections) and are stored in
#' \code{values.csv}. Only the country panels were digitised, not the aggregates (Former Soviet
#' Union, Other European Countries, The Rest of World). The series are flat (back-cast) before
#' about 2000; the first year of each country carries this flat level. See \code{SOURCE_INFO.txt}.
#'
#' @author Bennet Weiss
#' @return magpie object of end-use shares in percent, not normalised.
readCao2017 <- function() {
  data <- utils::read.csv(file.path("v1", "values.csv"))
  categoryMapping <- c(
    "RES" = "Residential",
    "NRES" = "Non-Residential",
    "CE" = "Civil Engineering"
  )
  return(toolCeEndUseTable(data, categoryMapping))
}
