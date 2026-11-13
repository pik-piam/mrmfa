#' Convert cement end-use shares from Cao et al. (2017)
#'
#' Normalises the digitised shares and maps the panels to countries with
#' \code{regionmappingCao2017.csv}. The aggregate panels are assigned to their member countries:
#' Former Soviet Union (FSU) as defined in section S1 of the supporting information, and Other
#' European Countries (OEU) as the countries of the region Europe in Table S2 without a panel of
#' their own (Albania, Andorra, Cyprus, Faroe Islands, Iceland, Malta, former Yugoslavia, and Slovakia and
#' Luxembourg from the former Czechoslovakia and Belgium-Luxembourg). The panel "The Rest of World"
#' (ROW) is dropped. Countries without data are filled with NA.
#' Which country series are trusted is decided in \code{\link{calcCeEndUseSplit}}.
#'
#' @author Bennet Weiss
#' @param x magpie object from \code{\link{readCao2017}}.
convertCao2017 <- function(x) {
  x <- toolCeNormalizeEndUseSplit(x)
  x <- x["ROW", , , invert = TRUE]
  mapping <- toolGetMapping("regionmappingCao2017.csv", type = "regional", where = "mrmfa")
  # unweighted disaggregation copies the shares of an aggregate panel to each member country
  x <- toolAggregate(x, rel = mapping, from = "RegionCode", to = "CountryCode", dim = 1)
  return(toolCountryFill(x, fill = NA, verbosity = 2))
}
