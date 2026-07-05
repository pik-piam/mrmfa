#' Generate validation data for the REMIND MFA
#'
#' @description
#' Function that assembles a regional dataset from various sources against which
#' the MFA model results can be compared, and writes it in IAMC format to
#' \code{validation.mif}. This mirrors \code{mrremind::fullVALIDATIONREMIND},
#' which writes \code{historical.mif} for REMIND. New sources are appended as
#' additional \code{calcOutput(..., file = valfile, append = TRUE)} blocks.
#'
#' @md
#' @param rev Unused parameter, but required by `madrat`.
#' @author Leonie Schweiger
#' @seealso
#' \code{\link[madrat]{readSource}}, \code{\link[madrat]{calcOutput}},
#' \code{\link[madrat]{retrieveData}}
#' @examples
#' \dontrun{
#' retrieveData("VALIDATIONMFA")
#' }
#'
fullVALIDATIONMFA <- function(rev = 0) {

  # get region mappings for aggregation ----
  # Determines all regions data should be aggregated to by examining the columns
  # of the `regionmapping` and `extramappings` currently configured.

  rel <- "global" # always compute global aggregate
  for (mapping in c(getConfig("regionmapping"), getConfig("extramappings"))) {
    columns <- setdiff(
      colnames(toolGetMapping(mapping, "regional", where = "mappingfolder")),
      c("X", "CountryCode")
    )

    if (any(columns %in% rel)) {
      warning(
        "The following column(s) from ", mapping,
        " exist in another mapping an will be ignored: ",
        paste(columns[columns %in% rel], collapse = ", ")
      )
    }

    rel <- unique(c(rel, columns))
  }

  columnsForAggregation <- gsub(
    "RegionCode", "region",
    paste(rel, collapse = "+")
  )

  # validation data ----
  valfile <- "validation.mif"

  # Pottinger 2024 plastics material flows ----

  calcOutput(
    type = "PlPottinger", file = valfile,
    aggregate = columnsForAggregation, append = FALSE,
    warnNA = FALSE, try = FALSE,
    writeArgs = list(scenario = "businessAsUsual", model = "Pottinger et al 2024")
  )

}
