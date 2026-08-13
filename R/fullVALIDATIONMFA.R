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
#' @param runSections Character vector or string selecting which parts to run.
#' Allowed values (see validSections): c("steel", "cement", "plastic"). NULL (default) runs all.
#' @author Leonie Schweiger
#' @seealso
#' \code{\link[madrat]{readSource}}, \code{\link[madrat]{calcOutput}},
#' \code{\link[madrat]{retrieveData}}
#' @examples
#' \dontrun{
#' retrieveData("VALIDATIONMFA")
#' }
#'
fullVALIDATIONMFA <- function(rev = 0, runSections = NULL) {

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

  # prepare section selector
  validSections <- c("steel", "cement", "plastic")

  if (is.null(runSections)) {
    runSections <- validSections
  } else {
    bad <- setdiff(runSections, validSections)
    if (length(bad)) warning("Invalid sections: ", paste(bad, collapse = ", "))
    runSections <- intersect(runSections, validSections)
  }

  runSection <- function(name) name %in% runSections

  if (runSection("steel")) {}

  if (runSection("cement")) {}

  if (runSection("plastic")) {
    # Pottinger 2024 plastics material flows (all scenarios) ----
    calcOutput(
      type = "PlPottinger", subtype = "all", file = valfile,
      aggregate = columnsForAggregation, append = FALSE,
      warnNA = FALSE, try = FALSE,
      writeArgs = list(model = "Pottinger et al 2024")
    )
    calcOutput(
      type = "PlPottinger", subtype = "all", perCapita = TRUE, file = valfile,
      aggregate = columnsForAggregation, append = TRUE,
      warnNA = FALSE, try = FALSE,
      writeArgs = list(model = "Pottinger et al 2024")
    )

    # Gao & Cabrera-Serrenho 2025 apparent polymer consumption ----
    calcOutput(
      type = "PlGaoCabreraValidation", file = valfile,
      aggregate = columnsForAggregation, append = TRUE,
      warnNA = FALSE, try = FALSE,
      writeArgs = list(scenario = "historical", model = "Gao & Cabrera-Serrenho 2025")
    )

    # Ren et al. 2025 plastics production & consumption (China) ----
    calcOutput(
      type = "PlRen2025Validation", file = valfile,
      aggregate = columnsForAggregation, append = TRUE,
      warnNA = FALSE, try = FALSE,
      writeArgs = list(scenario = "historical", model = "Ren et al 2025")
    )

    # Geyer et al. 2017 global plastics production ----
    calcOutput(
      type = "PlGeyer", file = valfile,
      aggregate = FALSE, append = TRUE,
      warnNA = FALSE, try = FALSE,
      writeArgs = list(scenario = "historical", model = "Geyer et al 2017")
    )

    # OECD Global Plastics Outlook 2022 projections ----
    calcOutput(
      type = "PlOECDProjection", subtype = "total", file = valfile,
      aggregate = columnsForAggregation, append = TRUE,
      warnNA = FALSE, try = FALSE,
      writeArgs = list(scenario = "Baseline scenario", model = "OECD Global Plastics Outlook 2022")
    )
    calcOutput(
      type = "PlOECDProjection", subtype = "perCapita", file = valfile,
      aggregate = columnsForAggregation, append = TRUE,
      warnNA = FALSE, try = FALSE,
      writeArgs = list(scenario = "Baseline scenario", model = "OECD Global Plastics Outlook 2022")
    )

    # Stegmann et al. 2022 (PLAIA/IMAGE) plastics production & demand ----
    calcOutput(
      type = "PlStegmann", subtype = "total", file = valfile,
      aggregate = columnsForAggregation, append = TRUE,
      warnNA = FALSE, try = FALSE,
      writeArgs = list(model = "Stegmann et al 2022")
    )
    calcOutput(
      type = "PlStegmann", subtype = "perCapita", file = valfile,
      aggregate = columnsForAggregation, append = TRUE,
      warnNA = FALSE, try = FALSE,
      writeArgs = list(model = "Stegmann et al 2022")
    )

    # IEA The Future of Petrochemicals 2018 global key-thermoplastics production ----
    calcOutput(
      type = "PlIEA", subtype = "total", file = valfile,
      aggregate = FALSE, append = TRUE,
      warnNA = FALSE, try = FALSE,
      writeArgs = list(
        scenario = "Reference Technology Scenario",
        model = "IEA The Future of Petrochemicals 2018"
      )
    )
    calcOutput(
      type = "PlIEA", subtype = "perCapita", file = valfile,
      aggregate = FALSE, append = TRUE,
      warnNA = FALSE, try = FALSE,
      writeArgs = list(
        scenario = "Reference Technology Scenario",
        model = "IEA The Future of Petrochemicals 2018"
      )
    )

    # Zanon-Zotin et al. 2024 global HVC production scenarios (COFFEE 1.5) ----
    calcOutput(
      type = "PlZanonZotin2024", subtype = "total", file = valfile,
      aggregate = FALSE, append = TRUE,
      warnNA = FALSE, try = FALSE,
      writeArgs = list(model = "COFFEE 1.5")
    )
    calcOutput(
      type = "PlZanonZotin2024", subtype = "perCapita", file = valfile,
      aggregate = FALSE, append = TRUE,
      warnNA = FALSE, try = FALSE,
      writeArgs = list(model = "COFFEE 1.5")
    )

    # Fritzeen et al. 2023 global plastics production scenarios (GCAM) ----
    calcOutput(
      type = "PlFritzeen2023", subtype = "total", file = valfile,
      aggregate = FALSE, append = TRUE,
      warnNA = FALSE, try = FALSE,
      writeArgs = list(model = "GCAM 5.4")
    )
    calcOutput(
      type = "PlFritzeen2023", subtype = "perCapita", file = valfile,
      aggregate = FALSE, append = TRUE,
      warnNA = FALSE, try = FALSE,
      writeArgs = list(model = "GCAM 5.4")
    )
  }

}
