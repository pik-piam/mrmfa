#' Read data from Levi and Cullen (2018)
#' @description
#' Read data from supplementary material of Levi and Cullen (2018) paper
#' 'Mapping Global Flows of Chemicals: From Fossil Fuel Feedstocks to Chemical Products'. https://doi.org/10.1021/acs.est.7b04573
#' @param subtype
#'        - "HVCbyProcess" HVC inputs for chemical processes in the plastics production chain from Table S20
#'          For PUR that is not covered separately in the paper, assumptions were made regarding a typical composition
#'        - "Production" flows and total production volumes of chemicals in the plastics production chain from several Tables in SI
#' @author Leonie Schweiger
readLeviCullen <- function(subtype) {
  version <- "v1.0"
  switchboard <- list(
    "Production" = function() {
      path <- file.path(".", version, "LeviCullen.xlsx")
      df <- readxl::read_excel(path = path, sheet = "production", range="A1:G47") %>%
        select("stage","type","from","to","flow (Mt)")
      x <- as.magpie(df)
      return(x)
    },
    "HVCbyProcess" = function() {
      path <- file.path(".", version, "LeviCullen.xlsx")
      df <- readxl::read_excel(path = path, sheet = "HVC input by process", range="A1:D25")
      x <- as.magpie(df)
      return(x)
    }
  )
  # ---- check if the subtype called is available ----
  if (is_empty(intersect(subtype, names(switchboard)))) {
    stop(
      "Invalid subtype -- supported subtypes are:",
      paste0(names(switchboard), collapse = ", ")
    )
  } else {
    # ---- load data and do whatever ----
    return(switchboard[[subtype]]())
  }
}
