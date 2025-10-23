#' Read data from GNR
#' Getting the Numbers Right (GNR) is a Project of the Global Cement and Concrete Association (GCCA)
#' [GNR](https://gccassociation.org/sustainability-innovation/gnr-gcca-in-numbers/)
#' Data not publically available. Data received from Abhishek Shukla, Sustainability Program Manager at GCCA.
#' Data received on 21.01.2025, personal communication.
#'
#' @author Bennet Weiss
#' @param subtype Variable to be read in. Currently, only "clinker_ratio" is supported.
readGNR <- function(subtype = "clinker_ratio") {
  path <- file.path("v1", "Postdam_GCCA_GNR_2022.xlsx")
  regions <- readxl::excel_sheets(path) %>%
    setdiff(c("Read me", "GNR Coverage"))
  data_list <- vector("list", length(regions))
  ranges <- list(
    "clinker_ratio" = c(
      "A1070:C1090",
      "A962:C982",
      "A1096:C1116",
      "A967:C987",
      "A1102:C1122",
      "A823:C843",
      "A869:C889",
      "A839:C859",
      "A787:C807",
      "A801:C821",
      "A931:C951",
      "A890:C910",
      "A806:C826",
      "A877:C897",
      "A849:C869",
      "A819:C839",
      "A789:C809",
      "A826:C846",
      "A834:C854",
      "A774:C794",
      "A794:C814",
      "A951:C971"
    )
  )
  if (!subtype %in% names(ranges)) {
    stop("Invalid subtype. Can only be 'clinker_ratio'.")
  }
  for (i in seq_along(regions)) {
    df <- readxl::read_xlsx(path, sheet = regions[i], range = ranges[[subtype]][i])
    synonyms <- c(
      "Czech Republic" = "Czechia",
      "United Kingdom" = "United Kingdom of Great Britain and Northern Ireland",
      "United States" = "United States of America"
    )
    if (!all(normalize_text(df$Region, synonyms) == normalize_text(regions[i], synonyms))) {
      stop(
        "Sheet name and Region should match:\n  Sheet:  ", regions[i],
        "\n  Region: ", unique(df$Region)
      )
    }
    data_list[[i]] <- df
  }

  data_list <- do.call(rbind, data_list)
  x <- magclass::as.magpie(data_list, spatial = 1, temporal = 2)
  getNames(x) <- NULL
  return(x)
}

#' Normalize strings for comparison.
#'
#' @param strings Character vector to normalize.
#' @param lookup Named character vector mapping originals to replacements.
#' @return Character vector of normalized strings.
normalize_text <- function(strings, lookup) {
  normalize <- function(s) {
    s <- tolower(s)
    return(gsub("[ /-]", "", s))
  }

  replaced <- ifelse(
    strings %in% names(lookup),
    lookup[strings],
    strings
  )

  return(normalize(replaced))
}
