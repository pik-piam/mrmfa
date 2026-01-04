#' Clean up region names in steel sources
#'
#' @description Clean up region names for a given data frame with steel data
#' read in from in \link{readWorldSteelDigitised} and \link{readWorldSteelDatabase}
#'
#' Applies data cleansing, removes ignored countries/regions and converts country names to ISO3
#' country codes.
#'
#' This operation would normally happen on magclass object in the convert function,
#' but as the cleansing can introduce duplicates for the given data, we resolve this on
#' the data frames in the respective read functions before converting to a MagPIE
#' object (MagPIE does not handle duplicate regions in first dimension well).
#'
#' @param df a data frame with steel data consisting of three columns 'country_name',
#' 'period' and 'value'
#'
#' @author Falk Benke
toolCleanSteelRegions <- function(df) {
  # clean up region names ----
  df <- df %>%
    filter(
      !grepl("total|other", .data$country_name, ignore.case = TRUE),
      !is.na(.data$country_name)
    ) %>%
    mutate(
      # drop hyphens and slashes
      "country_name" = gsub("-|/", " ", .data$country_name),
      # replace two or more whitespaces by one
      "country_name" = gsub("  +", " ", .data$country_name),
      # remove appended info in round brackets, e.g. "Taiwan (R.O.C.)" -> "Taiwan"
      "country_name" = gsub(" *\\(.*\\) *$", "", .data$country_name),
      # "German Dem . Rep ." -> "German Dem. Rep.",
      "country_name" = gsub(" \\.", "\\.", .data$country_name),
      # "Taiwan , China" -> "Taiwan, China
      "country_name" = gsub(" ,", ",", .data$country_name)
    )

  # remove ignored countries ----
  ignore <- toolGetMapping("MFA_ignore_regions.csv", where = "mrmfa")$reg

  df <- df %>%
    filter(!(.data$country_name %in% ignore)) %>%
    mutate("country_name" = as.factor(.data$country_name))


  # rename countries to ISO3 code ----
  # TODO: consider moving some of these to madrat,
  # see also https://github.com/pik-piam/madrat/pull/250/files
  m <- toolGetMapping("MFA_rename_regions.csv", where = "mrmfa")

  additionalIsoMappings <- m$to
  names(additionalIsoMappings) <- m$from

  levels(df$country_name) <- toolCountry2isocode(levels(df$country_name),
    mapping = additionalIsoMappings,
    warn = TRUE
  )

  # merge duplicates (e.g. "Brazil" and "Brazil (3)") ----
  df <- stats::aggregate(value ~ ., df, sum)

  return(df)
}
