# TODO: add documentation
toolCleanSteelRegions <- function(df) {

  # clean up region names ----
  df <- df %>%
    filter(!grepl("total|other", .data$country_name, ignore.case = TRUE),
           !is.na(.data$country_name)) %>%
    mutate(
      # drop hyphens and slashes
      "country_name" =  gsub("-|–|/", " ", .data$country_name),
      # replace two or more whitespaces by one
      "country_name" =  gsub("  +", " ", .data$country_name),
      # remove appended info in round brackets, e.g. "Taiwan (R.O.C.)" -> "Taiwan"
      "country_name" =  gsub(" *\\(.*\\) *$", "", .data$country_name),
      # "German Dem . Rep ." -> "German Dem. Rep.",
      "country_name" =  gsub(" \\.", "\\.", .data$country_name),
      # "Taiwan , China" -> "Taiwan, China
      "country_name" =  gsub(" ,", ",", .data$country_name)
  )

  # remove ignored countries ----
  ignore <- read.csv2(system.file("extdata", "MFA_ignore_regions.csv", package = "mrmfa"), header = TRUE)$reg

  df <- df %>%
    filter(!(.data$country_name %in% ignore)) %>%
    mutate("country_name" = as.factor(.data$country_name))


  # rename countries to ISO3 code ----
  # TODO: consider moving some of these to madrat, see also https://github.com/pik-piam/madrat/pull/250/files
  m <- read.csv2(system.file("extdata", "MFA_rename_regions.csv", package = "mrmfa"), header = TRUE)
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
