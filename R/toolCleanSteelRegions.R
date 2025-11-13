# change to ISO country codes
# TODO unify toolCountry2isocode handling in this function and consider moving it to convert
# altogether
toolFoo <- function(x) {

  # clean up region names ----
  # replace _ by dot
  regionNames <- gsub("_", "\\.", getItems(x, dim = 1))

  # replace two whitespaces by one
  regionNames <- gsub("  ", " ", regionNames)

  # remove appended info in round brackets, e.g. "Taiwan (R.O.C.)" -> "Taiwan"
  regionNames <- gsub(" *\\(.*\\) *$", "", regionNames)

  # "German Dem . Rep ." -> "German Dem. Rep."
  regionNames <- gsub(" \\.", "\\.", regionNames)

  # "Taiwan , China" -> "Taiwan, China
  regionNames <- gsub(" ,", ",", regionNames)

  # for now, we store all additional mappings in the mfa package
  # as a next step, move universal mappings to country2iso.csv in madrat
  m <- read.csv2(system.file("extdata", "MFA_rename_regions.csv", package = "mrmfa"), header = TRUE)
  additionalIsoMappings <- m$to
  names(additionalIsoMappings) <- m$from

  ignore <- read.csv2(system.file("extdata", "MFA_ignore_regions.csv", package = "mrmfa"), header = TRUE)$reg


  # check for duplicates introduced by country mapping

  d <- toolCountry2isocode(regionNames,
                           ignoreCountries = ignore,
                           mapping = additionalIsoMappings,
                           warn = TRUE
  ) %>% sort()

  if(length(unique(d)) < length(d)) {
    stop("Country to ISO conversion introduces duplicates")
  }

  # apply country to ISO mapping

  getItems(x, dim = 1) <- toolCountry2isocode(regionNames,
                                              ignoreCountries = ignore,
                                              mapping = additionalIsoMappings,
                                              warn = TRUE
  )



  # drop NAs introduced by ignoreCountries
  x <- x[!is.na(getItems(x, dim = 1)), ]

  # manual corrections ----

  # TODO: move to convert ....

  # BRG (West Germany before reunification) should be DEU after 1990
  # TODO: this could be a feature in toolISOhistorical, if we want to actively enhance the tool
  if ("BRG" %in% getItems(x, dim = 1) && any(getYears(x, as.integer = TRUE) > 1990)) {
    x <- add_columns(x, addnm = "DEU", dim = 1, fill = NA)
    x["DEU", getYears(x, as.integer = TRUE) > 1990, ] <- x["BRG", getYears(x, as.integer = TRUE) > 1990, ]
    x <- x["BRG", , invert = TRUE]
  }

  # split BLX into LUX and BEL

  if ("BLX" %in% getItems(x, dim = 1) && !any(c("LUX", "BEL") %in% getItems(x, dim = 1))) {
    x <- add_columns(x, addnm = c("BEL", "LUX"), dim = 1, fill = NA)
    x["BEL", , ] <- x["BLX", , ] * 0.8
    x["LUX", , ] <- x["BLX", , ] * 0.2
    x <- x["BLX", , , invert = TRUE]
  }

  return(x)
}

# TODO: add documentations

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
