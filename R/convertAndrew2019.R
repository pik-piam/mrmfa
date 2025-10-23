#' Convert data from Andrew 2019.
#' @author Bennet Weiss
#' @param x Magpie object
#' @param subtype Material subtype. Can be "cement" or "clinker".
convertAndrew2019 <- function(x, subtype) {
  if (subtype == "cement") {
    no_remove_warning <- c(
      "VDR", "East & West Pakistan", "Federation of Malaya-Singapore", "YMD", "FORMER YEMEN",
      "French Indo-China", "Japan (Excluding The Ruyuku Islands)", "ANT", "Netherland Antilles and Aruba",
      "Peninsular Malaysia", "Republic of South Vietnam", "Rhodesia-Nyasaland", "Rwanda-Urundi", "Sabah",
      "Sarawak", "SUDAN", "Tanganyika", "United Korea", "Zanzibar", "YEM (886)", "SDN (736)",
      # countries with NA entries
      "DDR", "DEW", # only one value in 1990
      "KNA___215", "KNA___216", "PCZ", "French Equatorial Africa", "Kuwaiti Oil Fires",
      "Leeward Islands", "Pacific Islands (Palau)", "KNA (659)", "KNA (658)", # zero anyways
      "French West Africa", "KSV", "Ryukyu Islands" # unsure
      # Puerto Rico is missing in dataset. 16.000 t cement production in 2024, possibly already included in US data.
    )

    magclass::getItems(x, dim = 1)[which(getItems(x, dim = 1) == "YEM (887)")] <- "YEM"
    magclass::getItems(x, dim = 1)[which(getItems(x, dim = 1) == "SDN (729)")] <- "SDN"
  } else if (subtype == "clinker") {
    # clinker data for a lot of countries is missing, those will default to NA
    no_remove_warning <- c("KSV") # data removed: probably Kosovo, unsure what to do with it
  } else {
    stop("Invalid subtype. Choose either 'cement' or 'clinker'.")
  }
  x <- suppressWarnings(madrat::toolISOhistorical(x)) # possibly not needed
  x <- madrat::toolCountryFill(x, fill = NA, verbosity = 2, no_remove_warning = no_remove_warning)
  return(x)
}
