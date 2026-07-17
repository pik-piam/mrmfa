#' Convert data from Andrew 2019.
#' @author Bennet Weiss
#' @param x Magpie object
#' @param subtype Material subtype. Can be "cement" or "clinker".
convertAndrew2019 <- function(x, subtype) {
  if (subtype == "cement") {
    # Puerto Rico (PRI) data is missing
    no_remove_warning <- c(
      # ── Only NA entries (all 145 years NA) ──
      "VDR", "East & West Pakistan", "Federation of Malaya-Singapore", "YMD",
      "French Indo-China", "Japan (Excluding The Ruyuku Islands)", "Netherland Antilles and Aruba",
      "Peninsular Malaysia", "Republic of South Vietnam", "Rhodesia-Nyasaland", "Rwanda-Urundi", "Sabah",
      "Sarawak", "Tanganyika", "United Korea", "Zanzibar", "YEM (886)", "SDN (736)",

      # ── Only NA/zero entries (mix of NA and zeros, no real values) ──
      "PCZ",                       # 75 zeros, 70 NA
      "French Equatorial Africa",  # 75 zeros, 70 NA
      "Kuwaiti Oil Fires",        # 34 zeros, 111 NA
      "Leeward Islands",           # 75 zeros, 70 NA
      "Pacific Islands (Palau)",   # 70 zeros, 75 NA
      "KNA (659)",                 # 44 zeros, 101 NA
      "KNA (658)",                 # 68 zeros, 75 NA

      # ── Other: removed or mapped to other regions ──
      "DDR",   # single real value in 1990: German Democratic Republic - already included in DEU
      "DEW",   # single real value in 1990: West Germany - already included in DEU
      "KSV",   # values 1965–1972 (8 yrs) + zeros 1952–1964, rest NA
      "Ryukyu Islands" # values 1965–1972 (8 yrs) + zeros 1952–1964, rest NA
    )

    # Values 1965–1972 (8 yrs) + zeros 1952–1964, rest NA
    x["JPN",] = x["JPN",] + toolNAreplace(x["Ryukyu Islands",])$x

    # Values 2004–2024 (21 yrs), rest NA
    x["SRB",] = x["SRB",] + toolNAreplace(x["KSV",])$x

    # Disaggregate colonial federation of French West Africa (data 1949–1957)
    add_map <- list(
      c("French West Africa", "SEN", "y1957"), # Senegal
      c("French West Africa", "MRT", "y1957"), # Mauritania
      c("French West Africa", "MLI", "y1957"), # Mali (French Sudan)
      c("French West Africa", "GIN", "y1957"), # Guinea (French Guinea)
      c("French West Africa", "CIV", "y1957"), # Côte d'Ivoire
      c("French West Africa", "NER", "y1957"), # Niger
      c("French West Africa", "BFA", "y1957"), # Burkina Faso (Upper Volta)
      c("French West Africa", "BEN", "y1957")  # Benin (Dahomey)
    )

    magclass::getItems(x, dim = 1)[which(getItems(x, dim = 1) == "YEM (887)")] <- "YEM"
    magclass::getItems(x, dim = 1)[which(getItems(x, dim = 1) == "SDN (729)")] <- "SDN"

  } else if (subtype == "clinker") {
    # clinker data for a lot of countries is missing, those will default to NA
    no_remove_warning <- c("KSV")

    x["SRB",] = x["SRB",] + toolNAreplace(x["KSV",])$x

    add_map <- list()
  } else {
    stop("Invalid subtype. Choose either 'cement' or 'clinker'.")
  }
  x <- suppressWarnings(madrat::toolISOhistorical(x, additional_mapping = add_map))
  x <- madrat::toolCountryFill(x, fill = NA, verbosity = 2, no_remove_warning = no_remove_warning)
  return(x)
}
