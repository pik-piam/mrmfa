#' Convert GHSOBAT
#' @param x magpie object
#' @author Bennet Weiss
#' @author subtype Data to extract.
#'        "surface" for total footprint building differentiated by residential and non-residential.
#'        "height" for average building height (not differentiated)
convertGHSOBAT <- function(x, subtype) {

  # Distribute unatributed region XXX proportionally
  scale_factor <- sum(x, na.rm = TRUE) / sum(x[getItems(x, dim = 1) != "XXX",], na.rm = TRUE)
  x <- x * scale_factor
  # Distribute other custom regions
  x["GBR",] <- x["GBR",] + x["XAD",] # Akrotiri and Dhekelia#
  x["AZE",] <- x["AZE",] + x["XCA",] # Caspian Sea
  x["SRB",] <- x["SRB",] + x["XKO",] # Kosovo
  x["CHN",] <- x["CHN",] + x["XPI",] # Paracel Islands
  x["CHN",] <- x["CHN",] + x["XSP",] * 0.5 # Spratly Islands
  x["VNM",] <- x["VNM",] + x["XSP",] * 0.35
  x["PHL",] <- x["PHL",] + x["XSP",] * 0.15
  x["CYP",] <- x["CYP",] + x["ZNC",] # Northern Cyprus
  no_remove_warning <- c("XAD", "XCA", "XKO", "XPI", "XSP", "XXX", "ZNC")

  if (subtype == "surface") {
    fill = 0
  } else if (subtype == "height") {
    fill = mean(x, na.rm = TRUE) # TODO: weighted average
  } else {
    stop("Invalid subtype for GHSOBAT conversion")
  }
  # No explicit data for Hong Kong, probably part of China
  x <- add_columns(x, addnm = "HKG", dim = 1, fill = fill)

  converted <- toolCountryFill(x, no_remove_warning = no_remove_warning, fill = fill)
  converted[is.na(converted)] <- fill # any values that are NA in source already
  return(converted)
}
