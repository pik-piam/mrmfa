#' Read Gapminder population data.
#' @description Read Gapminder population data from 1800-2100 in yearly resolution.
#' @author Merlin Jo Hosak
readGapminder <- function() {

  version <- "v1.0"
  path <- file.path(".", version, "GM-Population - Dataset - v8.xlsx")
  x <- readxl::read_excel(path = path, sheet = "Unpivot-countries-year") %>%
    select("region" = "geo", "period" = "Year", "value" = "Population") %>%
    mutate("region" = toupper(.data$region)) %>%
    as.magpie()

  # Rename Vatican Iso3 code (HOS [Holy See] to VAT)
  getItems(x, dim = 1)[getItems(x, dim = 1) == "HOS"] <- "VAT"

  return(x)

}
