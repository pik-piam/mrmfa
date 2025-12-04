#' Read OECD GDP
#' @description Read OECD GDP per capita data from 1500-2016, given in 2011 USD.
#' @author Merlin Jo Hosak
readOECD_GDP <- function() {

  version <- "v1.1"

  path <- file.path(".", version, "GDPperCapita_Broad.xlsx")
  x <- readxl::read_excel(path = path, range = "A1:SY208", sheet = 1)

  # delete duplicate rows where no data is available
  x <- x[-which(x$`country name` == "Canada" & is.na(x$`ccode`)), ]
  x <- x[-which(x$`country name` == "Morocco" & is.na(x$`ccode`)), ]
  x <- x[-which(x$`country name` == "Sudan" & !is.na(x$`ccode`)), ]

  # remove ccode column
  x$ccode <- NULL

  x <- x %>%
    tidyr::pivot_longer(cols = -c(1)) %>%
    select("region" = "country name", "period" = "name", "value") %>%
    filter(!is.na(.data$value))


  x <- as.magpie(x, spatial = "region") %>%
    magpiesort()

  return(x)

}
