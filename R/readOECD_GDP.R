#' Read OECD GDP
#' @description Read OECD GDP per capita data from 1500-2016, given in 2011 USD.
#' @author Merlin Jo Hosak
#' @param subtype Specific dataset used by that source
#' @export
readOECD_GDP <- function(subtype = "gdpPC") {
  # ---- list all available subtypes with functions doing all the work ----
  version <- "v1.1"
  switchboard <- list(
    "gdpPC" = function() {
      path <- paste0("./", version, "/GDPperCapita_Broad.xlsx")
      x <- readxl::read_excel(
        path = path,
        range = "A1:SY208"
      )

      # delete duplicate rows where no data is available
      x <- x[-which(x$`country name` == "Canada" & is.na(x$`ccode`)), ]
      x <- x[-which(x$`country name` == "Morocco" & is.na(x$`ccode`)), ]
      x <- x[-which(x$`country name` == "Sudan" & !is.na(x$`ccode`)), ]
      # remove ccode column
      x$ccode <- NULL
      x <- as.magpie(x, spatial = "country name")
      return(x)
    },
    NULL
  )
  # ---- check if the subtype called is available ----
  if (is_empty(intersect(subtype, names(switchboard)))) {
    stop(paste(
      "Invalid subtype -- supported subtypes are:",
      names(switchboard)
    ))
  } else {
    # ---- load data and do whatever ----
    return(switchboard[[subtype]]())
  }
}
