#' Read data from Supplementary Material in Xi et al. (2016)
#' "Substantial global carbon uptake by cement carbonation"
#' DOI: 10.1038/ngeo2840
#'
#' Only the average split of cement use in China is read (sheet "SI data 5", A18:B29). The yearly
#' building/civil shares are available for three years only (1998, 2000, 2001), so the average is used.
#' Note: the split within buildings is based on shares of completed floor area (note **** of the sheet).
#'
#' @author Bennet Weiss
readXi2016 <- function() {
  path <- file.path("v1", "Supplement.xlsx")
  # sheet SI data 2: split of mortar share across different use types (only relevant for china)
  # sheet SI data 5: split of cement across these same use types
  # China: clear differentiation between Res/Com/Ind/Civ possible
  # Sheet SI data 6: concrete strength split, for China also as function of use type

  # prepare China data
  end_use_mapping_china <- c(
    "Residential building" = "Residential",
    "Office building" = "Commercial",
    "Commercial building" = "Commercial",
    "Hospital" = "Commercial",
    "Education, culture and research building" = "Commercial",
    "Industrial building" = "Industrial",
    "Railway, Road, tunnel, and bridge" = "Civil Engineering",
    "Other Civil Engineering" = "Civil Engineering",
    "Dam, power station, and dock" = "Civil Engineering",
    "Other building" = "Commercial"
  )

  # cut out unnecessary aggregation of buildings (1st row)
  data_china <- readxl::read_xlsx(path, sheet = "SI data 5", range = "A18:B29")[-1, ]
  data_china$end_use <- end_use_mapping_china[data_china[["Cement consumption types"]]]
  data_china <- data_china[, -1]
  names(data_china) <- c("value", "End Use")
  data_china <- stats::aggregate(`value` ~ `End Use`, data = data_china, sum)

  data_china$region <- "CHN"

  data <- data_china[, c("region", "End Use", "value")]
  x <- as.magpie(data, spatial = 1)
  return(x)
}
