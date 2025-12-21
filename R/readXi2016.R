#' Read data from Supplementary Material in Xi et al. (2016)
#' "Substantial global carbon uptake by cement carbonation"
#' DOI: 10.1038/ngeo2840
#'
#' @author Bennet Weiss
#' @param subtype Variable to be read in.
readXi2016 <- function(subtype) {
  path <- file.path("v1", "Supplement.xlsx")
  # sheet SI data 2: split of mortar share across different use types (only relevant for china)
  # sheet SI data 5: split of cement across these same use types
  # China: clear differentiation between Res/Com/Ind/Civ possible
  # USA: Com/Ind differentiation not possible, Res/Civ possible
  # split could be inferred from China
  # Sheet SI data 6: concrete strength split, for China also as function of use type

  # prepare China data
  stock_type_mapping_china <- c(
    "Residential building" = "Res",
    "Office building" = "Com",
    "Commercial building" = "Com",
    "Hospital" = "Com",
    "Education, culture and research building" = "Com",
    "Industrial building" = "Ind",
    "Railway, Road, tunnel, and bridge" = "Civ",
    "Other Civil Engineering" = "Civ",
    "Dam, power station, and dock" = "Civ",
    "Other building" = "Com"
  )

  # cut out unnecessary aggregation of buildings (1st row)
  data_china <- readxl::read_xlsx(path, sheet = "SI data 5", range = "A18:B29")[-1, ]
  data_china$stock_type <- stock_type_mapping_china[data_china[["Cement consumption types"]]]
  data_china <- data_china[, -1]
  names(data_china) <- c("value", "stock_type")
  data_china <- stats::aggregate(`value` ~ stock_type, data = data_china, sum)

  data_china$region <- "CHN"

  # prepare USA data
  stock_type_mapping_usa <- c(
    "Residential buildings" = "Res",
    "Commercial and Industrial buildings" = "Com/Ind", # also Ind, but proportion Civ/Ind taken from China
    "Water and waste management" = "Civ",
    "Streets and highway" = "Civ",
    "Public buildings" = "Com/Ind",
    "Farm" = "Com/Ind", # Actually Ind, but proportion Civ/Ind taken from China
    "Other" = "Com/Ind",
    "Utilities" = "Civ"
  )

  # cut out temporal data and keep only first and last column
  data_usa <- readxl::read_xlsx(path, sheet = "SI data 5", range = "A5:H13")
  data_usa <- data_usa[, c(1, ncol(data_usa))]
  data_usa$stock_type <- stock_type_mapping_usa[data_usa[["Concrete utilization category"]]]
  data_usa <- data_usa[, -1]
  names(data_usa) <- c("value", "stock_type")
  data_usa <- stats::aggregate(`value` ~ stock_type, data = data_usa, sum)

  # split Com/Ind row
  # get split from China
  ind_share_china <- data_china[data_china$stock_type == "Ind", "value"]
  com_share_china <- data_china[data_china$stock_type == "Com", "value"]
  com_share <- com_share_china / (com_share_china + ind_share_china)

  # apply split from China
  comind_value <- data_usa$value[data_usa$stock_type == "Com/Ind"] # extract com value
  data_usa <- data_usa[data_usa$stock_type != "Com/Ind", ] # remove com value
  new_rows <- data.frame(
    stock_type = c("Ind", "Com"),
    value = c(comind_value * com_share, comind_value * (1 - com_share))
  )
  data_usa <- rbind(data_usa, new_rows)

  data_usa$region <- "USA"

  # combine china and USA
  data <- rbind(data_usa, data_china)
  data <- data[, c("region", "stock_type", "value")]
  x <- as.magpie(data, spatial = 1)
  return(x)
}
