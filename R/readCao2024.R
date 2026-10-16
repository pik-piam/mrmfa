#' Read data received on 04.08.2025, personal communication.
#' Data used for Kaufmann et al. (2024), DOI: 10.1088/1748-9326/ad236b
#' "Society's material stocks as carbon pool: an economy-wide quantification of global carbon stocks from 1900-2015"
#' Most variables are given as parameters of probability density functions.
#' This function only reads the parameters; means are calculated in the calc functions
#' (see \code{\link{toolCeDistributionMean}}).
#'
#' The returned magpie object has the source regions as spatial dimension and the
#' variable items plus a "parameter" subdimension in the data dimension.
#' The parameter names follow the layout of the distribution:
#' Weibull (scale, shape, min, max), Uniform (max, min), Triangular (mode, max, min),
#' Normal (mean, sd, min, max), Point (value).
#'
#' @author Bennet Weiss
#' @param subtype Variable to be read in.
#' Supported subtypes are:
#' "material_split", "concrete_application_split", "mortar_application_split",
#' "carbonation_rate_concrete", "carbonation_rate_mortar", "carbonation_rate_buried_concrete",
#' "carbonation_factor_additives", "carbonation_factor_co2", "carbonation_factor_coating",
#' "wall_thickness", "mortar_thickness", "concrete_cement_content", "cao_carbonation_share",
#' "cement_loss_construction", "clinker_loss_production", "CKD_landfill_share",
#' "clinker_cao_content", "CKD_cao_content", "waste_split", "waste_size_split"
readCao2024 <- function(subtype) {
  # columns: first Excel column of each variable (the distribution label, if the sheet gives one)
  # distribution: label expected in the sheet; determines the parameter columns that follow
  # parameters: overrides the parameter layout if the sheet gives no distribution label
  cao2024_specs <- list(
    material_split = list(
      columns = c(concrete = "B", mortar = "G"),
      dim = "Product Material",
      distribution = "Weibull"
    ),
    concrete_application_split = list(
      columns = c(C15 = "L", C20 = "Q", C30 = "V", C35 = "AA"),
      dim = "Product Application",
      distribution = "Weibull"
    ),
    mortar_application_split = list(
      columns = c(finishing = "DY", masonry = "ED", maintenance = "EI"),
      dim = "Product Application",
      distribution = "Weibull"
    ),
    carbonation_rate_concrete = list(
      columns = c(C15 = "AF", C20 = "AI", C30 = "AL", C35 = "AO"),
      dim = "Product Application",
      distribution = "Uniform"
    ),
    carbonation_rate_mortar = list(
      columns = c(mortar = "FC"),
      dim = "Product Material",
      distribution = "Triangular"
    ),
    carbonation_rate_buried_concrete = list(
      columns = c(C15 = "DU", C20 = "DV", C30 = "DW", C35 = "DX"),
      dim = "Product Application",
      parameters = "value"
    ),
    carbonation_factor_additives = list(
      columns = c(additives = "AR"),
      distribution = "Weibull"
    ),
    carbonation_factor_co2 = list(
      columns = c(co2 = "AW"),
      distribution = "Weibull"
    ),
    carbonation_factor_coating = list(
      columns = c(coating = "BB"),
      distribution = "Weibull"
    ),
    wall_thickness = list(
      columns = c(concrete = "BG"),
      dim = "Product Material",
      distribution = "Uniform"
    ),
    mortar_thickness = list(
      columns = c(finishing = "EN", masonry = "ES", maintenance = "EX"),
      dim = "Product Application",
      distribution = "Weibull"
    ),
    concrete_cement_content = list(
      columns = c(C15 = "BJ", C20 = "BM", C30 = "BP", C35 = "BS"),
      dim = "Product Application",
      distribution = "Uniform"
    ),
    cao_carbonation_share = list(
      columns = c(concrete = "CE", mortar = "FG"),
      dim = "Product Material",
      distribution = "Weibull"
    ),
    cement_loss_construction = list(
      columns = c(cement_loss_construction = "FL"),
      distribution = "Triangular"
    ),
    clinker_loss_production = list(
      columns = c(clinker_loss_production = "FP"),
      distribution = "Triangular"
    ),
    CKD_landfill_share = list(
      columns = c(CKD_landfill_share = "FT"),
      distribution = "Triangular"
    ),
    clinker_cao_content = list(
      columns = c(clinker_cao_content = "CA"),
      distribution = "Triangular"
    ),
    CKD_cao_content = list(
      columns = c(CKD_cao_content = "FX"),
      distribution = "Normal"
    ),
    waste_split = list(
      columns = c(`new concrete` = "CK", aggregates = "CL", landfill = "CM", asphalt = "CN"),
      dim = "Concrete Waste Type",
      parameters = "value"
    ),
    # Uniform distribution, but the sheet gives no label and the order (min, max)
    waste_size_split = list(
      columns = c(
        `new concrete.A` = "CO", `new concrete.B` = "CQ", `new concrete.C` = "CS", `new concrete.D` = "CU",
        aggregates.A = "CW", aggregates.B = "CY", aggregates.C = "DA", aggregates.D = "DC",
        landfill.A = "DE", landfill.B = "DG", landfill.C = "DI", landfill.D = "DK",
        asphalt.A = "DM", asphalt.B = "DO", asphalt.C = "DQ", asphalt.D = "DS"
      ),
      dim = c("Concrete Waste Type", "Particle Size"),
      parameters = c("min", "max")
    )
  )

  spec <- cao2024_specs[[subtype]]
  if (is.null(spec)) {
    stop("Subtype ", subtype, " not implemented.")
  }
  # parameter names in column order; taken from the distribution unless the spec overrides them
  parameters <- if (is.null(spec$parameters)) toolCeDistributionParameters(spec$distribution) else spec$parameters
  # name(s) of the data subdimension(s) holding the items
  item_dims <- if (is.null(spec$dim)) "variable" else spec$dim

  # rows 3-12 of the sheet contain the regions, rows 1-2 the headers
  # col_types = "list" keeps the type of each cell: text for regions and distribution labels, numeric otherwise
  path <- file.path("v1", "data_cement_GAS_EoL_MISO_9regions.xlsx")
  sheet <- readxl::read_xlsx(
    path,
    sheet = "Uptake", range = "A3:GB12", col_names = FALSE, col_types = "list", .name_repair = "minimal"
  )
  # first column holds the region names
  regions <- unlist(sheet[[1]])

  # collect one long-format chunk (region, item, parameter, value) per item and parameter
  chunks <- list()
  for (item in names(spec$columns)) {
    # Excel column letter -> column index in the sheet
    start <- cellranger::letter_to_num(spec$columns[[item]])
    if (!is.null(spec$distribution)) {
      # the first column holds the distribution label; check it, then move on to the parameters
      if (!identical(unlist(sheet[[start]]), rep(spec$distribution, length(regions)))) {
        stop("Expected distribution ", spec$distribution, " in column ", spec$columns[[item]], " for ", item, ".")
      }
      start <- start + 1
    }
    # parameters follow in consecutive columns
    for (i in seq_along(parameters)) {
      values <- unlist(sheet[[start + i - 1]])
      if (!is.numeric(values) || length(values) != length(regions) || anyNA(values)) {
        stop("Non-numeric or missing value for parameter ", parameters[[i]], " of ", item, " (subtype ", subtype, ").")
      }
      chunks[[length(chunks) + 1]] <- data.frame(region = regions, item = item, parameter = parameters[[i]], value = values)
    }
  }
  # stack the chunks and split multi-dimensional items ("new concrete.A") into one column per dimension
  data <- tidyr::separate_wider_delim(do.call(rbind, chunks), "item", delim = ".", names = item_dims)

  # regions become the spatial dimension; item(s) and parameter become data subdimensions
  x <- as.magpie(data, spatial = "region", datacol = "value")
  return(x)
}
