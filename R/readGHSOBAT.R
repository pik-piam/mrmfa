#' Read GHS-OBAT: Global Human Settlement - Open Buildings Attribute Table
#' Florio, Pietro; Politis, Panagiotis; Goch, Katarzyna; Uhl, Johannes H; Melchiorri, Michele; Pesaresi, Martino; Kemper, Thomas (2024):
#' GHS-OBAT R2024A - Global Open Building Attribute Table at footprint level, with age, function, height and compactness information (2020).
#' European Commission, Joint Research Centre (JRC) [Dataset] doi: 10.2905/f41a22f1-5741-4c41-86eb-6384654f6927
#' PID: http://data.europa.eu/89h/f41a22f1-5741-4c41-86eb-6384654f6927
#' Global countrystats downloaded from
#' https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_OBAT_GLOBE_R2024A/GHS_OBAT_COUNTRYSTATS_GLOBE_R2024A/V1-0/
#'
#' @param subtype Data to extract.
#'                "surface" for total footprint building differentiated by residential and non-residential.
#'                "height" for average building height (not differentiated)
#' @author Bennet Weiss
readGHSOBAT <- function(subtype) {
  version <- "vR2024A"

  name <- paste0("GHS_OBAT_COUNTRYSTATS_E2020_GLOBE_R2024A_V1_0.xlsx")
  path <- file.path(version, name)
  data <- readxl::read_xlsx(path)

  if (subtype == "height") {
    x <- data[c("country", "Height_average")]
    x <- dplyr::rename(x, Region = country, Value = Height_average)
    x <- magclass::as.magpie(x, temporal = 0, spatial = 1)
  } else if (subtype == "surface") {
    x <- data[c("country", "Footprint_surface_ha_Use_1", "Footprint_surface_ha_Use_2")]
    # Use 0 footprints outside of the built-up domain (found on OSM, but not on Sentinel-2 satellite data)
    # Use 1 residential (mostly) [following INSPIRE definition]
    # Use 2 non-residential (exclusively) [following INSPIRE definition]
    # INSPIRE: C. Corbane et al., “A global cloud free pixel- based image composite from Sentinel-2 data,”
    # Data in Brief, vol. 31, p. 105737, Aug. 2020, doi: 10.1016/j.dib.2020.105737.

    # split Use 0 into residential and non-residential based on the ratio of Use 1 and Use 2
    ratio_use_1 <- x[["Footprint_surface_ha_Use_1"]] / (x[["Footprint_surface_ha_Use_1"]] + x[["Footprint_surface_ha_Use_2"]])
    ratio_use_1[is.na(ratio_use_1)] <- mean(ratio_use_1, na.rm = TRUE) # TODO: weighted average
    ratio_use_2 <- 1 - ratio_use_1
    x["Footprint_surface_ha_Use_1"] <- x["Footprint_surface_ha_Use_1"] + data["Footprint_surface_ha_Use_0"] * ratio_use_1
    x["Footprint_surface_ha_Use_2"] <- x["Footprint_surface_ha_Use_2"] + data["Footprint_surface_ha_Use_0"] * ratio_use_2
    x <- dplyr::rename(x, residential = Footprint_surface_ha_Use_1,
                       non_residential = Footprint_surface_ha_Use_2)
    x <- tidyr::pivot_longer(x, c("residential", "non_residential"),
                             names_to = "variable", values_to = "Value")
    x <- magclass::as.magpie(x, temporal = 0, spatial = 1)
  } else {
    stop("Invalid subtype for GHSOBAT read")
  }
  return(x)
}
