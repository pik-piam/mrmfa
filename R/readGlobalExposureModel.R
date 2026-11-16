#' Read structure type, function type and total floor area of buildings from the Global Exposure Model (GEM).
#' Residential floor area is split into single and multi family homes by storeys and dwellings per building,
#' see \code{\link{toolSingleFamilyShare}}.
#'
#' Release v2026.0.0 (GEM Taxonomy v4.0, building counts and population of 2025).
#' Yepes-Estrada, C., Calderon, A., Costa, C., Crowley, H., Dabbeek, J., Hoyos,
#' M., Martins, L., Paul, N., Rao, A., Silva, V. (2023).
#' Global Building Exposure Model for Earthquake Risk Assessment. Earthquake Spectra. doi:10.1177/87552930231194048
#' Available on Github: https://github.com/gem/global_exposure_model
#' @author Bennet Weiss.
readGlobalExposureModel <- function() {
  version <- "v2026.0.0"
  regions <- list.dirs(path = version, full.names = TRUE, recursive = FALSE)
  regions <- regions[basename(regions) != "World"]
  # GEM uses upper case occupancy classes. NONRES (only New Zealand) has no split into commercial and industrial
  # and is counted as commercial.
  occupancyNames <- c(COM = "Com", IND = "Ind", RES = "Res", NONRES = "Com")
  all_data <- list()

  i <- 1
  for (region in regions) {
    countries <- list.dirs(path = region, full.names = TRUE, recursive = FALSE)
    countries <- countries[basename(countries) != "_region"]
    for (country in countries) {
      # read country data
      path <- file.path(country, "summaries", "Exposure_Summary_Taxonomy.csv")
      if (!file.exists(path)) {
        stop("GEM summary file not found: ", path)
      }
      data <- readr::read_csv(
        path,
        col_names = TRUE,
        col_select = c("ID_0", "OCCUPANCY", "TAXONOMY", "BUILDINGS", "DWELLINGS", "TOTAL_AREA_SQM"),
        show_col_types = FALSE
      )
      stopifnot("Unknown GEM occupancy" = data$OCCUPANCY %in% names(occupancyNames))
      data$OCCUPANCY <- unname(occupancyNames[data$OCCUPANCY])

      # remove rows with unknown area
      data <- data[!is.na(data$TOTAL_AREA_SQM), ]

      # split residential area into single and multi family homes, other functions are the occupancy
      isRes <- data$OCCUPANCY == "Res"
      rsShare <- toolSingleFamilyShare(data$TAXONOMY[isRes], data$DWELLINGS[isRes], data$BUILDINGS[isRes])
      singleFamily <- data[isRes, ]
      singleFamily$FUNCTION <- "Residential Single-Family"
      singleFamily$TOTAL_AREA_SQM <- singleFamily$TOTAL_AREA_SQM * rsShare
      multiFamily <- data[isRes, ]
      multiFamily$FUNCTION <- "Residential Multi-Family"
      multiFamily$TOTAL_AREA_SQM <- multiFamily$TOTAL_AREA_SQM * (1 - rsShare)
      other <- data[!isRes, ]
      functionNames <- c(Com = "Commercial", Ind = "Industrial")
      other$FUNCTION <- unname(functionNames[other$OCCUPANCY])
      data <- rbind(singleFamily, multiFamily, other)

      # structure is the material token of the taxonomy, e.g. MUR+ADO or HYB(W;M). It is mapped in convert.
      data$Structure <- sub("/.*$", "", data$TAXONOMY)

      # aggregate floor space
      aggregated_data <- dplyr::group_by(data, .data$ID_0, .data$OCCUPANCY, .data$Structure, .data$FUNCTION) %>%
        dplyr::summarise(TOTALAREA_SQM = sum(.data$TOTAL_AREA_SQM), .groups = "drop")

      all_data[[i]] <- aggregated_data
      i <- i + 1
    }
  }
  combined_data <- do.call(rbind, all_data)
  colnames(combined_data) <- c("ISO3", "Stock_Type", "Structure", "Function", "Total Area (sqm)")

  x <- magclass::as.magpie(combined_data, spatial = 1)

  return(x)
}

#' Share of single family homes in the floor area of residential GEM building classes.
#'
#' Definition: single family homes are detached and row houses, multi family homes are
#' apartment buildings. The share depends on the number of storeys h (midpoint of the GEM height range)
#' and the mean number of dwellings per building d:
#' \itemize{
#'   \item h = 1: single family. Low buildings with several dwellings are mostly row houses.
#'   \item h >= 4: multi family. Almost all such classes in GEM have more than 3 dwellings per building.
#'   \item 1 < h < 4: a mix of single family buildings and apartment buildings. Single family buildings
#'     have on average s = 1.5 dwellings (semi-detached and row houses), apartment buildings k = 2 h
#'     (2 flats per floor). The share of apartment buildings is f = (d - s) / (k - s), between 0 and 1.
#'     Single family dwellings are (assumed) alpha = 2 times larger than flats, so the single family share
#'     of floor area is alpha s (1 - f) / (alpha s (1 - f) + k f).
#'   \item no storey code, or no dwelling or building count: 0.5. These classes have almost no floor area.
#' }
#'
#' @param taxonomy GEM taxonomy strings, height codes as in H:1, H:2-3, H:>5, H:>5+HB:1 or HAPP:25.
#'   Only the storey codes H and HAPP are read, other height codes such as HB are ignored.
#' @param dwellings Number of dwellings of each class.
#' @param buildings Number of buildings of each class.
#' @return Share of single family homes in the floor area of each class, between 0 and 1.
#' @author Bennet Weiss
toolSingleFamilyShare <- function(taxonomy, dwellings, buildings) {
  dwellingsSingleFamily <- 1.5 # dwellings per single family building (semi-detached and row houses)
  flatsPerFloor <- 2 # dwellings per floor of an apartment building
  dwellingSizeRatio <- 2 # floor area of a single family dwelling relative to a flat

  storeys <- toolGemStoreys(taxonomy)
  dwellingsPerBuilding <- ifelse(dwellings > 0 & buildings > 0, dwellings / buildings, NA)

  # mix of single family and apartment buildings that gives the mean dwellings per building
  dwellingsApartment <- flatsPerFloor * storeys
  apartmentBuildingShare <- (dwellingsPerBuilding - dwellingsSingleFamily) / (dwellingsApartment - dwellingsSingleFamily)
  apartmentBuildingShare <- pmin(pmax(apartmentBuildingShare, 0), 1)
  singleFamilyArea <- (1 - apartmentBuildingShare) * dwellingsSingleFamily * dwellingSizeRatio
  apartmentArea <- apartmentBuildingShare * dwellingsApartment
  share <- singleFamilyArea / (singleFamilyArea + apartmentArea)

  share[is.na(share)] <- 0.5
  share[storeys %in% 1] <- 1
  share[!is.na(storeys) & storeys >= 4] <- 0
  return(share)
}

#' Number of storeys of GEM building classes.
#'
#' Ranges such as H:2-3 give their midpoint, open ranges such as H:>5 give the smallest value (6).
#'
#' @param taxonomy GEM taxonomy strings, see \code{\link{toolSingleFamilyShare}}.
#' @return Number of storeys, NA if the taxonomy has no storey code H or HAPP.
#' @author Bennet Weiss
toolGemStoreys <- function(taxonomy) {
  # only codes for storeys above ground count, not e.g. HB (below ground) or HF (ground floor height)
  storeyCodes <- "H|HAPP"
  heightCode <- ifelse(
    grepl(paste0("(^|/)(", storeyCodes, "):"), taxonomy),
    sub(paste0("^(.*/)?(", storeyCodes, "):([^/+]*).*$"), "\\3", taxonomy),
    NA
  )
  lower <- rep(NA_real_, length(taxonomy))
  upper <- rep(NA_real_, length(taxonomy))
  single <- grepl("^[0-9]+$", heightCode)
  range <- grepl("^[0-9]+-[0-9]+$", heightCode)
  open <- grepl("^>[0-9]+$", heightCode)
  lower[single] <- upper[single] <- as.numeric(heightCode[single])
  lower[range] <- as.numeric(sub("-.*$", "", heightCode[range]))
  upper[range] <- as.numeric(sub("^.*-", "", heightCode[range]))
  lower[open] <- upper[open] <- as.numeric(sub(">", "", heightCode[open])) + 1
  return((lower + upper) / 2)
}
