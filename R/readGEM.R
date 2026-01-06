#' Read structure type, function type, height, and total floor area of buildings from the Global Exposure Model (GEM).
#' Extract residential building type (single RS/multi family RM) from building height proxy.
#'
#' Yepes-Estrada, C., Calderon, A., Costa, C., Crowley, H., Dabbeek, J., Hoyos, M., Martins, L., Paul, N., Rao, A., Silva, V. (2023).
#' Global Building Exposure Model for Earthquake Risk Assessment. Earthquake Spectra. doi:10.1177/87552930231194048
#' Repository on: https://zenodo.org/records/8223926
#' Available on Github: https://github.com/gem/global_exposure_model
#' @author Bennet Weiss.
readGEM <- function() {
  # issues with this approach:
  # - I am omitting Mixed or unspecified housing types at the beginning, which may lead to errors.

  version <- "v2023.1.1"
  regions <- list.dirs(path = version, full.names = TRUE, recursive = FALSE)
  regions <- regions[basename(regions) != "World"]
  all_data <- list()

  i <- 1
  for (region in regions) {
    countries <- list.dirs(path = region, full.names = TRUE, recursive = FALSE)
    countries <- countries[basename(countries) != "_Metadata"]
    for (country in countries) {
      # read country data
      path <- file.path(country, "Exposure_Summary_Taxonomy.csv")
      data <- readr::read_csv(
        path,
        col_names = TRUE,
        col_select = c("ID_0", "OCCUPANCY", "MACRO_TAXO", "TAXONOMY", "TOTAL_AREA_SQM"),
        show_col_types = FALSE
      )

      # extract building function type from taxonomy
      data["FUNCTION"] <- toolInferResBuildingType(data)

      # align structure types with RASMI
      data$MACRO_TAXO <- toolAggregateFunctionType(data$MACRO_TAXO)

      # remove taxonomy column and "other" function/structure rows
      data <- data[, -which(names(data) == "TAXONOMY")]
      data <- data[!is.na(data$FUNCTION), ]
      data <- data[!is.na(data$MACRO_TAXO), ]

      # remove rows with unknown area
      data <- data[!is.na(data$TOTAL_AREA_SQM), ]

      # aggregate floor space
      aggregated_data <- group_by(data, ID_0, OCCUPANCY, MACRO_TAXO, FUNCTION) %>%
        summarise(TOTALAREA_SQM = sum(TOTAL_AREA_SQM), .groups = "drop")

      all_data[[i]] <- aggregated_data
      i <- i + 1
    }
  }
  combined_data <- do.call(rbind, all_data)
  colnames(combined_data) <- c("ISO3", "Stock_Type", "Structure", "Function", "Total Area (sqm)")

  # fix for NZL: sort NonRes to Com for Stock_Type and Function
  combined_data$Stock_Type[combined_data$Stock_Type == "NonRes"] <- "Com"
  combined_data$Function[combined_data$Function == "NonRes"] <- "Com"

  x <- magclass::as.magpie(combined_data, spatial = 1)
  return(x)
}

#' Extracts the int number of stories for a taxonomy string as used in GEM.
#' @param taxonomy string or list of strings to check.
#' @author Bennet Weiss
toolExtractNStories <- function(taxonomy) {
  # Primary pattern to match:
  # 1) H + optional non-colon chars + ":" + number or number-range
  # OR
  # 2) H + optional non-colon chars immediately followed by number or number-range (no colon)
  pattern1 <- "H[^:]*:([0-9]+(?:-[0-9]+)?)"
  pattern2 <- "H[^:]*([0-9]+(?:-[0-9]+)?)"
  pattern <- paste0(pattern1, "|", pattern2)

  m <- regexpr(pattern, taxonomy, perl = TRUE)
  matches <- regmatches(taxonomy, m)
  # Extract only the storey number
  n_stories <- rep(NA_character_, length(taxonomy))
  has_match <- m != -1
  remove_pattern <- "^[^0-9]*([0-9][-+0-9]*).*"
  n_stories[has_match] <- sub(remove_pattern, "\\1", matches)

  # Fallback: try Res information
  # implicitly set RES number to storey number. As RES1 is SF and RES2 is mobile home, they can both reasonably be grouped as SF later.
  no_match <- !has_match
  if (any(no_match)) {
    # Pattern to match: Res followed by a number
    pattern_RES <- "RES([0-9]+)"
    m_RES <- regexpr(pattern_RES, taxonomy[no_match], perl = TRUE)
    matches_RES <- regmatches(taxonomy[no_match], m_RES)
    has_RES <- m_RES != -1
    n_stories[no_match][has_RES] <- sub("RES", "", matches_RES[has_RES])
    no_match[no_match][has_RES] <- FALSE
  }

  # raise warning if still no height found and not in exceptions
  exceptions <- c("MIX", "UNK", "MATO")
  if (any(no_match)) {
    failed <- taxonomy[no_match]
    # Keep only failed entries that do not contain any exception
    non_exception <- failed[!sapply(failed, function(f) any(sapply(exceptions, function(e) grepl(e, f))))]

    if (length(non_exception) > 0) {
      warning("No height found in the following entries:\n",
              paste0(" - ", non_exception, collapse = "\n"))
    }
  }
  return(n_stories)
}

#' Categorizes res buildings into single family (RS) or multi family (RM) homes based on their number of stories.
#' @param data Dataframe that contains a column OCCUPANCY with building function types and a column TAXONOMY with taxonomy strings.
#' @author Bennet Weiss
toolInferResBuildingType <- function(data) {
  function_type <- data$OCCUPANCY
  res_idx <- function_type == "Res"
  n_stories <- toolExtractNStories(data$TAXONOMY[res_idx])
  # For storey ranges, get mean.
  splitted_stories <- strsplit(n_stories, "[+-]")
  numeric_stories <- lapply(splitted_stories, function(data) as.integer(data))
  avg_stories <- sapply(numeric_stories, mean)
  # Classify as single family (RS) if 1 or 2 stories, multi family (RM) otherwise.
  new_types <- ifelse(avg_stories <= 2, "RS", "RM")
  new_types[is.na(avg_stories)] <- NA

  function_type[res_idx] <- new_types
  return(function_type)
}


#' Aggregate building strucutre types to match RASMI definition.
#'
#' @author Bennet Weiss
#' @param structure Vector of building structure types as used in GEM.
toolAggregateFunctionType <- function(structure) {
  mapping = c(
    "S" = "S",     # Steel
    "W" = "T",     # Wood
    "RC" = "C",    # Reinforced Concrete
    "M" = "M",     # Masonry
    "MUR" = "M",   # Unreinforced Masonry
    "MR" = "M",    # Reinforced Masonry
    "MCF" = "M",   # Confined Masonry
    "ADO" = "M",   # Adobe (see material_intensity_db/data/buildings.csv)
    "ADO/E" = "M", # Adobe/Earth
    "MIX" = NA,    # Mixed-types (not in RASMI)
    "OT" = NA      # Other (not in RASMI)
  )
  mapped <- mapping[structure]

  unmatched <-  structure[!structure %in% names(mapping) & !is.na(structure)]
  if(length(unmatched) > 0) {
    warning("Unmatched elements: ", paste(unique(unmatched), collapse = ", "))
  }

  return(mapped)
}
