#' Get BACI Trade data for specific product groups
#'
#' @param subtype Character string specifying the scope
#'        - "plastics_UNCTAD": plastics trade data for HS codes from UNCTAD classification into
#'          primary, intermediate, manufactured, final and waste plastics
#'        - "plastics_UNEP": plastics trade data for HS codes from UNEP NGP
#'          (estimated plastics mass based on estimated plastics percentages in goods)
#' @param HS Character string specifying the year of the HS (Harmonized System) revision of the data
#'        - 92
#'        - 02
#'        - 17
#'        - 22
#'
#' @return magpie object of the BACI trade data
#'
#' @author Leonie Schweiger
#'
#' @seealso [calcOutput()]
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "BACI", subtype = "plastics_UNCTAD", HS = "02")
#' }
#' @importFrom dplyr select filter rename summarize ungroup
#' @importFrom magclass as.magpie getComment<-
#'
calcBACI <- function(subtype, HS = "02") {
  # Read raw data
  BACI_data <- readSource("BACI", subtype = subtype, subset = HS)
  df <- quitte::madrat_mule(BACI_data)

  if (subtype == "plastics_UNCTAD") {
    final <- df
  } else if (subtype == "plastics_UNEP") {
    # map UNEP-NGP sectors to sectors used in REMIND-MFA
    sector_map <- toolGetMapping("sectormappingUNEP_NGP.csv", type = "sectoral", where = "mrmfa")
    new1 <- df %>%
      left_join(sector_map, by = c("sector" = "Source")) %>%
      select(-"sector") %>%
      rename("sector" = "Target")

    # map UNEP-NGP polymers to polymers used in REMIND-MFA
    polymer_map <- toolGetMapping("polymermappingUNEP_NGP.csv", type = "sectoral", where = "mrmfa")
    # use polymer use by sector as weights (summarize over all Regions,
    # as polymer share by sector is constant over all Regions in OECD data);
    # use total polymer use over all sectors as weights for "General" sector
    use <- calcOutput("PlOECD", subtype = "Use_2019_region", aggregate = TRUE) %>% as.data.frame()
    use_by_sector <- use %>%
      rename(sector = "Data2", polymer = "Data1") %>%
      filter(.data$polymer != "Total") %>%
      group_by(.data$sector, .data$polymer) %>%
      summarize(value = sum(.data$Value)) %>%
      mutate(sector = case_when(.data$sector == "Total" ~ "General", .default = .data$sector))
    split <- merge(polymer_map, use_by_sector, by.y = "polymer", by.x = "Target") %>%
      group_by(.data$sector, .data$Source) %>%
      mutate(
        total = sum(.data$value, na.rm = TRUE),
        weight = .data$value / .data$total
      ) %>%
      select(-"total", -"value")
    new2 <- left_join(new1, split, by = c("polymer" = "Source", "sector")) %>%
      mutate(value = .data$value * .data$weight)
    # some weights are NaN for polymers that are not used in a specific sector according to OECD,
    # throw a warning if these combination appear in the dataset
    nan <- new2 %>% filter(is.na(.data$weight))
    if (nrow(nan) > 0) {
      warning(paste(
        "The following sector-polymer combinations cannot be mapped from the BACI data
        as they do not exist in the OECD dataset used for weighting:\n",
        paste(capture.output(print(nan)), collapse = "\n")
      ))
    }

    final <- new2 %>%
      select(-"polymer", -"weight") %>%
      rename("polymer" = "Target") %>%
      group_by(.data$t, .data$exporter, .data$importer, .data$polymer, .data$stage, .data$sector) %>%
      summarize(value = sum(.data$value)) %>%
      ungroup()
  }

  x <- as.magpie(final, temporal = "t", spatial = "importer")
  x <- toolCountryFill(x, fill = NA, verbosity = 2)
  x <- replace_non_finite(x, replace = 0)

  # define a custom aggregation function that filters out all intra-regional trade
  # and returns both imports and exports for each region in the region mapping
  .customAggregate <- function(x, rel) {
    df <- tibble::as_tibble(x)

    # get grouping variables
    group_vars <- setdiff(colnames(df), c("t", "importer", "exporter", "value"))

    # make sure that exporter_region != importer_region for every entry
    df <- df %>%
      left_join(rel[, c("country", "region")], by = c("importer" = "country")) %>%
      left_join(rel[, c("country", "region")], by = c("exporter" = "country")) %>%
      select("t", "importer" = "region.x", "exporter" = "region.y", all_of(group_vars), "value") %>%
      filter(.data$importer != .data$exporter)

    imports <- df %>%
      group_by(.data$t, .data$importer, across(all_of(group_vars))) %>%
      summarize(value = sum(.data$value, na.rm = TRUE)) %>%
      ungroup() %>%
      rename("region" = "importer") %>%
      mutate("type" = "Imports")

    exports <- df %>%
      group_by(.data$t, .data$exporter, across(all_of(group_vars))) %>%
      summarize(value = sum(.data$value, na.rm = TRUE)) %>%
      ungroup() %>%
      rename("region" = "exporter") %>%
      mutate("type" = "Exports")

    x <- rbind(imports, exports) %>%
      select("period" = "t", "region", "type", all_of(group_vars), "value") %>%
      as.magpie()

    return(x)
  }

  return(list(
    x = x,
    weight = NULL,
    unit = "Mt Plastic",
    aggregationFunction = .customAggregate,
    description = "Plastic trade data from BACI"
  ))
}
