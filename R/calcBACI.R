#' Get BACI Trade data for specific product groups
#'
#' @param subtype Character string specifying the scope
#'        - "plastics_UNCTAD": plastics trade data for HS codes from UNCTAD classification into primary, intermediate, manufactured, final and waste plastics
#'        - "plastics_UNEP": plastics trade data for HS codes from UNEP NGP (estimated plastics mass based on estimated plastics percentages in goods)
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
    # summarize by group
    final <- df %>% group_by(.data$t, .data$exporter, .data$importer, .data$Group) %>%
      summarize(value=sum(.data$value, na.rm=T)) %>%
      ungroup()
  }

  if (subtype == "plastics_UNEP"){
    # remove data that is unreasonable (extreme outliers) and interpolate instead
    unreasonable <- data.frame(
      exporter = c("MEX", "NGA", "NGA", "CHE"),
      importer = c("USA", "ATG", "ATG", "MOZ"),
      t   = c(2004, 2010, 2011, 2016),
      k   = c(392310, 550320, 550320, 6309)
    )
    df_clean <- df %>%
      left_join(
        unreasonable %>% mutate(flag_unreasonable = TRUE),
        by = c("t", "exporter", "importer", "k")
      ) %>%
      mutate(value = case_when(.data$flag_unreasonable ~ NA, .default = .data$value)) %>%
      group_by(.data$exporter, .data$importer, .data$k, .data$polymer, .data$stage, .data$sector) %>%
      dplyr::arrange(.data$t) %>%
      mutate(value_interp = zoo::na.approx(.data$value, x = .data$t, na.rm = FALSE)) %>%
      ungroup() %>%
    # remove trade data of 220190 "Waters; other than mineral and aerated, (not containing added sugar or other sweetening matter nor flavoured), ice and snow"
    # as this category unreasonably inflates trade between HKG and CHN
      filter(.data$k!=220190)

    # summarize df and include only relevant categories
    df_sum <- df_clean %>%
      group_by(.data$t, .data$exporter, .data$importer, .data$polymer, .data$stage, .data$sector) %>%
      summarize(value=sum(.data$value_interp)) %>%
      ungroup()

    # map UNEP-NGP sectors to sectors used in REMIND-MFA
    sector_map <- toolGetMapping("sectormappingUNEP_NGP.csv", type = "sectoral", where = "mrmfa")
    new1 <- df_sum %>%
      left_join(sector_map, by = c("sector" = "Source")) %>%
      select(-"sector") %>%
      rename("sector" = "Target") %>%
      group_by(.data$t, .data$exporter, .data$importer, .data$polymer, .data$stage, .data$sector) %>%
      summarize(value=sum(.data$value)) %>%
      ungroup()

    # map UNEP-NGP polymers to polymers used in REMIND-MFA
    polymer_map <- toolGetMapping("polymermappingUNEP_NGP.csv", type = "sectoral", where = "mrmfa")
    # use polymer use by sector as weights (summarize over all Regions, as polymer share by sector is constant over all Regions in OECD data)
    # use total polymer use over all sectors as weights for "General" sector
    use <- calcOutput("PlOECD",subtype = "Use_2019_region", aggregate = TRUE) %>% as.data.frame()
    use_by_sector <- use %>%
      rename(sector = "Data2", polymer = "Data1") %>%
      filter(.data$polymer!="Total") %>%
      group_by(.data$sector, .data$polymer) %>%
      summarize(value = sum(.data$Value)) %>%
      mutate(sector = case_when(.data$sector=="Total"~"General", .default=.data$sector))
    split <- merge(polymer_map, use_by_sector, by.y="polymer", by.x="Target") %>%
      group_by(.data$sector, .data$Source) %>%
      mutate(
        total = sum(.data$value, na.rm = TRUE),
        weight = .data$value / .data$total
      ) %>%
      select(-"total", -"value")
    new2 <- left_join(new1, split, by = c("polymer" = "Source", "sector")) %>%
      mutate(value = .data$value * .data$weight)
    # some weights are NaN for polymers that are not used in a specific sector according to OECD, throw a warning if these combination appear in the dataset
    nan <- new2 %>% filter(is.na(.data$weight))
    if (nrow(nan)>0){
      warning(paste(
        "The following sector-polymer combinations cannot be mapped from the BACI data as they do not exist in the OECD dataset used for weighting:\n",
        paste(capture.output(print(nan)), collapse = "\n")
      ))
    }

    final <- new2 %>%
      select(-"polymer", -"weight") %>%
      rename("polymer" = "Target") %>%
      group_by(.data$t, .data$exporter, .data$importer, .data$polymer, .data$stage, .data$sector) %>%
      summarize(value=sum(.data$value)) %>%
      ungroup()

  }

  x <- as.magpie(final, temporal = "t", spatial = c("exporter","importer"))
  x <- toolCountryFill(x)
  x <- replace_non_finite(x, replace = 0)

  # define a custom aggregation function that filters out all intra-regional trade
  # it should return both imports and exports for each region in the region mapping
  # i.e. sum up values once for all exporter countries within a region (label as exports) and once for all importer countries within a region (label as imports)
  # before that make sure that exporter_region != importer_region for every entry
  .customAggregate <- function(x, rel) {


    out <- toolAggregate(x, rel = rel)

    return(out)
  }

  return(list(
    x = x,
    weight = NULL,
    unit = "Mt Plastic",
    aggregationFunction = .customAggregate,
    description = "Plastic trade data from BACI"
  ))
}

# UNCTAD_codes <- product_groups %>% mutate(k4 = as.integer(as.integer(Code/100)))
# k4 <- merge(UNCTAD_codes, UNEP_codes, by.x="k4", by.y="code")
# k6 <- merge(UNCTAD_codes, UNEP_codes, by.x="Code", by.y="code")
# all <- rbind(k4, k6)
# test <- merge(all, UNCTAD_codes, by=c("Code","Label","Group","k4"), all.x=T, all.y=T)
# test <- merge(test, UNEP_codes_k4, by.x=c("region", "k4", "polymer", "application", "stage", "sector", "label"), by.y=c("region", "code", "polymer", "application", "stage", "sector", "label"), all.x=T, all.y=T)
# test <- merge(test, UNEP_codes_k6, by.x=c("region", "Code", "polymer", "application", "stage", "sector", "label"), by.y=c("region", "code", "polymer", "application", "stage", "sector", "label"), all.x=T, all.y=T)
