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
#' @param category Character string specifying the stage of trade, valid parameters depend on the subtype
#'        for plastics_UNCTAD:
#'        - "Final"        - Final plastics
#'        - "Primary"      - Primary plastics
#'        - "Intermediate" - Intermediate forms of plastic
#'        - "Manufactured" - Intermediate manufactured plastic goods
#'        - "Application"  - Plastic goods
#'        - "Waste"        - Plastic waste
#'        for plastics_UNEP:
#'        - "Primary"
#'        - "Application"
#'        - "Waste"
#'
#' @return magpie object of the BACI trade data
#'
#' @author Leonie Schweiger
#'
#' @seealso [calcOutput()]
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "BACI", subtype = "plastics_UNCTAD", HS = "02", category = "Plastics in primary forms")
#' }
#' @importFrom dplyr select filter rename summarize ungroup
#' @importFrom magclass as.magpie getComment<-
#'
calcBACI <- function(subtype, HS = "02", category) {

  # map category
  category <- switch(category,
              "Primary" = case_when(subtype=="plastics_UNCTAD"~"Plastics in primary forms", .default="Primary"),
              "Intermediate" = "Intermediate forms of plastic",
              "Manufactured" = "Intermediate manufactured plastic goods",
              "Final" = "Final manufactured plastics goods",
              "Waste" = case_when(subtype=="plastics_UNCTAD"~"Plastic waste", .default="Waste"),
              "Application" = "Application",
              stop("Unsupported category: ", category)
  )

  # Read raw data
  df <- readSource("BACI", subtype = paste(subtype, category, sep="-"), subset = HS) %>%
    quitte::madrat_mule()

  if (subtype == "plastics_UNEP") {
    # map UNEP-NGP sectors to sectors used in REMIND-MFA
    sector_map <- toolGetMapping("sectormappingUNEP_NGP.csv", type = "sectoral", where = "mrmfa")
    df <- df %>%
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
    df <- left_join(df, split, by = c("polymer" = "Source", "sector")) %>%
      mutate(value = .data$value * .data$weight)
    # some weights are NaN for polymers that are not used in a specific sector according to OECD,
    # throw a warning if these combination appear in the dataset
    nan <- df %>% filter(is.na(.data$weight))
    if (nrow(nan) > 0) {
      warning(paste(
        "The following sector-polymer combinations cannot be mapped from the BACI data
        as they do not exist in the OECD dataset used for weighting:\n",
        paste(utils::capture.output(print(nan)), collapse = "\n")
      ))
    }

    df <- df %>%
      select(-"polymer", -"weight") %>%
      rename("polymer" = "Target") %>%
      group_by(.data$t, .data$exporter, .data$importer, .data$polymer, .data$sector) %>%
      summarize(value = sum(.data$value)) %>%
      ungroup()

  }

  # historical ISO countries SCG and ANT split into SRB & MNE in 2006 and SXM & CUW in 2011, respectively
  # for simplicity, their trades are assigned to their major successor countries SRB and CUW before the split year
  # as they account for >90% of the total plastics trade volume of successor countries
  df$exporter[df$exporter == "SCG"] <- "SRB"
  df$importer[df$importer == "SCG"] <- "SRB"
  df$exporter[df$exporter == "ANT"] <- "CUW"
  df$importer[df$importer == "ANT"] <- "CUW"

  x <- quitte::madrat_mule(df)

  return(list(
    x = x,
    weight = NULL,
    unit = "Mt Plastic",
    description = "Plastic trade data from BACI"
  ))
}
