#' Read BACI Trade data
#'
#' @param subset Character string specifying the HS (Harmonized System) revision of the data
#'        - 92
#'        - 02
#'        - 17
#'        - 22
#' @param subtype Character string specifying the scope, combination of material HS codes and category
#'        material HS codes:
#'        - "plastics_UNCTAD": plastics trade data for HS codes from UNCTAD classification into
#'          primary, intermediate, manufactured, final and waste plastics
#'          valid parameters for category:
#'          - Plastics in primary forms
#'          - Intermediate forms of plastic
#'          - Intermediate manufactured plastic goods
#'          - Final manufactured plastics goods
#'          - Plastic waste
#'        - "plastics_UNEP": plastics trade data for HS codes from UNEP NGP
#'          (estimated plastics mass based on estimated plastics percentages in goods)
#'          valid parameters for category:
#'          - Primary
#'          - Application
#'          - Waste
#'
#'
#' @return magpie object of the BACI trade data
#'
#' @author Leonie Schweiger
#'
#' @seealso [readSource()]
#'
#' @examples
#' \dontrun{
#' a <- readSource(type = "BACI", subtype = "plastics_UNEP-Primary", subset = "02")
#' }
#' @importFrom dplyr select filter rename summarize ungroup
#'
readBACI <- function(subtype, subset = "02") {
  # check whether subset is one of the available HS revisions
  available <- c("92", "02", "17", "22")
  if (is_empty(intersect(subset, available))) {
    stop(
      "Invalid subset -- supported HS revisions are:",
      paste0(available, collapse = ", ")
    )
  }

  # Parse subtype and validate
  parts <- strsplit(subtype, "-")[[1]]
  if (length(parts) != 2) {
    stop("Subtype must have two components, e.g. 'plastics_UNEP-Primary'.")
  }
  key <- parts[1] # e.g., "plastics_UNEP"
  category <- parts[2] # e.g., "Primary"

  # read HS codes that are relevant for the scope defined in subtype
  if (key == "plastics_UNCTAD") {
    # if an older HS revision than 2002 is used, use the oldest available (2002)
    if (!(subset %in% c("02", "07", "12", "17", "22"))) {
      HS <- "02"
    } else {
      HS <- subset
    }
    UNCTAD_revision <- paste0("DimHS20", HS, "Products_Plastics_Hierarchy.xls")
    UNCTAD_product_codes <- read_excel(file.path("UNCTAD_PlasticsHSCodes", UNCTAD_revision), skip = 2)
    # Identify header rows
    is_header <- grepl("^P_", UNCTAD_product_codes[[1]])
    # Create a new variable from column 2 of header rows
    UNCTAD_product_codes$Group <- UNCTAD_product_codes[[2]][is_header][cumsum(is_header)]
    # Remove the header rows
    codes <- UNCTAD_product_codes[!is_header, ] %>%
      dplyr::rename(code = "Code", group = "Group") %>%
      filter(.data$group == category) %>% select(-"group")
    codes$code <- as.integer(codes$code)
  } else if (key == "plastics_UNEP") {
    codes <- read_excel(file.path("UNEP_NGP", "TOOL_T1.4a_v1.2_Trade data modelling.xlsx"),
      sheet = "SelectedCOMCodes", skip = 12
    ) %>%
      select("code" = "Code", "polymer" = "Polymer Type", "application" = "Application Type",
             "stage" = "Type", "sector" = "Sector", "label" = "Extensive description on comtrade",
             "plastic_percentage" = "Plastic percentage") %>%
      filter(.data$stage == category) %>% select(-"stage")

    # remove duplicates in raw data; label all polymers in the textile sector as "Fibres"
    codes <- unique(codes) %>%
      mutate(polymer = case_when(.data$sector == "Textile" ~ "Fibres", .default = .data$polymer))
    # get selected 4-digit and 6-digit COMTRADE codes from UNEP_NGP
    UNEP_codes_k4 <- codes %>% filter(.data$code < 10000)
    UNEP_codes_k6 <- codes %>% filter(.data$code > 10000)
  } else {
    stop("Invalid subtype. Choose either 'plastics_UNCTAD' or 'plastics_UNEP'.")
  }

  # Read raw trade data of respective HS classification system
  data_path <- paste0("BACI_HS", subset, "_V202501")
  files <- list.files(
    data_path,
    pattern = paste0("^BACI_HS", subset, "_Y[0-9]{4}_V[0-9]+\\.csv$"),
    full.names = TRUE
  )
  # Read country codes of the dataset
  country_codes <- utils::read.csv(file.path(data_path, "country_codes_V202501.csv")) %>%
    select("country_code", "country_iso3") %>%
    # country code 490 (country_iso3="S19") is used as a proxy for trade statistics for Taiwan
    # (see https://www.cepii.fr/DATA_DOWNLOAD/baci/doc/baci_webpage.html)
    mutate(country_iso3 = case_when(.data$country_iso3 == "S19" ~ "TWN", .default = .data$country_iso3))

  df_all <- NULL

  for (f in files[1:2]) {
    df <- data.table::fread(f)

    # filter HS codes that are relevant for the scope defined in subtype
    if (key == "plastics_UNCTAD") {
      # merge UNCTAD codes with BACI data
      df_filtered <- merge(df, codes, by.x = "k", by.y = "code") %>% select("t", "i", "j", "k", "q")
    } else if (key == "plastics_UNEP") {
      # UNEP Codes contain 4 digit and 5/6 digit codes;
      # in order to merge 4 digit codes, transform 6-digit codes in BACI database to 4 digits
      df_UNEP <- df %>% mutate(k4 = as.integer(as.integer(.data$k / 100)))
      df_plastics_k4 <- merge(UNEP_codes_k4, df_UNEP, by.y = "k4", by.x = "code") %>% select(-"k")
      df_plastics_k6 <- merge(UNEP_codes_k6, df_UNEP, by.y = "k", by.x = "code") %>% select(-"k4")
      # merge filtered data with 4 and 6 digit codes, calculate plastics content
      df_filtered <- rbind(df_plastics_k6, df_plastics_k4) %>%
        rename(k = "code") %>%
        mutate(q_plastic = .data$plastic_percentage * .data$q) %>%
        group_by(.data$t, .data$i, .data$j, .data$k, .data$polymer, .data$sector) %>%
        summarize(q = sum(.data$q_plastic, na.rm = TRUE)) %>%
        ungroup()
    }

    df_filtered <- df_filtered %>%
      merge(country_codes, by.x = "i", by.y = "country_code") %>%
      rename(exporter = "country_iso3") %>%
      merge(country_codes, by.x = "j", by.y = "country_code") %>%
      rename(importer = "country_iso3") %>%
      mutate(value = .data$q / 1000000) %>% # report quantity in Mt
      select(-"i", -"j", -"q")

    df_all <- rbind(df_all, df_filtered)
  }

  # which Codes are missing?
  diff <- setdiff(unique(codes$code), unique(df_all$k))
  missing <- codes %>% filter(.data$code %in% diff)
  if (length(diff) > 0) {
    warning(paste(
      "The following product codes are missing in the BACI dataset:\n",
      paste(utils::capture.output(print(missing)), collapse = "\n")
    ))
  }

  # get grouping variables
  group_vars <- setdiff(colnames(df_all), c("t", "k", "importer", "exporter", "value"))
  # remove data that is unreasonable (extreme outliers) and interpolate instead
  unreasonable <- data.frame(
    exporter = c("MEX", "NGA", "NGA", "CHE"),
    importer = c("USA", "ATG", "ATG", "MOZ"),
    t = c(2004, 2010, 2011, 2016),
    k = c(392310, 550320, 550320, 6309),
    flag_unreasonable = TRUE
  )
  df_clean <- df_all %>%
    # remove trade data of 220190 "Waters; other than mineral and aerated,
    # (not containing added sugar or other sweetening matter nor flavoured), ice and snow"
    # as this category unreasonably inflates trade between HKG and CHN
    filter(.data$k != 220190) %>%
    left_join(
      unreasonable,
      by = c("t", "exporter", "importer", "k")
    ) %>%
    mutate(value = case_when(.data$flag_unreasonable ~ NA, .default = .data$value)) %>%
    group_by(.data$exporter, .data$importer, .data$k, across(all_of(group_vars))) %>%
    dplyr::arrange(.data$t) %>%
    mutate(value_interp = zoo::na.approx(.data$value, x = .data$t, na.rm = FALSE)) %>%
    ungroup()

  # retain only relevant dimensions
  df_final <- df_clean %>%
    group_by(.data$t, .data$exporter, .data$importer, across(all_of(group_vars))) %>%
    summarize(value = sum(.data$value_interp)) %>%
    ungroup()

  return(quitte::madrat_mule(df_final))
}
