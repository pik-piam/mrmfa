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
#'        - "steel": steel trade data from custom mapping
#'          valid parameters for category:
#'          - direct
#'          - indirect
#'          - scrap
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
readBACI <- function(subtype, subset) {
  # check whether subset is one of the available HS revisions
  available <- c("92", "02", "17", "22")
  if (is_empty(intersect(subset, available))) {
    stop(
      "Invalid subset -- supported HS revisions are:",
      paste0(available, collapse = ", ")
    )
  }
  data_path <- paste0("BACI_HS", subset, "_V202501")

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

  } else if (key == "steel") {
    # read all BACI product codes
    product_codes <- utils::read.csv(file.path(data_path, paste0("product_codes_HS", subset, "_V202501.csv"))) %>%
      mutate(code_2=as.integer(as.numeric(code)/10000))
    # read indirect steel trade product codes and respective steel share
    indirect <- read_excel(file.path("Steel_HSCodes", "Steel_HSCodes.xlsx")) %>%
      mutate(steel_share=`Steel Weight Share (%)`/100) %>% select(-"Steel Weight Share (%)")
    codes <- switch(category,
      "direct" = product_codes %>% filter(grepl("^72",.data$code) & # filter all HS72
                                            !grepl("^7204",.data$code) & # except for HS7204 (steel scrap)
                                            !grepl("^7202",.data$code) & # except for HS7202 (ferro-alloys, no listed in WSA trade data)
                                            grepl("^7205",.data$code)) %>% select("code"), # except for HS7205 (granules and powders, not listed in WSA trade data)
      "scrap" = product_codes %>% filter(grepl("^7204",.data$code)) %>% select("code"), # filter all HS7204
      "indirect" = product_codes %>% merge(indirect, by.x="code_2", by.y="HS") %>%
        select(-c("Chapter Title", "description", "code_2")),
      stop("Unsupported steel trade category: ", category)
    )
  } else {
    stop("Invalid subtype. Choose either 'plastics_UNCTAD' or 'plastics_UNEP'.")
  }

  # Read raw trade data of respective HS classification system
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

  for (f in files) {
    df <- data.table::fread(f)

    # filter HS codes that are relevant for the scope defined in subtype
    if (key == "plastics_UNCTAD") {
      # merge UNCTAD codes with BACI data
      df_filtered <- merge(df, codes, by.x = "k", by.y = "code")  %>%
        mutate(value = .data$q / 1000000) %>% # report quantity in Mt
        select("t", "i", "j", "k", "value")
    } else if (key == "plastics_UNEP") {
      # UNEP Codes contain 4 digit and 5/6 digit codes;
      # in order to merge 4 digit codes, transform 6-digit codes in BACI database to 4 digits
      df_UNEP <- df %>% mutate(k4 = as.integer(as.integer(.data$k / 100)))
      df_plastics_k4 <- merge(UNEP_codes_k4, df_UNEP, by.y = "k4", by.x = "code") %>% select(-"k")
      df_plastics_k6 <- merge(UNEP_codes_k6, df_UNEP, by.y = "k", by.x = "code") %>% select(-"k4")
      # merge filtered data with 4 and 6 digit codes, calculate plastics content
      df_filtered <- rbind(df_plastics_k6, df_plastics_k4) %>%
        rename(k = "code") %>%
        mutate(q_plastic = .data$plastic_percentage * .data$q/1000000) %>% # report quantity in Mt
        group_by(.data$t, .data$i, .data$j, .data$k, .data$polymer, .data$sector) %>%
        summarize(value = sum(.data$q_plastic, na.rm = TRUE)) %>%
        ungroup()
    } else if (key == "steel"){
      # merge HS codes with BACI data
      df_filtered <- merge(df, codes %>% mutate(code=as.integer(code)), by.x = "k", by.y = "code")
      if (category %in% c("direct","scrap")){
        # for direct steel trade and scrap trade, sum over all product codes (steel share = 100%)
        df_filtered <- df_filtered %>%
          group_by(.data$t, .data$i, .data$j) %>%
          summarize(value = sum(.data$q, na.rm = TRUE)) %>%
          ungroup()
      } else if (category == "indirect"){
        # for indirect steel trade, multiply quantity with steel share and split by category
        df_filtered <- df_filtered %>%
          tidyr::pivot_longer(cols=c("Construction","Transport","Machinery","Products"), names_to = "sector", values_to="share") %>%
          mutate(q = .data$q * .data$steel_share * .data$share) %>%
          group_by(.data$t, .data$i, .data$j, .data$sector) %>%
          summarize(value = sum(.data$q, na.rm = TRUE)) %>%
          ungroup()
      }
    }

    df_filtered <- df_filtered %>%
      merge(country_codes, by.x = "i", by.y = "country_code") %>%
      rename(exporter = "country_iso3") %>%
      merge(country_codes, by.x = "j", by.y = "country_code") %>%
      rename(importer = "country_iso3") %>%
      select(-"i", -"j")

    df_all <- rbind(df_all, df_filtered)
  }

  # for plastics trade, check which codes are missing in the BACI dataset and clean data
  if (key %in% c("plastics_UNCTAD", "plastics_UNEP")){
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
    df_all <- df_all %>%
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
    df_all <- df_all %>%
      group_by(.data$t, .data$exporter, .data$importer, across(all_of(group_vars))) %>%
      summarize(value = sum(.data$value_interp)) %>%
      ungroup()
  }


  return(quitte::madrat_mule(df_all))
}
