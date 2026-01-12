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
#' a <- calcOutput(type = "BACI", subtype = "plastics_UNCTAD")
#' }
#' @importFrom data.table rbindlist
#' @importFrom readxl read_excel
#' @importFrom dplyr select filter rename summarize ungroup
#' @importFrom magclass as.magpie getComment<-
#'
calcBACI <- function(subtype, HS = "02") {

  # Read raw data
  BACI_data <- readSource("BACI", subtype = paste0("HS",HS), convert=FALSE)
  df <- BACI_data %>% as.data.frame(rev=3)

  if (subtype == "plastics_UNCTAD") {
    # get product groups from UNCTAD plastics trade data, if available for the respective HS revision, for the older ones where there is none, use the closest
    if (file.exists(paste0("C:/Users/leoniesc/madrat/sources/UNCTAD_PlasticsHSCodes/DimHS20",HS,"Products_Plastics_Hierarchy.xls"))) {
      UNCTAD_path <- paste0("C:/Users/leoniesc/madrat/sources/UNCTAD_PlasticsHSCodes/DimHS20",HS,"Products_Plastics_Hierarchy.xls")
    } else {
      UNCTAD_path <- "C:/Users/leoniesc/madrat/sources/UNCTAD_PlasticsHSCodes/DimHS2002Products_Plastics_Hierarchy.xls"
    }
    UNCTAD_product_codes <- read_excel(UNCTAD_path, skip = 2)
    # Identify header rows
    is_header <- grepl("^P_", UNCTAD_product_codes[[1]])
    # Create a new variable from column 2 of header rows
    UNCTAD_product_codes$Group <- UNCTAD_product_codes[[2]][is_header][cumsum(is_header)]
    # Remove the header rows
    product_groups <- UNCTAD_product_codes[!is_header, ]
    product_groups$Code <- as.integer(product_groups$Code)
    # merge UNCTAD codes with BACI data
    df_plastics_UNCTAD <- merge(product_groups, df, by.y="k", by.x="Code")
    # which Codes are missing?
    diff <- setdiff(unique(product_groups$Code),unique(df_plastics_UNCTAD$Code))
    missing <- product_groups %>% filter(Code %in%diff)
    if (length(diff)>0){
      warning(paste(
        "The following UNCTAD product codes are missing in the BACI dataset:\n",
        paste(capture.output(print(missing)), collapse = "\n")
      ))
    }

    x <- as.magpie(df_plastics_UNCTAD, temporal = 1, spatial = 2)
  }

  if (subtype == "plastics_UNEP"){
    # get selected 4-digit and 6-digit COMTRADE codes from UNEP_NGP; label all polymers in the textile sector as "Fibres"
    UNEP_codes_k4 <- readSource("UNEP_NGP", subtype="k4") %>% as.data.frame(rev=3) %>%
      mutate(polymer = case_when(sector=="Textile"~"Fibres", .default=polymer))
    UNEP_codes_k6 <- readSource("UNEP_NGP", subtype="k6") %>% as.data.frame(rev=3)%>%
      mutate(polymer = case_when(sector=="Textile"~"Fibres", .default=polymer))
    UNEP_codes <- readSource("UNEP_NGP", subtype="all") %>% as.data.frame(rev=3)%>%
      mutate(polymer = case_when(sector=="Textile"~"Fibres", .default=polymer))
    # UNEP Codes contain 4 digit and 5/6 digit codes; in order to merge 4 digit codes, transform 6-digit codes in BACI database to 4 digits
    df_UNEP <- df %>% mutate(k4 = as.integer(as.integer(k/100)))
    df_plastics_k4 <- merge(UNEP_codes_k4, df_UNEP, by.y="k4", by.x="code") %>% select(-k)
    df_plastics_k6 <- merge(UNEP_codes_k6, df_UNEP, by.y="k", by.x="code")%>% select(-k4)
    df_plastics_UNEP <- rbind(df_plastics_k6, df_plastics_k4) %>%
      mutate(q_plastic = .value.x*.value.y) %>%
      group_by(t, Region, type, code, polymer, application, stage, sector, label) %>%
      summarize(q=sum(q_plastic, na.rm=TRUE)) %>%
      dplyr::ungroup()
    # which Codes are missing?
    diff <- setdiff(unique(UNEP_codes$code),unique(df_plastics_UNEP$code))
    missing <- UNEP_codes %>% filter(code %in%diff)
    if (length(diff)>0){
      warning(paste(
        "The following UNEP NGP product codes are missing in the BACI dataset:\n",
        paste(capture.output(print(missing)), collapse = "\n")
      ))
    }

    # remove data that is unreasonable (extreme outliers) and interpolate instead
    unreasonable <- data.frame(
      Region = c("MEX", "USA", "NGA", "NGA", "CHE"),
      t   = c(2004, 2004, 2010, 2011, 2016),
      type   = c("exports", "imports", "exports", "exports", "exports"),
      code   = c(392310, 392310, 550320, 550320, 6309)
    )
    df_clean <- df_plastics_UNEP %>%
      left_join(
        unreasonable %>% mutate(flag_unreasonable = TRUE),
        by = c("Region", "t", "type", "code")
      ) %>%
      mutate(value = case_when(flag_unreasonable ~ NA, .default = q)) %>%
      #select(-flag_unreasonable) %>%
      group_by(Region, type, code, polymer, application, stage, sector) %>%
      arrange(t) %>%
      mutate(value_interp = zoo::na.approx(value, x = t, na.rm = FALSE)) %>%
      ungroup() %>%
    # remove trade data of 220190 "Waters; other than mineral and aerated, (not containing added sugar or other sweetening matter nor flavoured), ice and snow"
    # as this category unreasonably inflates trade of CHA region (traded between HKG and CHN in high volumes)
      filter(code!=220190)

    # summarize df and include only relevant categories
    df_sum <- df_clean %>%
      group_by(t, Region, type, polymer, stage, sector) %>%
      dplyr::summarize(q=sum(value_interp)) %>%
      dplyr::ungroup()

    # map UNEP-NGP sectors to sectors used in REMIND-MFA
    sector_map <- toolGetMapping("sectormappingUNEP_NGP.csv", type = "sectoral", where = "mrmfa")
    new1 <- df_sum %>%
      left_join(sector_map, by = c("sector" = "Source")) %>%
      select(-"sector") %>%
      rename("sector" = "Target") %>%
      group_by(t, Region, type, polymer, stage, sector) %>%
      dplyr::summarize(q=sum(q)) %>%
      dplyr::ungroup()

    # map UNEP-NGP polymers to polymers used in REMIND-MFA
    polymer_map <- toolGetMapping("polymermappingUNEP_NGP.csv", type = "sectoral", where = "mrmfa")
    # use polymer use by sector as weights (summarize over all Regions, as polymer share by sector is constant over all Regions in OECD data)
    # use total polymer use over all sectors as weights for "General" sector
    use <- calcOutput("PlOECD",subtype = "Use_2019_region", aggregate = TRUE) %>% as.data.frame()
    use_by_sector <- use %>%
      rename(sector = Data2, polymer = Data1) %>%
      filter(polymer!="Total") %>%
      group_by(.data$sector, .data$polymer) %>%
      summarize(value = sum(Value)) %>%
      mutate(sector = case_when(sector=="Total"~"General", .default=sector))
    split <- merge(polymer_map, use_by_sector, by.y="polymer", by.x="Target") %>%
      group_by(.data$sector, .data$Source) %>%
      dplyr::mutate(
        total = sum(.data$value, na.rm = TRUE),
        weight = .data$value / .data$total
      ) %>%
      select(-"total", -"value")
    new2 <- left_join(new1, split, by = c("polymer" = "Source", "sector")) %>%
      mutate("q" = .data$q * .data$weight)
    # some weights are NaN for polymers that are not used in a specific sector according to OECD, throw a warning if these combination appear in the dataset
    nan <- new2 %>% filter(is.na(weight))
    if (nrow(nan)>0){
      warning(paste(
        "The following sector-polymer combinations cannot be mapped from the BACI data as they do not exist in the OECD dataset used for weighting:\n",
        paste(capture.output(print(nan)), collapse = "\n")
      ))
    }

    final <- new2 %>%
      select(-"polymer", -"weight") %>%
      rename("polymer" = "Target") %>%
      dplyr::relocate("polymer", .after = "sector") %>%
      group_by(t, Region, type, polymer, stage, sector) %>%
      dplyr::summarize(q=sum(q)) %>%
      dplyr::ungroup()

    x <- as.magpie(final, temporal = 1, spatial = 2)

  }

  x <- toolCountryFill(x)
  x <- replace_non_finite(x, replace = 0)

  return(list(
    x = x,
    weight = NULL,
    unit = "Mt Plastic",
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
