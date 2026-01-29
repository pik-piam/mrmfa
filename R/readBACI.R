#' Read BACI Trade data
#'
#' @param HS Character string specifying the HS (Harmonized System) revision of the data
#'        - HS92
#'        - HS02
#'        - HS17
#'        - HS22
#' @param subtype Character string specifying the scope
#'        - "plastics_UNCTAD": plastics trade data for HS codes from UNCTAD classification into primary, intermediate, manufactured, final and waste plastics
#'        - "plastics_UNEP": plastics trade data for HS codes from UNEP NGP (estimated plastics mass based on estimated plastics percentages in goods)
#'
#' @return magpie object of the BACI trade data
#'
#' @author Leonie Schweiger
#'
#' @seealso [readSource()]
#'
#' @examples
#' \dontrun{
#' a <- readSource(type = "BACI", subtype = "HS17")
#' }
#' @importFrom data.table rbindlist
#' @importFrom readxl read_excel
#' @importFrom dplyr select filter rename summarize
#' @importFrom magclass as.magpie getComment<-
#'
readBACI <- function(HS = "HS02", subtype) {

  # read HS codes that are relevant for the scope defined in subtype
  if (subtype == "plastics_UNCTAD") {
    codes <- readSource("UNCTAD_PlasticsHSCodes", subtype=HS) %>% as.data.frame(rev=3) %>% rename(code=.value)
  }
  if (subtype == "plastics_UNEP"){
    # get selected 4-digit and 6-digit COMTRADE codes from UNEP_NGP; label all polymers in the textile sector as "Fibres"
    UNEP_codes_k4 <- readSource("UNEP_NGP", subtype="k4") %>% as.data.frame(rev=3) %>%
      mutate(polymer = case_when(sector=="Textile"~"Fibres", .default=polymer))
    UNEP_codes_k6 <- readSource("UNEP_NGP", subtype="k6") %>% as.data.frame(rev=3)%>%
      mutate(polymer = case_when(sector=="Textile"~"Fibres", .default=polymer))
    # get all codes to check which are missing
    codes <- readSource("UNEP_NGP", subtype="all") %>% as.data.frame(rev=3)%>%
      mutate(polymer = case_when(sector=="Textile"~"Fibres", .default=polymer))
  }

  # Read raw trade data of respective HS classification system
  data_path = paste0("BACI_", HS, "_V202501")
  files <- list.files(
    data_path,
    pattern = paste0("^BACI_",HS,"_Y[0-9]{4}_V[0-9]+\\.csv$"),
    full.names = TRUE
  )
  df_all <- NULL

  for (f in files) {
    df <- fread(f)

    # filter HS codes that are relevant for the scope defined in subtype
    if (subtype == "plastics_UNCTAD") {
      # merge UNCTAD codes with BACI data
      df_filtered <- merge(df, codes, by.x="k", by.y="code") %>% select(t, i, j, k, Group, q)
      }

    if (subtype == "plastics_UNEP"){
      # UNEP Codes contain 4 digit and 5/6 digit codes; in order to merge 4 digit codes, transform 6-digit codes in BACI database to 4 digits
      df_UNEP <- df %>% mutate(k4 = as.integer(as.integer(k/100)))
      df_plastics_k4 <- merge(UNEP_codes_k4, df_UNEP, by.y="k4", by.x="code") %>% select(-k)
      df_plastics_k6 <- merge(UNEP_codes_k6, df_UNEP, by.y="k", by.x="code")%>% select(-k4)
      # merge filtered data with 4 and 6 digit codes, calculate plastics content
      df_filtered <- rbind(df_plastics_k6, df_plastics_k4) %>% rename(k = code) %>%
        mutate(q_plastic = .value*q) %>%
        group_by(t, i, j, k, polymer, application, stage, sector) %>%
        summarize(q=sum(q_plastic, na.rm=TRUE)) %>%
        dplyr::ungroup()
      }

    df_all <- rbind(
      df_all,
      df_filtered,
      use.names = TRUE,
      fill = TRUE
    )
  }

  # which Codes are missing?
  diff <- setdiff(unique(codes$code),unique(df_all$k))
  missing <- codes %>% filter(code %in%diff)
  if (length(diff)>0){
    warning(paste(
      "The following product codes are missing in the BACI dataset:\n",
      paste(capture.output(print(missing)), collapse = "\n")
    ))
  }

  # Read country codes of the dataset and merge with dataset
  country_codes <- read.csv(file.path(data_path,"country_codes_V202501.csv")) %>% select("country_code","country_iso3") %>%
    # country code 490 (country_iso3="S19") is used as a proxy for trade statistics for Taiwan (see https://www.cepii.fr/DATA_DOWNLOAD/baci/doc/baci_webpage.html)
    mutate(country_iso3 = case_when(country_iso3=="S19"~"TWN", .default=country_iso3))
  df_merge <- df_all %>%
    merge(country_codes, by.x="i", by.y="country_code") %>% rename(exporter = country_iso3)%>%
    merge(country_codes, by.x="j", by.y="country_code") %>% rename(importer = country_iso3)%>%
    mutate(value=q/1000000) %>% #report quantity in Mt
    select(-i, -j, -q)

  # ---------------------------------------------------------------------------
  # Convert to magpie object
  # ---------------------------------------------------------------------------
  magpie_data <- as.magpie(df_merge, temporal = 1, spatial = 2)
  getComment(magpie_data) <- subtype

  return(magpie_data)
}
