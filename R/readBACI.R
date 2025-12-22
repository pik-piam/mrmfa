#' Read BACI Trade data
#'
#' @param subtype Character string specifying the scope
#'        - "plastics"
#' @param HS Character string specifying the year of the HS (Harmonized System) revision of the data
#'        - 92
#'        - 02
#'        - 22
#'
#' @return magpie object of the BACI trade data
#'
#' @author Leonie Schweiger
#'
#' @seealso [readSource()]
#'
#' @examples
#' \dontrun{
#' a <- readSource(type = "BACI", subtype = "plastics")
#' }
#' @importFrom data.table rbindlist
#' @importFrom readxl read_excel
#' @importFrom dplyr select filter
#' @importFrom magclass as.magpie getComment<-
#'
readBACI <- function(subtype, HS = "02") {

  # Read raw data
  data_path = paste0("C:/Users/leoniesc/madrat/sources/BACI/BACI_HS", HS, "_V202501")
  files <- list.files(
    data_path,
    pattern = paste0("^BACI_HS",HS,"_Y[0-9]{4}_V[0-9]+\\.csv$"),
    full.names = TRUE
  )
  df_all <- rbindlist(
    lapply(files, fread),
    use.names = TRUE,
    fill = TRUE
  )
  # Read country and product codes of the respective dataset and merge with dataset
  country_codes <- read.csv(file.path(data_path,"country_codes_V202501.csv")) %>% select("country_code","country_iso3")
  product_codes <- read.csv(file.path(data_path, paste0("product_codes_HS",HS,"_V202501.csv")))
  df_merge <- df_all %>% merge(country_codes, by.x="i", by.y="country_code") %>% rename("exporter"="country_iso3") %>%
    merge(country_codes, by.x="j", by.y="country_code") %>% rename("importer"="country_iso3") %>% select(-"i",-"j")
  # country code 490 (country_iso3="S19") is used as a proxy for trade statistics for Taiwan (see https://www.cepii.fr/DATA_DOWNLOAD/baci/doc/baci_webpage.html)
  df_merge <- df_merge %>% mutate(importer = case_when(importer=="S19"~"TWN", .default=importer), exporter = case_when(exporter=="S19"~"TWN", .default=exporter))

  if (subtype == "plastics") {
    # get product groups from UNCTAD plastics trade data, if available for the respective HS revision, for the older ones where there is none, use the closest
    if (file.exists(paste0("C:/Users/leoniesc/madrat/sources/BACI/UNCTAD_Plastic_Codes/DimHS20",HS,"/Products_Plastics_Hierarchy.xls"))) HS else "02"
    UNCTAD_product_codes <- read_excel(paste0("C:/Users/leoniesc/madrat/sources/BACI/UNCTAD_Plastic_Codes/DimHS20",HS,"Products_Plastics_Hierarchy.xls"), skip = 2)
    # Identify header rows
    is_header <- grepl("^P_", UNCTAD_product_codes[[1]])
    # Create a new variable from column 2 of header rows
    UNCTAD_product_codes$Group <- UNCTAD_product_codes[[2]][is_header][cumsum(is_header)]
    # Remove the header rows
    product_groups <- UNCTAD_product_codes[!is_header, ]
    product_groups$Code <- as.integer(product_groups$Code)

    df_plastics <- merge(df_merge, product_groups, by.x="k", by.y="Code")
    # which Codes are missing?
    diff <- setdiff(unique(product_groups$Code),unique(df_plastics$k))
    missing <- product_groups %>% filter(Code %in%diff)

    imports <- df_plastics %>% group_by(Group,k,Label,t,importer) %>%
      summarize(value=sum(q, na.rm=TRUE))
    exports <- df_plastics %>% group_by(Group,k,Label,t,exporter) %>%
      summarize(value=sum(q, na.rm=TRUE))
    group_imports <- imports %>% group_by(Group,t,importer) %>%
      summarize(value=sum(value))
    group_exports <- exports %>% group_by(Group,t,exporter) %>%
      summarize(value=sum(value))

    # # for comparison:
    # primary_UNCTAD <- calcOutput("PlUNCTAD", subtype = "Primary", aggregate=FALSE) %>% as.data.frame()
    # comparison_exports <- merge(primary_UNCTAD %>% filter(Data1=="Exports"), group_exports %>% filter(Group=="Plastics in primary forms"), by.x=c("Region","Year"), by.y=c("exporter","t"), all.x=TRUE)
    # comparison_imports <- merge(primary_UNCTAD %>% filter(Data1=="Imports"), group_imports %>% filter(Group=="Plastics in primary forms"), by.x=c("Region","Year"), by.y=c("importer","t"), all.x=TRUE)
    # ggplot(comparison_exports, aes(x=value/1000000, y=Value))+geom_point()+scale_x_log10()+scale_y_log10()
    # which countries are missing?
    # diff <- setdiff(unique(primary_UNCTAD$Region),unique(df_plastics$exporter))
  }

  # ---------------------------------------------------------------------------
  # Convert to magpie object
  # ---------------------------------------------------------------------------
  magpie_data <- as.magpie(df, temporal = 1, spatial = 2)
  getComment(magpie_data) <- subtype

  return(magpie_data)
}
