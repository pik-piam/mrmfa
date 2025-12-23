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
#' @importFrom dplyr select filter rename summarize
#' @importFrom magclass as.magpie getComment<-
#'
calcBACI <- function(subtype, HS = "17") {

  # Read raw data
  BACI_data <- readSource("BACI", subtype = paste0("HS",HS), convert=FALSE)
  df <- BACI_data %>% as.data.frame(rev=3)

  if (subtype == "plastics_UNCTAD") {
    # get product groups from UNCTAD plastics trade data, if available for the respective HS revision, for the older ones where there is none, use the closest
    if (file.exists(paste0("C:/Users/leoniesc/madrat/sources/UNCTAD_PlasticsHSCodes/DimHS20",HS,"/Products_Plastics_Hierarchy.xls"))) HS else "02"
    UNCTAD_product_codes <- read_excel(paste0("C:/Users/leoniesc/madrat/sources/UNCTAD_PlasticsHSCodes/DimHS20",HS,"Products_Plastics_Hierarchy.xls"), skip = 2)
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
        "The following product codes are missing in the BACI dataset:\n",
        paste(capture.output(print(missing)), collapse = "\n")
      ))
    }

    x <- as.magpie(df_plastics_UNCTAD, temporal = 1, spatial = 2)

    # group_trade <- df_plastics_UNCTAD %>% group_by(Group,t,type,Region) %>%
    #   summarize(value=sum(value))

    # ggplot(group_trade, aes(x=t, y=value, color=Group))+
    #   geom_line()+
    #   facet_wrap(~Region, scales="free")

    # # for comparison:
    # primary_UNCTAD <- calcOutput("PlUNCTAD", subtype = "Primary", aggregate=FALSE) %>% as.data.frame()
    # comparison_exports <- merge(primary_UNCTAD %>% filter(Data1=="Exports"), group_trade %>% filter(Group=="Plastics in primary forms", type=="exports"), by.x=c("Region","Year"), by.y=c("Region","t"), all.x=TRUE)
    # comparison_imports <- merge(primary_UNCTAD %>% filter(Data1=="Imports"), group_trade %>% filter(Group=="Plastics in primary forms", type=="imports"), by.x=c("Region","Year"), by.y=c("Region","t"), all.x=TRUE)
    # ggplot(comparison_exports, aes(x=value, y=Value))+geom_point()+scale_x_log10()+scale_y_log10()
    # which countries are missing?
    # diff <- setdiff(unique(primary_UNCTAD$Region),unique(df_plastics$Region))
  }

  if (subtype == "plastics_UNEP"){
    # get selected 4-digit and 6-digit COMTRADE codes from UNEP_NGP
    UNEP_codes_k4 <- readSource("UNEP_NGP", subtype="k4") %>% as.data.frame(rev=3)
    UNEP_codes_k6 <- readSource("UNEP_NGP", subtype="k6") %>% as.data.frame(rev=3)
    UNEP_codes <- readSource("UNEP_NGP", subtype="all") %>% as.data.frame(rev=3)
    # UNEP Codes contain 4 digit and 5/6 digit codes; in order to merge 4 digit codes, transform 6-digit codes in BACI database to 4 digits
    df_UNEP <- df %>% mutate(k4 = as.integer(as.integer(k/100)))
    df_plastics_k4 <- merge(UNEP_codes_k4, df_UNEP, by.y="k4", by.x="code") %>% select(-k)
    df_plastics_k6 <- merge(UNEP_codes_k6, df_UNEP, by.y="k", by.x="code")%>% select(-k4)
    df_plastics_UNEP <- rbind(df_plastics_k6, df_plastics_k4) %>%
      mutate(q_plastic = .value.x*.value.y) %>%
      group_by(t, Region, type, code, polymer, application, stage, sector, label) %>%
      summarize(q=sum(q_plastic, na.rm=TRUE))
    # which Codes are missing?
    diff <- setdiff(unique(UNEP_codes$code),unique(df_plastics_UNEP$code))
    missing <- UNEP_codes %>% filter(code %in%diff)
    if (length(diff)>0){
      warning(paste(
        "The following product codes are missing in the BACI dataset:\n",
        paste(capture.output(print(missing)), collapse = "\n")
      ))
    }
    x <- as.magpie(df_plastics_UNEP, temporal = 1, spatial = 2)

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
