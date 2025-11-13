#' Read data from Cullen et al. (2012)
#' @description
#' Read data from supplementary material of Cullen et al. 2012 paper
#' 'Mapping the global flow of steel: from steelmaking to end‐use goods'.
#' Files were digitalized from pdf to Excel.
#' @param subtype Subtype of Cullen et al. (2012) data to load. Currently
#' supported subtypes are flows & giMatrix.
#' @author Merlin Jo Hosak
readCullen2012 <- function(subtype) {
  # ---- list all available subtypes with functions doing all the work ----
  version <- "v1.0"
  switchboard <- list(
    "flows" = function() {
      path <- paste0("./", version, "/Cullen_2012_Flows.xlsx")
      df <- readxl::read_excel(
        path = path,
        sheet = "Data"
      )

      identifier <- createFlowsIdentifier(df)

      flows <- new.magpie(
        names = identifier,
        fill = df$Value
      )

      return(flows)
    },
    "giMatrix" = function() {
      path <- paste0("./", version, "/Cullen_2012_GI_Matrix.xlsx")
      df <- readxl::read_excel(
        path = path,
        sheet = "Data"
      )
      df_long <- tidyr::pivot_longer(
        df,
        cols = -1, # all columns except the first
        names_to = "variable", # column headers become values here
        values_to = "value" # cell values here
      )

      giMatrix <- as.magpie(df_long)

      return(giMatrix)
    },
    NULL
  )
  # ---- check if the subtype called is available ----
  if (is_empty(intersect(subtype, names(switchboard)))) {
    stop(paste(
      "Invalid subtype -- supported subtypes are:",
      names(switchboard)
    ))
  } else {
    # ---- load data and do whatever ----
    return(switchboard[[subtype]]())
  }
}

createFlowsIdentifier <- function(df) {
  df$Description <- paste0(" (", df$Description, ")")
  df$Description[df$Description == " (NA)"] <- ""

  identifier <- paste(df$ID, df$Source, sep = ": ")
  identifier <- paste(identifier, df$Target, sep = " -> ")
  identifier <- paste(identifier, df$Description, sep = "")

  return(identifier)
}
