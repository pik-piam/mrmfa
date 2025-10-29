#' TODOMERLIN: document
#' 
#' @author Merlin Jo Hosak
#' @param subtype TODOMERLIN: document
#' @export
readCullen2012 <- function(subtype) {
  # ---- list all available subtypes with functions doing all the work ----
  switchboard <- list(
    'flows' = function() {
      version<-"v1.0"
      path <- paste0('./',version,'/Cullen_2012_Flows.xlsx')
      df <- readxl::read_excel(path=path,
                               sheet='Data')
      
      identifier <- createFlowsIdentifier(df)
      
      flows <- new.magpie(names=identifier,
                          fill=df$Value)
      
      return(flows)
    },
    
    'gi_matrix' = function() {
      version<-"v1.0"
      path <- paste0('./',version,'/Cullen_2012_GI_Matrix.xlsx')
      df <- readxl::read_excel(path=path,
                               sheet='Data')
      df_long <- tidyr::pivot_longer(
        df,
        cols = -1,                  # all columns except the first
        names_to = "variable",       # column headers become values here
        values_to = "value"          # cell values here
      )
      
      gi_matrix <- as.magpie(df_long)
      
      return(gi_matrix)
    },
    
    NULL)
  # ---- check if the subtype called is available ----
  if (is_empty(intersect(subtype, names(switchboard)))) {
    stop(paste('Invalid subtype -- supported subtypes are:',
               names(switchboard)))
  } else {
    # ---- load data and do whatever ----
    return(switchboard[[subtype]]())
  }
}

createFlowsIdentifier <- function(df) {
  df$Description <- paste0(' (', df$Description, ')')
  df$Description[df$Description==' (NA)']<-''
  
  identifier<-paste(df$ID, df$Source, sep=": ")
  identifier<-paste(identifier, df$Target, sep = " -> ")
  identifier<-paste(identifier, df$Description, sep = "")
  
  return(identifier)
}
