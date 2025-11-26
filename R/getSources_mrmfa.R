#' Get Sources mrmfa
#'
#' Collects the sources of all calcFunctions that are called in fullMFA
#' and their respective bibtex entries in SOURCE_INFO.txt if available
#' Saves a table with the calcFunctions, source names and bibtex keys as csv
#' Saves all bibtex entries in a .bib file
#'
#' @author Leonie Schweiger
#'
#' @importFrom tibble as_tibble_row
#' @importFrom knitr kable
#'
getSources_mrmfa <- function(){
  calcFunctions <- getDependencies("fullMFA", direction = "din")
  GDP_sources <- getSources("calcGDP")

  rows <- list()
  bibtex_list <- character(0)

  for (i in calcFunctions$func) {
    # get sources of calc function
    sources <- getSources(i)

    if (nrow(sources) == 0) {
      # add an empty row
      rows[[length(rows) + 1]] <- list(
        CalcFunction = i,
        Source       = "",
        Bibtex       = ""
      )
    } else {
      for (j in sources$source) {
        # skip GDP sources
        if (j %in% GDP_sources$source) next
        # get source folders and bibtex entries for each source
        sourceFolder <- getSourceFolder(j, subtype = NULL)
        sourceFile   <- find_source_info(sourceFolder)

        bibtex_entries <- character(0)
        bibtex_keys    <- character(0)

        if (file.exists(sourceFile)) {
          # Read all text
          sourceInfo <- paste(readLines(sourceFile, warn = FALSE), collapse = "\n")

          # Extract all BibTeX entries
          bibtex_entries <- extract_bib_entries_from_text(sourceInfo)  # character vector
          # Add BibTex entries to list of all BibTex entries
          bibtex_list <- c(bibtex_list, bibtex_entries)

          # Extract keys for each entry
          bibtex_keys <- get_bibtex_key(bibtex_entries)                # character vector
        }

        # Wrap keys in square brackets and join into a single string
        bibtex_str <- if (length(bibtex_keys) == 0) "" else paste0("[@", bibtex_keys, "]", collapse = ", ")

        # Add row
        rows[[length(rows) + 1]] <- list(
          CalcFunction = i,
          Source       = j,
          Bibtex       = bibtex_str
        )
      }
    }
  }

  # make dataframe from list
  table <- bind_rows(lapply(rows, tibble::as_tibble_row))
  # summarize dataframe by concatenating the sources, sourcefolders and bibtex entries for each calcFunction
  table_summary <- table %>%
    group_by(CalcFunction) %>%
    summarise(across(
      everything(),
      ~ {
        vals <- .x[.x != "" & !is.na(.x)]     # keep only non-empty values
        if (length(vals) == 0) "" else paste(vals, collapse = ", ")
      }
    ))
  # map calc functions to parameters by parsing fullMFA
  mapping <- extract_calc_output_calls(fullMFA)
  table_final <- merge(mapping, table_summary, by="CalcFunction") %>%
    select("Filename", "CalcFunction", "Source", "Bibtex")

  # export as csv file
  write.csv(table_final, "mrmfa_sources.csv", row.names=FALSE)
  # export bibtex list to .bib file, remove duplicates
  writeLines(unique(bibtex_list), "mrmfa_sources.bib")
}

# Get the SOURCE_INFO.txt from the newest version of the source folder
find_source_info <- function(sourceFolder) {
  # Case 1 — SOURCE_INFO.txt directly in folder
  direct_file <- file.path(sourceFolder, "SOURCE_INFO.txt")
  if (file.exists(direct_file)) {
    return(direct_file)
  }

  # Case 2 — versioned subfolders: v1, v1.0, v1.2.3, v10, etc.
  subdirs <- list.dirs(sourceFolder, full.names = TRUE, recursive = FALSE)
  basenames <- basename(subdirs)

  # Match folders that start with "v" followed by digits/dots
  # Examples allowed: v1, v2.0, v1.2.3, v2023.10.15
  version_dirs <- grep("^v[0-9]+(\\.[0-9]+)*$", basenames, value = TRUE)

  if (length(version_dirs) == 0) {
    return(NA_character_)
  }

  # Extract version numbers (remove leading "v")
  version_numbers <- as.numeric(sub("^v", "", version_dirs))

  # Pick the highest version
  best_version <- version_dirs[which.max(version_numbers)]

  # Check for SOURCE_INFO.txt in that version folder
  version_file <- file.path(sourceFolder, best_version, "SOURCE_INFO.txt")

  if (file.exists(version_file)) {
    return(version_file)
  }

  # Nothing found
  return(NA_character_)
}


# Read a file and extract all bibtex entries while handling nested braces/parentheses
extract_bib_entries_from_text <- function(text) {
  chars <- unlist(strsplit(text, "", useBytes = TRUE))
  n <- length(chars)
  i <- 1
  entries <- character(0)

  while (i <= n) {
    if (chars[i] == "@") {
      start <- i
      # find next '{' or '(' after @
      j <- i + 1
      while (j <= n && !(chars[j] %in% c("{", "("))) j <- j + 1
      if (j > n) { i <- i + 1; next }
      open_char <- chars[j]
      close_char <- if (open_char == "{") "}" else ")"
      depth <- 1
      k <- j + 1
      # walk forward tracking depth
      while (k <= n && depth > 0) {
        if (chars[k] == open_char) depth <- depth + 1
        else if (chars[k] == close_char) depth <- depth - 1
        k <- k + 1
      }
      if (depth == 0) {
        entry <- paste(chars[start:(k-1)], collapse = "")
        entries <- c(entries, entry)
        i <- k
        next
      } else {
        # unmatched — skip this @ and continue
        i <- j + 1
        next
      }
    } else {
      i <- i + 1
    }
  }

  entries
}

# Return the bibtex keys from a vector of bibtex entries
get_bibtex_key <- function(entry) {
  sapply(entry, function(e) {
    m <- regexpr("@[a-zA-Z]+\\{([^,]+),", e, perl = TRUE)
    if (m == -1) return(NA_character_)
    key <- regmatches(e, m)
    sub("@[a-zA-Z]+\\{([^,]+),", "\\1", key, perl = TRUE)
  }, USE.NAMES = FALSE)
}

# Parse fullMFA function to obtain a mapping of CalcFunction and filenames
extract_calc_output_calls <- function(func) {
  # Convert the function body to text
  func_text <- deparse(func)
  func_text <- paste(func_text, collapse = "\n")

  # Regex to match calcOutput("FunctionName", ..., file = "filename.cs4r")
  pattern <- 'calcOutput\\s*\\(\\s*"([A-Za-z0-9_]+)"[^\\)]*file\\s*=\\s*"([A-Za-z0-9_./]+)"'

  # Find matches
  matches <- gregexpr(pattern, func_text, perl = TRUE)
  captures <- regmatches(func_text, matches)[[1]]

  # Extract function names and filenames
  extract <- lapply(captures, function(x) {
    m <- regmatches(x, regexec(pattern, x, perl = TRUE))[[1]]
    data.frame(CalcFunction = paste("calc",m[2], sep=""), Filename = m[3], stringsAsFactors = FALSE)
  })

  # Combine into one data.frame
  mapping_df <- do.call(rbind, extract)
  return(mapping_df)
}

