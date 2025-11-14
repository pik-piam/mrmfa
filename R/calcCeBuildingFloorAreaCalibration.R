#' Calculates a factor to translate energy-related floor area to material-related floor area.
#' Related to from net to gross floor area.
#'
#' @author Bennet Weiss
calcCeBuildingFloorAreaCalibration <- function() {

  # data for 2020 (Mm2)
  # TODO check if 2020 data is scenario agnostic (as it should be)
  edgeb_floor_area <- calcOutput(
    type = "Floorspace",
    regionmapping = "regionmapping_ISO_2_ISO.csv",
    scenario = "SSP2"
  )[,2020]
  edgeb_floor_area <- dimReduce(edgeb_floor_area)
  # remove buildings total
  edgeb_floor_area <- edgeb_floor_area[,,(Variable = "buildings"), invert = TRUE]

  # data for 2020 (m2)
  eubucco_floor_area <- readSource("EUBUCCO") * 1e-6 # convert from m2 to Mm2
  eubucco_floor_area[is.na(eubucco_floor_area)] <- 0 # TODO remove; this is just for plotting

  # TODO: See how this can be used
  # data for 2021 (m2)
  gem_floor_area <- calcOutput(
    type = "CeBuildingFloorArea",
    regionmapping = "regionmapping_ISO_2_ISO.csv",
    subtype = "Stock_Type"
  ) * 1e-6 # convert from m2 to Mm2
  # rename
  expected_names <- c("Com", "Res")
  stopifnot(identical(getNames(gem_floor_area), expected_names))
  gem_floor_area <- setNames(gem_floor_area, c("commercial", "residential"))

  plot_floor_area_comparison(edgeb_floor_area, eubucco_floor_area, gem_floor_area)
}

#' Plots a comparison of floor area data from EDGE-B, EUBUCCO and GEM.
#' @author Bennet Weiss
plot_floor_area_comparison <- function(edgeb_floor_area, eubucco_floor_area, gem_floor_area) {
  savefolder <- "figures/floor_area_comparison_2020_"
  ylab <- "Floor Area (Million m2)"

  # global plot
  filename <- paste0(savefolder, "global", ".png")
  floorlist <- list(
    "(1) EDGE-B" = edgeb_floor_area,
    "(2) GEM" = gem_floor_area,
    "(3) EUBUCCO" = eubucco_floor_area
  )
  toolMplotMulti(floorlist, title = "Global", xlab = "", ylab = ylab, filename = filename)

  # country plots
  countries <- getItems(gem_floor_area, dim = 1)
  regionmap <- toolGetMapping("H12.csv", type = "regional", where = "mappingfolder")
  regions <- regionmap$RegionCode[match(countries, regionmap$CountryCode)]
  for (i in seq_along(countries)) {
    country <- countries[i]
    region <- regions[i]
    edgeb <- edgeb_floor_area[country,,]
    eubucco <- eubucco_floor_area[country,,]
    gem <- gem_floor_area[country,,]
    floorlist <- list("(1) EDGE-B" = edgeb, "(2) GEM" = gem, "(3) EUBUCCO" = eubucco)
    filename <- paste0(savefolder, region, "_", country, ".png")
    toolMplotMulti(floorlist, title = country, xlab = "", ylab = ylab, filename = filename)
  }
}

#' mplot_multi
#'
#' Get a quick visualization of the content of most magpie objects.
#' Based on magclass::mplot. Now supports plotting a named list of magclass objects for comparison.
#'
#' @param px The magpie object or a named list of magpie objects to be visualized.
#' @param global Whether data should be aggregated over regions to global values.
#' @param total Whether the total of all data values should also be visualized.
#' @param title A string to be used as the plot's main title. Defaults to NULL.
#' @author Bennet Weiss, Pascal Sauer (mplot), Patrick Rein (mplot)
#' @importFrom rlang .data
#' @export
toolMplotMulti <- function(px, global = TRUE, total = FALSE, title = NULL, xlab = NULL, ylab = NULL, filename = NULL) {

  rlang::check_installed("ggplot2")

  # --- Handle single object or list of objects ---
  if (inherits(px, "magpie")) {
    # Single object case
    pxList <- list(px) # Treat as a list of one
    isSingleObject <- TRUE
  } else if (is.list(px) && all(sapply(px, inherits, "magpie"))) {
    # List of objects case
    pxList <- px
    isSingleObject <- FALSE
    # Ensure the list is named for plotting
    if (is.null(names(pxList))) {
      warning("The list of magpie objects is unnamed. Using numeric indices as names.")
      names(pxList) <- seq_along(pxList)
    }
  } else {
    stop("'px' must be a 'magpie' object or a list of 'magpie' objects.")
  }

  # --- Determine plot structure from the *first* object ---
  # This assumes all objects in the list are compatible (e.g., same dimensions)
  templatePx <- pxList[[1]]
  originalDimNames <- names(dimnames(templatePx))
  firstDimensionPart <- function(aName) strsplit(aName, ".", fixed = TRUE)[[1]][[1]]
  temporalCategorial <- FALSE

  # Check for "timeless" 2nd dim
  if (is.null(magclass::getItems(templatePx, 2))) {
    temporalCategorial <- TRUE
  }

  # Get Data dim name
  dataDimName <- firstDimensionPart(originalDimNames[[3]])

  # Get Temporal dim name
  temporalDimName <- originalDimNames[[2]]
  if (grepl(".", temporalDimName, fixed = TRUE)) {
    temporalDimName <- "temporal"
    temporalCategorial <- TRUE # Has subdimensions, treat as categorial
  }

  # Get Spatial dim name
  spatialDimName <- originalDimNames[[1]]
  if (global) {
    # We need to know the name *after* aggregation
    tmpPx <- magclass::dimSums(templatePx, 1)
    spatialDimName <- names(dimnames(tmpPx))[[1]]
  } else {
    if (grepl(".", spatialDimName, fixed = TRUE)) {
      spatialDimName <- "spatial" # Has subdimensions, simplify name
    }
  }

  # --- Process all objects and bind them ---
  processedDfs <- list()
  for (i in seq_along(pxList)) {
    currentPx <- pxList[[i]]
    currentName <- names(pxList)[i]

    # Ensure all dimensions have items or are somewhat normalized
    if (is.null(magclass::getItems(currentPx, 1))) {
      magclass::getItems(currentPx, 1) <- "global"
    }
    if (is.null(magclass::getItems(currentPx, 2))) {
      magclass::getItems(currentPx, 2) <- "timeless"
    }
    if (is.null(magclass::getItems(currentPx, 3))) {
      magclass::getItems(currentPx, 3) <- "total"
    }

    # Rewrite names of data dimension (use template name)
    magclass::getNames(currentPx) <- gsub(".", "_", magclass::getNames(currentPx), fixed = TRUE)
    names(dimnames(currentPx))[[3]] <- dataDimName

    # Rewrite names of temporal dimension (use template name)
    if (grepl(".", originalDimNames[[2]], fixed = TRUE)) {
      dimnames(currentPx)[[2]] <- gsub(".", "_", dimnames(currentPx)[[originalDimNames[[2]]]], fixed = TRUE)
    }
    names(dimnames(currentPx))[[2]] <- temporalDimName

    # Handle spatial dimension
    if (global) {
      currentPx <- magclass::dimSums(currentPx, 1)
    } else {
      if (grepl(".", originalDimNames[[1]], fixed = TRUE)) {
        dimnames(currentPx)[[1]] <- gsub(".", "_", dimnames(currentPx)[[originalDimNames[[1]]]], fixed = TRUE)
      }
    }
    names(dimnames(currentPx))[[1]] <- spatialDimName # Force template name

    # Calculate and add total
    if (total && !temporalCategorial) {
      if (magclass::ndata(currentPx) > 1) {
        currentPx <- magclass::mbind(currentPx, magclass::setNames(magclass::dimSums(currentPx, 3), "total"))
      } else {
        magclass::getNames(currentPx) <- "total"
      }
    }

    # Convert to data.frame
    df <- as.data.frame(currentPx, rev = 3)

    # --- Add source column if this is a list ---
    if (!isSingleObject) {
      df$source <- currentName
    }

    processedDfs[[i]] <- df
  }

  # Combine all data.frames
  px_df <- do.call(rbind, processedDfs)

  # --- Plotting ---

  # Base plot
  plot <- ggplot2::ggplot(data = px_df, ggplot2::aes(x = .data[[temporalDimName]]))

  if (!temporalCategorial) {
    # --- LINE PLOT ---
    # Define aesthetics
    line_aes <- ggplot2::aes(y = .data$.value,
                             color = .data[[dataDimName]])

    if (isSingleObject) {
      # Original behavior
      line_aes <- line_aes + ggplot2::aes(group = .data[[dataDimName]])
    } else {
      # Multi-object: add linetype for 'source' and update group
      line_aes <- line_aes +
        ggplot2::aes(linetype = .data$source,
                     group = interaction(.data[[dataDimName]], .data$source))
    }

    plot <- plot + ggplot2::geom_line(linewidth = 1.5, mapping = line_aes)

  } else {
    # --- BAR PLOT ---
    # Original 'color' maps to 'fill' for bars
    bar_aes <- ggplot2::aes(weight = .data$.value,
                            color = .data[[dataDimName]], # This becomes fill
                            group = .data[[dataDimName]])

    plot <- plot + ggplot2::geom_bar(linewidth = 1.5, mapping = bar_aes)
  }

  # --- Faceting ---
  facetBySpatial <- !global
  facetBySource  <- !isSingleObject && temporalCategorial # Only facet source for bars

  if (facetBySpatial && !facetBySource) {
    # Case 1: Spatial only (Line plots, or single object bar plots)
    plot <- plot + ggplot2::facet_wrap(stats::as.formula(paste("~", spatialDimName)))
  } else if (!facetBySpatial && facetBySource) {
    # Case 2: Source only (Global, multi-object bar plots)
    plot <- plot + ggplot2::facet_wrap(~ .data$source)
  } else if (facetBySpatial && facetBySource) {
    # Case 3: Both (Spatial, multi-object bar plots)
    plot <- plot + ggplot2::facet_grid(stats::as.formula(paste("source ~", spatialDimName)))
  }
  # Case 4 (Neither) requires no faceting.

  # --- Add Labels ---
  plot <- plot + ggplot2::labs(title = title, x = xlab, y = ylab)

  if (!is.null(filename)) {
    ggplot2::ggsave(filename, plot = plot)
  } else {
    print(plot)
  }
}

