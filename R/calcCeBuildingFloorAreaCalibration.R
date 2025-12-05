#' Calculates a factor to translate energy-related floor area to material-related floor area.
#' Related to from net to gross floor area.
#'
#' @author Bennet Weiss
calcCeBuildingFloorAreaCalibration <- function(plotting = NULL) {

  # ---Read and prepare data---

  # EDGE-B data for 2020 (m2)
  # TODO check if 2020 data is scenario agnostic (as it should be)
  edgeb_floor_area <- calcOutput(
    type = "CeEDGEBFloorSpace",
    aggregate = FALSE
  )[,2020]
  edgeb_floor_area <- dimReduce(edgeb_floor_area) # remove year 2020 dimension

  # EUBUCCO data for 2020 (m2)
  eubucco_floor_area <- readSource("EUBUCCO")

  # ---Calculate correction factor (in progress)---

  # TODO: a correction factor differentiated by countries
  ratio <- eubucco_floor_area / edgeb_floor_area

  #
  availability_mask <- (eubucco_floor_area > 0)
  total_eubucco <- sum(eubucco_floor_area[availability_mask])
  total_edgeb <- sum(edgeb_floor_area[availability_mask])
  correction_factor <- total_eubucco / total_edgeb

  # ---Output--- # TODO
  description <- "PLACEHOLDER"
  note <- "PLACEHOLDER"
  output <- list(x = eubucco_floor_area, weight = NULL, unit = "PLACEHOLDER", description = description, note = note)
  return(output)
}

#' Generates different plots of floor area data.
#' @author Bennet Weiss
#' @param plotting Type of plot to generate. Options are "floor area comparison" and "ratio over cement production". Defaults to no plot.
plot_floorspace_data <- function(plotting) {

  edgeb_floor_area <- calcOutput(
    type = "CeEDGEBFloorSpace",
    aggregate = FALSE
  )[,2020]
  edgeb_floor_area <- dimReduce(edgeb_floor_area) # remove year 2020 dimension

  # EUBUCCO data for 2020 (m2)
  eubucco_floor_area <- readSource("EUBUCCO")

  if (!is.null(plotting)) {
    if (plotting == "floor area comparison") {

      # GEM data for 2021 (m2)
      gem_floor_area <- calcOutput(
        type = "CeBuildingFloorArea",
        aggregate = FALSE,
        subtype = "Stock_Type"
      )
      # rename
      expected_names <- c("Com", "Res")
      stopifnot(identical(getNames(gem_floor_area), expected_names))
      gem_floor_area <- setNames(gem_floor_area, c("commercial", "residential"))

      # GHS-OBAT data for 2020 (m2)
      ghsoobat_floor_area <- calcOutput("CeGHSOBATFloorArea", aggregate = FALSE)
      ghsoobat_floor_area <- setNames(ghsoobat_floor_area, c("residential", "commercial"))

      plot_floor_area_comparison(edgeb_floor_area, eubucco_floor_area, gem_floor_area, ghsoobat_floor_area)

    } else if (plotting == "ratio eubucco/edgeb over cement production") {

      cement_production <- calcOutput(
        type = "CeBinderProduction",
        subtype = "cement",
        aggregate = FALSE
      )[,2020] # tonnes

      plot_ratio_over_x(edgeb_floor_area, eubucco_floor_area, cement_production, "Cement Production (tonnes)")

    } else {
      stop("Invalid plotting option. Choose either 'floor area comparison' or 'ratio eubucco/edgeb over cement production'.")
    }
  }
}

#' Plots edge_b / eubucco ratio as function of given x variable.
#' @author Bennet Weiss
#' @param edgeb EDGE-B floor area magpie object
#' @param eubucco EUBUCCO floor area magpie object
#' @param x magpie object to plot ratio against (e.g., Production, GDP)
#' @param xlabel label for x axis
plot_ratio_over_x <- function(edgeb, eubucco, x, xlabel) {

  filename <- "../madrat_wd/figures/floor_area_ratio_eubucco_edgeb/floor_area_ratio_eubucco_edgeb.png"
  ratio <- dimSums(eubucco) / dimSums(edgeb)
  ratio_df <- as.data.frame(ratio)
  x_df <- as.data.frame(x)
  ratio_df$x <- x_df$Value
  ratio_df <- ratio_df[ratio_df$Value > 0,]

  plot <- ggplot2::ggplot(ratio_df, ggplot2::aes(x = x, y = Value)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm", se = FALSE) +
    ggplot2::labs(x = xlabel, y = "Floor-area ratio: EUBUCCO/EDGE-B") +
    ggplot2::theme_minimal()
  ggplot2::ggsave(filename, plot = plot)

}


#' Plots a comparison of floor area data from EDGE-B, EUBUCCO and GEM.
#' @author Bennet Weiss
plot_floor_area_comparison <- function(edgeb_floor_area, eubucco_floor_area, gem_floor_area, ghsoobat_floor_area) {
  savefolder <- "../madrat_wd/figures/floor_area_comparison_2020/floor_area_comparison_2020_"
  ylab <- "Floor Area (m2)"

  # global plot
  filename <- paste0(savefolder, "global", ".png")
  floorlist <- list(
    "(1) EDGE-B" = edgeb_floor_area,
    "(2) GEM" = gem_floor_area,
    "(3) GHS-OBAT" = ghsoobat_floor_area,
    "(4) EUBUCCO" = eubucco_floor_area
  )
  toolMplotMulti(floorlist, title = "Global", xlab = "", ylab = ylab,
                 filename = filename, ncol = length(floorlist), nrow = 1)

  # European plot (EUBUCCO Data)
  filename <- paste0(savefolder, "EU27", ".png")
  eubucco_mask <- !is.na(eubucco_floor_area)
  floorlist <- list(
    "(1) EDGE-B" = edgeb_floor_area * eubucco_mask,
    "(2) GEM" = gem_floor_area * eubucco_mask,
    "(3) GHS-OBAT" = ghsoobat_floor_area * eubucco_mask,
    "(4) EUBUCCO" = eubucco_floor_area * eubucco_mask
  )
  toolMplotMulti(floorlist, title = "EU27", xlab = "", ylab = ylab,
                 filename = filename, ncol = length(floorlist), nrow = 1)

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
    ghsobat <- ghsoobat_floor_area[country,,]
    floorlist <- list("(1) EDGE-B" = edgeb, "(2) GEM" = gem, "(4) EUBUCCO" = eubucco, "(3) GHS-OBAT" = ghsobat)
    filename <- paste0(savefolder, region, "_", country, ".png")
    toolMplotMulti(floorlist, title = country, xlab = "", ylab = ylab,
                   filename = filename, ncol = length(floorlist), nrow = 1)
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
#' @param nrow Number of rows for the facet layout (passed to facet_wrap).
#' @param ncol Number of columns for the facet layout (passed to facet_wrap).
#' @param ignore_na Logical indicating whether NA, NaN, Inf, and -Inf values should be ignored (removed) before plotting. Defaults to TRUE.
#' @param show_shares Logical indicating whether stacked bars should display percentage shares for each category. Defaults to FALSE.
#' @author Bennet Weiss, Pascal Sauer (mplot), Patrick Rein (mplot)
#' @importFrom rlang .data
#' @export
toolMplotMulti <- function(px, global = TRUE, total = FALSE, title = NULL, xlab = NULL, ylab = NULL, filename = NULL,
                           nrow = NULL, ncol = NULL, ignore_na = TRUE, show_shares = TRUE) {

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

  if (show_shares && !temporalCategorial) {
    warning("'show_shares' is only available for categorical temporal data (stacked bar plots). Ignoring.")
    show_shares <- FALSE
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

    if (ignore_na) {
      # Replace all NA, NaN, Inf, and -Inf values with 0.
      # This cleans the data before any transformations or aggregations.
      currentPx <- magclass::replace_non_finite(currentPx, replace = 0)
    }

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

    if (ignore_na) {
      # Filter out rows where the value column is NA.
      # The value column is named ".value" when using rev = 3.
      df <- df[!is.na(df$.value), ]
    }

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

  facetBySpatial <- !global
  facetBySource  <- !isSingleObject && temporalCategorial

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
    bar_aes <- ggplot2::aes(weight = .data$.value,
                            color = .data[[dataDimName]],
                            group = .data[[dataDimName]])

    plot <- plot + ggplot2::geom_bar(linewidth = 1.5, mapping = bar_aes)

    if (show_shares) {
      groupCols <- c(temporalDimName)
      if (facetBySpatial) {
        groupCols <- c(groupCols, spatialDimName)
      }
      if (!isSingleObject) {
        groupCols <- c(groupCols, "source")
      }

      if (length(groupCols) == 0) {
        totals <- sum(px_df$.value)
        totals <- rep(totals, nrow(px_df))
      } else {
        totals <- ave(px_df$.value, interaction(px_df[groupCols], drop = TRUE, lex.order = TRUE), FUN = sum)
      }

      share_df <- px_df
      share_df$share <- ifelse(totals > 0, share_df$.value / totals, NA_real_)
      share_df <- share_df[!is.na(share_df$share) & share_df$share > 0, , drop = FALSE]

      if (nrow(share_df) > 0) {
        share_df$label <- sprintf("%.1f%%", share_df$share * 100)
        plot <- plot + ggplot2::geom_text(
          data = share_df,
          mapping = ggplot2::aes(y = .data$.value, label = .data$label, group = .data[[dataDimName]]),
          position = ggplot2::position_stack(vjust = 0.5),
          show.legend = FALSE,
          color = "white"
        )
      }
    }
  }

  # --- Faceting ---

  if (facetBySpatial && !facetBySource) {
    # Case 1: Spatial only
    plot <- plot + ggplot2::facet_wrap(stats::as.formula(paste("~", spatialDimName)),
                                       nrow = nrow, ncol = ncol) # <--- MODIFIED
  } else if (!facetBySpatial && facetBySource) {
    # Case 2: Source only (This is your specific case with 4 objects)
    plot <- plot + ggplot2::facet_wrap(~ .data$source,
                                       nrow = nrow, ncol = ncol) # <--- MODIFIED
  } else if (facetBySpatial && facetBySource) {
    # Case 3: Both
    # Note: facet_grid does NOT accept nrow/ncol as dimensions are fixed by variables
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

