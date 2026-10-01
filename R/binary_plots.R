# block heatmaps ---------------------------------------------------------------

## helpers ---------------------------------------------------------------------

#' Lay out a grouped heatmap as one raster per block
#'
#' @description
#' Places the columns and rows of a heatmap in data units with gaps pushed in
#' between groups and draws every block of column group x row group as one
#' [ggplot2::annotation_raster()]. y counts down from the top, so the first
#' row sits on top.
#'
#' @param colour_mat Character matrix of colours, rows x columns, already in
#' display order.
#' @param col_annot data.table with `col_idx` and the factor `group`, one row
#' per column of `colour_mat`, sorted by `col_idx`.
#' @param row_annot data.table with `row_idx`, the factor `group` and
#' `feature`, one row per row of `colour_mat`, sorted by `row_idx`.
#' @param group_gap Numeric between `[0, 0.1]`. Width of each gap between
#' groups as fraction of the axis length.
#'
#' @returns A list with:
#' \itemize{
#'   \item layers - List of raster layers, one per block.
#'   \item col_blocks - data.table with `group`, `xmin`, `xmax`, `cols`.
#'   \item row_blocks - data.table with `group`, `ymin`, `ymax`, `rows`.
#'   \item row_dt - `row_annot` with the top edge `y_top` of every row.
#'   \item x_max, y_min - Extent of the matrix.
#'   \item strip_x, strip_y - Strip heights in data units.
#' }
#'
#' @keywords internal
.heatmap_blocks <- function(colour_mat, col_annot, row_annot, group_gap) {
  checkmate::assertMatrix(colour_mat, mode = "character")
  checkmate::assertDataTable(col_annot, nrows = ncol(colour_mat))
  checkmate::assertNames(names(col_annot), must.include = c("col_idx", "group"))
  checkmate::assertFactor(col_annot$group)
  checkmate::assertDataTable(row_annot, nrows = nrow(colour_mat))
  checkmate::assertNames(
    names(row_annot),
    must.include = c("row_idx", "group", "feature")
  )
  checkmate::assertFactor(row_annot$group)
  checkmate::qassert(group_gap, "N1[0,0.1]")

  gap_x <- group_gap * ncol(colour_mat)
  gap_y <- group_gap * nrow(colour_mat)
  col_dt <- data.table::copy(col_annot)
  col_dt[, x0 := col_idx - 1 + (as.integer(group) - 1L) * gap_x]
  row_dt <- data.table::copy(row_annot)
  row_dt[, y_top := -(row_idx - 1 + (as.integer(group) - 1L) * gap_y)]

  col_blocks <- col_dt[,
    .(xmin = min(x0), xmax = max(x0) + 1, cols = list(col_idx)),
    by = group
  ]
  row_blocks <- row_dt[,
    .(ymin = min(y_top) - 1, ymax = max(y_top), rows = list(row_idx)),
    by = group
  ]

  block_grid <- data.table::CJ(
    col_grp = seq_len(nrow(col_blocks)),
    row_grp = seq_len(nrow(row_blocks))
  )
  layers <- purrr::map2(
    block_grid$col_grp,
    block_grid$row_grp,
    \(i, j) {
      annotation_raster(
        colour_mat[row_blocks$rows[[j]], col_blocks$cols[[i]], drop = FALSE],
        xmin = col_blocks$xmin[i],
        xmax = col_blocks$xmax[i],
        ymin = row_blocks$ymin[j],
        ymax = row_blocks$ymax[j],
        interpolate = FALSE
      )
    }
  )

  x_max <- max(col_blocks$xmax)
  y_min <- min(row_blocks$ymin)

  list(
    layers = layers,
    col_blocks = col_blocks,
    row_blocks = row_blocks,
    row_dt = row_dt,
    x_max = x_max,
    y_min = y_min,
    strip_x = 0.015 * x_max,
    strip_y = 0.03 * abs(y_min)
  )
}

#' Add group strips, feature labels and margins to a block heatmap
#'
#' @description
#' Group strips are only drawn where there is more than one group. The strips
#' sit inside the panel, only their labels go into the margins. Column and row
#' groups share one colour mapping, so a row group with the same name as a
#' column group gets the same colour.
#'
#' @param p A \code{\link[ggplot2]{ggplot}} object holding the block rasters.
#' @param layout List. Output of [.heatmap_blocks()].
#' @param show_feature_labels Boolean. Shall the row labels be written to the
#' right of the matrix.
#' @param col_strip_offset Numeric. Pushes the column group strip up, to make
#' room for an extra strip on top of the matrix.
#' @param palette String. Discrete palette for the group strips, see
#' [bx_colors()].
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @keywords internal
.heatmap_strips <- function(
  p,
  layout,
  show_feature_labels,
  col_strip_offset = 0,
  palette = "main"
) {
  checkmate::assertClass(p, "ggplot")
  checkmate::assertList(layout)
  checkmate::assertNames(
    names(layout),
    must.include = c(
      "col_blocks",
      "row_blocks",
      "row_dt",
      "x_max",
      "y_min",
      "strip_x",
      "strip_y"
    )
  )
  checkmate::qassert(show_feature_labels, "B1")
  checkmate::qassert(col_strip_offset, "N1[0,)")
  checkmate::assertChoice(palette, BX_PALETTES)

  col_blocks <- data.table::copy(layout$col_blocks)
  row_blocks <- data.table::copy(layout$row_blocks)
  row_dt <- layout$row_dt
  strip_x <- layout$strip_x
  strip_y <- layout$strip_y
  show_col_strip <- nrow(col_blocks) > 1L
  show_row_strip <- nrow(row_blocks) > 1L

  p <- p +
    coord_cartesian(
      xlim = c(if (show_row_strip) -strip_x * 1.5 else 0, layout$x_max),
      ylim = c(
        layout$y_min,
        if (show_col_strip) strip_y * 1.5 + col_strip_offset else 0
      ),
      expand = FALSE,
      clip = "off"
    )

  if (show_col_strip || show_row_strip) {
    grp_levels <- unique(c(
      if (show_col_strip) levels(col_blocks$group),
      if (show_row_strip) levels(row_blocks$group)
    ))
    grp_cols <- stats::setNames(
      bx_colors(palette, n = length(grp_levels)),
      grp_levels
    )
  }

  if (show_col_strip) {
    col_blocks[, `:=`(
      ymin = strip_y * 0.5 + col_strip_offset,
      ymax = strip_y * 1.5 + col_strip_offset
    )]
    p <- p +
      geom_rect(
        data = col_blocks,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        fill = grp_cols[as.character(col_blocks$group)]
      ) +
      geom_text(
        data = col_blocks,
        aes(x = (xmin + xmax) / 2, y = ymax + strip_y * 0.2, label = group),
        angle = 90,
        hjust = 0,
        size = 2.5
      )
  }

  if (show_row_strip) {
    row_blocks[, `:=`(xmin = -strip_x * 1.5, xmax = -strip_x * 0.5)]
    p <- p +
      geom_rect(
        data = row_blocks,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        fill = grp_cols[as.character(row_blocks$group)]
      ) +
      geom_text(
        data = row_blocks,
        aes(x = xmin - strip_x * 0.2, y = (ymin + ymax) / 2, label = group),
        hjust = 1,
        size = 2.5
      )
  }

  if (show_feature_labels) {
    p <- p +
      annotate(
        "text",
        x = layout$x_max + strip_x * 0.5,
        y = row_dt$y_top - 0.5,
        label = row_dt$feature,
        hjust = 0,
        size = 2
      )
  }

  # the gaps between blocks are background, so it cannot stay transparent.
  # margins in pt from the longest label, ~2.2 pt per char per mm text size
  label_pt <- \(labels, size) 5 + 2.2 * size * max(nchar(as.character(labels)))
  p +
    theme(
      plot.background = element_rect(fill = "white", colour = NA),
      plot.margin = margin(
        t = if (show_col_strip) label_pt(col_blocks$group, 2.5) else 5,
        r = if (show_feature_labels) label_pt(row_dt$feature, 2) else 5,
        b = 5,
        l = if (show_row_strip) label_pt(row_blocks$group, 2.5) else 5
      )
    )
}

## binary heatmap --------------------------------------------------------------

#' Binary heatmap, e.g. SCENIC regulon on/off calls
#'
#' @description
#' Draws a samples x features on/off matrix in the style of the SCENIC paper:
#' features as rows, samples as columns, black for on and white for off. Every
#' block of sample group x feature group is drawn as one raster, so the plot
#' stays light no matter how many samples there are, and the blocks are
#' separated by white gaps. Groups get a thin colour strip with their labels
#' written next to it. Sample and feature groups share one colour mapping, so a
#' feature group with the same name as a sample group gets the same colour.
#' `group_annotation` adds a second strip with a legend between the matrix and
#' the sample group strip, e.g. the response group of each cell line.
#'
#' Filtering, ordering and binning happen in
#' [bixverse::extract_binary_heatmap_data()]. If the samples were binned the
#' cells show the fraction of samples on as a grey ramp.
#'
#' PDF viewers tend to smooth embedded rasters. If the blocks look blurry in a
#' PDF, save as PNG instead.
#'
#' @param x Either a `BinaryHeatmapData` from
#' [bixverse::extract_binary_heatmap_data()] or a logical matrix of samples x
#' features, which then gets passed through it.
#' @param sample_groups Optional named character vector or factor mapping
#' samples to groups. Only used if `x` is a matrix.
#' @param feature_groups Optional named character vector or factor mapping
#' features to groups. Only used if `x` is a matrix.
#' @param heatmap_params List. Output of [bixverse::params_binary_heatmap()].
#' Only used if `x` is a matrix.
#' @param show_feature_labels Optional boolean. Shall the feature names be
#' shown. `NULL` (default) shows them for up to 80 features.
#' @param group_gap Numeric between `[0, 0.1]`. Width of each gap between
#' groups as fraction of the axis length. Applied per boundary, so with many
#' groups keep it small. `0` removes the gaps.
#' @param group_annotation Optional named character vector or factor mapping
#' every sample group to a coarser label. Factor levels set the legend order.
#' Ignored if there is only one sample group.
#' @param annotation_colours Optional named character vector of colours for
#' the `group_annotation` labels. `NULL` (default) uses viridis.
#' @param palette String. Discrete palette for the group strips, see
#' [bx_colors()].
#' @param .verbose Boolean. Controls verbosity of the extraction.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @references Aibar, et al., Nat Methods, 2017
#'
#' @export
#' @import ggplot2
plot_binary_heatmap <- function(
  x,
  sample_groups = NULL,
  feature_groups = NULL,
  heatmap_params = bixverse::params_binary_heatmap(),
  show_feature_labels = NULL,
  group_gap = 0.001,
  group_annotation = NULL,
  annotation_colours = NULL,
  palette = c("main", "sequential", "diverging", "viridis", "spectral"),
  .verbose = FALSE
) {
  palette <- match.arg(palette)
  checkmate::assert(
    checkmate::checkClass(x, "BinaryHeatmapData"),
    checkmate::checkMatrix(x, mode = "logical")
  )
  checkmate::qassert(show_feature_labels, c("0", "B1"))
  checkmate::qassert(group_gap, "N1[0,0.1]")
  checkmate::assert(
    checkmate::checkNull(group_annotation),
    checkmate::checkCharacter(group_annotation, any.missing = FALSE),
    checkmate::checkFactor(group_annotation, any.missing = FALSE)
  )
  checkmate::assertCharacter(
    annotation_colours,
    any.missing = FALSE,
    names = "unique",
    null.ok = TRUE
  )
  checkmate::assertChoice(palette, BX_PALETTES)
  checkmate::qassert(.verbose, "B1")

  if (is.matrix(x)) {
    x <- bixverse::extract_binary_heatmap_data(
      x,
      sample_groups = sample_groups,
      feature_groups = feature_groups,
      heatmap_params = heatmap_params,
      .verbose = .verbose
    )
  }

  show_feature_labels <- show_feature_labels %||% (nrow(x$mat) <= 80L)

  layout <- .heatmap_blocks(
    colour_mat = matrix(grDevices::gray(1 - x$mat), nrow(x$mat)),
    col_annot = x$col_annot,
    row_annot = x$row_annot,
    group_gap = group_gap
  )
  col_blocks <- data.table::copy(layout$col_blocks)
  strip_y <- layout$strip_y

  show_annot_strip <- nrow(col_blocks) > 1L && !is.null(group_annotation)
  # the annotation strip sits right on top of the matrix and pushes the group
  # strip up by one strip height
  col_strip_offset <- if (show_annot_strip) strip_y else 0

  p <- ggplot() +
    layout$layers +
    theme_void()

  if (show_annot_strip) {
    checkmate::assertNames(
      names(group_annotation),
      must.include = as.character(col_blocks$group)
    )
    annot_map <- as.factor(group_annotation)
    annot_cols <- annotation_colours %||%
      stats::setNames(
        bx_colors("viridis", n = nlevels(annot_map)),
        levels(annot_map)
      )
    checkmate::assertNames(names(annot_cols), must.include = levels(annot_map))

    # neighbouring groups with the same label merge into one rect
    col_blocks[, annot := annot_map[as.character(group)]]
    annot_runs <- col_blocks[,
      .(xmin = min(xmin), xmax = max(xmax), annot = annot[1]),
      by = .(run = data.table::rleid(annot))
    ]
    p <- p +
      geom_rect(
        data = annot_runs,
        aes(
          xmin = xmin,
          xmax = xmax,
          ymin = strip_y * 0.5,
          ymax = strip_y * 1.5,
          fill = annot
        )
      ) +
      scale_fill_manual(values = annot_cols, name = NULL, drop = TRUE) +
      theme(legend.position = "bottom")
  }

  .heatmap_strips(
    p,
    layout = layout,
    show_feature_labels = show_feature_labels,
    col_strip_offset = col_strip_offset,
    palette = palette
  )
}
