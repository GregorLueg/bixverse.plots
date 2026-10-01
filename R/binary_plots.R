# binary heatmaps --------------------------------------------------------------

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

  n_rows <- nrow(x$mat)
  n_cols <- ncol(x$mat)
  show_feature_labels <- show_feature_labels %||% (n_rows <= 80L)

  # positions in data units, gaps pushed in between groups. y counts down
  # from the top so the first feature sits on top.
  gap_x <- group_gap * n_cols
  gap_y <- group_gap * n_rows
  col_dt <- data.table::copy(x$col_annot)
  col_dt[, x0 := col_idx - 1 + (as.integer(group) - 1L) * gap_x]
  row_dt <- data.table::copy(x$row_annot)
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
  raster_layers <- purrr::map2(
    block_grid$col_grp,
    block_grid$row_grp,
    \(i, j) {
      sub_mat <- x$mat[
        row_blocks$rows[[j]],
        col_blocks$cols[[i]],
        drop = FALSE
      ]
      annotation_raster(
        matrix(grDevices::gray(1 - sub_mat), nrow(sub_mat)),
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

  # group strips, only where there is more than one group. The strips sit
  # inside the panel, only their labels go into the margins.
  strip_x <- 0.015 * x_max
  strip_y <- 0.03 * abs(y_min)
  show_col_strip <- nrow(col_blocks) > 1L
  show_row_strip <- nrow(row_blocks) > 1L

  p <- ggplot() +
    raster_layers +
    coord_cartesian(
      xlim = c(if (show_row_strip) -strip_x * 1.5 else 0, x_max),
      ylim = c(y_min, if (show_col_strip) strip_y * 1.5 else 0),
      expand = FALSE,
      clip = "off"
    ) +
    theme_void()

  if (show_col_strip || show_row_strip) {
    grp_levels <- unique(c(
      if (show_col_strip) levels(x$col_annot$group),
      if (show_row_strip) levels(x$row_annot$group)
    ))
    grp_cols <- stats::setNames(
      bx_colors(palette, n = length(grp_levels)),
      grp_levels
    )
    p <- p + scale_fill_manual(values = grp_cols, guide = "none")
  }

  if (show_col_strip) {
    col_blocks[, `:=`(ymin = strip_y * 0.5, ymax = strip_y * 1.5)]
    p <- p +
      geom_rect(
        data = col_blocks,
        aes(
          xmin = xmin,
          xmax = xmax,
          ymin = ymin,
          ymax = ymax,
          fill = as.character(group)
        )
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
        aes(
          xmin = xmin,
          xmax = xmax,
          ymin = ymin,
          ymax = ymax,
          fill = as.character(group)
        )
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
        x = x_max + strip_x * 0.5,
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
