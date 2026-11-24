# atac plots -------------------------------------------------------------------

## helpers ---------------------------------------------------------------------

#' One line per group of an ATAC fragment profile
#'
#' @param x data.table. A table of [bixverse::fragment_profiles_sc()] with a
#' `group` column.
#' @param x_col String. Column on the x-axis.
#' @param y_col String. Column on the y-axis.
#' @param palette String. Discrete palette for the groups, see [bx_colors()].
#'
#' @return A \code{\link[ggplot2]{ggplot}} object without labels. The legend
#' is only drawn with more than one group.
#'
#' @keywords internal
.plot_group_lines <- function(x, x_col, y_col, palette) {
  checkmate::assertDataFrame(x)
  checkmate::qassert(x_col, "S1")
  checkmate::qassert(y_col, "S1")
  checkmate::assertNames(names(x), must.include = c("group", x_col, y_col))
  checkmate::assertChoice(palette, BX_PALETTES)

  n_groups <- data.table::uniqueN(x[["group"]])

  ggplot(
    x,
    aes(
      x = .data[[x_col]],
      y = .data[[y_col]],
      colour = factor(.data[["group"]])
    )
  ) +
    geom_line(linewidth = 0.5) +
    scale_color_bx(palette = palette) +
    theme_bx() +
    theme(legend.position = if (n_groups > 1) "right" else "none") +
    labs(colour = NULL)
}

## plots -----------------------------------------------------------------------

#' Correlation of each LSI component with sequencing depth
#'
#' @description
#' Signac's `DepthCor()`. A component that tracks sequencing depth carries
#' depth, not biology; components the LSI left out of the embedding are drawn
#' hollow.
#'
#' @param x data.table. Output of [bixverse::get_lsi_depth_cor()], with
#' `component` and `depth_cor`, and optionally the logical `dropped`.
#' @param threshold Numeric. Dashed guides at `+-threshold`. Between 0 and 1.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @export
#' @import ggplot2
lsi_depth_plot_sc <- function(x, threshold = 0.75) {
  checkmate::assertDataFrame(x)
  checkmate::assertNames(names(x), must.include = c("component", "depth_cor"))
  checkmate::qassert(threshold, "N1[0, 1]")

  dt <- data.table::as.data.table(x)
  if (!"dropped" %in% names(dt)) {
    dt[, dropped := FALSE]
  }
  checkmate::qassert(dt$dropped, "B+")
  colour <- bx_colors("main", n = 5)[1]

  ggplot(dt, aes(x = .data[["component"]], y = .data[["depth_cor"]])) +
    geom_hline(
      yintercept = c(-threshold, threshold),
      colour = "grey",
      linetype = "dashed",
      linewidth = 0.3
    ) +
    geom_hline(yintercept = 0, colour = "black", linewidth = 0.2) +
    geom_point(data = dt[!(dropped)], colour = colour, size = 2) +
    geom_point(data = dt[(dropped)], colour = colour, size = 2, shape = 1) +
    scale_x_continuous(breaks = \(lim) unique(floor(pretty(lim)))) +
    scale_y_continuous(limits = c(-1.05, 1.05)) +
    theme_bx() +
    labs(x = "LSI component", y = "Correlation with depth")
}

#' Tn5 insertions around the TSSs
#'
#' @description
#' Signac's `TSSPlot()`, one line per group.
#'
#' @param x data.table. The `tss` element of
#' [bixverse::fragment_profiles_sc()], with `group`, `offset`, `insertions`
#' and `enrichment`.
#' @param value String. One of `c("enrichment", "insertions")`: enrichment
#' over the flank background, or the raw insertions.
#' @param palette String. Discrete palette for the groups, see [bx_colors()].
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @export
#' @import ggplot2
tss_profile_plot_sc <- function(
  x,
  value = c("enrichment", "insertions"),
  palette = "main"
) {
  value <- match.arg(value)
  checkmate::assertDataFrame(x)
  checkmate::assertChoice(value, c("enrichment", "insertions"))
  checkmate::assertNames(
    names(x),
    must.include = c("group", "offset", value)
  )
  checkmate::assertChoice(palette, BX_PALETTES)

  .plot_group_lines(x, "offset", value, palette) +
    geom_vline(
      xintercept = 0,
      colour = "grey",
      linetype = "dashed",
      linewidth = 0.3
    ) +
    labs(
      x = "Distance from TSS (bp)",
      y = if (value == "enrichment") "TSS enrichment" else "Insertions"
    )
}

#' Fragment length distribution
#'
#' @description
#' Signac's `FragmentHistogram()`, one line per group. A good library shows
#' the nucleosome-free peak under 147 bp and the mono- and di-nucleosome peaks
#' near 200 and 400 bp.
#'
#' @param x data.table. The `lengths` element of
#' [bixverse::fragment_profiles_sc()], with `group`, `length` and `fraction`.
#' @param log_scale Boolean. Use a log10 y-axis.
#' @param palette String. Discrete palette for the groups, see [bx_colors()].
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @export
#' @import ggplot2
fragment_length_plot_sc <- function(x, log_scale = FALSE, palette = "main") {
  checkmate::assertDataFrame(x)
  checkmate::assertNames(
    names(x),
    must.include = c("group", "length", "fraction")
  )
  checkmate::qassert(log_scale, "B1")
  checkmate::assertChoice(palette, BX_PALETTES)

  p <- .plot_group_lines(x, "length", "fraction", palette) +
    geom_vline(
      xintercept = c(147, 294),
      colour = "grey",
      linetype = "dashed",
      linewidth = 0.3
    ) +
    labs(x = "Fragment length (bp)", y = "Fraction of fragments")

  if (log_scale) {
    p <- p + scale_y_log10()
  }

  p
}
