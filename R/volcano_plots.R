# volcano plots ----------------------------------------------------------------

#' Create a volcano plot
#'
#' @description Helper function to generate volcano plots for differental
#' gene/transcript/protein expression analysis. The function applies under the
#' hood a `-log10(stat)` transformation to the respective statistic you want
#' to display on the y axis. If you provide labels, the top features to label
#' will be chosen by ranking the `-log10(stat)`.
#'
#' @param dt A data.table/data.frame holding the differential expression results.
#' @param volcano_plot_params A list as returned by [params_volcano()].
#' @param x_lab Optional string. Overwrite of the x label of the plot. If not
#' provided, will default to `volcano_plot_params$x_axis`.
#' @param y_lab Optional string. Overwrite of the x label of the plot. If not
#' provided, will default to `-log10(volcano_plot_params$y_axis)`.
#' @param plot_title Optional string. If provided, the plot will get a title.
#' @param plot_sub_title Optional string. If provided, adds this as a sub title
#' to the plot.
#'
#' @returns A `ggplot` object.
#'
#' @export
#'
#' @import ggplot2
volcano_plot <- function(
  dt,
  volcano_plot_params = params_volcano(),
  x_lab = NULL,
  y_lab = NULL,
  plot_title = NULL,
  plot_sub_title = NULL
) {
  # checks
  checkmate::assertDataTable(dt)
  assertVolcanoParams(volcano_plot_params, dt = dt)
  checkmate::qassert(x_lab, c("S1", "0"))
  checkmate::qassert(y_lab, c("S1", "0"))
  checkmate::qassert(plot_title, c("S1", "0"))
  checkmate::qassert(plot_sub_title, c("S1", "0"))

  # warn if plot_sub_title is provided without plot title
  if (is.null(plot_title) & !is.null(plot_sub_title)) {
    warning(paste(
      "You provided 'plot_sub_title' without 'plot_title'.",
      "No title will be added"
    ))
  }

  # function body
  p <- volcano_plot_params
  dt <- data.table::copy(dt)

  x <- p[["x_axis"]]
  y <- p[["y_axis"]]
  colour <- if (is.null(p[["colour"]])) x else p[["colour"]]

  x_lab <- x_lab %||% x
  y_lab <- sprintf("-log10(%s)", y_lab %||% y)

  dt[, `.volcano_y` := -log10(get(y))]

  do_labels <- !is.null(p[["label_column"]]) &&
    !is.null(p[["top_features_to_label"]])
  if (do_labels) {
    label_col <- p[["label_column"]]
    data.table::setorderv(dt, cols = y, order = 1L)
    top <- dt[[label_col]][seq_len(min(p[["top_features_to_label"]], nrow(dt)))]
    dt[,
      `.volcano_label` := data.table::fifelse(
        get(label_col) %in% top,
        get(label_col),
        ""
      )
    ]
  }

  plt <- ggplot(
    dt,
    aes(x = .data[[x]], y = .data[[".volcano_y"]])
  ) +
    geom_point(
      aes(fill = .data[[colour]]),
      size = 3,
      shape = 21,
      alpha = 0.7
    ) +
    scale_fill_gradient2(high = "#421401", low = "#235070") +
    geom_vline(xintercept = 0, linewidth = 0.25, linetype = "dashed") +
    xlab(x_lab) +
    ylab(y_lab) +
    theme_bx() +
    theme(legend.position = "none")

  if (do_labels) {
    plt <- plt +
      ggrepel::geom_text_repel(
        aes(label = .data[[".volcano_label"]]),
        max.overlaps = 1000,
        size = 3
      )
  }

  if (!is.null(plot_title)) {
    plt <- plt +
      ggtitle(label = plot_title, subtitle = plot_sub_title %||% waiver())
  }

  return(plt)
}
