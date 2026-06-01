# qc plots ---------------------------------------------------------------------

## helpers ---------------------------------------------------------------------

### plot workers ---------------------------------------------------------------

#' Density plot worker
#'
#' @param df data.table. Plotting-ready data.
#' @param grouping_column Character. Column used to group the densities.
#' @param variable Character. Numeric column to plot on the x-axis.
#' @param outlier_groups data.table. Outlier groups to label, with columns
#' `group_id` and `group_median`.
#' @param var_name Character. x-axis label (default: NULL).
#' @param log_scale Logical. Apply a log10 x-axis (default: FALSE).
#' @param adjust_position_label Numeric. x-offset for the labels (default: 0).
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @keywords internal
.plot_density <- function(
  df,
  grouping_column,
  variable,
  outlier_groups,
  var_name = NULL,
  log_scale = FALSE,
  adjust_position_label = 0
) {
  checkmate::assertDataTable(df)
  checkmate::qassert(grouping_column, "S1")
  checkmate::qassert(variable, "S1")
  checkmate::qassert(log_scale, "B1")
  checkmate::qassert(adjust_position_label, "N1")
  checkmate::qassert(var_name, c("S1", "0"))

  if (is.null(var_name)) {
    var_name <- variable
  }

  max_y <- df[,
    .(max_density = max(density(.SD[[variable]])$y)),
    by = grouping_column
  ][, max(max_density)]

  p <- ggplot(
    df,
    aes(x = .data[[variable]], fill = .data[[grouping_column]])
  ) +
    geom_density(alpha = 0.2) +
    ggrepel::geom_label_repel(
      data = outlier_groups,
      mapping = aes(
        x = .data[["group_median"]] + adjust_position_label,
        y = max_y,
        label = .data[["group_id"]],
        color = .data[["group_id"]]
      ),
      alpha = 0.4,
      vjust = 1,
      inherit.aes = FALSE
    ) +
    theme_bx() +
    theme(legend.position = "none") +
    labs(x = var_name, y = "Density", title = var_name)

  if (log_scale) {
    p <- p + scale_x_log10() + labs(x = paste(var_name, "(log10)"))
  }

  return(p)
}

#' Violin plot worker
#'
#' @param df data.table. Plotting-ready data.
#' @param grouping_column Character. Column used for the x-axis groups.
#' @param variable Character. Numeric column to plot on the y-axis.
#' @param outlier_column Character. Logical column used to colour the jitter.
#' @param group_name Character. x-axis label (default: NULL).
#' @param var_name Character. y-axis label (default: NULL).
#' @param log_scale Logical. Apply a log10 y-axis (default: TRUE).
#' @param show_outlier Logical. Overlay jittered points coloured by
#' `outlier_column` (default: TRUE).
#' @param raster Boolean. Shall [scattermore::geom_scattermore()] be used.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @keywords internal
.plot_violin <- function(
  df,
  grouping_column,
  variable,
  outlier_column = "global_outlier",
  group_name = NULL,
  var_name = NULL,
  log_scale = TRUE,
  show_outlier = TRUE,
  raster = FALSE
) {
  checkmate::assertDataTable(df)
  checkmate::qassert(grouping_column, "S1")
  checkmate::qassert(variable, "S1")
  checkmate::qassert(log_scale, "B1")
  checkmate::qassert(show_outlier, "B1")
  checkmate::qassert(raster, "B1")
  checkmate::qassert(group_name, c("S1", "0"))
  checkmate::qassert(var_name, c("S1", "0"))
  if (show_outlier) {
    checkmate::assertNames(colnames(df), must.include = outlier_column)
  }

  if (is.null(group_name)) {
    group_name <- grouping_column
  }
  if (is.null(var_name)) {
    var_name <- variable
  }

  p <- ggplot(
    df,
    aes(x = .data[[grouping_column]], y = .data[[variable]])
  ) +
    geom_violin(alpha = 0.6) +
    theme_bx() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = -45, hjust = 0)
    ) +
    labs(x = group_name, y = var_name, title = var_name)

  if (log_scale) {
    p <- p + scale_y_log10() + labs(y = paste(var_name, "(log10)"))
  }

  if (show_outlier) {
    outlier_colours <- c("FALSE" = "lightgrey", "TRUE" = "orange")
    jitter_layer <- if (raster) {
      scattermore::geom_scattermore(
        mapping = aes(colour = .data[[outlier_column]]),
        position = position_jitter(width = 0.05),
        pointsize = 1,
        show.legend = FALSE
      )
    } else {
      geom_jitter(
        mapping = aes(colour = .data[[outlier_column]]),
        width = 0.05,
        size = 0.4,
        alpha = 0.5,
        show.legend = FALSE
      )
    }
    p <- p +
      jitter_layer +
      scale_colour_manual(values = outlier_colours)
  }

  return(p)
}

#' Joint hexbin plot worker
#'
#' @param df data.table. Plotting-ready data.
#' @param library_size Character. Column with the library size per cell.
#' @param nb_features Character. Column with the number of features per cell.
#' @param log_scale Logical. Log10-transform both axes (default: FALSE).
#'
#' @return A \code{ggExtraPlot} object.
#'
#' @keywords internal
.plot_joint <- function(
  df,
  library_size = "lib_size",
  nb_features = "nnz",
  log_scale = FALSE
) {
  checkmate::assertDataTable(df)
  checkmate::assertNames(
    colnames(df),
    must.include = c(library_size, nb_features)
  )
  checkmate::qassert(log_scale, "B1")

  if (log_scale) {
    df <- data.table::copy(df)
    df[, `:=`(
      .__x = log10(get(nb_features)),
      .__y = log10(get(library_size))
    )]
    x_col <- ".__x"
    y_col <- ".__y"
  } else {
    x_col <- nb_features
    y_col <- library_size
  }

  p <- ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    geom_point(alpha = 0) +
    geom_hex(bins = 80) +
    scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Blues")[4:9]) +
    theme_bx() +
    theme(legend.position = "none") +
    labs(x = "Genes per cell (log10)", y = "UMIs per cell (log10)")

  ggExtra::ggMarginal(
    p,
    type = "histogram",
    fill = "steelblue3",
    color = "black",
    bins = 50
  )
}

### methods --------------------------------------------------------------------

#' Per-metric violin plots from a CellQc object
#'
#' @param x A `CellQc` object.
#' @param log_scale Logical. Apply a log10 y-axis (default: FALSE).
#' @param show_outlier Logical. Overlay outlier points (default: TRUE).
#' @param raster Optional boolean. Shall the plot be rasterised. If `NULL` and
#' number of cells is larger than `1e5`, defaults to TRUE.
#' @param ... Ignored.
#'
#' @return A named list of ggplot objects, one per metric.
#'
#' @export
#'
#' @import ggplot2
violin_plot_sc.CellQc <- function(
  x,
  log_scale = FALSE,
  show_outlier = TRUE,
  raster = NULL,
  ...
) {
  plot_df <- bixverse::get_data(x)

  n_cells <- nrow(plot_df)
  raster <- raster %||% (n_cells > 1e5)

  if (raster) {
    message(paste(
      "Raster was set to TRUE or n_cells > 1e5 -> Rasterising the plot"
    ))
  }

  plots <- purrr::map(names(x$metrics), function(metric_name) {
    .plot_violin(
      df = plot_df,
      grouping_column = "grp",
      variable = metric_name,
      outlier_column = sprintf("%s_is_outlier", metric_name),
      var_name = metric_name,
      log_scale = log_scale,
      show_outlier = show_outlier,
      raster = raster
    )
  })

  setNames(plots, names(x$metrics))
}

#' Violin plot from a data.table
#'
#' Per-cell outliers are recomputed within each `grouping_column` group.
#'
#' @param df data.table. Input data containing the QC metric.
#' @param grouping_column Character. Column used for the x-axis groups.
#' @param variable Character. Numeric column to plot on the y-axis.
#' @param direction Character. One of `"twosided"`, `"below"`, `"above"`.
#' @param threshold Numeric. Number of MADs for outlier detection (default: 3).
#' @param group_name Character. x-axis label (default: NULL).
#' @param var_name Character. y-axis label (default: NULL).
#' @param log_scale Logical. Apply a log10 y-axis (default: TRUE).
#' @param show_outlier Logical. Overlay outlier points (default: TRUE).
#' @param raster Optional boolean. Shall the plot be rasterised. If `NULL` and
#' number of cells is larger than `1e5`, defaults to TRUE.
#' @param ... Ignored.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @export
#'
#' @import ggplot2
violin_plot_sc.data.table <- function(
  df,
  grouping_column,
  variable,
  direction = c("twosided", "below", "above"),
  threshold = 3,
  group_name = NULL,
  var_name = NULL,
  log_scale = TRUE,
  show_outlier = TRUE,
  raster = NULL,
  ...
) {
  direction <- match.arg(direction)
  checkmate::assertDataTable(df)
  checkmate::assertNames(
    colnames(df),
    must.include = c(grouping_column, variable)
  )
  checkmate::qassert(threshold, "N1")
  checkmate::qassert(show_outlier, "B1")

  n_cells <- nrow(df)
  raster <- raster %||% (n_cells > 1e5)

  outlier_column <- NULL
  if (show_outlier) {
    df <- data.table::copy(df)
    df[,
      .qc_outlier := per_cell_qc_outlier(
        metric = get(variable),
        threshold = threshold,
        direction = direction
      )$outlier,
      by = grouping_column
    ]
    outlier_column <- ".qc_outlier"
  }

  .plot_violin(
    df = df,
    grouping_column = grouping_column,
    variable = variable,
    outlier_column = outlier_column,
    group_name = group_name,
    var_name = var_name,
    log_scale = log_scale,
    show_outlier = show_outlier,
    raster = raster
  )
}

#' Per-metric density plots from a CellQc object
#'
#' Requires grouped data; outlier groups are read from `per_group_stats`.
#'
#' @param x A `CellQc` object.
#' @param adjust_position_label Numeric. x-offset for the labels (default: 0).
#' @param ... Ignored.
#'
#' @return A named list of ggplot objects, one per metric.
#'
#' @export
#'
#' @import ggplot2
density_plot_sc.CellQc <- function(x, adjust_position_label = 0, ...) {
  if (is.null(x$per_group_stats)) {
    stop("Density QC requires grouped data; `per_group_stats` is NULL.")
  }

  plot_df <- get_data(x)
  stats <- x$per_group_stats

  plots <- purrr::map(names(x$metrics), function(metric_name) {
    outlier_groups <- stats[
      metric == metric_name & (is_outlier),
      .(group_id = group, group_median)
    ]
    .plot_density(
      df = plot_df,
      grouping_column = "grp",
      variable = metric_name,
      outlier_groups = outlier_groups,
      var_name = metric_name,
      log_scale = FALSE,
      adjust_position_label = adjust_position_label
    )
  })

  setNames(plots, names(x$metrics))
}

#' Density plot from a data.table
#'
#' Group-level outliers are recomputed from per-group medians.
#'
#' @param df data.table. Input data containing the QC metric.
#' @param grouping_column Character. Column used to group the densities.
#' @param variable Character. Numeric column to plot on the x-axis.
#' @param direction Character. One of `"twosided"`, `"below"`, `"above"`.
#' @param threshold Numeric. Number of MADs for outlier detection (default: 3).
#' @param var_name Character. x-axis label (default: NULL).
#' @param log_scale Logical. Apply a log10 x-axis (default: TRUE).
#' @param adjust_position_label Numeric. x-offset for the labels (default: 0).
#' @param ... Ignored.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @export
#'
#' @import ggplot2
density_plot_sc.data.table <- function(
  df,
  grouping_column,
  variable,
  direction = c("twosided", "below", "above"),
  threshold = 3,
  var_name = NULL,
  log_scale = TRUE,
  adjust_position_label = 0,
  ...
) {
  direction <- match.arg(direction)
  checkmate::assertDataTable(df)
  checkmate::assertNames(
    colnames(df),
    must.include = c(grouping_column, variable)
  )
  checkmate::qassert(threshold, "N1")

  medians <- df[,
    .(group_median = median(.SD[[variable]])),
    by = grouping_column
  ]
  res <- per_cell_qc_outlier(
    metric = medians$group_median,
    threshold = threshold,
    direction = direction
  )
  medians[, is_outlier := res$outlier]
  outlier_groups <- medians[
    (is_outlier),
    .(group_id = get(grouping_column), group_median)
  ]

  .plot_density(
    df = df,
    grouping_column = grouping_column,
    variable = variable,
    outlier_groups = outlier_groups,
    var_name = var_name,
    log_scale = log_scale,
    adjust_position_label = adjust_position_label
  )
}

#' Joint QC plot from a CellQc object
#'
#' @param x A `CellQc` object.
#' @param library_size Character. Column with the library size per cell.
#' @param nb_features Character. Column with the number of features per cell.
#' @param ... Ignored.
#'
#' @return A \code{ggExtraPlot} object.
#'
#' @export
#' @import ggplot2
joint_plot_sc.CellQc <- function(
  x,
  library_size = "log10_lib_size",
  nb_features = "log10_nnz",
  ...
) {
  plot_df <- get_data(x)
  checkmate::assertNames(
    colnames(plot_df),
    must.include = c(library_size, nb_features)
  )

  .plot_joint(
    df = plot_df,
    library_size = library_size,
    nb_features = nb_features,
    log_scale = FALSE
  )
}

#' Joint QC plot from a data.table
#'
#' @param df data.table. Input data containing the QC metrics.
#' @param library_size Character. Column with the library size per cell.
#' @param nb_features Character. Column with the number of features per cell.
#' @param log_scale Logical. Log10-transform both axes (default: FALSE).
#' @param ... Ignored.
#'
#' @return A \code{ggExtraPlot} object.
#'
#' @export
#' @import ggplot2
joint_plot_sc.data.table <- function(
  df,
  library_size = "lib_size",
  nb_features = "nnz",
  log_scale = FALSE,
  ...
) {
  .plot_joint(
    df = df,
    library_size = library_size,
    nb_features = nb_features,
    log_scale = log_scale
  )
}
