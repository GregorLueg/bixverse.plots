#' QC Density Plot
#'
#' Creates a log10-scale density plot of a QC variable grouped by a categorical
#' column (e.g. donor, sample). Groups whose per-group median falls more than 3 (by default)
#' MADs below the global median are automatically flagged and labelled on the
#' plot.
#'
#' @param df Dataframe. Input data table containing QC metrics.
#' @param grouping_column Character. Name of the column used to group density plot
#' @param variable Character. Name of the numeric QC column to plot on the x-axis.
#' @param var_name Character. Label for the x-axis (default: NULL).
#' @param nmads Integer. Number of MADs to use for outlier detection (default: 3).
#' @param log_scale Boolean. Log scale (default: TRUE)
#' @param adjust_position_label Numeric. Value to adjust the position of the labels on the X axis (default: 0)
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @details
#' ## Outlier detection
#' Outliers are flagged at the **group level**. For each group the median of
#' \code{variable} is computed; a group is considered an outlier when its median
#' satisfies:
#'
#' \deqn{\tilde{x}_g \leq \mathrm{median}(\tilde{x}) - nmads \times
#' \mathrm{MAD}(\tilde{x})}{}
#'
#' where \eqn{\tilde{x}_g} is the per-group median and the median and MAD are
#' taken across all groups.
#'
#' @keywords internal
#'
plot_density_sc <- function(
  df,
  grouping_column,
  variable,
  var_name = NULL,
  nmads = 3,
  log_scale = TRUE,
  adjust_position_label = 0
) {
  ## checkmates
  checkmate::assertDataTable(df)
  checkmate::assert(all(c(grouping_column, variable) %in% colnames(df)))
  checkmate::assertLogical(log_scale)
  checkmate::assert(
    checkmate::checkCharacter(var_name),
    checkmate::checkNull(var_name)
  )
  checkmate::assertNumeric(adjust_position_label)

  ## Identify outlier samples
  #  Flag outliers per donor using MAD on log(nnz)
  df <- df[, log_variable := log10(.SD[[variable]])]

  median_by_donor <- df[,
    .(
      median_var = median(.SD[[variable]]),
      median_log_var = median(log_variable)
    ),
    by = grouping_column
  ]
  median_var <- median(median_by_donor$median_var)
  mad_var <- mad(median_by_donor$median_var)
  ##
  outliers_var_neg <- median_var - nmads * mad_var
  outliers_var_pos <- median_var + nmads * mad_var
  median_by_donor <- median_by_donor[,
    var_outlier := (median_var <= outliers_var_neg |
      median_var >= outliers_var_pos)
  ]
  outlier_donors <- median_by_donor[(var_outlier)]

  ## Max y for plotting label
  max_y <- df[,
    .(max_density = {
      d <- density(.SD[[variable]])
      max(d$y)
    }),
    by = grouping_column
  ][, max(max_density)]
  ## Same for log scale
  max_log_y <- df[,
    .(max_density = {
      d <- density(log10(.SD[[variable]]))
      max(d$y)
    }),
    by = grouping_column
  ][, max(max_density)]
  ## X label
  if (is.null(var_name)) {
    var_name = variable
  }
  ## Plot
  if (log_scale) {
    p <- ggplot(
      df,
      aes(x = .data[[variable]], fill = .data[[grouping_column]])
    ) +
      geom_density(alpha = 0.2) +
      geom_label(
        data = outlier_donors,
        mapping = aes(
          x = median_var,
          y = max_log_y + 0.2,
          label = .data[[grouping_column]],
          fill = .data[[grouping_column]]
        ),
        alpha = 0.4,
        vjust = 1,
        inherit.aes = FALSE
      ) +
      theme_classic() +
      theme(legend.position = "none") +
      scale_x_log10() +
      labs(
        x = paste(var_name, "(log10)"),
        y = "Density"
      )
  } else {
    p <- ggplot(
      df,
      aes(x = .data[[variable]], fill = .data[[grouping_column]])
    ) +
      geom_density(alpha = 0.2) +
      geom_label(
        data = outlier_donors,
        mapping = aes(
          x = median_var + adjust_position_label,
          y = max_y,
          label = .data[[grouping_column]],
          fill = .data[[grouping_column]]
        ),
        alpha = 0.4,
        vjust = 1,
        inherit.aes = FALSE
      ) +
      theme_classic() +
      theme(legend.position = "none") +
      labs(
        x = var_name,
        y = "Density",
        title = var_name
      )
  }
  return(p)
}

#' QC Density Plot
#'
#' Creates a log10-scale density plot of a QC variable grouped by a categorical
#' column (e.g. donor, sample). Groups whose per-group median falls more than 3 (by default)
#' MADs below the global median are automatically flagged and labelled on the
#' plot.
#'
#' @param df Dataframe. Input data table containing QC metrics.
#' @param grouping_column Character. Name of the column used to group density plot
#' @param variable Character. Name of the numeric QC column to plot on the x-axis.
#' @param group_name Character. Label for the x-axis (default: NULL).
#' @param var_name Character. Label for the y-axis (default: NULL).
#' @param log_scale Boolean. Log scale (default: TRUE)
#' @param show_outlier Boolean. Show the cells that are identified as outliers (default: TRUE)
#' @param outlier_column Character. Which column contains the cells identified as outliers
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' set.seed(42)
#' n_cells <- 500
#' df <- data.table(
#'   donor_id = sample(paste0("D", 1:5), n_cells, replace = TRUE),
#'   nnz      = rnbinom(n_cells, mu = 2000, size = 5)
#' )
#' # Simulate a low-quality donor
#' df[donor_id == "D5", nnz := rnbinom(.N, mu = 200, size = 5)]
#'
#' plot_violin_sc(
#'   df = df,
#'   grouping_column = "donor_id",
#'   variable = "nnz",
#'   var_name = "# Features"
#' )
#' }
#'
plot_violin_sc <- function(
  df,
  grouping_column,
  variable,
  group_name = NULL,
  var_name = NULL,
  log_scale = TRUE,
  show_outlier = TRUE,
  outlier_column = "global_outlier"
) {
  ## checkmates
  checkmate::assertDataTable(df)
  checkmate::assert(all(
    c(grouping_column, variable) %in% colnames(df)
  ))
  checkmate::assertLogical(log_scale)
  checkmate::assert(
    checkmate::checkCharacter(var_name),
    checkmate::checkNull(var_name)
  )
  checkmate::assert(
    checkmate::checkCharacter(group_name),
    checkmate::checkNull(group_name)
  )
  checkmate::assertLogical(show_outlier)
  ## If outlier, then the column must exist
  if (show_outlier) {
    checkmate::assertNames(colnames(df), must.include = outlier_column)
  }
  ##
  if (is.null(group_name)) {
    group_name = grouping_column
  }
  if (is.null(var_name)) {
    var_name = variable
  }
  p <- ggplot(
    df,
    aes(
      x = .data[[grouping_column]],
      y = .data[[variable]]
    )
  ) +
    geom_violin(alpha = 0.6) +
    theme_classic() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = -45, hjust = 0)
    ) +
    labs(
      x = group_name,
      y = var_name,
      title = var_name
    )
  if (log_scale) {
    p <- p +
      scale_y_log10() +
      labs(
        y = paste(var_name, "(log10)")
      )
  }
  if (show_outlier) {
    outlier_colours <- c("FALSE" = "lightgrey", "TRUE" = "orange")
    p <- p +
      geom_jitter(
        mapping = aes(colour = .data[[outlier_column]]),
        width = 0.05,
        size = 0.4,
        alpha = 0.5,
        show.legend = FALSE
      ) +
      scale_colour_manual(values = outlier_colours)
  }
  return(p)
}

#' Plot joint QC metrics
#'
#' Creates a joint hexbin plot of gene counts vs UMI counts per cell, with
#' marginal histograms. Useful for visualizing cell quality and detecting
#' outliers, doublets or empty droplets.
#'
#' @param df Dataframe. Input data table containing QC metrics.
#' @param library_size Character. Column containing library size information per cell
#' @param nb_features Character. Column containing information on number of features per cell
#' @param log_scale Boolean. If TRUE, will log-scale the data. (Default: FALSE)
#'
#' @return A \code{ggExtraPlot} object with a hexbin center plot and marginal
#'   histograms on log10 axes.
#' @keywords internal
plot_joint_sc <- function(
  df,
  library_size = "lib_size",
  nb_features = "nnz",
  log_scale = FALSE
) {
  checkmate::assertNames(
    colnames(df),
    must.include = c(library_size, nb_features)
  )

  if (log_scale) {
    df[["log10_lib_size"]] <- log10(df[[library_size]])
    df[["log10_nnz"]] <- log10(df[[nb_features]])
  }
  p <- ggplot(df, aes(x = log10_nnz, y = log10_lib_size)) +
    geom_point(alpha = 0) + # invisible, just satisfies ggMarginal
    geom_hex(bins = 80) +
    scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Blues")[4:9]) +
    theme_classic() +
    theme(legend.position = "none") +
    labs(x = "Genes per cell (log10)", y = "UMIs per cell (log10)")

  p <- ggExtra::ggMarginal(
    p,
    type = "histogram",
    fill = "steelblue3",
    color = "black",
    bins = 50
  )

  return(p)
}

## Bixverse SC generics

### generics -----------------------------------------------------------------------

#' Generic violin plot function
#'
#' @param x An object to plot.
#' @param ... Additional arguments passed to methods.
#'
#' @return A named list of ggplot objects.
#'
#' @export
#' @keywords internal
violin_plot_sc <- function(x, ...) {
  UseMethod("violin_plot_sc")
}

#' @export
violin_plot_sc.default <- function(x, ...) {
  stop(
    "No violin_plot_sc method for object of class: ",
    paste(class(x), collapse = ", ")
  )
}


#' Generic density plot function
#'
#' @param x An object to plot.
#' @param ... Additional arguments passed to methods.
#'
#' @return A named list of ggplot objects.
#'
#' @export
#' @keywords internal
density_plot_sc <- function(x, ...) {
  UseMethod("density_plot_sc")
}

#' @export
density_plot_sc.default <- function(x, ...) {
  stop(
    "No density_plot_sc method for object of class: ",
    paste(class(x), collapse = ", ")
  )
}


#' Generic joint plot function
#'
#' @param x An object to plot.
#' @param ... Additional arguments passed to methods.
#'
#' @return A named list of ggplot objects.
#'
#' @export
#' @keywords internal
joint_plot_sc <- function(x, ...) {
  UseMethod("joint_plot_sc")
}

#' @export
joint_plot_sc.default <- function(x, ...) {
  stop(
    "No joint_plot_sc method for object of class: ",
    paste(class(x), collapse = ", ")
  )
}

### plots -----------------------------------------------------------------------

#' Plot per-cell QC violin plots from a CellQc object
#'
#' @param x A `CellQc` object.
#' @param ... Ignored.
#'
#' @return A named list of ggplot objects, one per metric.
#'
#' @export
#'
#' @import ggplot2
#'
violin_plot_sc.CellQc <- function(
  x,
  grouping_column = "grp",
  variable,
  group_name = NULL,
  var_name = NULL,
  log_scale = FALSE,
  outlier_column = "global_outlier",
  show_outlier = TRUE,
  ...
) {
  outlier_colours <- c("FALSE" = "lightgrey", "TRUE" = "orange")
  plot_df <- get_data(x)

  plots <- purrr::map(names(x$metrics), function(metric) {
    p <- plot_violin_sc(
      df = plot_df,
      grouping_column = grouping_column,
      variable = metric,
      var_name = metric,
      log_scale = log_scale,
      outlier_column = outlier_column,
      show_outlier = show_outlier
    )
    p
  })
  setNames(plots, names(x$metrics))
}

#' Plot joint QC metrics
#'
#' Creates a joint hexbin plot of gene counts vs UMI counts per cell, with
#' marginal histograms. Useful for visualizing cell quality and detecting
#' outliers, doublets or empty droplets.
#'
#' @param df data.table Input data table containing QC metrics.
#' @param library_size Character. Column containing library size information per cell
#' @param nb_features Character. Column containing information on number of features per cell
#' @param log_scale Boolean. If TRUE, will log-scale the data. (Default: FALSE)
#'
#' @return A \code{ggExtraPlot} object with a hexbin center plot and marginal
#'   histograms on log10 axes.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' set.seed(42)
#' n_cells <- 500
#' df <- data.table(
#'   donor_id = sample(paste0("D", 1:5), n_cells, replace = TRUE),
#'   nnz      = rnbinom(n_cells, mu = 2000, size = 5)
#' )
#' # Simulate a low-quality donor
#' df[donor_id == "D5", nnz := rnbinom(.N, mu = 200, size = 5)]
#'
#' plot_violin_sc(
#'   df = df,
#'   grouping_column = "donor_id",
#'   variable = "nnz",
#'   var_name = "Number of non-zero genes",
#'   show_outlier = FALSE
#' )
#' }
violin_plot_sc.data.table <- function(
  df,
  grouping_column,
  variable,
  group_name = NULL,
  var_name = NULL,
  log_scale = TRUE,
  show_outlier = TRUE,
  outlier_column = "global_outlier",
  ...
) {
  plot_violin_sc(
    df = df,
    grouping_column = grouping_column,
    variable = variable,
    group_name = group_name,
    var_name = var_name,
    log_scale = log_scale,
    show_outlier = show_outlier,
    outlier_column = outlier_column
  )
}

#' Plot per-cell QC density plots from a CellQc object
#'
#' @param x A `CellQc` object.
#' @param ... Ignored.
#'
#' @return A named list of ggplot objects, one per metric.
#'
#' @export
#'
#' @import ggplot2
density_plot_sc.CellQc <- function(x, ...) {
  plot_df <- get_data(x)

  plots <- purrr::map(names(x$metrics), function(metric) {
    p <- plot_density_sc(
      df = plot_df,
      grouping_column = "grp",
      variable = metric,
      var_name = metric,
      log_scale = FALSE
    )
    p
  })
  setNames(plots, names(x$metrics))
}

#' Plot per-cell QC density plots from a data.table
#'
#' @param df data.table. Input data table containing QC metrics.
#' @param grouping_column Character. Name of the column used to group density plot
#' @param variable Character. Name of the numeric QC column to plot on the x-axis.
#' @param group_name Character. Label for the x-axis (default: NULL).
#' @param var_name Character. Label for the y-axis (default: NULL).
#' @param log_scale Boolean. Log scale (default: TRUE)
#' @param show_outlier Boolean. Show the cells that are identified as outliers (default: TRUE)
#' @param outlier_column Character. Which column contains the cells identified as outliers
#'
#'
#' @return A named list of ggplot objects, one per metric.
#'
#' @export
#'
#' @import ggplot2
#'
#'
#' @examples
#' \dontrun{
#'
#' set.seed(42)
#' n_cells <- 500
#' df <- data.table(
#'   donor_id = sample(paste0("D", 1:5), n_cells, replace = TRUE),
#'   nnz      = rnbinom(n_cells, mu = 2000, size = 5)
#' )
#' # Simulate a low-quality donor
#' df[donor_id == "D5", nnz := rnbinom(.N, mu = 200, size = 5)]
#'
#' plot_density_sc(
#'   df = df,
#'   grouping_column = "donor_id",
#'   variable = "nnz",
#'   var_name = "Number of non-zero genes"
#' )
#' }
density_plot_sc.data.table <- function(
  df,
  grouping_column,
  variable,
  var_name = NULL,
  nmads = 3,
  log_scale = TRUE,
  adjust_position_label = 0
) {
  plot_density_sc(
    df = df,
    grouping_column = "grp",
    variable = variable,
    var_name = var_name,
    nmads = 3,
    log_scale = log_scale,
    adjust_position_label = adjust_position_label
  )
}


#' Joint QC plots from a CellQc object
#'
#' @param x A `CellQc` object.
#' @param ... Ignored.
#'
#' @return A named list of ggplot objects, one per metric.
#'
#' @export
#'
#' @import ggplot2
#'
joint_plot_sc.CellQc <- function(x, ...) {
  plot_df <- get_data(x)

  plot_joint_sc(
    df = plot_df,
    library_size = "log10_lib_size",
    nb_features = "log10_nnz",
    log_scale = FALSE
  )
}

#' Joint QC plots from a data.table object
#'
#' Creates a joint hexbin plot of gene counts vs UMI counts per cell, with
#' marginal histograms. Useful for visualizing cell quality and detecting
#' outliers, doublets or empty droplets.
#'
#' @param df data.table. Input data table containing QC metrics.
#' @param library_size Character. Column containing library size information per cell
#' @param nb_features Character. Column containing information on number of features per cell
#' @param log_scale Boolean. If TRUE, will log-scale the data. (Default: FALSE)
#'
#' @return A \code{ggExtraPlot} object with a hexbin center plot and marginal
#'   histograms on log10 axes.
#'
#' @export
#'
#' @import ggplot2
#'
#'
#' @examples
#' \dontrun{
#'
#' set.seed(42)
#' n_cells <- 500
#' df <- data.table(
#'   donor_id = sample(paste0("D", 1:5), n_cells, replace = TRUE),
#'   nnz      = rnbinom(n_cells, mu = 2000, size = 5),
#'   lib_size      = rnbinom(n_cells, mu = 10000, size = 100),
#' )
#' joint_plot_sc(
#'   df = df,
#'   library_size = "lib_size",
#'   nb_features = "nnz",
#'   log_scale = TRUE
#' )
#' }
joint_plot_sc.data.table <- function(
  df,
  library_size = "lib_size",
  nb_features = "nnz",
  log_scale = FALSE,
  ...
) {
  plot_joint_sc(
    df = df,
    library_size = library_size,
    nb_features = nb_features,
    log_scale = log_scale
  )
}
