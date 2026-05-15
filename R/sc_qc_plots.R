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
#' plot_qc_density(
#'   df = df,
#'   grouping_column = "donor_id",
#'   variable = "nnz",
#'   var_name = "Number of non-zero genes"
#' )
#' }
#'
#' @export
plot_qc_density <- function(
  df,
  grouping_column,
  variable,
  var_name = NULL,
  nmads = 3,
  log_scale = TRUE
) {
  ## checkmates
  checkmate::assertDataTable(df)
  checkmate::assert(all(c(grouping_column, variable) %in% colnames(df)))
  checkmate::assertLogical(log_scale)
  checkmate::assert(
    checkmate::checkCharacter(group_name),
    checkmate::checkNull(group_name)
  )

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
  mad_nnz <- mad(median_by_donor$median_var)
  ##
  outliers_nnz <- median_var - nmads * mad_nnz
  median_by_donor <- median_by_donor[,
    nnz_outlier := median_var <= outliers_nnz
  ]
  outlier_donors <- median_by_donor[(nnz_outlier)]

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
  ## For the log scale, we need to probably adjust the position of the label to be readible
  ## We'll add 10% quantile of the density distribution for readibility
  pos_label_x <- quantile(density(df[[variable]])$x, probs = 0.1)
  ## X label
  if (is.null(group_name)) {
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
          x = median_var + pos_label_x,
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
        y = "Density"
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
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
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
#' plot_qc_violin(
#'   df = df,
#'   grouping_column = "donor_id",
#'   variable = "nnz",
#'   var_name = "# Features"
#' )
#' }
#'
#' @export
plot_qc_violin <- function(
  df,
  grouping_column,
  variable,
  group_name = NULL,
  var_name = NULL,
  log_scale = TRUE
) {
  ## checkmates
  checkmate::assertDataTable(df)
  checkmate::assert(all(c(grouping_column, variable) %in% colnames(df)))
  checkmate::assertLogical(log_scale)
  checkmate::assert(
    checkmate::checkCharacter(var_name),
    checkmate::checkNull(var_name)
  )
  checkmate::assert(
    checkmate::checkCharacter(group_name),
    checkmate::checkNull(group_name)
  )

  if (is.null(group_name)) {
    group_name = grouping_column
  }
  if (is.null(var_name)) {
    var_name = variable
  }
  p <- ggplot(
    toplot,
    aes(
      x = .data[[grouping_column]],
      y = .data[[variable]],
      fill = .data[[grouping_column]]
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
      y = var_name
    )
  if (log_scale) {
    p <- p +
      scale_y_log10() +
      labs(
        y = paste(var_name, "(log10)")
      )
  }
  return(p)
}

#' Plot joint QC metrics
#'
#' Creates a joint hexbin plot of gene counts vs UMI counts per cell, with
#' marginal histograms. Useful for visualizing cell quality and detecting
#' outliers, doublets or empty droplets.
#'
#' @param df A data frame with columns \code{nnz} (genes per cell) and
#'   \code{lib_size} (UMIs per cell).
#'
#' @return A \code{ggExtraPlot} object with a hexbin center plot and marginal
#'   histograms on log10 axes.
#'
#' @export
plot_joined_qc <- function(df) {
  checkmate::assertNames(colnames(df), must.include = c("nnz", "lib_size"))

  p <- ggplot(toplot, aes(x = log10(nnz), y = log10(lib_size))) +
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
