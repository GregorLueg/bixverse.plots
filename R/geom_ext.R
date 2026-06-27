# geom extensions --------------------------------------------------------------

## centroid labelling ----------------------------------------------------------

#' Label Centroids in Scatter Plots
#'
#' Adds boxed text labels at the centroid position of each group in a scatter
#' plot. Computes group centroids with data.table for efficient summarisation.
#' Useful for labelling cluster centres in embedding or dimensionality
#' reduction plots (UMAP, t-SNE, PCA).
#'
#' @param data A \code{data.frame} or \code{data.table} containing the scatter
#' plot points. If \code{NULL}, the data from the parent ggplot is used. Must
#' contain the columns referenced by the parent plot's x/y aesthetics and by
#' \code{label_by}.
#' @param label_by Character. Name of the (discrete) column to group and label
#' by.
#' @param colour Text colour. Default: \code{"black"}.
#' @param fill Box fill colour. Default: \code{"white"}.
#' @param alpha Box fill transparency in \code{[0, 1]}. Default: \code{0.5}.
#' @param label.size Box border line width in mm. Set to \code{0} to hide the
#' border. Default: \code{0}.
#' @param size Text size in mm. Default: \code{4}.
#' @param fontface Font face. Default: \code{"bold"}.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{geom_label}}.
#'
#' @return A ggplot layer.
#'
#' @examples
#' \dontrun{
#' embedding_plot_sc(
#'   object = sc_object,
#'   embedding = "umap",
#'   colour_by = "donor_id"
#' ) +
#'   label_centroids(label_by = "donor_id")
#' }
#'
#' @importFrom rlang .data as_name
#' @importFrom ggplot2 update_ggplot class_ggplot aes geom_label
#' @importFrom S7 method "method<-" new_S3_class
#'
#' @export
label_centroids <- function(
  data = NULL,
  label_by,
  colour = "black",
  fill = "white",
  alpha = 0.5,
  label.size = 0,
  size = 4,
  fontface = "bold",
  ...
) {
  structure(
    list(
      data = data,
      label_by = label_by,
      colour = colour,
      fill = fill,
      alpha = alpha,
      label.size = label.size,
      size = size,
      fontface = fontface,
      extra = list(...)
    ),
    class = "label_centroids"
  )
}

#' @export
method(
  update_ggplot,
  list(new_S3_class("label_centroids"), class_ggplot)
) <- function(object, plot, ...) {
  ## resolve data
  if (is.null(object$data) && length(plot@data) == 0) {
    stop(
      paste(
        "label_centroids(): could not identify data object,",
        "please provide either a data frame or",
        "pass data directly in ggplot(data = df)"
      )
    )
  }
  dt <- data.table::as.data.table(
    if (is.null(object$data)) plot@data else object$data
  )

  ## resolve x/y from parent plot mapping, fall back to layer mappings
  x_mapping <- plot@mapping$x
  y_mapping <- plot@mapping$y
  for (layer in plot@layers) {
    if (!is.null(x_mapping) && !is.null(y_mapping)) {
      break
    }
    x_mapping <- x_mapping %||% layer$mapping$x
    y_mapping <- y_mapping %||% layer$mapping$y
  }
  if (is.null(x_mapping) || is.null(y_mapping)) {
    stop(
      paste(
        "label_centroids(): x/y aesthetics not found in plot or layer mappings.",
        "Set them via ggplot(aes(x = ..., y = ...)) or geom_*(aes(x = ..., y = ...))."
      )
    )
  }
  x_var <- as_name(x_mapping)
  y_var <- as_name(y_mapping)

  ## checks
  checkmate::assertNames(
    colnames(dt),
    must.include = c(object$label_by, x_var, y_var)
  )
  if (is.numeric(dt[[object$label_by]])) {
    stop("label_centroids(): label_by is continuous.")
  }

  ## centroids
  centroids <- dt[,
    .(
      x_centroid = mean(get(x_var), na.rm = TRUE),
      y_centroid = mean(get(y_var), na.rm = TRUE)
    ),
    by = c(object$label_by)
  ]
  data.table::setnames(
    centroids,
    c(object$label_by, "x_centroid", "y_centroid"),
    c("label", "x", "y")
  )

  ## layer
  layer <- do.call(
    geom_label,
    c(
      list(
        data = centroids,
        mapping = aes(x = .data$x, y = .data$y, label = .data$label),
        colour = object$colour,
        fill = object$fill,
        alpha = object$alpha,
        label.size = object$label.size,
        size = object$size,
        fontface = object$fontface,
        inherit.aes = FALSE
      ),
      object$extra
    )
  )

  plot + layer
}
