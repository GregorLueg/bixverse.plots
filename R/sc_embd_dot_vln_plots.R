# dot and feature plots --------------------------------------------------------

## helpers ---------------------------------------------------------------------

#' Scatter plot worker for embeddings
#'
#' @param df data.table. Must contain `dim_1`, `dim_2` and `colour`.
#' @param colour Character. Column to colour by. A factor/character/logical
#' column yields a discrete scale, a numeric column a continuous one.
#' @param facet Character. Optional column to facet by (default: NULL).
#' @param embedding Character. Embedding name for axis labels (default: NULL).
#' @param point_size Numeric. Point size (default: 0.5).
#' @param point_alpha Numeric. Alpha parameter (default: 0.5).
#' @param raster Boolean. Shall [scattermore::geom_scattermore()] be used.
#' @param raster_dpi Two numerics. Pixel resolution for rasterized plots, passed
#' to geom_scattermore(). Default is `c(512, 512)`.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @keywords internal
.plot_embedding <- function(
  df,
  colour,
  facet = NULL,
  embedding = NULL,
  point_size = 0.3,
  point_alpha = 0.5,
  raster = FALSE,
  raster_dpi = c(512, 512),
  highlight = FALSE,
  highlight_quantile = 0.25
) {
  checkmate::assertDataTable(df)
  checkmate::qassert(colour, "S1")
  checkmate::qassert(facet, c("0", "S1"))
  checkmate::qassert(point_size, "N1")
  checkmate::qassert(point_alpha, "N1")
  checkmate::qassert(raster, "B1")
  checkmate::qassert(raster_dpi, "N2")
  checkmate::qassert(highlight, "B1")
  checkmate::assertNames(names(df), must.include = c("dim_1", "dim_2", colour))
  checkmate::qassert(highlight, "B1")
  checkmate::qassert(highlight_quantile, "N[0,1]")

  discrete <- is.factor(df[[colour]]) ||
    is.character(df[[colour]]) ||
    is.logical(df[[colour]])

  n_cells <- length(unique(df$cell_id))

  if (highlight && !discrete) {
    # path to strongly highlight rare genes
    threshold <- quantile(
      df[[colour]],
      probs = highlight_quantile,
      na.rm = TRUE
    )
    bg <- df[df[[colour]] <= threshold, ]
    fg <- df[df[[colour]] > threshold, ]

    if (raster) {
      p <- ggplot() +
        scattermore::geom_scattermore(
          data = bg,
          aes(x = dim_1, y = dim_2),
          colour = "lightgrey",
          pointsize = point_size,
          pixels = raster_dpi
        ) +
        geom_point(
          data = fg,
          aes(x = dim_1, y = dim_2, colour = .data[[colour]]),
          # use the auto point detection here...
          size = auto_point_size(n_samples = n_cells, raster = FALSE) * 2,
          alpha = point_alpha
        ) +
        theme_bw()
    } else {
      p <- ggplot() +
        geom_point(
          data = bg,
          aes(x = dim_1, y = dim_2),
          colour = "lightgrey",
          size = point_size,
          alpha = point_alpha
        ) +
        geom_point(
          data = fg,
          aes(x = dim_1, y = dim_2, colour = .data[[colour]]),
          size = point_size + 1,
          alpha = point_alpha
        ) +
        theme_bw()
    }

    p <- p + bixverse.plots:::scale_colour_single_cell(discrete = FALSE)
  } else {
    p <- ggplot(df, aes(x = dim_1, y = dim_2))

    if (raster) {
      p <- p +
        scattermore::geom_scattermore(
          mapping = aes(colour = .data[[colour]]),
          pointsize = point_size,
          pixels = raster_dpi
        ) +
        theme_bw()
    } else {
      p <- p +
        geom_point(
          aes(colour = .data[[colour]]),
          size = point_size,
          alpha = point_alpha
        ) +
        theme_bw()
    }

    p <- p + bixverse.plots:::scale_colour_single_cell(discrete = discrete)
  }

  if (!is.null(facet)) {
    p <- p + facet_wrap(stats::as.formula(paste("~", facet)))
  }

  labels <- if (is.null(embedding)) {
    c("dim 1", "dim 2")
  } else {
    sprintf("%s %i", embedding, 1:2)
  }

  p + labs(x = labels[1], y = labels[2], colour = colour)
}

#' Dot plot worker
#'
#' @param df data.table. Must contain `gene`, `group`, `pct_exp`, `scaled_exp`.
#' @param feature_labels Optional named character vector mapping gene ids to
#' display labels (default: NULL).
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @keywords internal
.plot_dotplot <- function(df, feature_labels = NULL) {
  checkmate::assertDataTable(df)
  checkmate::assertNames(
    names(df),
    must.include = c("gene", "group", "pct_exp", "scaled_exp")
  )

  df <- data.table::copy(df)
  gene_levels <- levels(df$gene)

  if (!is.null(feature_labels)) {
    df[,
      gene := factor(
        feature_labels[as.character(gene)],
        levels = feature_labels[gene_levels]
      )
    ]
  }

  # reverse so the first feature sits at the top of the y-axis
  df[, gene := factor(gene, levels = rev(levels(gene)))]

  ggplot(df, aes(x = group, y = gene)) +
    geom_point(aes(size = pct_exp, colour = scaled_exp)) +
    scale_colour_single_cell(discrete = FALSE) +
    scale_size_continuous(range = c(0, 6)) +
    theme_bw() +
    labs(
      size = "% expressed",
      colour = "Scaled\nexpression",
      x = "Group",
      y = ""
    )
}

#' Stacked violin plot worker
#'
#' @param df data.table. Must contain `group`, `gene`, `expression`. `gene` is
#' expected to be an ordered factor; the first level sits at the top.
#' @param feature_labels Optional named character vector mapping gene ids to
#' display labels (default: NULL).
#' @param scale_y Character. `geom_violin` scaling, passed as `scale`
#' (default: "width").
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @keywords internal
.plot_stacked_violin <- function(df, feature_labels = NULL, scale_y = "width") {
  checkmate::assertDataTable(df)
  checkmate::assertNames(
    names(df),
    must.include = c("group", "gene", "expression")
  )
  checkmate::qassert(scale_y, "S1")

  df <- data.table::copy(df)
  gene_levels <- levels(df$gene)

  if (!is.null(feature_labels)) {
    df[,
      gene := factor(
        feature_labels[as.character(gene)],
        levels = feature_labels[gene_levels]
      )
    ]
  }

  ggplot(df, aes(x = group, y = expression, fill = group)) +
    geom_violin(scale = scale_y, alpha = 0.8, linewidth = 0.2) +
    scale_fill_single_cell(discrete = TRUE) +
    facet_grid(gene ~ ., scales = "free_y", switch = "y") +
    theme_bw() +
    theme(
      legend.position = "none",
      strip.text.y.left = element_text(angle = 0),
      strip.background = element_blank(),
      axis.text.x = element_text(angle = -45, hjust = 0),
      panel.spacing = unit(0, "lines")
    ) +
    labs(x = "Group", y = "Expression")
}

## plot functions --------------------------------------------------------------

### embedding with obs ---------------------------------------------------------

#' Embedding plot coloured by an obs column
#'
#' @param object A single cell class.
#' @param embedding String. Name of the embedding (e.g. `"umap"`).
#' @param colour_by String. Obs column to colour by.
#' @param discrete Optional boolean. Force a discrete scale by coercing
#' `colour_by` to a factor. `NULL` (default) picks the scale from the column
#' type.
#' @param point_size Optional numeric. Defines the point size. If not provided,
#' will be auto-determined.
#' @param raster Optional boolean. Shall the plot be rasterised. If `NULL` and
#' number of cells is larger than `1e5`, defaults to TRUE.
#' @param raster_dpi Two numerics. Pixel resolution for rasterized plots, passed
#' to geom_scattermore(). Default is `c(512, 512)`.
#' @param ... Additional arguments forwarded to
#' [bixverse::extract_embedding_data()].
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @export
#' @import ggplot2
embedding_plot_sc <- function(
  object,
  embedding,
  colour_by,
  discrete = NULL,
  point_size = NULL,
  raster = NULL,
  raster_dpi = c(512, 512),
  ...
) {
  checkmate::qassert(colour_by, "S1")
  checkmate::qassert(discrete, c("0", "B1"))
  checkmate::qassert(raster, c("0", "B1"))
  checkmate::qassert(raster_dpi, c("N2"))
  checkmate::qassert(point_size, c("N1", "0"))

  dt <- bixverse::extract_embedding_data(
    object,
    embedding = embedding,
    obs_cols = colour_by,
    ...
  )

  if (isTRUE(discrete)) {
    dt[, (colour_by) := as.factor(get(colour_by))]
  }

  n_cells <- length(unique(dt$cell_id))

  raster <- raster %||% (n_cells > 1e5)
  point_size <- point_size %||%
    auto_point_size(n_samples = n_cells, raster = raster)

  if (raster) {
    message(paste(
      "Raster was set to TRUE or n_cells > 1e5 -> Rasterising the plot"
    ))
  }

  .plot_embedding(
    df = dt,
    colour = colour_by,
    embedding = embedding,
    point_size = point_size,
    raster = raster,
    raster_dpi = raster_dpi
  )
}

### embedding with feature -----------------------------------------------------

#' Faceted feature plot over an embedding
#'
#' @param object A single cell class.
#' @param features Character vector. Gene IDs to plot.
#' @param embedding String. Name of the embedding.
#' @param feature_labels Optional named character vector mapping gene ids to
#' display labels (default: NULL).
#' @param scale Boolean. Whether to z-score the expression values.
#' @param clip Optional numeric. Clip z-scores if `scale = TRUE`.
#' @param modality String. One of `c("rna", "adt")`.
#' @param point_size Optional numeric. Defines the point size. If not provided,
#' will be auto-determined.
#' @param raster Optional boolean. Shall the plot be rasterised. If `NULL` and
#' number of cells is larger than `1e5`, defaults to TRUE.
#' @param raster_dpi Two numerics. Pixel resolution for rasterized plots, passed
#' to geom_scattermore(). Default is `c(512, 512)`.
#' @param highlight_features Boolean. Shall the features be more strongly
#' highlighted. Useful for sparsely expressed genes.
#' @param highlight_quantile Numeric between `[0, 1]`. Defines the threshold.
#' @param ... Additional arguments forwarded to
#' [bixverse::extract_embedding_data()].
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @export
#' @import ggplot2
feature_plot_sc <- function(
  object,
  features,
  embedding,
  feature_labels = NULL,
  scale = FALSE,
  clip = NULL,
  modality = c("rna", "adt"),
  point_size = NULL,
  raster = NULL,
  raster_dpi = c(512, 512),
  highlight_features = FALSE,
  highlight_quantile = 0.25,
  ...
) {
  modality <- match.arg(modality)

  checkmate::qassert(raster, c("0", "B1"))
  checkmate::qassert(raster_dpi, c("N2"))
  checkmate::qassert(point_size, c("N1", "0"))
  checkmate::qassert(highlight_features, "B1")
  checkmate::qassert(highlight_quantile, "N1[0,1]")

  dt <- bixverse::extract_feature_plot_data(
    object,
    features = features,
    embedding = embedding,
    scale = scale,
    clip = clip,
    modality = modality,
    ...
  )

  data.table::setorder(dt, expression)

  if (!is.null(feature_labels)) {
    present <- intersect(features, levels(dt$gene))
    dt[,
      gene := factor(
        feature_labels[as.character(gene)],
        levels = feature_labels[present]
      )
    ]
  }

  n_cells <- length(unique(dt$cell_id))

  raster <- raster %||% (n_cells > 1e5)
  point_size <- point_size %||%
    auto_point_size(n_samples = n_cells, raster = raster)

  if (raster) {
    message(paste(
      "Raster was set to TRUE or n_cells > 1e5 -> Rasterising the plot"
    ))
  }

  .plot_embedding(
    df = dt,
    colour = "expression",
    facet = "gene",
    embedding = embedding,
    point_size = point_size,
    raster = raster,
    raster_dpi = raster_dpi,
    highlight = highlight_features
  )
}

### dot plot -------------------------------------------------------------------

#' Dot plot of marker gene expression across groups
#'
#' @param object A single cell class.
#' @param features Character vector. Gene IDs to plot.
#' @param grouping_variable String. Obs column to group by.
#' @param feature_labels Optional named character vector mapping gene ids to
#' display labels (default: NULL).
#' @param scale_exp Boolean. Whether to min-max scale mean expression per gene.
#' @param modality String. One of `c("rna", "adt")`.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @export
#' @import ggplot2
dot_plot_sc <- function(
  object,
  features,
  grouping_variable,
  feature_labels = NULL,
  scale_exp = TRUE,
  modality = c("rna", "adt")
) {
  modality <- match.arg(modality)

  dt <- bixverse::extract_dot_plot_data(
    object,
    features = features,
    grouping_variable = grouping_variable,
    scale_exp = scale_exp,
    modality = modality
  )

  .plot_dotplot(df = dt, feature_labels = feature_labels)
}

### stacked vln plot -----------------------------------------------------------

#' Stacked violin plot of gene expression across groups
#'
#' @param object A single cell class.
#' @param features Character vector. Gene IDs to plot, one row each.
#' @param grouping_variable String. Obs column to group by.
#' @param feature_labels Optional named character vector mapping gene ids to
#' display labels (default: NULL).
#' @param scale Boolean. Whether to z-score the expression values.
#' @param clip Optional numeric. Clip z-scores if `scale = TRUE`.
#' @param modality String. One of `c("rna", "adt")`.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @export
#' @import ggplot2
stacked_violin_plot_sc <- function(
  object,
  features,
  grouping_variable,
  feature_labels = NULL,
  scale = FALSE,
  clip = NULL,
  modality = c("rna", "adt")
) {
  modality <- match.arg(modality)

  dt <- bixverse::extract_gene_violin_data(
    object,
    features = features,
    grouping_variable = grouping_variable,
    scale = scale,
    clip = clip,
    modality = modality
  )

  .plot_stacked_violin(df = dt, feature_labels = feature_labels)
}
