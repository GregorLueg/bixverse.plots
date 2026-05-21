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
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @keywords internal
.plot_embedding <- function(
  df,
  colour,
  facet = NULL,
  embedding = NULL,
  point_size = 0.5
) {
  checkmate::assertDataTable(df)
  checkmate::qassert(colour, "S1")
  checkmate::qassert(facet, c("0", "S1"))
  checkmate::qassert(point_size, "N1")
  checkmate::assertNames(names(df), must.include = c("dim_1", "dim_2", colour))

  discrete <- is.factor(df[[colour]]) ||
    is.character(df[[colour]]) ||
    is.logical(df[[colour]])

  p <- ggplot(df, aes(x = dim_1, y = dim_2)) +
    geom_point(aes(colour = .data[[colour]]), size = point_size) +
    theme_bw()

  p <- p +
    if (discrete) scale_colour_viridis_d() else scale_colour_viridis_c()

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
    scale_colour_viridis_c() +
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
    scale_fill_viridis_d() +
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
#' @param point_size Numeric. Point size (default: 0.5).
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
  point_size = 0.5,
  ...
) {
  checkmate::qassert(colour_by, "S1")
  checkmate::qassert(discrete, c("0", "B1"))

  dt <- bixverse::extract_embedding_data(
    object,
    embedding = embedding,
    obs_cols = colour_by,
    ...
  )

  if (isTRUE(discrete)) {
    dt[, (colour_by) := as.factor(get(colour_by))]
  }

  .plot_embedding(
    df = dt,
    colour = colour_by,
    embedding = embedding,
    point_size = point_size
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
#' @param point_size Numeric. Point size (default: 0.3).
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
  point_size = 0.3,
  ...
) {
  modality <- match.arg(modality)

  dt <- bixverse::extract_feature_plot_data(
    object,
    features = features,
    embedding = embedding,
    scale = scale,
    clip = clip,
    modality = modality,
    ...
  )

  if (!is.null(feature_labels)) {
    present <- intersect(features, levels(dt$gene))
    dt[,
      gene := factor(
        feature_labels[as.character(gene)],
        levels = feature_labels[present]
      )
    ]
  }

  .plot_embedding(
    df = dt,
    colour = "expression",
    facet = "gene",
    embedding = embedding,
    point_size = point_size
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
