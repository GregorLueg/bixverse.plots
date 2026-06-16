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
          pixels = raster_dpi,
          alpha = point_alpha
        ) +
        geom_point(
          data = fg,
          aes(x = dim_1, y = dim_2, colour = .data[[colour]]),
          # use the auto point detection here...
          size = auto_point_size(n_samples = n_cells, raster = FALSE) * 2
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
          size = point_size + 1
        ) +
        theme_bw()
    }

    p <- p + scale_color_bx_c()
  } else {
    p <- ggplot(df, aes(x = dim_1, y = dim_2))

    if (raster) {
      p <- p +
        scattermore::geom_scattermore(
          mapping = aes(colour = .data[[colour]]),
          pointsize = point_size,
          pixels = raster_dpi,
          alpha = point_alpha
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

    if (discrete) {
      p <- p + scale_color_bx()
    } else {
      p <- p + scale_color_bx_c()
    }
  }

  if (!is.null(facet)) {
    p <- p + facet_wrap(stats::as.formula(paste("~", facet)))
  }

  labels <- if (is.null(embedding)) {
    c("dim 1", "dim 2")
  } else {
    sprintf("%s %i", embedding, 1:2)
  }

  p <- p + labs(x = labels[1], y = labels[2], colour = colour)
}

#' Dot plot worker
#'
#' @param df data.table. Must contain `gene`, `group`, `pct_exp`, `scaled_exp`.
#' @param feature_labels Optional named character vector mapping gene ids to
#' display labels (default: NULL).
#' @param feature_grouping Optional named character vector mapping gene ids to
#' grouping labels, e.g. cell type labels. If feature_labels is provided,
#' the character vectors should contain the mapping of feature display labels to
#' their respecitve groups (e.g. c(CD3E = "T cell", CD8A = "T cell",
#' MS4A1 = "B cell", ...). (default: NULL).
#' @param cluster_groups Boolean. Use hierarchical clustering on the grouping variable
#' to re-order the group labels based on expression similarity.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @keywords internal
.plot_dotplot <- function(
  df,
  feature_labels = NULL,
  feature_grouping = NULL,
  cluster_groups = TRUE
) {
  ## Checkmate
  checkmate::assertDataTable(df)
  checkmate::assertFlag(cluster_groups)
  checkmate::assertNames(
    names(df),
    must.include = c("gene", "group", "pct_exp", "scaled_exp")
  )
  if (!is.null(feature_grouping)) {
    checkmate::assertCharacter(feature_grouping)
  }
  if (!is.null(feature_labels)) {
    ## does this always return a factor
    checkmate::assertNames(
      as.character(unique(df$gene)),
      subset.of = names(feature_labels)
    )
  }
  if (is.null(feature_labels) & !is.null(feature_grouping)) {
    checkmate::assertNames(
      as.character(unique(df$gene)),
      subset.of = names(feature_grouping)
    )
  } else if (!is.null(feature_labels) & !is.null(feature_grouping)) {
    checkmate::assertNames(
      as.character(unique(feature_labels)),
      subset.of = names(feature_grouping)
    )
  }

  df <- data.table::copy(df)
  gene_levels <- levels(df$gene)

  # Optional feature-label remapping
  if (!is.null(feature_labels)) {
    df[,
      gene := factor(
        feature_labels[as.character(gene)],
        levels = feature_labels[gene_levels]
      )
    ]
    gene_levels <- levels(df$gene)
  }

  # Hierarchical clustering of groups on their expression profiles
  if (cluster_groups) {
    wide <- data.table::dcast(
      df,
      group ~ gene,
      value.var = "scaled_exp",
      fill = 0
    )
    mat <- as.matrix(wide[, -1L, with = FALSE])
    rownames(mat) <- as.character(wide$group)

    hc <- stats::hclust(stats::dist(mat))
    df[, group := factor(as.character(group), levels = hc$labels[hc$order])]
  }

  # Reverse gene order so the first feature sits at the top
  df[, gene := factor(gene, levels = rev(gene_levels))]

  # Add cell marker group labels
  if (!is.null(feature_grouping)) {
    facet_levels <- unique(feature_grouping[rev(gene_levels)]) # respect display order
    facet_levels <- facet_levels[!is.na(facet_levels)]
    df[,
      .gene_group := factor(
        feature_grouping[as.character(gene)],
        levels = facet_levels
      )
    ]
  }

  # Base plot
  p <- ggplot(df, aes(x = group, y = gene)) +
    geom_point(aes(size = pct_exp, colour = scaled_exp)) +
    scale_color_bx_c() +
    scale_size_continuous(range = c(0, 3)) +
    theme_bx(base_size = 10) +
    labs(
      size = "% expressed",
      colour = "Scaled\nexpression",
      x = "Group",
      y = ""
    )
  # Add optional cell type grouping
  if (!is.null(feature_grouping)) {
    p <- p +
      facet_grid(.gene_group ~ ., scales = "free_y", space = "free_y") +
      theme(strip.text.y = element_text(angle = 0, hjust = 0))
  }
  p
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
    geom_violin(scale = scale_y, alpha = 0.5, linewidth = 0.2) +
    scale_fill_bx() +
    facet_grid(gene ~ ., scales = "free_y", switch = "y") +
    theme_bx() +
    theme(
      legend.position = "none",
      strip.text.y.left = element_text(
        size = 9,
        margin = margin(t = 5, b = 5, l = 5, r = 5)
      ),
      strip.background = element_rect(fill = "white", color = "grey80"),
      axis.text.x = element_text(angle = -45, hjust = 0),
      panel.spacing = unit(1, "lines")
    ) +
    labs(x = "", y = "Expression")
}

#' Compute per-point 2D kernel density
#'
#' @param x Numeric. X coordinates.
#' @param y Numeric. Y coordinates.
#' @param smoothness Numeric. Bandwidth multiplier of the per-axis standard
#' deviation, passed to [MASS::kde2d()].
#'
#' @return Numeric vector of density values, one per `(x, y)` pair.
#'
#' @keywords internal
.compute_point_density <- function(x, y, smoothness = 10) {
  checkmate::qassert(x, "N+")
  checkmate::qassert(y, "N+")
  checkmate::assertTRUE(length(x) == length(y))
  checkmate::qassert(smoothness, "N1")

  coords <- cbind(x, y)
  dens <- MASS::kde2d(
    coords[, 1],
    coords[, 2],
    n = 100,
    h = c(sd(coords[, 1]) * smoothness, sd(coords[, 2]) * smoothness)
  )
  fields::interp.surface(dens, coords)
}

#' Feature-pair scatter / hex plot worker
#'
#' @param df data.table. Must contain `feature_1` and `feature_2`.
#' @param features Optional length-2 character vector with axis labels
#' (default: NULL, falls back to column names).
#' @param geom Character. One of `c("density", "hex")`. `"density"` colours
#' each point by 2D KDE; `"hex"` bins points into hexagons.
#' @param smoothness Numeric. Bandwidth multiplier for the KDE
#' (default: 10). Only used for `geom = "density"`.
#' @param bins Numeric. Number of hex bins (default: 60). Only used for
#' `geom = "hex"`.
#' @param point_size Numeric. Point size for density plots (default: 2.5).
#' @param point_alpha Numeric. Alpha for density plots (default: 0.5).
#' @param raster Boolean. Use [scattermore::geom_scattermore()] for the
#' density variant.
#' @param raster_dpi Two numerics. Pixel resolution for rasterised plots.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @keywords internal
.plot_feature_pair <- function(
  df,
  features = NULL,
  geom = c("density", "hex"),
  smoothness = 10,
  bins = 60,
  point_size = 2.5,
  point_alpha = 0.5,
  raster = FALSE,
  raster_dpi = c(512, 512)
) {
  geom <- match.arg(geom)

  checkmate::assertDataTable(df)
  checkmate::assertNames(names(df), must.include = c("feature_1", "feature_2"))
  checkmate::qassert(features, c("0", "S2"))
  checkmate::qassert(smoothness, "N1")
  checkmate::qassert(bins, "N1")
  checkmate::qassert(point_size, "N1")
  checkmate::qassert(point_alpha, "N1")
  checkmate::qassert(raster, "B1")
  checkmate::qassert(raster_dpi, "N2")

  labels <- features %||% c("feature_1", "feature_2")

  if (geom == "density") {
    df <- data.table::copy(df)
    df[,
      density := .compute_point_density(feature_1, feature_2, smoothness)
    ]

    if (raster) {
      p <- ggplot(df, aes(x = feature_1, y = feature_2)) +
        scattermore::geom_scattermore(
          mapping = aes(colour = density),
          pointsize = point_size,
          pixels = raster_dpi,
          alpha = point_alpha
        ) +
        scale_fill_bx_c(palette = "viridis") +
        labs(colour = "Density")
    } else {
      p <- ggplot(df, aes(x = feature_1, y = feature_2)) +
        geom_point(
          aes(fill = density),
          size = point_size,
          shape = 21,
          stroke = 0,
          alpha = point_alpha
        ) +
        scale_fill_bx_c(palette = "viridis") +
        labs(fill = "Density")
    }
  } else {
    p <- ggplot(df, aes(x = feature_1, y = feature_2)) +
      geom_hex(bins = bins) +
      scale_fill_bx_c(palette = "viridis") +
      labs(fill = "Count")
  }

  p +
    theme_bw() +
    labs(x = labels[1], y = labels[2])
}

## plot functions --------------------------------------------------------------

### embedding with obs ---------------------------------------------------------

#' Embedding plot coloured by an obs column
#'
#' @param object A single cell class.
#' @param embedding String. Name of the embedding (e.g. `"umap"`).
#' @param colour_by String. Obs column to colour by.
#' @param label_by String. Optional obs column to label by. (default: NULL).
#' @param discrete Optional boolean. Force a discrete scale by coercing
#' `colour_by` to a factor. `NULL` (default) picks the scale from the column
#' type.
#' @param embd_modality String. Modality the embedding is pulled from. One of
#' `c("rna", "adt", "wnn")`. Use `"wnn"` for WNN-derived embeddings.
#' @param point_size Optional numeric. Defines the point size. If not provided,
#' will be auto-determined.
#' @param point_alpha Numeric. Defines the alpha.
#' @param raster Optional boolean. Shall the plot be rasterised. If `NULL` and
#' number of cells is larger than `1e5`, defaults to TRUE.
#' @param raster_dpi Two numerics. Pixel resolution for rasterized plots, passed
#' to geom_scattermore(). Default is `c(512, 512)`.
#' @param label_size Numeric. Size of the labels
#' @param label_color String. Color fo the labels.
#' @param label_font String. Font of the labels.
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
  label_by = NULL,
  discrete = NULL,
  embd_modality = c("rna", "adt", "wnn"),
  point_size = NULL,
  point_alpha = 0.5,
  raster = NULL,
  raster_dpi = c(512, 512),
  label_size = 3,
  label_color = "black",
  label_font = "bold"
) {
  embd_modality <- match.arg(embd_modality)
  checkmate::qassert(colour_by, "S1")
  checkmate::qassert(label_by, c("S1", "0"))
  checkmate::qassert(discrete, c("0", "B1"))
  checkmate::qassert(raster, c("0", "B1"))
  checkmate::qassert(raster_dpi, c("N2"))
  checkmate::qassert(point_size, c("N1", "0"))
  checkmate::qassert(point_alpha, c("N1"))
  checkmate::qassert(label_size, c("N1"))
  checkmate::qassert(label_color, c("S1"))
  checkmate::qassert(label_font, c("S1"))

  ## extract data
  c_names <- c(colour_by, label_by)
  dt <- bixverse::extract_embedding_data(
    object,
    embedding = embedding,
    obs_cols = c_names,
    modality = embd_modality
  )

  if (isTRUE(discrete)) {
    dt[, (colour_by) := as.factor(get(colour_by))]
    if (!is.null(label_by)) {
      dt[, (label_by) := as.factor(get(label_by))]
    }
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

  plot <- .plot_embedding(
    df = dt,
    colour = colour_by,
    embedding = embedding,
    point_size = point_size,
    point_alpha = point_alpha,
    raster = raster,
    raster_dpi = raster_dpi
  )

  if (!is.null(label_by)) {
    plot <- plot +
      label_centroids(
        data = dt,
        label_by = label_by,
        colour = label_color,
        size = label_size,
        fontface = label_font
      )
  }
  plot
}

### embedding with feature -----------------------------------------------------

#' Faceted feature plot over an embedding
#'
#' @param object A single cell class.
#' @param features Character vector. Gene/feature IDs to plot, taken from
#' `expr_modality`.
#' @param embedding String. Name of the embedding.
#' @param feature_labels Optional named character vector mapping gene ids to
#' display labels (default: NULL).
#' @param scale Boolean. Whether to z-score the expression values.
#' @param clip Optional numeric. Clip z-scores if `scale = TRUE`.
#' @param expr_modality String. Modality the expression is pulled from. One of
#' `c("rna", "adt")`.
#' @param embd_modality String. Modality the embedding is pulled from. One of
#' `c("rna", "adt", "wnn")`. Use `"wnn"` for WNN-derived embeddings.
#' @param point_size Optional numeric. Defines the point size. If not provided,
#' will be auto-determined.
#' @param point_alpha Numeric. Defines the alpha.
#' @param raster Optional boolean. Shall the plot be rasterised. If `NULL` and
#' number of cells is larger than `1e5`, defaults to TRUE.
#' @param raster_dpi Two numerics. Pixel resolution for rasterized plots, passed
#' to geom_scattermore(). Default is `c(512, 512)`.
#' @param highlight_features Boolean. Shall the features be more strongly
#' highlighted. Useful for sparsely expressed genes.
#' @param highlight_quantile Numeric between `[0, 1]`. Defines the threshold.
#' @param label_by String. Optional obs column to label by. (default: NULL).
#' @param label_size Numeric. Size of the labels
#' @param label_color String. Color fo the labels.
#' @param label_font String. Font of the labels.
#' @param ... Additional arguments forwarded to
#' [bixverse::extract_feature_plot_data()] and onward to [get_embedding()]. Do
#' not pass `modality` here; use `embd_modality` instead.
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
  expr_modality = c("rna", "adt"),
  embd_modality = c("rna", "adt", "wnn"),
  point_size = NULL,
  point_alpha = 0.5,
  raster = NULL,
  raster_dpi = c(512, 512),
  label_by = NULL,
  label_size = 3,
  label_color = "black",
  label_font = "bold",
  highlight_features = FALSE,
  highlight_quantile = 0.25,
  ...
) {
  expr_modality <- match.arg(expr_modality)
  embd_modality <- match.arg(embd_modality)

  checkmate::qassert(label_by, c("S1", "0"))
  checkmate::qassert(raster, c("0", "B1"))
  checkmate::qassert(raster_dpi, c("N2"))
  checkmate::qassert(point_size, c("N1", "0"))
  checkmate::qassert(point_alpha, c("N1"))
  checkmate::qassert(label_size, c("N1"))
  checkmate::qassert(label_color, c("S1"))
  checkmate::qassert(label_font, c("S1"))
  checkmate::qassert(highlight_features, "B1")
  checkmate::qassert(highlight_quantile, "N1[0,1]")

  if (!is.null(label_by)) {
    c_names <- c(label_by)
  } else {
    c_names <- NULL
  }
  dt <- bixverse::extract_feature_plot_data(
    object,
    features = features,
    embedding = embedding,
    scale = scale,
    clip = clip,
    obs_col = c_names,
    expr_modality = expr_modality,
    embd_modality = embd_modality,
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

  plot <- .plot_embedding(
    df = dt,
    colour = "expression",
    facet = "gene",
    embedding = embedding,
    point_size = point_size,
    point_alpha = point_alpha,
    raster = raster,
    raster_dpi = raster_dpi,
    highlight = highlight_features
  )

  if (!is.null(label_by)) {
    plot <- plot +
      label_centroids(
        data = dt,
        label_by = label_by,
        colour = label_color,
        size = label_size,
        fontface = label_font
      )
  }
  plot
}

### dot plot -------------------------------------------------------------------

#' Dot plot of marker gene expression across groups
#'
#' @param object A single cell class.
#' @param features Character vector. Gene IDs to plot.
#' @param grouping_variable String. Obs column to group by.
#' @param feature_labels Optional named character vector mapping gene ids to
#' display labels (default: NULL).
#' @param feature_grouping Optional named character vector mapping gene ids to
#' grouping labels, e.g. cell type labels. If feature_labels is provided,
#' the character vectors should contain the mapping of feature display labels to
#' their respecitve groups (e.g. c(CD3E = "T cell", CD8A = "T cell",
#' MS4A1 = "B cell", ...). (default: NULL).
#' @param scale_exp Boolean. Whether to min-max scale mean expression per gene.
#' @param modality String. One of `c("rna", "adt")`.
#' @param cluster_groups Boolean. Use hierarchical clustering on the grouping variable
#' to re-order the group labels based on expression similarity.
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
  feature_grouping = NULL,
  scale_exp = TRUE,
  modality = c("rna", "adt"),
  cluster_groups = TRUE
) {
  modality <- match.arg(modality)

  dt <- bixverse::extract_dot_plot_data(
    object,
    features = features,
    grouping_variable = grouping_variable,
    scale_exp = scale_exp,
    modality = modality
  )

  .plot_dotplot(
    df = dt,
    feature_labels = feature_labels,
    feature_grouping = feature_grouping,
    cluster_groups = cluster_groups
  )
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

### scatter plot ---------------------------------------------------------------

#' Scatter / hex plot of two features against each other
#'
#' @description
#' Plots two features against each other (typical use case: protein vs mRNA
#' on ADT/RNA data). Each feature may carry a `_rna` or `_adt` suffix to
#' select its modality independently; unsuffixed features fall back to
#' `modality`. With `geom = "density"` points are coloured by 2D KDE, with
#' `geom = "hex"` cells are binned into hexagons.
#'
#' @param object A single cell class.
#' @param feature_1 String. First feature (x-axis), optionally `_rna` / `_adt`
#' suffixed.
#' @param feature_2 String. Second feature (y-axis), optionally `_rna` / `_adt`
#' suffixed.
#' @param geom String. `"density"` or `"hex"` (default: `"density"`).
#' @param remove_zeros Boolean. Drop cells where both features are zero
#' (default: TRUE).
#' @param smoothness Numeric. Bandwidth multiplier for the KDE (default: 10).
#' Only used for `geom = "density"`.
#' @param bins Numeric. Number of hex bins (default: 60). Only used for
#' `geom = "hex"`.
#' @param modality String. Fallback modality for unsuffixed features. One
#' of `c("rna", "adt")`.
#' @param point_size Numeric. Point size for density plots (default: 2.5).
#' @param point_alpha Numeric. Alpha for density plots (default: 0.5).
#' @param raster Optional boolean. Shall the plot be rasterised. If `NULL`
#' and `n_cells > 1e5`, defaults to TRUE. Only applies to
#' `geom = "density"`.
#' @param raster_dpi Two numerics. Pixel resolution for rasterised plots
#' (default: `c(512, 512)`).
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @export
#' @import ggplot2
feature_scatter_plot_sc <- function(
  object,
  feature_1,
  feature_2,
  geom = c("density", "hex"),
  remove_zeros = TRUE,
  smoothness = 10,
  bins = 60,
  modality = c("rna", "adt"),
  point_size = 2.5,
  point_alpha = 0.5,
  raster = NULL,
  raster_dpi = c(512, 512)
) {
  geom <- match.arg(geom)
  modality <- match.arg(modality)

  checkmate::qassert(remove_zeros, "B1")
  checkmate::qassert(raster, c("0", "B1"))
  checkmate::qassert(raster_dpi, "N2")

  dt <- bixverse::extract_feature_pair(
    object,
    feature_1 = feature_1,
    feature_2 = feature_2,
    modality = modality
  )

  if (remove_zeros) {
    dt <- dt[feature_1 > 0 | feature_2 > 0]
  }

  features <- attr(dt, "features")
  n_cells <- nrow(dt)
  raster <- raster %||% (n_cells > 1e5)

  if (raster && geom == "density") {
    message("Raster was set to TRUE or n_cells > 1e5 -> Rasterising the plot")
  }

  .plot_feature_pair(
    df = dt,
    features = features,
    geom = geom,
    smoothness = smoothness,
    bins = bins,
    point_size = point_size,
    point_alpha = point_alpha,
    raster = raster && geom == "density",
    raster_dpi = raster_dpi
  )
}
