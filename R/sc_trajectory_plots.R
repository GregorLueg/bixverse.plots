# trajectory plots -------------------------------------------------------------

## plot functions --------------------------------------------------------------

### paga -----------------------------------------------------------------------

#' PAGA abstracted graph over an embedding
#'
#' @description
#' Draws the PAGA graph with every cluster sitting at the centroid of its cells
#' in an embedding, over a faint scatter of the cells themselves. A free layout
#' of the abstracted graph puts the nodes somewhere arbitrary, which the reader
#' then has to relate back to the embedding by hand. This does not.
#'
#' The abstracted graph is close to complete on real data, so `threshold` is
#' doing real work and dropping it to zero gives a hairball. `tree_only` is the
#' shortcut to the backbone.
#'
#' @param object A single cell class.
#' @param paga_res `PagaRes` class. The output of [bixverse::run_paga_sc()],
#' run on this object.
#' @param embedding String. Name of the embedding to position the nodes in.
#' @param threshold Numeric. Edges below this connectivity are dropped
#' (default: 0.01).
#' @param tree_only Boolean. Draw the maximum spanning forest rather than the
#' full abstracted graph (default: FALSE).
#' @param node_colour_by Optional string. A numeric obs column summarised per
#' cluster, e.g. `"palantir_pseudotime"`, giving continuously coloured nodes.
#' `NULL` (default) colours the nodes discretely by cluster.
#' @param show_cells Boolean. Draw the cells underneath the graph
#' (default: TRUE).
#' @param label Boolean. Label the nodes with their cluster (default: TRUE).
#' @param embd_modality String. One of `c("rna", "adt", "wnn")`. Modality the
#' embedding is pulled from.
#' @param centroid String. One of `c("median", "mean")`. How a cluster's
#' position is summarised. Median by default, since embeddings throw stragglers
#' that drag a mean off its cluster.
#' @param point_size Optional numeric. Size of the cells. If not provided, will
#' be auto-determined.
#' @param point_alpha Numeric. Alpha of the cells (default: 0.2).
#' @param raster Optional boolean. Shall the cell layer be rasterised. If `NULL`
#' and the number of cells is larger than `1e5`, defaults to TRUE.
#' @param raster_dpi Two numerics. Pixel resolution for rasterized plots, passed
#' to geom_scattermore(). Default is `c(512, 512)`.
#' @param cell_colour String. Colour of the cell layer (default: "grey85").
#' @param edge_colour String. Colour of the edges (default: "grey40").
#' @param edge_width Two numerics. Range the edge widths are scaled into
#' (default: `c(0.2, 3)`).
#' @param max_node_size Numeric. Size of the largest node. Node *area* tracks
#' the cell count, which is what people read off it (default: 12).
#' @param palette Optional string. Palette for the nodes, see [bx_colors()].
#' `NULL` (default) resolves to `"main"` for the discrete case and
#' `"sequential"` when `node_colour_by` is given.
#' @param label_size Numeric. Size of the labels.
#' @param label_color String. Colour of the labels.
#' @param label_font String. Font of the labels.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @references Wolf, et al., Genome Biol., 2019.
#'
#' @export
#' @import ggplot2
paga_plot_sc <- function(
  object,
  paga_res,
  embedding = "umap",
  threshold = 0.01,
  tree_only = FALSE,
  node_colour_by = NULL,
  show_cells = TRUE,
  label = TRUE,
  embd_modality = c("rna", "adt", "wnn"),
  centroid = c("median", "mean"),
  point_size = NULL,
  point_alpha = 0.4,
  raster = NULL,
  raster_dpi = c(512, 512),
  cell_colour = "grey80",
  edge_colour = "grey40",
  edge_width = c(0.2, 3),
  max_node_size = 12,
  palette = NULL,
  label_size = 3,
  label_color = "black",
  label_font = "bold"
) {
  embd_modality <- match.arg(embd_modality)
  centroid <- match.arg(centroid)

  # checks
  checkmate::qassert(embedding, "S1")
  checkmate::qassert(threshold, "N1[0,)")
  checkmate::qassert(tree_only, "B1")
  checkmate::qassert(node_colour_by, c("S1", "0"))
  checkmate::qassert(show_cells, "B1")
  checkmate::qassert(label, "B1")
  checkmate::qassert(point_size, c("N1", "0"))
  checkmate::qassert(point_alpha, "N1")
  checkmate::qassert(raster, c("0", "B1"))
  checkmate::qassert(raster_dpi, "N2")
  checkmate::qassert(cell_colour, "S1")
  checkmate::qassert(edge_colour, "S1")
  checkmate::qassert(edge_width, "N2")
  checkmate::qassert(max_node_size, "N1(0,)")
  checkmate::qassert(label_size, "N1")
  checkmate::qassert(label_color, "S1")
  checkmate::qassert(label_font, "S1")
  checkmate::assertChoice(palette, BX_PALETTES, null.ok = TRUE)

  ## extract data
  paga_dt <- bixverse::extract_paga_plot_data(
    object = object,
    paga_res = paga_res,
    embedding = embedding,
    threshold = threshold,
    tree_only = tree_only,
    centroid = centroid,
    node_stat_col = node_colour_by,
    modality = embd_modality
  )

  nodes <- paga_dt$nodes
  edges <- paga_dt$edges

  plot <- ggplot()

  ## cells, underneath everything else
  if (show_cells) {
    cells <- bixverse::extract_embedding_data(
      object,
      embedding = embedding,
      modality = embd_modality
    )

    n_cells <- nrow(cells)
    raster <- raster %||% (n_cells > 1e5)
    point_size <- point_size %||%
      auto_point_size(n_samples = n_cells, raster = raster)

    if (raster) {
      message(paste(
        "Raster was set to TRUE or n_cells > 1e5 -> Rasterising the plot"
      ))
      plot <- plot +
        scattermore::geom_scattermore(
          data = cells,
          mapping = aes(x = dim_1, y = dim_2),
          colour = cell_colour,
          pointsize = point_size,
          pixels = raster_dpi,
          alpha = point_alpha
        )
    } else {
      plot <- plot +
        geom_point(
          data = cells,
          mapping = aes(x = dim_1, y = dim_2),
          colour = cell_colour,
          size = point_size,
          alpha = point_alpha
        )
    }
  }

  ## edges over the cells, nodes over the edges
  if (nrow(edges) > 0) {
    plot <- plot +
      geom_segment(
        data = edges,
        mapping = aes(
          x = x,
          y = y,
          xend = xend,
          yend = yend,
          linewidth = weight
        ),
        colour = edge_colour,
        alpha = 0.8,
        lineend = "round"
      ) +
      scale_linewidth(range = edge_width, breaks = scales::breaks_pretty(4))
  }

  # shape 21 with a white stroke keeps a node legible where it lands on top of
  # dense cells
  node_fill <- if (is.null(node_colour_by)) "cluster" else "stat"

  plot <- plot +
    geom_point(
      data = nodes,
      mapping = aes(
        x = dim_1,
        y = dim_2,
        size = n_cells,
        fill = .data[[node_fill]]
      ),
      shape = 21,
      colour = "white",
      stroke = 0.6
    ) +
    # area rather than radius, otherwise the count is read wrong
    scale_size_area(max_size = max_node_size)

  fill_scale <- if (is.null(node_colour_by)) {
    scale_fill_bx(palette = palette %||% "main")
  } else {
    scale_fill_bx_c(palette = palette %||% "sequential")
  }

  plot <- plot + fill_scale

  if (label) {
    # repelled rather than centred: a label sitting on its node hides the fill,
    # which is the whole point of `node_colour_by`. `seed` keeps the placement
    # stable across redraws, `max.overlaps` stops labels being dropped in
    # silence on a crowded graph.
    plot <- plot +
      ggrepel::geom_label_repel(
        data = nodes,
        mapping = aes(x = dim_1, y = dim_2, label = cluster),
        colour = label_color,
        fill = "white",
        alpha = 0.8,
        label.size = 0,
        size = label_size,
        fontface = label_font,
        box.padding = 0.6,
        point.padding = 0.4,
        min.segment.length = 0,
        segment.colour = "grey50",
        max.overlaps = Inf,
        seed = 42L
      )
  }

  plot +
    # the size keys inherit the fill aesthetic, which draws them white on white
    guides(size = guide_legend(override.aes = list(fill = "grey60"))) +
    theme_bx() +
    labs(
      x = sprintf("%s 1", embedding),
      y = sprintf("%s 2", embedding),
      size = "Cells",
      linewidth = "Connectivity",
      fill = node_colour_by %||% "Cluster"
    )
}
