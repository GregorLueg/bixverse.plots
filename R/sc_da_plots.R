# differential abundance plots -------------------------------------------------

## plot functions --------------------------------------------------------------

### milo -----------------------------------------------------------------------

#' miloR neighbourhood graph over an embedding
#'
#' @description
#' The Milo neighbourhood graph figure: every tested neighbourhood sits at its
#' index cell in an embedding, sized by its cell count and connected to the
#' neighbourhoods it shares cells with. With `colour_by = "logFC"` the
#' significant neighbourhoods are filled by their logFC and drawn on top,
#' strongest last, while the rest stay white underneath.
#'
#' @param object A single cell class. The one the miloR result was built on.
#' @param milo_res `miloR` class, run through [bixverse::test_nhoods()].
#' @param embedding String. Name of the embedding to position the nodes in.
#' @param alpha Numeric. Spatial FDR threshold for a neighbourhood to count as
#' significant (default: 0.1).
#' @param overlap Integer. Minimum number of shared cells for an edge
#' (default: 1L).
#' @param colour_by String. One of `c("logFC", "majority_celltype")`. The
#' latter needs [bixverse::add_nhoods_info()] to have been run.
#' @param show_cells Boolean. Draw the cells underneath the graph
#' (default: TRUE).
#' @param embd_modality String. One of `c("rna", "adt", "wnn")`. Modality the
#' embedding is pulled from.
#' @param point_size Optional numeric. Size of the cells. If not provided, will
#' be auto-determined.
#' @param point_alpha Numeric. Alpha of the cells (default: 0.4).
#' @param raster Optional boolean. Shall the cell layer be rasterised. If `NULL`
#' and the number of cells is larger than `1e5`, defaults to TRUE.
#' @param raster_dpi Two numerics. Pixel resolution for rasterized plots, passed
#' to geom_scattermore(). Default is `c(512, 512)`.
#' @param cell_colour String. Colour of the cell layer (default: "grey90").
#' @param edge_colour String. Colour of the edges (default: "grey50").
#' @param edge_width Two numerics. Range the edge widths are scaled into
#' (default: `c(0.1, 1.5)`).
#' @param size_range Two numerics. Range the node sizes are scaled into
#' (default: `c(0.5, 4)`).
#' @param palette Optional string. Palette for the nodes, see [bx_colors()].
#' `NULL` (default) resolves to `"diverging"` for logFC, centred at zero, and
#' `"main"` for the cell types.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @references Dann, et al., Nat Biotechnol, 2022.
#'
#' @export
#' @import ggplot2
milo_nhood_plot_sc <- function(
  object,
  milo_res,
  embedding = "umap",
  alpha = 0.1,
  overlap = 1L,
  colour_by = c("logFC", "majority_celltype"),
  show_cells = TRUE,
  embd_modality = c("rna", "adt", "wnn"),
  point_size = NULL,
  point_alpha = 0.4,
  raster = NULL,
  raster_dpi = c(512, 512),
  cell_colour = "grey90",
  edge_colour = "grey50",
  edge_width = c(0.1, 1.5),
  size_range = c(0.5, 4),
  palette = NULL
) {
  colour_by <- match.arg(colour_by)
  embd_modality <- match.arg(embd_modality)

  # checks
  checkmate::qassert(embedding, "S1")
  checkmate::qassert(alpha, "N1[0,1]")
  checkmate::qassert(overlap, "I1[1,)")
  checkmate::assertChoice(colour_by, c("logFC", "majority_celltype"))
  checkmate::qassert(show_cells, "B1")
  checkmate::qassert(point_size, c("N1", "0"))
  checkmate::qassert(point_alpha, "N1")
  checkmate::qassert(raster, c("0", "B1"))
  checkmate::qassert(raster_dpi, "N2")
  checkmate::qassert(cell_colour, "S1")
  checkmate::qassert(edge_colour, "S1")
  checkmate::qassert(edge_width, "N2")
  checkmate::qassert(size_range, "N2")
  checkmate::assertChoice(palette, BX_PALETTES, null.ok = TRUE)

  ## extract data
  milo_dt <- bixverse::extract_milo_plot_data(
    object = object,
    milo_res = milo_res,
    embedding = embedding,
    alpha = alpha,
    overlap = overlap,
    modality = embd_modality
  )

  nodes <- milo_dt$nodes
  edges <- milo_dt$edges

  if (!colour_by %in% names(nodes)) {
    stop("No `majority_celltype` found. Run bixverse::add_nhoods_info() first.")
  }

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
        alpha = 0.2
      ) +
      scale_linewidth(range = edge_width)
  }

  if (colour_by == "logFC") {
    # non-significant first and blanked to NA, which the scale draws white;
    # significant last by |logFC| so the strongest calls are not buried
    nodes <- nodes[order(nodes$is_sig, abs(nodes$logFC))]
    data.table::set(
      nodes,
      i = which(!nodes$is_sig),
      j = "logFC",
      value = NA_real_
    )

    lim <- max(abs(nodes$logFC), 0, na.rm = TRUE)
    fill_scale <- scale_fill_bx_c(
      palette = palette %||% "diverging",
      # Hiroshige runs red to blue, flipped so red means enriched
      reverse = is.null(palette),
      limits = if (lim > 0) c(-lim, lim),
      na.value = "white"
    )
  } else {
    fill_scale <- scale_fill_bx(palette = palette %||% "main")
  }

  plot +
    geom_point(
      data = nodes,
      mapping = aes(
        x = dim_1,
        y = dim_2,
        size = size,
        fill = .data[[colour_by]]
      ),
      shape = 21,
      colour = "grey30",
      stroke = 0.2
    ) +
    scale_size(range = size_range) +
    fill_scale +
    # the size keys inherit the fill aesthetic, which draws them white on white
    guides(size = guide_legend(override.aes = list(fill = "grey60"))) +
    theme_bx() +
    labs(
      x = sprintf("%s 1", embedding),
      y = sprintf("%s 2", embedding),
      size = "Nhood size",
      linewidth = "Shared cells",
      fill = if (colour_by == "logFC") "logFC" else "Majority cell type"
    )
}
