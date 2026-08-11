# PAGA abstracted graph over an embedding

Draws the PAGA graph with every cluster sitting at the centroid of its
cells in an embedding, over a faint scatter of the cells themselves. A
free layout of the abstracted graph puts the nodes somewhere arbitrary,
which the reader then has to relate back to the embedding by hand. This
does not.

The abstracted graph is close to complete on real data, so `threshold`
is doing real work and dropping it to zero gives a hairball. `tree_only`
is the shortcut to the backbone.

## Usage

``` r
paga_plot_sc(
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
  point_alpha = 0.2,
  raster = NULL,
  raster_dpi = c(512, 512),
  cell_colour = "grey85",
  edge_colour = "grey40",
  edge_width = c(0.2, 3),
  max_node_size = 12,
  palette = NULL,
  label_size = 3,
  label_color = "black",
  label_font = "bold"
)
```

## Arguments

- object:

  A single cell class.

- paga_res:

  `PagaRes` class. The output of
  [`bixverse::run_paga_sc()`](https://gregorlueg.github.io/bixverse/reference/run_paga_sc.html),
  run on this object.

- embedding:

  String. Name of the embedding to position the nodes in.

- threshold:

  Numeric. Edges below this connectivity are dropped (default: 0.01).

- tree_only:

  Boolean. Draw the maximum spanning forest rather than the full
  abstracted graph (default: FALSE).

- node_colour_by:

  Optional string. A numeric obs column summarised per cluster, e.g.
  `"palantir_pseudotime"`, giving continuously coloured nodes. `NULL`
  (default) colours the nodes discretely by cluster.

- show_cells:

  Boolean. Draw the cells underneath the graph (default: TRUE).

- label:

  Boolean. Label the nodes with their cluster (default: TRUE).

- embd_modality:

  String. One of `c("rna", "adt", "wnn")`. Modality the embedding is
  pulled from.

- centroid:

  String. One of `c("median", "mean")`. How a cluster's position is
  summarised. Median by default, since embeddings throw stragglers that
  drag a mean off its cluster.

- point_size:

  Optional numeric. Size of the cells. If not provided, will be
  auto-determined.

- point_alpha:

  Numeric. Alpha of the cells (default: 0.2).

- raster:

  Optional boolean. Shall the cell layer be rasterised. If `NULL` and
  the number of cells is larger than `1e5`, defaults to TRUE.

- raster_dpi:

  Two numerics. Pixel resolution for rasterized plots, passed to
  geom_scattermore(). Default is `c(512, 512)`.

- cell_colour:

  String. Colour of the cell layer (default: "grey85").

- edge_colour:

  String. Colour of the edges (default: "grey40").

- edge_width:

  Two numerics. Range the edge widths are scaled into (default:
  `c(0.2, 3)`).

- max_node_size:

  Numeric. Size of the largest node. Node *area* tracks the cell count,
  which is what people read off it (default: 12).

- palette:

  Optional string. Palette for the nodes, see
  [`bx_colors()`](https://gregorlueg.github.io/bixverse.plots/reference/bx_colors.md).
  `NULL` (default) resolves to `"main"` for the discrete case and
  `"sequential"` when `node_colour_by` is given.

- label_size:

  Numeric. Size of the labels.

- label_color:

  String. Colour of the labels.

- label_font:

  String. Font of the labels.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## References

Wolf, et al., Genome Biol., 2019.
