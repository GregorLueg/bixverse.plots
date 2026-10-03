# miloR neighbourhood graph over an embedding

The Milo neighbourhood graph figure: every tested neighbourhood sits at
its index cell in an embedding, sized by its cell count and connected to
the neighbourhoods it shares cells with. With `colour_by = "logFC"` the
significant neighbourhoods are filled by their logFC and drawn on top,
strongest last, while the rest stay white underneath.

## Usage

``` r
milo_nhood_plot_sc(
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
)
```

## Arguments

- object:

  A single cell class. The one the miloR result was built on.

- milo_res:

  `miloR` class, run through
  [`bixverse::test_nhoods()`](https://gregorlueg.github.io/bixverse/reference/test_nhoods.html).

- embedding:

  String. Name of the embedding to position the nodes in.

- alpha:

  Numeric. Spatial FDR threshold for a neighbourhood to count as
  significant (default: 0.1).

- overlap:

  Integer. Minimum number of shared cells for an edge (default: 1L).

- colour_by:

  String. One of `c("logFC", "majority_celltype")`. The latter needs
  [`bixverse::add_nhoods_info()`](https://gregorlueg.github.io/bixverse/reference/add_nhoods_info.html)
  to have been run.

- show_cells:

  Boolean. Draw the cells underneath the graph (default: TRUE).

- embd_modality:

  String. One of `c("rna", "adt", "wnn")`. Modality the embedding is
  pulled from.

- point_size:

  Optional numeric. Size of the cells. If not provided, will be
  auto-determined.

- point_alpha:

  Numeric. Alpha of the cells (default: 0.4).

- raster:

  Optional boolean. Shall the cell layer be rasterised. If `NULL` and
  the number of cells is larger than `1e5`, defaults to TRUE.

- raster_dpi:

  Two numerics. Pixel resolution for rasterized plots, passed to
  geom_scattermore(). Default is `c(512, 512)`.

- cell_colour:

  String. Colour of the cell layer (default: "grey90").

- edge_colour:

  String. Colour of the edges (default: "grey50").

- edge_width:

  Two numerics. Range the edge widths are scaled into (default:
  `c(0.1, 1.5)`).

- size_range:

  Two numerics. Range the node sizes are scaled into (default:
  `c(0.5, 4)`).

- palette:

  Optional string. Palette for the nodes, see
  [`bx_colors()`](https://gregorlueg.github.io/bixverse.plots/reference/bx_colors.md).
  `NULL` (default) resolves to `"diverging"` for logFC, centred at zero,
  and `"main"` for the cell types.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## References

Dann, et al., Nat Biotechnol, 2022.
