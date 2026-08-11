# Embedding plot coloured by an obs column

Embedding plot coloured by an obs column

## Usage

``` r
embedding_plot_sc(
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
  palette = NULL,
  label_size = 3,
  label_color = "black",
  label_font = "bold"
)
```

## Arguments

- object:

  A single cell class.

- embedding:

  String. Name of the embedding (e.g. `"umap"`).

- colour_by:

  String. Obs column to colour by.

- label_by:

  String. Optional obs column to label by. (default: NULL).

- discrete:

  Optional boolean. Force a discrete scale by coercing `colour_by` to a
  factor. `NULL` (default) picks the scale from the column type.

- embd_modality:

  String. Modality the embedding is pulled from. One of
  `c("rna", "adt", "wnn")`. Use `"wnn"` for WNN-derived embeddings.

- point_size:

  Optional numeric. Defines the point size. If not provided, will be
  auto-determined.

- point_alpha:

  Numeric. Defines the alpha.

- raster:

  Optional boolean. Shall the plot be rasterised. If `NULL` and number
  of cells is larger than `1e5`, defaults to TRUE.

- raster_dpi:

  Two numerics. Pixel resolution for rasterized plots, passed to
  geom_scattermore(). Default is `c(512, 512)`.

- palette:

  Optional string. Palette for the colour scale, see
  [`bx_colors()`](https://gregorlueg.github.io/bixverse.plots/reference/bx_colors.md).
  `NULL` (default) resolves to `"main"` if `colour_by` is discrete and
  `"sequential"` if it is continuous.

- label_size:

  Numeric. Size of the labels

- label_color:

  String. Color fo the labels.

- label_font:

  String. Font of the labels.
  [`bixverse::extract_embedding_data()`](https://gregorlueg.github.io/bixverse/reference/extract_embedding_data.html).

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.
