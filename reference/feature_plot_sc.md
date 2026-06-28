# Faceted feature plot over an embedding

Faceted feature plot over an embedding

## Usage

``` r
feature_plot_sc(
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
)
```

## Arguments

- object:

  A single cell class.

- features:

  Character vector. Gene/feature IDs to plot, taken from
  `expr_modality`.

- embedding:

  String. Name of the embedding.

- feature_labels:

  Optional named character vector mapping gene ids to display labels
  (default: NULL).

- scale:

  Boolean. Whether to z-score the expression values.

- clip:

  Optional numeric. Clip z-scores if `scale = TRUE`.

- expr_modality:

  String. Modality the expression is pulled from. One of
  `c("rna", "adt")`.

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

- label_by:

  String. Optional obs column to label by. (default: NULL).

- label_size:

  Numeric. Size of the labels

- label_color:

  String. Color fo the labels.

- label_font:

  String. Font of the labels.

- highlight_features:

  Boolean. Shall the features be more strongly highlighted. Useful for
  sparsely expressed genes.

- highlight_quantile:

  Numeric between `[0, 1]`. Defines the threshold.

- ...:

  Additional arguments forwarded to
  [`bixverse::extract_feature_plot_data()`](https://rdrr.io/pkg/bixverse/man/extract_feature_plot_data.html)
  and onward to
  [`get_embedding()`](https://rdrr.io/pkg/bixverse/man/get_embedding.html).
  Do not pass `modality` here; use `embd_modality` instead.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.
