# Feature plot over an embedding

Plots the expression of one or more features over an embedding. By
default every feature gets its own panel and its own colour bar
(`scale_mode = "free"`), so a weakly expressed gene is not flattened by
whatever the loudest gene in the set happens to be. Use
`scale_mode = "shared"` for a single faceted plot with one colour bar
across all features.

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
  scale_mode = c("free", "shared"),
  palette = c("sequential", "spectral", "viridis", "diverging"),
  ncol = NULL,
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

- scale_mode:

  String. One of `c("free", "shared")`. With `"free"` each feature is
  drawn as its own plot with its own colour bar and the panels are
  combined with
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).
  With `"shared"` all features go into one faceted plot with a single
  colour bar fitted to the pooled expression.

- palette:

  String. Continuous palette for the expression values. One of
  `c("sequential", "spectral", "viridis", "diverging")`, see
  [`bx_colors()`](https://gregorlueg.github.io/bixverse.plots/reference/bx_colors.md).

- ncol:

  Optional integer. Number of columns of the panel grid. Only has an
  effect if `scale_mode = "free"`. If `NULL`, patchwork picks the
  layout.

- ...:

  Additional arguments forwarded to
  [`bixverse::extract_feature_plot_data()`](https://gregorlueg.github.io/bixverse/reference/extract_feature_plot_data.html)
  and onward to
  [`get_embedding()`](https://gregorlueg.github.io/bixverse/reference/get_embedding.html).
  Do not pass `modality` here; use `embd_modality` instead.

## Value

A
[`patchwork`](https://patchwork.data-imaginist.com/reference/patchwork-package.html)
object if `scale_mode = "free"`, otherwise a
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) object.
