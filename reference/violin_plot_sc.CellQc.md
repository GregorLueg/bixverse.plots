# Per-metric violin plots from a CellQc object

Per-metric violin plots from a CellQc object

## Usage

``` r
# S3 method for class 'CellQc'
violin_plot_sc(
  x,
  log_scale = FALSE,
  show_outlier = TRUE,
  raster = NULL,
  palette = BX_PALETTES,
  ...
)
```

## Arguments

- x:

  A `CellQc` object.

- log_scale:

  Logical. Apply a log10 y-axis (default: FALSE).

- show_outlier:

  Logical. Overlay outlier points (default: TRUE).

- raster:

  Optional boolean. Shall the plot be rasterised. If `NULL` and number
  of cells is larger than `1e5`, defaults to TRUE.

- palette:

  String. Discrete palette for the group colours. One of
  `c("main", "sequential", "diverging", "viridis", "spectral")`, see
  [`bx_colors()`](https://gregorlueg.github.io/bixverse.plots/reference/bx_colors.md).
  Ignored when `show_outlier = TRUE`.

- ...:

  Ignored.

## Value

A named list of ggplot objects, one per metric.
