# Per-metric density plots from a CellQc object

Requires grouped data; outlier groups are read from `per_group_stats`.

## Usage

``` r
# S3 method for class 'CellQc'
density_plot_sc(x, adjust_position_label = 0, palette = BX_PALETTES, ...)
```

## Arguments

- x:

  A `CellQc` object.

- adjust_position_label:

  Numeric. x-offset for the labels (default: 0).

- palette:

  String. Discrete palette for the group fills and labels. One of
  `c("main", "sequential", "diverging", "viridis", "spectral")`, see
  [`bx_colors()`](https://gregorlueg.github.io/bixverse.plots/reference/bx_colors.md).

- ...:

  Ignored.

## Value

A named list of ggplot objects, one per metric.
