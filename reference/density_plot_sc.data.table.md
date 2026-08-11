# Density plot from a data.table

Group-level outliers are recomputed from per-group medians.

## Usage

``` r
# S3 method for class 'data.table'
density_plot_sc(
  x,
  grouping_column,
  variable,
  direction = c("twosided", "below", "above"),
  threshold = 3,
  var_name = NULL,
  log_scale = TRUE,
  adjust_position_label = 0,
  palette = BX_PALETTES,
  ...
)
```

## Arguments

- x:

  data.table. Input data containing the QC metric.

- grouping_column:

  Character. Column used to group the densities.

- variable:

  Character. Numeric column to plot on the x-axis.

- direction:

  Character. One of `"twosided"`, `"below"`, `"above"`.

- threshold:

  Numeric. Number of MADs for outlier detection (default: 3).

- var_name:

  Character. x-axis label (default: NULL).

- log_scale:

  Logical. Apply a log10 x-axis (default: TRUE).

- adjust_position_label:

  Numeric. x-offset for the labels (default: 0).

- palette:

  String. Discrete palette for the group fills and labels. One of
  `c("main", "sequential", "diverging", "viridis", "spectral")`, see
  [`bx_colors()`](https://gregorlueg.github.io/bixverse.plots/reference/bx_colors.md).

- ...:

  Ignored.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.
