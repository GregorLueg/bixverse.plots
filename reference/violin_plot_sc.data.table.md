# Violin plot from a data.table

Per-cell outliers are recomputed within each `grouping_column` group.

## Usage

``` r
# S3 method for class 'data.table'
violin_plot_sc(
  x,
  grouping_column,
  variable,
  direction = c("twosided", "below", "above"),
  threshold = 3,
  group_name = NULL,
  var_name = NULL,
  log_scale = TRUE,
  show_outlier = TRUE,
  raster = NULL,
  ...
)
```

## Arguments

- x:

  data.table. Input data containing the QC metric.

- grouping_column:

  Character. Column used for the x-axis groups.

- variable:

  Character. Numeric column to plot on the y-axis.

- direction:

  Character. One of `"twosided"`, `"below"`, `"above"`.

- threshold:

  Numeric. Number of MADs for outlier detection (default: 3).

- group_name:

  Character. x-axis label (default: NULL).

- var_name:

  Character. y-axis label (default: NULL).

- log_scale:

  Logical. Apply a log10 y-axis (default: TRUE).

- show_outlier:

  Logical. Overlay outlier points (default: TRUE).

- raster:

  Optional boolean. Shall the plot be rasterised. If `NULL` and number
  of cells is larger than `1e5`, defaults to TRUE.

- ...:

  Ignored.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.
