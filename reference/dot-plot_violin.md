# Violin plot worker

Violin plot worker

## Usage

``` r
.plot_violin(
  df,
  grouping_column,
  variable,
  outlier_column = "global_outlier",
  group_name = NULL,
  var_name = NULL,
  log_scale = TRUE,
  show_outlier = TRUE,
  raster = FALSE
)
```

## Arguments

- df:

  data.table. Plotting-ready data.

- grouping_column:

  Character. Column used for the x-axis groups.

- variable:

  Character. Numeric column to plot on the y-axis.

- outlier_column:

  Character. Logical column used to colour the jitter.

- group_name:

  Character. x-axis label (default: NULL).

- var_name:

  Character. y-axis label (default: NULL).

- log_scale:

  Logical. Apply a log10 y-axis (default: TRUE).

- show_outlier:

  Logical. Overlay jittered points coloured by `outlier_column`
  (default: TRUE).

- raster:

  Boolean. Shall
  [`scattermore::geom_scattermore()`](https://rdrr.io/pkg/scattermore/man/geom_scattermore.html)
  be used.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.
