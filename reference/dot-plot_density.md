# Density plot worker

Density plot worker

## Usage

``` r
.plot_density(
  df,
  grouping_column,
  variable,
  outlier_groups,
  var_name = NULL,
  log_scale = FALSE,
  adjust_position_label = 0
)
```

## Arguments

- df:

  data.table. Plotting-ready data.

- grouping_column:

  Character. Column used to group the densities.

- variable:

  Character. Numeric column to plot on the x-axis.

- outlier_groups:

  data.table. Outlier groups to label, with columns `group_id` and
  `group_median`.

- var_name:

  Character. x-axis label (default: NULL).

- log_scale:

  Logical. Apply a log10 x-axis (default: FALSE).

- adjust_position_label:

  Numeric. x-offset for the labels (default: 0).

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.
