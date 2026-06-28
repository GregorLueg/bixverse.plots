# Per-metric density plots from a CellQc object

Requires grouped data; outlier groups are read from `per_group_stats`.

## Usage

``` r
# S3 method for class 'CellQc'
density_plot_sc(x, adjust_position_label = 0, ...)
```

## Arguments

- x:

  A `CellQc` object.

- adjust_position_label:

  Numeric. x-offset for the labels (default: 0).

- ...:

  Ignored.

## Value

A named list of ggplot objects, one per metric.
