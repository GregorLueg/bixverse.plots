# Stacked violin plot worker

Stacked violin plot worker

## Usage

``` r
.plot_stacked_violin(df, feature_labels = NULL, scale_y = "width")
```

## Arguments

- df:

  data.table. Must contain `group`, `gene`, `expression`. `gene` is
  expected to be an ordered factor; the first level sits at the top.

- feature_labels:

  Optional named character vector mapping gene ids to display labels
  (default: NULL).

- scale_y:

  Character. `geom_violin` scaling, passed as `scale` (default:
  "width").

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.
