# Dot plot worker

Dot plot worker

## Usage

``` r
.plot_dotplot(
  df,
  feature_labels = NULL,
  feature_grouping = NULL,
  cluster_groups = TRUE
)
```

## Arguments

- df:

  data.table. Must contain `gene`, `group`, `pct_exp`, `scaled_exp`.

- feature_labels:

  Optional named character vector mapping gene ids to display labels
  (default: NULL).

- feature_grouping:

  Optional named character vector mapping gene ids to grouping labels,
  e.g. cell type labels. If feature_labels is provided, the character
  vectors should contain the mapping of feature display labels to their
  respecitve groups (e.g. c(CD3E = "T cell", CD8A = "T cell", MS4A1 = "B
  cell", ...). (default: NULL).

- cluster_groups:

  Boolean. Use hierarchical clustering on the grouping variable to
  re-order the group labels based on expression similarity.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.
