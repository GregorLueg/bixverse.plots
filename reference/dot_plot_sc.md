# Dot plot of marker gene expression across groups

Dot plot of marker gene expression across groups

## Usage

``` r
dot_plot_sc(
  object,
  features,
  grouping_variable,
  feature_labels = NULL,
  feature_grouping = NULL,
  scale_exp = TRUE,
  modality = c("rna", "adt"),
  cluster_groups = TRUE
)
```

## Arguments

- object:

  A single cell class.

- features:

  Character vector. Gene IDs to plot.

- grouping_variable:

  String. Obs column to group by.

- feature_labels:

  Optional named character vector mapping gene ids to display labels
  (default: NULL).

- feature_grouping:

  Optional named character vector mapping gene ids to grouping labels,
  e.g. cell type labels. If feature_labels is provided, the character
  vectors should contain the mapping of feature display labels to their
  respecitve groups (e.g. c(CD3E = "T cell", CD8A = "T cell", MS4A1 = "B
  cell", ...). (default: NULL).

- scale_exp:

  Boolean. Whether to min-max scale mean expression per gene.

- modality:

  String. One of `c("rna", "adt")`.

- cluster_groups:

  Boolean. Use hierarchical clustering on the grouping variable to
  re-order the group labels based on expression similarity.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.
