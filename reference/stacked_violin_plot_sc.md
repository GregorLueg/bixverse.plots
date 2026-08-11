# Stacked violin plot of gene expression across groups

Stacked violin plot of gene expression across groups

## Usage

``` r
stacked_violin_plot_sc(
  object,
  features,
  grouping_variable,
  feature_labels = NULL,
  scale = FALSE,
  clip = NULL,
  modality = c("rna", "adt"),
  palette = BX_PALETTES
)
```

## Arguments

- object:

  A single cell class.

- features:

  Character vector. Gene IDs to plot, one row each.

- grouping_variable:

  String. Obs column to group by.

- feature_labels:

  Optional named character vector mapping gene ids to display labels
  (default: NULL).

- scale:

  Boolean. Whether to z-score the expression values.

- clip:

  Optional numeric. Clip z-scores if `scale = TRUE`.

- modality:

  String. One of `c("rna", "adt")`.

- palette:

  String. Discrete palette for the group fills. One of
  `c("main", "sequential", "diverging", "viridis", "spectral")`, see
  [`bx_colors()`](https://gregorlueg.github.io/bixverse.plots/reference/bx_colors.md).

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.
