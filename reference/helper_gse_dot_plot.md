# Helper to generate a GSE dot plot

Helper to generate a GSE dot plot

## Usage

``` r
helper_gse_dot_plot(
  res,
  size_range = c(2, 5),
  viridis_option = "D",
  direction = -1
)
```

## Arguments

- res:

  data.table with the enrichment results. Needs to have the columns
  `c("hits", "target_set_lengths", "gene_set_name", "fdr")`.

- size_range:

  Numerical vector of size 2. Defines the size range for the dots in the
  plot.

- viridis_option:

  String. The option to forward to
  [`ggplot2::scale_fill_viridis_c()`](https://ggplot2.tidyverse.org/reference/scale_viridis.html).

- direction:

  `1` or `-1`. The direction in the colour palette.

## Value

The GSE dot plot.
