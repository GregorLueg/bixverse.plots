# Generate GSE dotplots

This function can take in the output of
[`bixverse::gse_hypergeometric()`](https://rdrr.io/pkg/bixverse/man/gse_hypergeometric.html)
or
[`bixverse::gse_hypergeometric_list()`](https://rdrr.io/pkg/bixverse/man/gse_hypergeometric_list.html)
and generates in the former case a single plot and in the latter case a
list of plots per target set.

## Usage

``` r
plot_gse_dotplot(
  res,
  size_range = c(2, 5),
  viridis_option = "D",
  direction = -1,
  .verbose = TRUE
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

- .verbose:

  Boolean. Controls verbosity of the function.

## Value

If the output of
[`bixverse::gse_hypergeometric_list()`](https://rdrr.io/pkg/bixverse/man/gse_hypergeometric_list.html)
was provided, a list of dotplots per target gene set. Otherwise, a
single GSE OAE dot plot.
