# Generate GSE dotplots

This function can take in the output of
[`bixverse::gse_hypergeometric()`](https://gregorlueg.github.io/bixverse/reference/gse_hypergeometric.html)
or
[`bixverse::gse_hypergeometric_list()`](https://gregorlueg.github.io/bixverse/reference/gse_hypergeometric_list.html)
and generates in the former case a single plot and in the latter case a
list of plots per target set.

## Usage

``` r
plot_gse_dotplot(
  res,
  size_range = c(2, 5),
  viridis_option = "D",
  direction = -1,
  max_terms = NULL,
  .verbose = TRUE,
  ...
)
```

## Arguments

- res:

  data.table with the enrichment results. Needs to have the columns
  `c("hits", "target_set_lengths", "gene_set_name", "gene_set_lengths", "fdr")`.

- size_range:

  Numerical vector of size 2. Defines the size range for the dots in the
  plot.

- viridis_option:

  String. The option to forward to
  [`ggplot2::scale_fill_viridis_c()`](https://ggplot2.tidyverse.org/reference/scale_viridis.html).

- direction:

  `1` or `-1`. The direction in the colour palette.

- max_terms:

  Optional integer. Show only the this many most significant gene sets.
  Applied per target set when several were tested. Defaults to `NULL`,
  i.e. everything that passed the enrichment threshold.

- .verbose:

  Boolean. Controls verbosity of the function.

- ...:

  Further parameters to forward to
  [`wrap_and_truncate()`](https://gregorlueg.github.io/bixverse.plots/reference/wrap_and_truncate.md),
  which shortens the gene set labels.

## Value

If the output of
[`bixverse::gse_hypergeometric_list()`](https://gregorlueg.github.io/bixverse/reference/gse_hypergeometric_list.html)
was provided, a list of dotplots per target gene set. Otherwise, a
single GSE OAE dot plot.
