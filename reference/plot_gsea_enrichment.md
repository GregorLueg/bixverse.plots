# Plot GSEA enrichment results

Helper function to create the classical GSEA plots for a set of pathways
of interest. You can also provide the underlying GSEA results to add the
FDR and NES information to the plot.

## Usage

``` r
plot_gsea_enrichment(
  stats,
  pathways,
  pathways_of_interest,
  gsea_results = NULL,
  gsea_param = 1,
  tick_size = 0.2,
  text_size = 5
)
```

## Arguments

- stats:

  Named numeric vector. The gene level statistic.

- pathways:

  List. A named list with each element containing the genes for this
  pathway.

- pathways_of_interest:

  String vector. Names of the pathways to plot. These strings need to be
  represented in the names of pathways.

- gsea_results:

  Optional data.table with the bixverse GSEA results. If provided, the
  FDR and NES for the given pathway of interest will be also added to
  the plot.

- gsea_param:

  Numeric. Defaults to `1`.

- tick_size:

  Numeric. The tick size. Defaults ot `0.2`.

- text_size:

  Numeric. The text size. Defaults ot `8`. Only relevant when
  `gsea_results` is provided.
