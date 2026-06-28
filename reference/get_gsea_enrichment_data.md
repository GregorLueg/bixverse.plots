# Helper function to get the plot data for GSEA plots

Helper function to get the plot data for GSEA plots

## Usage

``` r
get_gsea_enrichment_data(
  stats,
  pathways,
  pathways_of_interest,
  gsea_results = NULL,
  gsea_param = 1
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

  Numeric. Defaults to `1`

## Value

A list of `gsea_par_plot_data`
