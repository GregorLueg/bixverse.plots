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

  Numeric. Defaults to `1`.

## Value

A named list of `gsea_par_plot_data` objects, one per pathway that
survived the size filters. Each one is a list with the following
elements:

- curve_dt - data.table with the running enrichment score, columns
  `rank` and `ES`.

- ticks_dt - data.table with the positions of the pathway genes, columns
  `rank` and `stat`.

- stats_dt - data.table with the ranked statistic, columns `rank` and
  `stat`.

- key_points - Named numeric with `pos_es`, `neg_es` and `spread_es`,
  plus `nes` and `fdr` if `gsea_results` was provided.

- additional_label - String or `NULL`. The NES/FDR annotation.
