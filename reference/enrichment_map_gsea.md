# Generate enrichment map igraph (for GSEA)

Helper function to generate an enrichment map based on GSEA results.
Similar enriched gene sets are clustered together via their Jaccard
similarity (alternatively overlap coefficient) and the function returns
an igraph object for subsequent visualisations.

## Usage

``` r
enrichment_map_gsea(
  res,
  threshold,
  pathways,
  overlap_coefficient = FALSE,
  min_sim = 0.2,
  resolution = 1,
  layout_func = igraph::layout_with_fr,
  ...
)
```

## Arguments

- res:

  data.table with the enrichment results. Needs to have the columns
  `c("pathway_name", "nes", "fdr")`, i.e. the output of
  [`bixverse::calc_fgsea()`](https://gregorlueg.github.io/bixverse/reference/calc_fgsea.html)
  or
  [`bixverse::calc_blitzgsea()`](https://gregorlueg.github.io/bixverse/reference/calc_blitzgsea.html).
  A `geneset_name` column is accepted with a deprecation warning.

- threshold:

  Numeric. The FDR threshold you wish to filter for.

- pathways:

  Named list. The original pathway list used for the calculation of the
  overenrichment analysis.

- overlap_coefficient:

  Boolean. Shall the overlap coefficient be used instead of the Jaccard
  similarity.

- min_sim:

  Numeric. Minimum similarity between two gene sets to be connected.

- resolution:

  Numeric. The resolution parameter for the Louvain clustering.

- layout_func:

  Layout function. Please see
  [`igraph::add_layout_()`](https://r.igraph.org/reference/add_layout_.html)
  for options. This one will be used to layout the graph.

- ...:

  Further parameters to forward to
  [`wrap_and_truncate()`](https://gregorlueg.github.io/bixverse.plots/reference/wrap_and_truncate.md).

## Value

`igraph` object representing the enrichment map.
