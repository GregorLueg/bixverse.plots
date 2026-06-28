# igraph enrichment map to VisNetwork interactive network

Takes in the output from
[`enrichment_map_oae()`](https://gregorlueg.github.io/bixverse.plots/reference/enrichment_map_oae.md)
or
[`enrichment_map_gsea()`](https://gregorlueg.github.io/bixverse.plots/reference/enrichment_map_gsea.md)
and generates an interactive VisNetwork widget.

## Usage

``` r
plot_enrichment_map_visnetwork(g)
```

## Arguments

- g:

  igraph. Output from enrichment_map functions.

## Value

The interactive visnetwork
