# igraph enrichment map to ggraph plot

Takes in the output from
[`enrichment_map_oae()`](https://gregorlueg.github.io/bixverse.plots/reference/enrichment_map_oae.md)
or
[`enrichment_map_gsea()`](https://gregorlueg.github.io/bixverse.plots/reference/enrichment_map_gsea.md)
and generates a ggraph object for subsequent saving, etc.

## Usage

``` r
plot_enrichment_map_ggraph(
  g,
  label_nodes = "adaptive",
  labels_to_include = NULL,
  adaptive_thresholds = c(`15` = 3, `5` = 2, `2` = 1, `1` = 0),
  font_size = 4,
  ...
)
```

## Arguments

- g:

  igraph. Output from enrichment_map functions.

- label_nodes:

  String. Controls which nodes to label. Options:

  - `"all"`: Label all nodes

  - `"adaptive"`: Adaptive labelling based on community size (default)

  - `NULL`: No labels

  - Integer: Label top N nodes by size

- labels_to_include:

  Optional string. These are labels you want to include no matter what.

- adaptive_thresholds:

  Named numeric. The names indicate the community size and the values
  how many pathways per community to show. An example would be
  `c(15 = 3, 5 = 2, 2 = 1, 1 = 0)`

- font_size:

  Numeric. Font size of the labels on top of the enrichment map.

- ...:

  Other parameters you wish to forward to
  [`ggraph::geom_node_text()`](https://ggraph.data-imaginist.com/reference/geom_node_text.html).

## Value

A ggplot2 object with the enrichment map
