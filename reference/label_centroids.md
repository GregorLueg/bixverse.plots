# Label Centroids in Scatter Plots

Adds boxed text labels at the centroid position of each group in a
scatter plot. Computes group centroids with data.table for efficient
summarisation. Useful for labelling cluster centres in embedding or
dimensionality reduction plots (UMAP, t-SNE, PCA).

## Usage

``` r
label_centroids(
  data = NULL,
  label_by,
  colour = "black",
  fill = "white",
  alpha = 0.5,
  label.size = 0,
  size = 4,
  fontface = "bold",
  ...
)
```

## Arguments

- data:

  A `data.frame` or `data.table` containing the scatter plot points. If
  `NULL`, the data from the parent ggplot is used. Must contain the
  columns referenced by the parent plot's x/y aesthetics and by
  `label_by`.

- label_by:

  Character. Name of the (discrete) column to group and label by.

- colour:

  Text colour. Default: `"black"`.

- fill:

  Box fill colour. Default: `"white"`.

- alpha:

  Box fill transparency in `[0, 1]`. Default: `0.5`.

- label.size:

  Box border line width in mm. Set to `0` to hide the border. Default:
  `0`.

- size:

  Text size in mm. Default: `4`.

- fontface:

  Font face. Default: `"bold"`.

- ...:

  Additional arguments passed to
  [`geom_label`](https://ggplot2.tidyverse.org/reference/geom_text.html).

## Value

A ggplot layer.

## Examples

``` r
if (FALSE) { # \dontrun{
embedding_plot_sc(
  object = sc_object,
  embedding = "umap",
  colour_by = "donor_id"
) +
  label_centroids(label_by = "donor_id")
} # }
```
