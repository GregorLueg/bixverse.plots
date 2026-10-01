# Binary heatmap, e.g. SCENIC regulon on/off calls

Draws a samples x features on/off matrix in the style of the SCENIC
paper: features as rows, samples as columns, black for on and white for
off. Every block of sample group x feature group is drawn as one raster,
so the plot stays light no matter how many samples there are, and the
blocks are separated by white gaps. Groups get a thin colour strip with
their labels written next to it. Sample and feature groups share one
colour mapping, so a feature group with the same name as a sample group
gets the same colour. `group_annotation` adds a second strip with a
legend between the matrix and the sample group strip, e.g. the response
group of each cell line.

Filtering, ordering and binning happen in
[`bixverse::extract_binary_heatmap_data()`](https://gregorlueg.github.io/bixverse/reference/extract_binary_heatmap_data.html).
If the samples were binned the cells show the fraction of samples on as
a grey ramp.

PDF viewers tend to smooth embedded rasters. If the blocks look blurry
in a PDF, save as PNG instead.

## Usage

``` r
plot_binary_heatmap(
  x,
  sample_groups = NULL,
  feature_groups = NULL,
  heatmap_params = bixverse::params_binary_heatmap(),
  show_feature_labels = NULL,
  group_gap = 0.001,
  group_annotation = NULL,
  annotation_colours = NULL,
  palette = c("main", "sequential", "diverging", "viridis", "spectral"),
  .verbose = FALSE
)
```

## Arguments

- x:

  Either a `BinaryHeatmapData` from
  [`bixverse::extract_binary_heatmap_data()`](https://gregorlueg.github.io/bixverse/reference/extract_binary_heatmap_data.html)
  or a logical matrix of samples x features, which then gets passed
  through it.

- sample_groups:

  Optional named character vector or factor mapping samples to groups.
  Only used if `x` is a matrix.

- feature_groups:

  Optional named character vector or factor mapping features to groups.
  Only used if `x` is a matrix.

- heatmap_params:

  List. Output of
  [`bixverse::params_binary_heatmap()`](https://gregorlueg.github.io/bixverse/reference/params_binary_heatmap.html).
  Only used if `x` is a matrix.

- show_feature_labels:

  Optional boolean. Shall the feature names be shown. `NULL` (default)
  shows them for up to 80 features.

- group_gap:

  Numeric between `[0, 0.1]`. Width of each gap between groups as
  fraction of the axis length. Applied per boundary, so with many groups
  keep it small. `0` removes the gaps.

- group_annotation:

  Optional named character vector or factor mapping every sample group
  to a coarser label. Factor levels set the legend order. Ignored if
  there is only one sample group.

- annotation_colours:

  Optional named character vector of colours for the `group_annotation`
  labels. `NULL` (default) uses viridis.

- palette:

  String. Discrete palette for the group strips, see
  [`bx_colors()`](https://gregorlueg.github.io/bixverse.plots/reference/bx_colors.md).

- .verbose:

  Boolean. Controls verbosity of the extraction.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## References

Aibar, et al., Nat Methods, 2017
