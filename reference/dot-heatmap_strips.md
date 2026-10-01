# Add group strips, feature labels and margins to a block heatmap

Group strips are only drawn where there is more than one group. The
strips sit inside the panel, only their labels go into the margins.
Column and row groups share one colour mapping, so a row group with the
same name as a column group gets the same colour.

## Usage

``` r
.heatmap_strips(
  p,
  layout,
  show_feature_labels,
  col_strip_offset = 0,
  palette = "main"
)
```

## Arguments

- p:

  A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
  object holding the block rasters.

- layout:

  List. Output of
  [`.heatmap_blocks()`](https://gregorlueg.github.io/bixverse.plots/reference/dot-heatmap_blocks.md).

- show_feature_labels:

  Boolean. Shall the row labels be written to the right of the matrix.

- col_strip_offset:

  Numeric. Pushes the column group strip up, to make room for an extra
  strip on top of the matrix.

- palette:

  String. Discrete palette for the group strips, see
  [`bx_colors()`](https://gregorlueg.github.io/bixverse.plots/reference/bx_colors.md).

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.
