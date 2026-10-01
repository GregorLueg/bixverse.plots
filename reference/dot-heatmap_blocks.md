# Lay out a grouped heatmap as one raster per block

Places the columns and rows of a heatmap in data units with gaps pushed
in between groups and draws every block of column group x row group as
one
[`ggplot2::annotation_raster()`](https://ggplot2.tidyverse.org/reference/annotation_raster.html).
y counts down from the top, so the first row sits on top.

## Usage

``` r
.heatmap_blocks(colour_mat, col_annot, row_annot, group_gap)
```

## Arguments

- colour_mat:

  Character matrix of colours, rows x columns, already in display order.

- col_annot:

  data.table with `col_idx` and the factor `group`, one row per column
  of `colour_mat`, sorted by `col_idx`.

- row_annot:

  data.table with `row_idx`, the factor `group` and `feature`, one row
  per row of `colour_mat`, sorted by `row_idx`.

- group_gap:

  Numeric between `[0, 0.1]`. Width of each gap between groups as
  fraction of the axis length.

## Value

A list with:

- layers - List of raster layers, one per block.

- col_blocks - data.table with `group`, `xmin`, `xmax`, `cols`.

- row_blocks - data.table with `group`, `ymin`, `ymax`, `rows`.

- row_dt - `row_annot` with the top edge `y_top` of every row.

- x_max, y_min - Extent of the matrix.

- strip_x, strip_y - Strip heights in data units.
