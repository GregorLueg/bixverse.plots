# Joint hexbin plot worker

Joint hexbin plot worker

## Usage

``` r
.plot_joint(
  df,
  library_size = "lib_size",
  nb_features = "nnz",
  log_scale = FALSE,
  palette = "sequential"
)
```

## Arguments

- df:

  data.table. Plotting-ready data.

- library_size:

  Character. Column with the library size per cell.

- nb_features:

  Character. Column with the number of features per cell.

- log_scale:

  Logical. Log10-transform both axes (default: FALSE).

- palette:

  String. Continuous palette for the hexbin fill, see
  [`bx_colors()`](https://gregorlueg.github.io/bixverse.plots/reference/bx_colors.md).
  The marginal histograms take the palette's mid colour.

## Value

A `ggExtraPlot` object.
