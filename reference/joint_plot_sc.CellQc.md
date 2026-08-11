# Joint QC plot from a CellQc object

Joint QC plot from a CellQc object

## Usage

``` r
# S3 method for class 'CellQc'
joint_plot_sc(
  x,
  library_size = "log10_lib_size",
  nb_features = "log10_nnz",
  palette = c("sequential", "spectral", "viridis", "diverging"),
  ...
)
```

## Arguments

- x:

  A `CellQc` object.

- library_size:

  Character. Column with the library size per cell.

- nb_features:

  Character. Column with the number of features per cell.

- palette:

  String. Continuous palette for the hexbin fill. One of
  `c("sequential", "spectral", "viridis", "diverging")`, see
  [`bx_colors()`](https://gregorlueg.github.io/bixverse.plots/reference/bx_colors.md).

- ...:

  Ignored.

## Value

A `ggExtraPlot` object.
