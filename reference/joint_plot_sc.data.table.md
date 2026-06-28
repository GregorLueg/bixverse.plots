# Joint QC plot from a data.table

Joint QC plot from a data.table

## Usage

``` r
# S3 method for class 'data.table'
joint_plot_sc(
  x,
  library_size = "lib_size",
  nb_features = "nnz",
  log_scale = FALSE,
  ...
)
```

## Arguments

- x:

  data.table. Input data containing the QC metrics.

- library_size:

  Character. Column with the library size per cell.

- nb_features:

  Character. Column with the number of features per cell.

- log_scale:

  Logical. Log10-transform both axes (default: FALSE).

- ...:

  Ignored.

## Value

A `ggExtraPlot` object.
