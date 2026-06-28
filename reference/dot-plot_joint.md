# Joint hexbin plot worker

Joint hexbin plot worker

## Usage

``` r
.plot_joint(
  df,
  library_size = "lib_size",
  nb_features = "nnz",
  log_scale = FALSE
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

## Value

A `ggExtraPlot` object.
