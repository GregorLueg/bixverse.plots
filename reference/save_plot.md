# General helper to save all types of plots to file

General helper to save all types of plots to file

## Usage

``` r
save_plot(plot, file_name = NULL, path, plot_params = params_plots())
```

## Arguments

- plot:

  The plot you want to save to disk.

- file_name:

  Optional String. If not provided, the plot will be named after the R
  variable.

- path:

  Directory. Where to save the plot to.

- plot_params:

  List. Output of
  [`params_plots()`](https://gregorlueg.github.io/bixverse.plots/reference/params_plots.md).
  A list with the following elements:

  - width - Width of the plot.

  - height - Height of the plot.

  - file_type - Which file type.

  - unit - Which unit do width and height describe.

  - res - Resolution for PNG plots

## Value

Saves the plot to disk and returns `invisible`.
