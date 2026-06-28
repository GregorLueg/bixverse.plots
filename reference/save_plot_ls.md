# Save a list of plots

Save a list of plots

## Usage

``` r
save_plot_ls(plot_ls, path, plot_params = params_plots())
```

## Arguments

- plot_ls:

  Named list of plots. The names will serve as file names (the extension
  will be added by the function)

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

Saves the plots to disk and returns `invisible`.
