# Create a volcano plot

Helper function to generate volcano plots for differental
gene/transcript/protein expression analysis. The function applies under
the hood a `-log10(stat)` transformation to the respective statistic you
want to display on the y axis. If you provide labels, the top features
to label will be chosen by ranking the `-log10(stat)`.

## Usage

``` r
volcano_plot(
  dt,
  volcano_plot_params = params_volcano(),
  x_lab = NULL,
  y_lab = NULL,
  plot_title = NULL,
  plot_sub_title = NULL
)
```

## Arguments

- dt:

  A data.table/data.frame holding the differential expression results.

- volcano_plot_params:

  A list as returned by
  [`params_volcano()`](https://gregorlueg.github.io/bixverse.plots/reference/params_volcano.md).

- x_lab:

  Optional string. Overwrite of the x label of the plot. If not
  provided, will default to `volcano_plot_params$x_axis`.

- y_lab:

  Optional string. Overwrite of the x label of the plot. If not
  provided, will default to `-log10(volcano_plot_params$y_axis)`.

- plot_title:

  Optional string. If provided, the plot will get a title.

- plot_sub_title:

  Optional string. If provided, adds this as a sub title to the plot.

## Value

A `ggplot` object.
