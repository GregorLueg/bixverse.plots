# Generic joint plot function

Dispatches to a joint hexbin plot of library size vs feature counts. See
[`joint_plot_sc.CellQc`](https://gregorlueg.github.io/bixverse.plots/reference/joint_plot_sc.CellQc.md)
and
[`joint_plot_sc.data.table`](https://gregorlueg.github.io/bixverse.plots/reference/joint_plot_sc.data.table.md)
for the available methods.

## Usage

``` r
joint_plot_sc(x, ...)
```

## Arguments

- x:

  An object to plot.

- ...:

  Arguments passed to the dispatched method.

## Value

A `ggExtraPlot` object.
