# Generic violin plot function

Dispatches to per-metric violin plots. See
[`violin_plot_sc.CellQc`](https://gregorlueg.github.io/bixverse.plots/reference/violin_plot_sc.CellQc.md)
and
[`violin_plot_sc.data.table`](https://gregorlueg.github.io/bixverse.plots/reference/violin_plot_sc.data.table.md)
for the available methods.

## Usage

``` r
violin_plot_sc(x, ...)
```

## Arguments

- x:

  An object to plot.

- ...:

  Arguments passed to the dispatched method.

## Value

A ggplot object, or a named list of ggplot objects (one per metric) for
a `CellQc` object.
