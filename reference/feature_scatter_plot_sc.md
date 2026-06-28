# Scatter / hex plot of two features against each other

Plots two features against each other (typical use case: protein vs mRNA
on ADT/RNA data). Each feature may carry a `_rna` or `_adt` suffix to
select its modality independently; unsuffixed features fall back to
`modality`. With `geom = "density"` points are coloured by 2D KDE, with
`geom = "hex"` cells are binned into hexagons.

## Usage

``` r
feature_scatter_plot_sc(
  object,
  feature_1,
  feature_2,
  geom = c("density", "hex"),
  remove_zeros = TRUE,
  smoothness = 10,
  bins = 60,
  modality = c("rna", "adt"),
  point_size = 2.5,
  point_alpha = 0.5,
  raster = NULL,
  raster_dpi = c(512, 512)
)
```

## Arguments

- object:

  A single cell class.

- feature_1:

  String. First feature (x-axis), optionally `_rna` / `_adt` suffixed.

- feature_2:

  String. Second feature (y-axis), optionally `_rna` / `_adt` suffixed.

- geom:

  String. `"density"` or `"hex"` (default: `"density"`).

- remove_zeros:

  Boolean. Drop cells where both features are zero (default: TRUE).

- smoothness:

  Numeric. Bandwidth multiplier for the KDE (default: 10). Only used for
  `geom = "density"`.

- bins:

  Numeric. Number of hex bins (default: 60). Only used for
  `geom = "hex"`.

- modality:

  String. Fallback modality for unsuffixed features. One of
  `c("rna", "adt")`.

- point_size:

  Numeric. Point size for density plots (default: 2.5).

- point_alpha:

  Numeric. Alpha for density plots (default: 0.5).

- raster:

  Optional boolean. Shall the plot be rasterised. If `NULL` and
  `n_cells > 1e5`, defaults to TRUE. Only applies to `geom = "density"`.

- raster_dpi:

  Two numerics. Pixel resolution for rasterised plots (default:
  `c(512, 512)`).

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.
