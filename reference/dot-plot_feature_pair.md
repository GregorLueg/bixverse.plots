# Feature-pair scatter / hex plot worker

Feature-pair scatter / hex plot worker

## Usage

``` r
.plot_feature_pair(
  df,
  features = NULL,
  geom = c("density", "hex"),
  smoothness = 10,
  bins = 60,
  point_size = 2.5,
  point_alpha = 0.5,
  raster = FALSE,
  raster_dpi = c(512, 512)
)
```

## Arguments

- df:

  data.table. Must contain `feature_1` and `feature_2`.

- features:

  Optional length-2 character vector with axis labels (default: NULL,
  falls back to column names).

- geom:

  Character. One of `c("density", "hex")`. `"density"` colours each
  point by 2D KDE; `"hex"` bins points into hexagons.

- smoothness:

  Numeric. Bandwidth multiplier for the KDE (default: 10). Only used for
  `geom = "density"`.

- bins:

  Numeric. Number of hex bins (default: 60). Only used for
  `geom = "hex"`.

- point_size:

  Numeric. Point size for density plots (default: 2.5).

- point_alpha:

  Numeric. Alpha for density plots (default: 0.5).

- raster:

  Boolean. Use
  [`scattermore::geom_scattermore()`](https://rdrr.io/pkg/scattermore/man/geom_scattermore.html)
  for the density variant.

- raster_dpi:

  Two numerics. Pixel resolution for rasterised plots.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.
