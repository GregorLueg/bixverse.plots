# Scatter plot worker for embeddings

Scatter plot worker for embeddings

## Usage

``` r
.plot_embedding(
  df,
  colour,
  facet = NULL,
  embedding = NULL,
  point_size = 0.3,
  point_alpha = 0.5,
  raster = FALSE,
  raster_dpi = c(512, 512),
  highlight = FALSE,
  highlight_quantile = 0.25
)
```

## Arguments

- df:

  data.table. Must contain `dim_1`, `dim_2` and `colour`.

- colour:

  Character. Column to colour by. A factor/character/logical column
  yields a discrete scale, a numeric column a continuous one.

- facet:

  Character. Optional column to facet by (default: NULL).

- embedding:

  Character. Embedding name for axis labels (default: NULL).

- point_size:

  Numeric. Point size (default: 0.5).

- point_alpha:

  Numeric. Alpha parameter (default: 0.5).

- raster:

  Boolean. Shall
  [`scattermore::geom_scattermore()`](https://rdrr.io/pkg/scattermore/man/geom_scattermore.html)
  be used.

- raster_dpi:

  Two numerics. Pixel resolution for rasterized plots, passed to
  geom_scattermore(). Default is `c(512, 512)`.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.
