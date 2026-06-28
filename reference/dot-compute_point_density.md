# Compute per-point 2D kernel density

Compute per-point 2D kernel density

## Usage

``` r
.compute_point_density(x, y, smoothness = 10)
```

## Arguments

- x:

  Numeric. X coordinates.

- y:

  Numeric. Y coordinates.

- smoothness:

  Numeric. Bandwidth multiplier of the per-axis standard deviation,
  passed to [`MASS::kde2d()`](https://rdrr.io/pkg/MASS/man/kde2d.html).

## Value

Numeric vector of density values, one per `(x, y)` pair.
