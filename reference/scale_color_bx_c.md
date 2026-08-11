# Bixverse Color Scale (Continuous)

Bixverse Color Scale (Continuous)

## Usage

``` r
scale_color_bx_c(palette = "sequential", reverse = FALSE, n = 20, ...)
```

## Arguments

- palette:

  Palette name (default: "sequential")

- reverse:

  Reverse colors? (default: FALSE)

- n:

  Integer. Number of colours to interpolate over (default: 20), see
  [`bx_colors()`](https://gregorlueg.github.io/bixverse.plots/reference/bx_colors.md).

- ...:

  Additional arguments passed to scale_color_gradientn

## Value

A ggplot2 scale object
