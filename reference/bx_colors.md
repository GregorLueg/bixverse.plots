# Bixverse Color Palette

Official bixverse color palette

## Usage

``` r
bx_colors(palette = "main", reverse = FALSE, n = 20, ...)
```

## Arguments

- palette:

  Palette name. One of `"main"`, `"sequential"`, `"diverging"`,
  `"viridis"` or `"spectral"`. The latter is a reversed RColorBrewer
  Spectral, i.e. dark indigo for low and dark red for high values.

- reverse:

  Logical, reverse the color order? (default: FALSE)

- n:

  Integer, number of colours to return. Palettes shorter than `n`
  (`"sequential"`, `"diverging"`, `"spectral"`) are ramped up to `n`
  colours with
  [`grDevices::colorRampPalette()`](https://rdrr.io/r/grDevices/colorRamp.html).

- ...:

  Ignored. Present for backwards compatibility.

## Value

A vector of color hex codes
