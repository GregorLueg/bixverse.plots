# Interpolate the blitzGSEA gamma parameters at a given gene set size

Mirrors `interp_log_linear_at()` on the Rust side: bracket on the anchor
grid, then interpolate linearly in log space. Note that the bracket
index is clamped to the interior, so values outside the grid are
*extrapolated* along the terminal segment rather than held flat. That is
deliberate, [`stats::approx()`](https://rdrr.io/r/stats/approxfun.html)
with `rule = 2` would clamp instead and quietly disagree with the
p-values the scoring returned.

## Usage

``` r
blitz_tail_at(null_model, size)
```

## Arguments

- null_model:

  `BlitzGseaNull` object from
  [`bixverse::blitzgsea_calibrate()`](https://gregorlueg.github.io/bixverse/reference/blitzgsea_calibrate.html).

- size:

  Numeric. The gene set size to evaluate at.

## Value

A named list with `shape_pos`, `scale_pos`, `shape_neg`, `scale_neg` and
`pos_ratio`, the last one clamped to `[0, 1]`.
