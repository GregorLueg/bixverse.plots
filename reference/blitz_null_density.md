# Evaluate the blitzGSEA null density

The null is a two-sided mixture: the positive tail carries weight
`pos_ratio`, the negative tail the rest, and each is a gamma on the
absolute enrichment score. Same decomposition the p-value is read off.

## Usage

``` r
blitz_null_density(es, tail)
```

## Arguments

- es:

  Numeric vector. Enrichment scores to evaluate the density at.

- tail:

  List. Output of
  [`blitz_tail_at()`](https://gregorlueg.github.io/bixverse.plots/reference/blitz_tail_at.md).

## Value

Numeric vector of densities, same length as `es`.
