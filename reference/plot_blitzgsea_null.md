# Plot the blitzGSEA calibration

Diagnostic for a `BlitzGseaNull`. Shows the smoothed gamma parameters
against anchor set size for both tails, plus the fraction of positive
null scores. A fit that sags at one end of the size range is obvious
here and invisible in the results table.

The KS goodness-of-fit p-values are in the subtitle. Low values mean the
gamma tails do not describe the permutation scores well and the p-values
are optimistic. More permutations is the usual fix.

## Usage

``` r
plot_blitzgsea_null(null_model, line_size = 0.8, point_size = 1.2)
```

## Arguments

- null_model:

  `BlitzGseaNull` object from
  [`bixverse::blitzgsea_calibrate()`](https://gregorlueg.github.io/bixverse/reference/blitzgsea_calibrate.html).

- line_size:

  Numeric. Line width. Defaults to `0.8`.

- point_size:

  Numeric. Point size for the anchor knots. Defaults to `1.2`.

## Value

A `ggplot` object, facetted over the parameter type.

## References

Lachmann, et al., Bioinformatics, 2022
