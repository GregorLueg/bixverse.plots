# Plot observed enrichment scores against the blitzGSEA null

For each pathway of interest, draws the fitted null density at that
pathway's size and drops the observed enrichment score on top, with the
tail beyond it shaded. This is where the p-value comes from: blitzGSEA
never permutes per pathway, it reads the score off the gamma tail
interpolated to the set size.

The raw permutation scores never cross the Rust boundary, so this is the
fitted density, not a histogram of null draws.

## Usage

``` r
plot_blitzgsea_es_null(
  null_model,
  res,
  pathways_of_interest,
  n_points = 512L,
  text_size = 3
)
```

## Arguments

- null_model:

  `BlitzGseaNull` object from
  [`bixverse::blitzgsea_calibrate()`](https://gregorlueg.github.io/bixverse/reference/blitzgsea_calibrate.html).
  Has to be the null the results were scored against.

- res:

  data.table. Output of
  [`bixverse::calc_blitzgsea()`](https://gregorlueg.github.io/bixverse/reference/calc_blitzgsea.html).
  Needs the columns `c("pathway_name", "es", "size", "pvals", "fdr")`.

- pathways_of_interest:

  String vector. Names of the pathways to plot. These need to be
  represented in `res$pathway_name`.

- n_points:

  Integer. Resolution of the density curve. Defaults to `512L`.

- text_size:

  Numeric. Size of the annotation text. Defaults to `3`.

## Value

A named list of `ggplot` objects, one per element of
`pathways_of_interest`.

## References

Lachmann, et al., Bioinformatics, 2022
