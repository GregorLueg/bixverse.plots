# Generate adjusted scores for plotting mitch results

Function that takes results from
[`bixverse::calc_mitch()`](https://rdrr.io/pkg/bixverse/man/calc_mitch.html),
extracts the individual scores and p-values for each contrast, runs an
FDR correction on top of the p-values and sets scores above the provided
FDR threshold to 0.

## Usage

``` r
prepare_mitch_scores(res, fdr_threshold = 0.05)
```

## Arguments

- res:

  data.table. Output of
  [`bixverse::calc_mitch()`](https://rdrr.io/pkg/bixverse/man/calc_mitch.html).

- fdr_threshold:

  Numeric. The FDR threshold you want to apply.

## Value

A list with the following two elements

- fdr_corrections - FDR-corrected scores as a matrix of the individual
  contrast p-values.

- adj_scores - The extract scores per contrast with scores above the
  individual FDR threshold set to 0.
