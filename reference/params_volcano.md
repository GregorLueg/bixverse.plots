# Wrapper function for volcano plot parameters

Wrapper function for volcano plot parameters

## Usage

``` r
params_volcano(
  x_axis = "log2FC",
  y_axis = "FDR",
  colour = NULL,
  label_column = NULL,
  top_features_to_label = NULL
)
```

## Arguments

- x_axis:

  String. Column holding the effect size (e.g. `"log2FC"`).

- y_axis:

  String. Column holding the raw significance values (e.g. `"FDR"`,
  `"fdr"`, `"q_value"`). The function applies `-log10()` internally.

- colour:

  String or `NULL`. Column to colour points by (continuous gradient). If
  `NULL`, points are coloured by `x_axis`. Defaults to `NULL`.

- label_column:

  String or `NULL`. Column holding feature labels. Required if
  `top_features_to_label` is set. Defaults to `NULL`.

- top_features_to_label:

  Integer or `NULL`. Number of features to label, ranked by `y_axis`
  ascending (most significant first). Defaults to `NULL`.

## Value

A list with the parameters for usage in subsequent functions.
