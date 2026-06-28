# Wrapper function for standard plot parameters

Wrapper function for standard plot parameters

## Usage

``` r
params_plots(
  width = 5,
  height = 5,
  file_type = c(".png", ".pdf"),
  unit = c("in", "px", "cm"),
  res = 450L,
  create_dir = TRUE
)
```

## Arguments

- width:

  Float. Width of the plot.

- height:

  Float. Height of the plot.

- file_type:

  String. One of `c(".png", "pdf")`. Plot type to save. Might be
  expanded to other file types. Defaults to `".png"`

- unit:

  String. One of `c("in", "px", "cm")`. Unit type for `width` and
  `height`. Defaults to `"in"`.

- res:

  Integer. Resolution for PNGs.

- create_dir:

  Boolean. Shall the plot directory be generated recursively.

## Value

A list with the parameters for usage in subsequent functions.
