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

  Numeric. Width of the plot. Defaults to `5.0`.

- height:

  Numeric. Height of the plot. Defaults to `5.0`.

- file_type:

  String. Plot type to save. Might be expanded to other file types. One
  of `c(".png", ".pdf")`. Defaults to `".png"`.

- unit:

  String. Unit type for `width` and `height`. One of
  `c("in", "px", "cm")`. Defaults to `"in"`.

- res:

  Integer. Resolution for PNGs. Defaults to `450L`.

- create_dir:

  Boolean. Shall the plot directory be generated recursively. Defaults
  to `TRUE`.

## Value

A named list with the following elements:

- width - Numeric. Width of the plot. Defaults to `5.0`.

- height - Numeric. Height of the plot. Defaults to `5.0`.

- file_type - String. Plot type to save. Might be expanded to other file
  types. One of `c(".png", ".pdf")`. Defaults to `".png"`.

- unit - String. Unit type for `width` and `height`. One of
  `c("in", "px", "cm")`. Defaults to `"in"`.

- res - Integer. Resolution for PNGs. Defaults to `450L`.

- create_dir - Boolean. Shall the plot directory be generated
  recursively. Defaults to `TRUE`.
