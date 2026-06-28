# Check volcano plot parameters

Checkmate extension for checking the volcano plot parameters. If `dt` is
supplied, also verifies that the referenced columns exist in it.

## Usage

``` r
checkVolcanoParams(x, dt = NULL)
```

## Arguments

- x:

  The list to check/assert.

- dt:

  Optional data.table/data.frame to cross-check column names against.

## Value

`TRUE` if the check was successful, otherwise an error message.
