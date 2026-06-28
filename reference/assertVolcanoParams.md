# Assert volcano plot parameters

Checkmate extension for asserting the volcano plot parameters.

## Usage

``` r
assertVolcanoParams(x, dt = NULL, .var.name = checkmate::vname(x), add = NULL)
```

## Arguments

- x:

  The list to check/assert.

- dt:

  Optional data.table/data.frame to cross-check column names against.

- .var.name:

  Name of the checked object to print in assertions. Defaults to the
  heuristic implemented in checkmate.

- add:

  Collection to store assertion messages. See
  [`checkmate::makeAssertCollection()`](https://mllg.github.io/checkmate/reference/AssertCollection.html).

## Value

Invisibly returns the checked object if the assertion is successful.
