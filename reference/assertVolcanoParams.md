# Assert volcano params

Assert volcano params

## Usage

``` r
assertVolcanoParams(x, dt = NULL, .var.name = checkmate::vname(x), add = NULL)
```

## Arguments

- x:

  The object to check.

- dt:

  Extra context for the check.

- .var.name:

  Name of the checked object to print in assertions.

- add:

  Collection to store assertion messages. See
  [`checkmate::makeAssertCollection()`](https://mllg.github.io/checkmate/reference/AssertCollection.html).

## Value

Invisibly returns the checked object if the assertion is successful.
