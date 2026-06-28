# Helper function to wrap and truncate text

Helper function to wrap and truncate text

## Usage

``` r
wrap_and_truncate(text, width = 40L, max_lines = 2L, whitespace_only = TRUE)
```

## Arguments

- text:

  String. The string to truncate.

- width:

  Integer. Maximum width of a given line.

- max_lines:

  Integer. Maximum lines before truncating.

- whitespace_only:

  Boolean. Shall the string wrapping happen only around whitespaces.

## Value

The string wrapped and/or truncated.
