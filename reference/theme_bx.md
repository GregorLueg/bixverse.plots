# Bixverse ggplot2 Theme

A custom theme for the bixverse ecosystem based on the
[`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)

## Usage

``` r
theme_bx(
  base_size = 12,
  base_family = "Helvetica",
  base_line_size = 0.5,
  base_rect_size = 0.5
)
```

## Arguments

- base_size:

  Base font size (default: 12)

- base_family:

  Base font family (default: "Helvetica")

- base_line_size:

  Base line size (default: 0.5)

- base_rect_size:

  Base rectangle size (default: 0.5)

## Value

A ggplot2 theme object

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  theme_bx()
} # }
```
