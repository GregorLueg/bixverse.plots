#' Muna Custom ggplot2 Theme
#'
#' A custom theme for company visualizations with branded colors and typography
#'
#' @param base_size Base font size (default: 12)
#' @param base_family Base font family (default: "Helvetica")
#' @param base_line_size Base line size (default: 0.5)
#' @param base_rect_size Base rectangle size (default: 0.5)
#'
#' @return A ggplot2 theme object
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   theme_bx()
#' }
#'
theme_bx <- function(
  base_size = 12,
  base_family = "Helvetica",
  base_line_size = 0.5,
  base_rect_size = 0.5
) {
  # Start with a base theme (theme_minimal or theme_bw are good starting points)
  theme_minimal(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) +
    theme(
      # Plot background
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),

      # Grid lines
      panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
      panel.grid.minor = element_blank(),

      # Axes
      axis.line = element_line(color = "grey20", linewidth = 0.5),
      axis.ticks = element_line(color = "grey20", linewidth = 0.5),
      axis.text = element_text(color = "grey20", size = base_size * 0.9),
      axis.title = element_text(
        color = "grey20",
        size = base_size,
        face = "bold"
      ),

      # Plot title and subtitle
      plot.title = element_text(
        color = "grey10",
        size = base_size * 1.3,
        face = "bold",
        hjust = 0,
        margin = margin(b = base_size * 0.5)
      ),
      plot.subtitle = element_text(
        color = "grey30",
        size = base_size * 1.1,
        hjust = 0,
        margin = margin(b = base_size * 0.5)
      ),
      plot.caption = element_text(
        color = "grey50",
        size = base_size * 0.8,
        hjust = 1,
        margin = margin(t = base_size * 0.5)
      ),

      # Legend
      legend.background = element_rect(fill = "white", color = NA),
      legend.key = element_rect(fill = "white", color = NA),
      legend.text = element_text(color = "grey20", size = base_size * 0.9),
      legend.title = element_text(
        color = "grey20",
        size = base_size,
        face = "bold"
      ),
      legend.position = "right",

      # Facets
      strip.background = element_rect(fill = "grey95", color = "grey80"),
      strip.text = element_text(
        color = "grey20",
        size = base_size,
        face = "bold",
        margin = margin(t = base_size * 0.3, b = base_size * 0.3)
      )
    )
}

#' Bixverse Color Palette
#'
#' Official bixverse color palette
#'
#' @param palette Palette name: "main", "diverging", "sequential"
#' @param n Integer, number of colours to return.
#' @param reverse Logical, reverse the color order? (default: FALSE)
#'
#' @return A vector of color hex codes
#' @export
#'
#' @keywords internal
#'
bx_colors <- function(palette = "main", reverse = FALSE, n = 20, ...) {
  # Define your company colors
  colors <- list(
    # Main brand colors
    main = MetBrewer::met.brewer("Austria", n)[1:n],

    # SEQUENTIAL PALETTE (9 colors)
    sequential = MetBrewer::met.brewer("Benedictus", 10)[6:10],

    # DIVERGING PALETTE (11 colors)
    diverging = MetBrewer::met.brewer("Hiroshige", 10)[1:10]
  )

  pal <- colors[[palette]]

  if (reverse) {
    pal <- rev(pal)
  }

  return(pal)
}

#' Bixverse Color Scale (Discrete)
#'
#' @param palette Palette name (default: "main")
#' @param reverse Reverse colors? (default: FALSE)
#' @param ... Additional arguments passed to scale_color_manual
#'
#' @return A ggplot2 scale object
#' @export
#'
scale_color_bx <- function(palette = "main", reverse = FALSE, ...) {
  pal_fn <- function(n) bx_colors(palette = palette, reverse = reverse, n = n)

  ggplot2::discrete_scale(
    aesthetics = "color",
    palette = pal_fn,
    ...
  )
}

#' Bixverse Fill Scale (Discrete)
#'
#' @param palette Palette name (default: "main")
#' @param reverse Reverse colors? (default: FALSE)
#' @param ... Additional arguments passed to scale_fill_manual
#'
#' @return A ggplot2 scale object
#' @export
#'
scale_fill_bx <- function(palette = "main", reverse = FALSE, ...) {
  pal_fn <- function(n) bx_colors(palette = palette, reverse = reverse, n = n)

  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = pal_fn,
    ...
  )
}

#' Bixverse Color Scale (Continuous)
#'
#' @param palette Palette name (default: "sequential")
#' @param reverse Reverse colors? (default: FALSE)
#' @param ... Additional arguments passed to scale_color_gradientn
#'
#' @return A ggplot2 scale object
#' @export
#'
scale_color_bx_c <- function(
  palette = "sequential",
  reverse = FALSE,
  ...
) {
  pal <- bx_colors(palette, reverse, ...)
  ggplot2::scale_color_gradientn(colors = pal, ...)
}

#' Bixverse Fill Scale (Continuous)
#'
#' @param palette Palette name (default: "sequential")
#' @param reverse Reverse colors? (default: FALSE)
#' @param ... Additional arguments passed to scale_fill_gradientn
#'
#' @return A ggplot2 scale object
#' @export
#'
scale_fill_bx_c <- function(palette = "sequential", reverse = FALSE, ...) {
  pal <- bx_colors(palette, reverse)
  ggplot2::scale_fill_gradientn(colors = pal, ...)
}

#' Set Bixverse Theme as Default
#'
#' Sets the Bixverse theme as the default for all ggplot2 plots in the session
#'
#' @param base_size Base font size (default: 12)
#' @param base_family Base font family (default: "Arial")
#'
#' @return NULL (sets theme invisibly)
#' @export
#'
#' @keywords internal
#' @examples \dontrun{
#' set_bx_theme()
#' }
#'
set_bx_theme <- function(base_size = 12, base_family = "Helvetica") {
  ggplot2::theme_set(theme_bx(
    base_size = base_size,
    base_family = base_family
  ))
  invisible()
}
