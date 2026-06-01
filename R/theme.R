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
      ),

      # Panel spacing
      panel.spacing = unit(1, "lines"),

      # # Plot margins
      # plot.margin = margin(
      #   t = base_size * 0.5,
      #   r = base_size * 0.5,
      #   b = base_size * 0.5,
      #   l = base_size * 0.5
      # )
    )
}

#' Bixverse Color Palette
#'
#' Official bixverse color palette
#'
#' @param palette Palette name: "main", "diverging", "sequential"
#' @param reverse Logical, reverse the color order? (default: FALSE)
#'
#' @return A vector of color hex codes
#' @export
#'
#' @keywords internal
#'
bx_colors <- function(palette = "main", reverse = FALSE) {
  # Define your company colors
  colors <- list(
    # Main brand colors
    main = c(
      "#0086ab", #  1  brand teal
      "#c43800", #  2  burnt orange
      "#7200c4", #  3  purple
      "#2d9600", #  4  forest green
      "#003ec4", #  5  royal blue
      "#c49400", #  6  golden
      "#c4006a", #  7  raspberry
      "#009652", #  8  emerald
      "#0066c4", #  9  cobalt
      "#c47200", # 10  amber
      "#b84ec4", # 11  orchid
      "#4ac84a", # 12  lime green
      "#6b46d6", # 13  violet
      "#c4d400", # 14  acid yellow
      "#e04848", # 15  coral red
      "#48c8b8", # 16  aqua
      "#2818b8", # 17  indigo
      "#b8a818", # 18  olive gold
      "#b81870", # 19  hot pink
      "#18b87a" # 20  green teal
    ),

    # SEQUENTIAL PALETTE (9 colors)

    sequential = c(
      "#e4eef0", #  1  lightest
      "#badde7", #  2
      "#84cee4", #  3
      "#42c2e8", #  4
      "#00a8d6", #  5  mid
      "#0086ab", #  6  brand
      "#006b88", #  7
      "#005066", #  8
      "#003344" #  9  darkest
    ),

    # DIVERGING PALETTE (11 colors)
    diverging = c(
      "#84380a", #  1  dark warm
      "#ae5010", #  2
      "#d67228", #  3
      "#e8a06a", #  4
      "#f0ccae", #  5  light warm
      "#f4f4f4", #  6  neutral midpoint
      "#b8dce6", #  7  light cool
      "#72bcd8", #  8
      "#2898c8", #  9
      "#0072a4", # 10
      "#004e72" # 11  dark cool
    )
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
  pal <- bx_colors(palette, reverse)
  ggplot2::scale_color_manual(values = pal, ...)
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
  pal <- bx_colors(palette, reverse)
  ggplot2::scale_fill_manual(values = pal, ...)
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
  pal <- bx_colors(palette, reverse)
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
  ggplot2::theme_set(theme_muna(
    base_size = base_size,
    base_family = base_family
  ))
  invisible()
}
