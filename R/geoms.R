# internal geom implementations ------------------------------------------------

## helpers ---------------------------------------------------------------------

#' @title Single-cell colour palette function for discrete scales
#'
#' @description Uses the MetBrewer "Austria" palette, auto-detecting the
#' required number of colours from the data.
#'
#' @param n Integer. Number of groups found.
#'
#' @keywords internal
.palette_austria <- function(n) {
  MetBrewer::met.brewer("Austria", n)
}

#' @title Colour vector for continuous single-cell scales
#'
#' @description Runs from light grey into the blue shades of MetBrewer
#' "Cassatt1".
#'
#' @keywords internal
.colours_continuous_sc <- c(
  "lightgrey",
  MetBrewer::met.brewer("Cassatt1", 10)[6:10]
)

## geoms -----------------------------------------------------------------------

### colours --------------------------------------------------------------------

#' Single-cell colour scale
#'
#' Dispatches to a discrete scale backed by the MetBrewer "Austria" palette
#' when \code{discrete = TRUE}, or to a continuous gradient from light grey
#' into the blue shades of MetBrewer "Cassatt1" when \code{discrete = FALSE}.
#'
#' @param discrete Logical. \code{TRUE} for categorical data, \code{FALSE} for
#' continuous.
#' @param ... Additional arguments passed to
#' \code{\link[ggplot2]{discrete_scale}} or
#' \code{\link[ggplot2]{scale_colour_gradientn}}.
#'
#' @return A \code{\link[ggplot2]{Scale}} object.
#'
#' @keywords internal
#'
#' @export
scale_colour_single_cell <- function(discrete, ...) {
  if (discrete) {
    ggplot2::discrete_scale(
      "colour",
      "austria",
      palette = .palette_austria,
      ...
    )
  } else {
    ggplot2::scale_colour_gradientn(colours = .colours_continuous_sc, ...)
  }
}

#' Single-cell fill scale
#'
#' Dispatches to a discrete scale backed by the MetBrewer "Austria" palette
#' when \code{discrete = TRUE}, or to a continuous gradient from light grey
#' into the blue shades of MetBrewer "Cassatt1" when \code{discrete = FALSE}.
#'
#' @param discrete Logical. \code{TRUE} for categorical data, \code{FALSE} for
#' continuous.
#' @param ... Additional arguments passed to
#' \code{\link[ggplot2]{discrete_scale}} or
#' \code{\link[ggplot2]{scale_fill_gradientn}}.
#'
#' @return A \code{\link[ggplot2]{Scale}} object.
#'
#' @keywords internal
#'
#' @export
scale_fill_single_cell <- function(discrete, ...) {
  if (discrete) {
    ggplot2::discrete_scale("fill", "austria", palette = .palette_austria, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = .colours_continuous_sc, ...)
  }
}
