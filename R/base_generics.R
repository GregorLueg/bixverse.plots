# generics ---------------------------------------------------------------------

## single cell plot generics ---------------------------------------------------

### qc -------------------------------------------------------------------------

#' Generic violin plot function
#'
#' Dispatches to per-metric violin plots. See
#' \code{\link{violin_plot_sc.CellQc}} and
#' \code{\link{violin_plot_sc.data.table}} for the available methods.
#'
#' @param x An object to plot.
#' @param ... Arguments passed to the dispatched method.
#'
#' @return A ggplot object, or a named list of ggplot objects (one per metric)
#' for a `CellQc` object.
#'
#' @export
#' @keywords internal
violin_plot_sc <- function(x, ...) {
  UseMethod("violin_plot_sc")
}

#' @export
violin_plot_sc.default <- function(x, ...) {
  stop(
    "No violin_plot_sc method for object of class: ",
    paste(class(x), collapse = ", ")
  )
}


#' Generic density plot function
#'
#' Dispatches to per-metric density plots with outlier groups labelled. See
#' \code{\link{density_plot_sc.CellQc}} and
#' \code{\link{density_plot_sc.data.table}} for the available methods.
#'
#' @param x An object to plot.
#' @param ... Arguments passed to the dispatched method.
#'
#' @return A ggplot object, or a named list of ggplot objects (one per metric)
#' for a `CellQc` object.
#'
#' @export
#' @keywords internal
density_plot_sc <- function(x, ...) {
  UseMethod("density_plot_sc")
}

#' @export
density_plot_sc.default <- function(x, ...) {
  stop(
    "No density_plot_sc method for object of class: ",
    paste(class(x), collapse = ", ")
  )
}


#' Generic joint plot function
#'
#' Dispatches to a joint hexbin plot of library size vs feature counts. See
#' \code{\link{joint_plot_sc.CellQc}} and
#' \code{\link{joint_plot_sc.data.table}} for the available methods.
#'
#' @param x An object to plot.
#' @param ... Arguments passed to the dispatched method.
#'
#' @return A \code{ggExtraPlot} object.
#'
#' @export
#' @keywords internal
joint_plot_sc <- function(x, ...) {
  UseMethod("joint_plot_sc")
}

#' @export
joint_plot_sc.default <- function(x, ...) {
  stop(
    "No joint_plot_sc method for object of class: ",
    paste(class(x), collapse = ", ")
  )
}
