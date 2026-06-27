# param wrappers ---------------------------------------------------------------

## general plot params ---------------------------------------------------------

#' Wrapper function for standard plot parameters
#'
#' @param width Float. Width of the plot.
#' @param height Float. Height of the plot.
#' @param file_type String. One of `c(".png", "pdf")`. Plot type to save. Might
#' be expanded to other file types. Defaults to `".png"`
#' @param unit String. One of `c("in", "px", "cm")`. Unit type for `width` and
#' `height`. Defaults to `"in"`.
#' @param res Integer. Resolution for PNGs.
#' @param create_dir Boolean. Shall the plot directory be generated recursively.
#'
#' @returns A list with the parameters for usage in subsequent functions.
#'
#' @export
params_plots <- function(
  width = 5,
  height = 5,
  file_type = c(".png", ".pdf"),
  unit = c("in", "px", "cm"),
  res = 450L,
  create_dir = TRUE
) {
  # defaults
  file_type <- match.arg(file_type)
  unit <- match.arg(unit)

  # checks
  checkmate::qassert(width, "N1")
  checkmate::qassert(height, "N1")
  checkmate::qassert(res, "I1")
  checkmate::assertChoice(file_type, c(".png", ".pdf"))
  checkmate::assertChoice(unit, c("in", "px", "cm"))
  checkmate::qassert(create_dir, "B1")

  # return
  return(list(
    width = width,
    height = height,
    file_type = file_type,
    unit = unit,
    res = res,
    create_dir = create_dir
  ))
}

## volcano plot params ---------------------------------------------------------

#' Wrapper function for volcano plot parameters
#'
#' @param x_axis String. Column holding the effect size (e.g. `"log2FC"`).
#' @param y_axis String. Column holding the raw significance values (e.g.
#' `"FDR"`, `"fdr"`, `"q_value"`). The function applies `-log10()` internally.
#' @param colour String or `NULL`. Column to colour points by (continuous
#' gradient). If `NULL`, points are coloured by `x_axis`. Defaults to `NULL`.
#' @param label_column String or `NULL`. Column holding feature labels. Required
#' if `top_features_to_label` is set. Defaults to `NULL`.
#' @param top_features_to_label Integer or `NULL`. Number of features to label,
#' ranked by `y_axis` ascending (most significant first). Defaults to `NULL`.
#'
#' @returns A list with the parameters for usage in subsequent functions.
#'
#' @export
params_volcano <- function(
  x_axis = "log2FC",
  y_axis = "FDR",
  colour = NULL,
  label_column = NULL,
  top_features_to_label = NULL
) {
  # checks
  checkmate::qassert(x_axis, "S1")
  checkmate::qassert(y_axis, "S1")
  checkmate::assertString(colour, null.ok = TRUE)
  checkmate::assertString(label_column, null.ok = TRUE)
  checkmate::assertInt(top_features_to_label, lower = 1, null.ok = TRUE)

  # return
  return(list(
    x_axis = x_axis,
    y_axis = y_axis,
    colour = colour,
    label_column = label_column,
    top_features_to_label = top_features_to_label
  ))
}
