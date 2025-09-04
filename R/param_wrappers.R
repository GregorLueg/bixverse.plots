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
