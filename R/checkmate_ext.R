# checks -----------------------------------------------------------------------

## blitzgsea null --------------------------------------------------------------

#' Check a blitzGSEA null model
#'
#' @description Checkmate extension for checking a `BlitzGseaNull` object, i.e.
#' the output of [bixverse::blitzgsea_calibrate()].
#'
#' @param x The object to check/assert.
#'
#' @return \code{TRUE} if the check was successful, otherwise an error message.
#'
#' @keywords internal
checkBlitzGseaNull <- function(x) {
  res <- checkmate::checkClass(x, "BlitzGseaNull")
  if (!isTRUE(res)) {
    return(res)
  }

  anchor_params <- c(
    "shape_pos",
    "scale_pos",
    "shape_neg",
    "scale_neg",
    "pos_ratio"
  )
  res <- checkmate::checkNames(
    names(x),
    must.include = c("anchor_sizes", anchor_params, "ks_pos", "ks_neg")
  )
  if (!isTRUE(res)) {
    return(res)
  }

  res <- checkmate::checkNumeric(
    x[["anchor_sizes"]],
    min.len = 2L,
    any.missing = FALSE,
    sorted = TRUE
  )
  if (!isTRUE(res)) {
    return(sprintf("Element `anchor_sizes` does not conform: %s", res))
  }

  # the interpolator reads every parameter vector in lockstep with the anchor
  # grid, so a short one would index out of bounds
  n_anchors <- length(x[["anchor_sizes"]])
  res <- purrr::map_lgl(anchor_params, \(name) {
    isTRUE(checkmate::checkNumeric(
      x[[name]],
      len = n_anchors,
      any.missing = FALSE
    ))
  })
  if (!all(res)) {
    broken_elem <- anchor_params[which(!res)][1]
    return(
      sprintf(
        paste(
          "Element `%s` is not a numeric of length %i, i.e. it is not in",
          "lockstep with the anchor grid."
        ),
        broken_elem,
        n_anchors
      )
    )
  }

  return(TRUE)
}

#' Assert a blitzGSEA null model
#'
#' @description Checkmate extension for asserting a `BlitzGseaNull` object.
#'
#' @inheritParams checkBlitzGseaNull
#'
#' @param .var.name Name of the checked object to print in assertions. Defaults
#' to the heuristic implemented in checkmate.
#' @param add Collection to store assertion messages. See
#' [checkmate::makeAssertCollection()].
#'
#' @return Invisibly returns the checked object if the assertion is successful.
#'
#' @keywords internal
assertBlitzGseaNull <- checkmate::makeAssertionFunction(checkBlitzGseaNull)
