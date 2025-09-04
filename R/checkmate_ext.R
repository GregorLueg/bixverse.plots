# checks -----------------------------------------------------------------------

## plot params -----------------------------------------------------------------

#' Check general graph parameters
#'
#' @description Checkmate extension for checking the general plot parameters.
#'
#' @param x The list to check/assert
#'
#' @return \code{TRUE} if the check was successful, otherwise an error message.
checkPlotParams <- function(x) {
  # Checkmate extension
  res <- checkmate::checkList(x)
  if (!isTRUE(res)) {
    return(res)
  }
  res <- checkmate::checkNames(
    names(x),
    must.include = c("width", "height", "file_type", "unit", "res")
  )
  if (!isTRUE(res)) {
    return(res)
  }

  # general rules
  rules <- list(
    "width" = "N1",
    "height" = "N1",
    "res" = "N1",
    "create_dir" = "B1"
  )
  res <- purrr::imap_lgl(x, \(x, name) {
    if (name %in% names(rules)) {
      checkmate::qtest(x, rules[[name]])
    } else {
      TRUE
    }
  })
  if (!isTRUE(all(res))) {
    broken_elem <- names(res)[which(!res)][1]
    return(
      sprintf(
        paste(
          "The following element `%s` in plot params does not conform to the",
          "expected format. width, height need to be numericals",
          "and res an integer."
        ),
        broken_elem
      )
    )
  }

  # choice rules
  res <- checkmate::checkChoice(
    x[['file_type']],
    c(".png", ".pdf")
  )
  if (!isTRUE(res)) {
    return(res)
  }
  res <- checkmate::checkChoice(
    x[['unit']],
    c("in", "px", "cm")
  )
  if (!isTRUE(res)) {
    return(res)
  }

  return(TRUE)
}

# asserts ----------------------------------------------------------------------

## plot params ----------------------------------------------------------

#' Assert general graph parameters
#'
#' @description Checkmate extension for asserting the general plot parameters.
#'
#' @inheritParams checkPlotParams
#'
#' @param .var.name Name of the checked object to print in assertions. Defaults
#' to the heuristic implemented in checkmate.
#' @param add Collection to store assertion messages. See
#' [checkmate::makeAssertCollection()].
#'
#' @return Invisibly returns the checked object if the assertion is successful.
assertPlotParams <- checkmate::makeAssertionFunction(checkPlotParams)
