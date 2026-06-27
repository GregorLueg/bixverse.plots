# checks -----------------------------------------------------------------------

## plot params -----------------------------------------------------------------

#' Check general graph parameters
#'
#' @description Checkmate extension for checking the general plot parameters.
#'
#' @param x The list to check/assert
#'
#' @return \code{TRUE} if the check was successful, otherwise an error message.
#'
#' @keywords internal
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
#'
#' @keywords internal
assertPlotParams <- checkmate::makeAssertionFunction(checkPlotParams)

## volcano plot params ---------------------------------------------------------

#' Check volcano plot parameters
#'
#' @description Checkmate extension for checking the volcano plot parameters. If
#' `dt` is supplied, also verifies that the referenced columns exist in it.
#'
#' @param x The list to check/assert.
#' @param dt Optional data.table/data.frame to cross-check column names against.
#'
#' @return \code{TRUE} if the check was successful, otherwise an error message.
#'
#' @keywords internal
checkVolcanoParams <- function(x, dt = NULL) {
  # Checkmate extension
  res <- checkmate::checkList(x)
  if (!isTRUE(res)) {
    return(res)
  }
  res <- checkmate::checkNames(
    names(x),
    must.include = c(
      "x_axis",
      "y_axis",
      "colour",
      "label_column",
      "top_features_to_label"
    )
  )
  if (!isTRUE(res)) {
    return(res)
  }

  # type rules
  res <- checkmate::checkString(x[["x_axis"]])
  if (!isTRUE(res)) {
    return(res)
  }
  res <- checkmate::checkString(x[["y_axis"]])
  if (!isTRUE(res)) {
    return(res)
  }
  res <- checkmate::checkString(x[["colour"]], null.ok = TRUE)
  if (!isTRUE(res)) {
    return(res)
  }
  res <- checkmate::checkString(x[["label_column"]], null.ok = TRUE)
  if (!isTRUE(res)) {
    return(res)
  }
  res <- checkmate::checkInt(
    x[["top_features_to_label"]],
    lower = 1,
    null.ok = TRUE
  )
  if (!isTRUE(res)) {
    return(res)
  }

  # cross rule: labelling needs a label column
  if (!is.null(x[["top_features_to_label"]]) && is.null(x[["label_column"]])) {
    return("`top_features_to_label` is set but `label_column` is missing.")
  }

  # column existence in dt (NULL entries are dropped by c())
  if (!is.null(dt)) {
    res <- checkmate::checkDataFrame(dt)
    if (!isTRUE(res)) {
      return(res)
    }
    needed <- c(
      x[["x_axis"]],
      x[["y_axis"]],
      x[["colour"]],
      x[["label_column"]]
    )
    res <- checkmate::checkSubset(needed, choices = names(dt))
    if (!isTRUE(res)) {
      return(
        sprintf(
          paste(
            "The following columns are referenced",
            "in the params but missing in dt: %s"
          ),
          paste(setdiff(needed, names(dt)), collapse = ", ")
        )
      )
    }
  }

  return(TRUE)
}

#' Assert volcano plot parameters
#'
#' @description Checkmate extension for asserting the volcano plot parameters.
#'
#' @inheritParams checkVolcanoParams
#'
#' @param .var.name Name of the checked object to print in assertions. Defaults
#' to the heuristic implemented in checkmate.
#' @param add Collection to store assertion messages. See
#' [checkmate::makeAssertCollection()].
#'
#' @return Invisibly returns the checked object if the assertion is successful.
#'
#' @keywords internal
assertVolcanoParams <- checkmate::makeAssertionFunction(checkVolcanoParams)
