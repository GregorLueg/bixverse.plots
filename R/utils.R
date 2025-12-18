# string utils -----------------------------------------------------------------

#' Helper function to wrap and truncate text
#'
#' @param text String. The string to truncate.
#' @param width Integer. Maximum width of a given line.
#' @param max_lines Integer. Maximum lines before truncating.
#' @param whitespace_only Boolean. Shall the string wrapping happen only around
#' whitespaces.
#'
#' @returns The string wrapped and/or truncated.
#'
#' @export
wrap_and_truncate <- function(
  text,
  width = 40L,
  max_lines = 2L,
  whitespace_only = TRUE
) {
  # checks
  checkmate::qassert(text, "S1")
  checkmate::qassert(width, "I1")
  checkmate::qassert(max_lines, "I1")
  checkmate::qassert(whitespace_only, "B1")

  # wrap the lines and truncate if need be
  wrapped <- stringr::str_wrap(
    text,
    width = width,
    whitespace_only = whitespace_only
  )
  lines <- stringr::str_split(wrapped, "\n")[[1]]

  if (length(lines) > max_lines) {
    lines <- lines[1:max_lines]
    lines[max_lines] <- paste0(substr(lines[max_lines], 1, width - 3), "...")
  }

  paste(lines, collapse = "\n")
}
