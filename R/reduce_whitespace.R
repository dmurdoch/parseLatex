#' Remove excess whitespace recursively
#'
#' @param items A [LaTeX2] object.
#' @param recursive Apply to all lists within `items`.
#' @param all If `TRUE`, remove all white space, not just doubles.
#'
#' @returns `items` with double spaces or double newlines set to single,
#' and trailing spaces removed (or all whitespace removed, if `all` is `TRUE`).
#' @export
#'
#' @examples
#' parsed <- parseLatex("a  {b\n\nc}")
#' parsed
#' reduce_whitespace(parsed)
reduce_whitespace <- function(items, recursive = TRUE,
                              all = FALSE) {
  n <- length(items)
  keep <- rep(TRUE, n)
  for (i in seq_len(n)) {
    if (all && is_whitespace(items[[i]]))
      keep[i] <- FALSE
    else if (i < n && is_whitespace(items[[i]]) && is_whitespace(items[[i + 1]])) {
      if (catcode(items[[i]]) == SPACE ||
        (catcode(items[[i]]) == NEWLINE &&
               catcode(items[[i + 1]]) == NEWLINE))
        keep[i] <- FALSE
    } else if (recursive && is.list(items[[i]]))
      items[[i]] <- reduce_whitespace(items[[i]])
  }
  if (!all(keep)) {
    attrs <- attributes(items)
    items <- items[keep]
    attributes(items) <- attrs
  }
  items
}
