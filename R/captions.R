#' Find or drop captions
#'
#' @param items  A [LaTeX2] or other list of [LaTeX2item]s.
#'
#' @returns `find_captions()` returns the indices within the items of any caption
#' text, with an attribute `extra` holding indices of
#' associated  macros and whitespace.
#' @export
#'
#' @examples
#' parsed <- parseLatex("before \\caption{This is a caption} \\\\ after")
#' idx <- find_captions(parsed)
#' get_item(parsed, idx)
#' get_range(parsed, LaTeX2range(NULL, attr(idx, "extra")[[1]]))
find_captions <- function(items) {
  macros <- find_macro(items, "\\caption")
  actual <- integer()
  extra <- as.list(macros)
  for (i in seq_along(macros)) {
    j <- macros[i] + 1
    if (is_char(get_item(items, j), "*")) j <- j + 1 # \caption*
    while (is_whitespace(get_item(items, j))) j <- j + 1
    if (is_bracket(get_item(items, j), "[")) {
      j <- j + 1
      while (!is_bracket(get_item(items, j), "]")) j <- j + 1
    }
    while (is_whitespace(get_item(items, j))) j <- j + 1
    actual <- c(actual, j) # the actual caption, typically a block
    j <- j + 1
    while (is_whitespace(get_item(items, j))) j <- j + 1
    if (is_macro(get_item(items, j), "\\\\")) j <- j + 1
    extra[[i]] <- macros[i]:(j-1)
  }
  structure(actual, extra = extra)
}

#' @rdname find_captions
#' @param idx `NULL` or a vector of the same length as `items`
#' @returns `drop_captions()` returns the `items` with
#' captions dropped as a [LaTeX2] object.  It has an attribute
#' named `idx` that is the `idx` argument with corresponding
#' elements dropped.
#' @export
#'
#' @examples
#' drop_captions(parsed)
drop_captions <- function(items, idx = NULL) {
  caps <- find_captions(items)
  extras <- unlist(attr(caps, "extra"))
  if (length(extras)) {
    items <- items[-extras]
    if (!is.null(idx))
      idx <- idx[-extras]
  }
  structure(latex2(items), idx = idx)
}

#' @rdname find_captions
#' @returns `path_to_caption()` returns a path
#'  containing the location of the first caption
#'  block within `items`.  It has an attribute `idx`
#'  containing a [LaTeX2range] object for the
#'  associated macros and whitespace.
#' @export
#'
#' @examples
#' path_to_caption(parsed)
path_to_caption <- function(items) {
  result <- NULL
  loc <- find_captions(items)
  if (length(loc)) {
    result <- loc[1]
    idx <- attr(loc, "idx")[[1]]
    attr(result, "idx") <- LaTeX2range(NULL, idx)
  } else {
    for (i in seq_along(items))
      if (is.list(items[[i]])) {
        result <- path_to_caption(items[[i]])
        if (length(result)) {
          idx <- attr(result, "idx")
          idx$path <- c(i, idx$path)
          result <- c(i, result)
          attr(result, "idx") <- idx
          break
        }
      }
  }
  result
}
