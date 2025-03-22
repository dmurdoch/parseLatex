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
#' parsed[idx]
#' parsed[attr(idx, "extra")[[1]]]
find_captions <- function(items) {
  nextitem <- function(j) {
    j[length(j)] <- j[length(j)] + 1
    j
  }
  previtem <- function(j) {
    j[length[j]] <- j[length(j)] - 1
    j
  }
  macros <- find_macro(items, "\\caption")
  actual <- list()
  extra <- lapply(macros, path_to_range)
  for (i in seq_along(macros)) {
    j <- nextitem(macros[[i]])
    if (is_char(items[[j]], "*")) j <- nextitem(j) # \caption*
    while (is_whitespace(items[[j]])) j <- nextitem(j)
    if (is_bracket(items[[j]], "[")) {
      j <- nextitem(j)
      while (!is_bracket(items[[j]], "]")) j <- nextitem(j)
    }
    while (is_whitespace(items[[j]])) j <- nextitem(j)
    actual <- c(actual, list(j)) # the actual caption, typically a block
    j <- nextitem(j)
    while (is_whitespace(items[[j]])) j <- nextitem(j)
    if (is_macro(items[[j]], "\\\\")) j <- nextitem(j)
    extra[[i]] <- extend_range(extra[[i]], path_to_range(previtem(j)))
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
