#' Find or drop captions
#'
#' @param items  A [LaTeX2] or other list of [LaTeX2item]s.
#'
#' @returns `find_caption()` returns a [LaTeX2range] object
#' for any caption
#' text, with an attribute `extra` holding the range of
#' associated  macros and whitespace.
#' @export
#'
#' @examples
#' parsed <- parseLatex("before \\caption{This is a caption} \\\\ after")
#' idx <- find_caption(parsed)
#' get_range(parsed, idx)
#' get_range(parsed, attr(idx, "extra"))
find_caption <- function(items) {
  items0 <- items
  macro <- find_macro(items, "\\caption", all = FALSE, path = TRUE)
  result <- NULL
  if (length(macro) == 0) return(NULL)
  start <- macro[length(macro)]
  path <- macro[-length(macro)]
  if (length(path) > 0)
    items <- items[[path]]
  n <- length(items)
  j <- start + 1L
  if (j > n) return(NULL)
  if (is_char(items[[j]], "*")) j <- j + 1L # \caption*
  if (j > n) return(NULL)
  block <- find_brace_options(items, start = j)
  if (length(block) > 0L) {
    result <- LaTeX2range(c(path, block), NULL)
    j <- block + 1L
    while(j < n && (is_whitespace(items[[j]]))) j <- j + 1L
    if (j < n && is_macro(items[[j]], "\\\\")) j < j + 1L
    attr(result, "extra") <- LaTeX2range(path, start:j)
  }
  result
}

#' @rdname find_caption
#' @param idx `NULL` or a vector of the same length as `items`
#' @returns `drop_caption()` returns the `items` with
#' captions dropped as a [LaTeX2] object.  It has an attribute
#' named `idx` that is the `idx` argument with corresponding
#' elements dropped.
#' @export
#'
#' @examples
#' drop_caption(parsed)
drop_caption <- function(items, idx = NULL) {
  has_itemlist <- has_itemlist(items)
  caps <- find_caption(items)
  extras <- unlist(attr(caps, "extra"))
  if (length(extras)) {
    items <- items[-extras]
    if (!is.null(idx))
      idx <- idx[-extras]
  }
  structure(latex2(items), idx = idx, has_itemlist = has_itemlist)
}

#' @rdname find_caption
#' @returns `path_to_caption()` returns a path
#'  containing the location of the first caption
#'  block within `items`.  It has an attribute `extra`
#'  containing a [LaTeX2range] object for the
#'  associated macros and whitespace.
#' @export
#'
#' @examples
#' path_to_caption(parsed)
path_to_caption <- function(items) {
  result <- NULL
  loc <- find_caption(items)
  if (length(loc))
    result <- structure(loc$path, extra = attr(loc, "extra"))
  else {
    for (i in seq_along(items))
      if (is.list(items[[i]])) {
        result <- path_to_caption(items[[i]])
        if (length(result)) {
          extra <- attr(result, "extra")
          extra$path <- c(i, extra$path)
          result <- c(i, result)
          attr(result, "extra") <- extra
          break
        }
      }
  }
  result
}
