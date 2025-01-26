#' @title Miscellaneous utilities
#' @rdname Utilities
#' @name utils

#' @param items A LaTeX2 object or list of items.
#' @param which Which items to operate on.
#' @returns `drop_items()` returns the list of items with specific items removed.
#' @export
drop_items <- function(items, which) {
  if (length(which))
    items <- items[-which]
  as_LaTeX2(items)
}

#' @rdname Utilities
#'
#' @returns `select_items()` returns the list of subsetted items.
#' @export
select_items <- function(items, which) {
  as_LaTeX2(items[which])
}

#' @rdname Utilities
#'
#' @returns `drop_whitespace()` returns the list of items with whitespace (blanks, tabs, newlines) removed.
#' @export
drop_whitespace <- function(items)
  drop_items(items, find_whitespace(items))

#' @rdname Utilities
#'
#' @returns `include_whitespace()` returns `which` with
#' following whitespace (blanks, tabs, newlines) included.
#' @export
include_whitespace <- function(items, which) {
  whitespace <- find_whitespace(items)
  repeat {
    following <- setdiff(intersect(whitespace, which + 1),
                         which)
    if (!length(following))
      return(sort(which))
    which <- c(which, following)
  }
}

#' @rdname Utilities
#' @param splits Which items divide the parts?
#' @returns `split_list()` returns a list of pieces
#' separated at the splits.
#' @export
split_list <- function(items, splits) {
  prev <- 0
  result <- lapply(splits, function(s) {
    if (s > prev + 1) {
      sel <- items[(prev + 1):(s - 1)]
      prev <<- s
      sel
    } else
      items[FALSE]
  })
  # Add in one more after the last split
  extras <- items[seq_along(items) > max(splits)]
  result <- c(result, list(extras))
  result
}

#' @rdname Utilities
#' @param splits Which items divide the parts?
#' @returns `split_latex()` returns a list of pieces
#' separated at the splits.  Each piece is marked as
#' a LaTeX2 object.
#' @export
split_latex <- function(items, splits) {
  lapply(split_list(items, splits), as_LaTeX2)
}

#' Convenience function to get contents from an item
#'
#' @param item An item from a Latex list (or a list with one item).
#'
#' @returns `get_contents` returns the contents of the item as a Latex list, or as a
#' character string.
#' @export
#' @examples
#' get_contents(parseLatex("{abc}"))
#'
get_contents <- function(item) {
  tag <- latexTag(item)
  if (is.null(tag) && is.list(item)) {
    if (length(item) > 1)
      warning("only using the first element")
    item <- item[[1]]
    tag <- latexTag(item)
  }
  if (is.null(tag))
    NULL
  else if (tag == "ENVIRONMENT")
    as_LaTeX2(item[[2]])
  else if (tag == "BLOCK")
    as_LaTeX2(item)
  else
    as.character(item)
}
