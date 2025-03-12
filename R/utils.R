#' @title Miscellaneous utilities
#' @name Utilities
#'
#' @param items A [LaTeX2] object or list of items, or
#' a [LaTeX2item] which is a list.
#' @param which Which items to operate on.
#' @returns `drop_items()` returns the list of items with specific items removed.
#' @export
drop_items <- function(items, which) {
  saveattr <- attributes(items)
  if (length(which))
    items <- items[-which]
  attributes(items) <- saveattr
  if (inherits(items, "LaTeX2item"))
    items
  else
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
#' @returns `drop_whitespace()` returns the items with
#'  whitespace (blanks, tabs, newlines) removed.

#' @note `drop_whitespace()` will drop the whitespace that separates text items, so deparsing will merge
#' them into a single item.
#'
#' @seealso `drop_whitespace()` does not act recursively; use [reduce_whitespace] for that.
#' @export
drop_whitespace <- function(items)
  drop_items(items, find_whitespace(items))

#' @rdname Utilities
#'
#' @returns `trim_whitespace()` returns the items with
#'  leading and trailing whitespace (blanks, tabs, newlines) removed.
#' @export
trim_whitespace <- function(items) {
  saveattr <- attributes(items)
  while(length(items) && is_whitespace(items[[length(items)]]))
    items <- items[-length(items)]
  while(length(items) && is_whitespace(items[[1]]))
    items <- items[-1]
  attributes(items) <- saveattr
  if (inherits(items, "LaTeX2item"))
    items
  else
    as_LaTeX2(items)
}

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
#' a [LaTeX2] object.
#' @export
split_latex <- function(items, splits) {
  lapply(split_list(items, splits), as_LaTeX2)
}

#' Convenience functions to get or set contents of item
#'
#' @param item An item from a Latex list (or a [LaTeX2] list with one item).
#'
#' @returns `get_contents` returns the contents of the item as a [LaTeX2] list.
#' @export
#' @examples
#' get_contents(parseLatex("{abc}"))
#'
get_contents <- function(item) {

  if (inherits(item, "LaTeX2")) {
    if (length(item) > 1)
      warning("only using the first element")
    item <- item[[1]]
  }
  if (is.list(item))
    as_LaTeX2(unclass(item))
  else
    NULL
}

#' @rdname get_contents
#' @param value An object that can be coerced to be
#' a [LaTeX2] object.
#'
#' @returns `set_contents` returns the original `item` with the contents
#' replaced by `value`.
#' @export
#'
#' @examples
#' set_contents(parseLatex("{abc}"), "def")
set_contents <- function(item, value) {
  if (!inherits(item, "LaTeX2item") && is.list(item)) {
    if (length(item) > 1)
      warning("only using the first element")
    item <- item[[1]]
  }
  stopifnot(inherits(item, "LaTeX2item") && is.list(item))
  value <- as_LaTeX2(value)
  range <- LaTeX2range(NULL, NULL)
  set_range(item, range, value)
}

#' @rdname Utilities
#' @param ... Items to be passed to `latex2()`.
#' @returns `new_block()` returns a `BLOCK` item containing the items.
#' @export
#'
#' @examples
#' new_block(parseLatex("abc"))
new_block <- function(...) {
  items <- latex2(...)
  attr(items, "latex_tag") <- "BLOCK"
  class(items) <- "LaTeX2item"
  items
}

#' @rdname Utilities
#' @param name The desired environment name.
#' @returns `new_env()` returns an environment item containing the other items.
#' @export
#'
#' @examples
#' new_env("itemize", parseLatex("\\item An item"))
new_env <- function(name, ...) {
  result <- latex2(...)
  if (length(result) == 0 ||
      !is_char(result[[1]], "\n"))
    insert_values(result, 1, "\n")
  n <- length(result)
  if (!is_char(result[[n]], "\n"))
    insert_values(result, n+1, "\n")
  attr(result, "latex_tag") <- "ENVIRONMENT"
  class(result) <- "LaTeX2item"
  envName(result) <- name
  result
}
